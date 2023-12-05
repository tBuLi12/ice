use std::collections::HashMap;

use ast::{
    BindPattern, BindingType, BlockItem, Expr, Ident, Module, NarrowTypePattern, Pattern,
    PatternBody, PropsTy, Spanned,
};
use iiv::{
    builder::BlockRef,
    diagnostics, err,
    fun::{Function, Signature},
    pool::{FuncRef, List},
    str::Str,
    ty::{PropRef, Type, TypeOverlap, TypeRef},
    Ctx, Package, Span, Value,
};

mod ty;

pub struct Generator<'i> {
    global_scope: HashMap<Str<'i>, Object<'i>>,
    ty: &'i iiv::ty::Pool<'i>,
    fun_pool: &'i iiv::pool::FunPool<'i>,
    messages: &'i iiv::diagnostics::Diagnostics,
}

pub struct FunctionGenerator<'i, 'g> {
    global_scope: &'g HashMap<Str<'i>, Object<'i>>,
    scopes: Vec<HashMap<Str<'i>, Object<'i>>>,
    ty: &'i iiv::ty::Pool<'i>,
    messages: &'i iiv::diagnostics::Diagnostics,
    constant_depth: usize,
    ret_type: Option<TypeRef<'i>>,
    iiv: iiv::builder::FunctionBuilder<'i>,
}

impl<'i> Generator<'i> {
    pub fn new(ctx: &'i Ctx<'i>) -> Self {
        Generator {
            global_scope: HashMap::new(),
            ty: &ctx.type_pool,
            fun_pool: &ctx.fun_pool,
            messages: &ctx.diagnostcs,
        }
    }

    pub fn emit_iiv(&mut self, modules: &[Module<'i>]) -> Package<'i> {
        self.global_scope
            .insert(self.ty.str_pool.get("int"), self.ty.get_int().obj());
        self.global_scope
            .insert(self.ty.str_pool.get("bool"), self.ty.get_ty_bool().obj());

        let funcs = &modules[0].functions;

        let mut package_funs = vec![];
        for fun_node in funcs {
            let mut fun_gen = FunctionGenerator::new(&self);

            let fun = self.fun_pool.insert(Function {
                sig: Signature {
                    name: fun_node.signature.name.value,
                    params: self.ty.get_ty_list(
                        fun_node
                            .signature
                            .params
                            .iter()
                            .map(|param| fun_gen.check_type(&param.ty))
                            .collect(),
                    ),
                    ret_ty: fun_node
                        .signature
                        .return_ty
                        .as_ref()
                        .map(|expr| fun_gen.check_type(&expr))
                        .unwrap_or_else(|| self.ty.get_null()),
                },
                body: vec![],
            });

            let name = fun_node.signature.name.value;

            self.global_scope
                .insert(fun_node.signature.name.value, Object::Fun(fun));
            package_funs.push(fun);
        }

        for (&fun, fun_node) in package_funs.iter().zip(funcs) {
            let gen = FunctionGenerator::new(&self);
            gen.emit_function_body(&mut *fun.borrow_mut(), fun_node);
        }

        let main =
            if let Some(Object::Fun(main)) = self.global_scope.get(&self.ty.str_pool.get("main")) {
                Some(*main)
            } else {
                None
            };

        Package {
            funcs: package_funs,
            main,
        }
    }
}

impl<'i, 'g> FunctionGenerator<'i, 'g> {
    pub fn new(parent: &'g Generator<'i>) -> Self {
        Self {
            global_scope: &parent.global_scope,
            scopes: Vec::new(),
            ty: &parent.ty,
            messages: &parent.messages,
            constant_depth: 0,
            ret_type: None,
            iiv: iiv::builder::FunctionBuilder::new(parent.ty),
        }
    }

    fn emit_function_body(
        mut self,
        fun: &mut iiv::fun::Function<'i>,
        fun_node: &ast::Function<'i>,
    ) {
        self.scopes.push(HashMap::new());

        for (&ty, param) in fun.sig.params.iter().zip(&fun_node.signature.params) {
            self.scopes
                .last_mut()
                .unwrap()
                .insert(param.name.value, Object::Value(self.iiv.param(ty)));
        }

        self.ret_type = Some(fun.sig.ret_ty);

        let body = self.check_val(&fun_node.body);
        let body = self.ensure_ty(&fun_node.body.span(), fun.sig.ret_ty, body);
        self.iiv.ret(body);
        fun.body = self.iiv.build();
    }

    fn check_val(&mut self, expr: &Expr<'i>) -> Value<'i> {
        let obj = self.check(expr, None);
        self.get_val(&expr.span(), obj)
    }

    fn in_const_ctx<T>(&mut self, fun: impl FnOnce(&mut Self) -> T) -> T {
        self.constant_depth += 1;
        let res = fun(self);
        self.constant_depth -= 1;
        res
    }

    fn check_type(&mut self, expr: &Expr<'i>) -> TypeRef<'i> {
        self.in_const_ctx(|g| {
            let obj = g.check(expr, None);
            g.get_type(&expr.span(), obj)
        })
    }

    fn msg(&mut self, message: diagnostics::Diagnostic) {
        println!("adding error");
        self.messages.add(message);
    }

    fn ensure_ty(&mut self, span: &Span, ty: TypeRef<'i>, value: Value<'i>) -> Value<'i> {
        if value.ty != ty {
            match (&*value.ty, &*ty) {
                (Type::Variant(props1), Type::Variant(props2)) => {
                    if props1
                        .iter()
                        .all(|prop| props2.iter().find(|prop2| prop == *prop2).is_some())
                    {
                        return self.iiv.variant_cast(ty, value);
                    };
                }
                _ => {
                    self.msg(err!(span, "expected type {}, got {} instead", ty, value.ty));
                }
            }
        }
        Value { raw: value.raw, ty }
    }

    fn make_type_union(&mut self, span: &Span, types: &[TypeRef<'i>]) -> TypeRef<'i> {
        let mut unqiue = vec![];

        'outer: for &ty in types {
            for &member in unqiue.iter() {
                match dbg!(ty.intersects(member)) {
                    TypeOverlap::Complete => continue 'outer,
                    TypeOverlap::Partial => {
                        self.msg(err!(
                            span,
                            "union members {} and {} may refer to the same type",
                            member,
                            ty
                        ));
                    }
                    TypeOverlap::None => {}
                }
            }
            unqiue.push(ty);
        }

        dbg!(&unqiue);
        if let [only] = unqiue[..] {
            return only;
        }
        self.ty.get_union(unqiue)
    }

    // fn bind(&mut self, pattern: &Pattern, value: Object) {
    //     match (pattern.body, value) {
    //         (PatternBody::Bind(BindingType::Let, ident), value) => {
    //             self.read_rt_value(value);
    //         }
    //         (PatternBody::Bind(BindingType::Var, ident), value) => {
    //             self.read_rt_value(value);
    //         }
    //         (PatternBody::Bind(BindingType::Const, ident), value) => {
    //             self.read_ct_value(value);
    //         }
    //         _ => panic!(),
    //     }
    // }

    fn null(&self) -> Value<'i> {
        Value {
            raw: iiv::RawValue::NULL,
            ty: self.ty.get_null(),
        }
    }

    fn never(&self) -> Value<'i> {
        Value {
            raw: iiv::RawValue::NULL,
            ty: self.ty.get_ty_never(),
        }
    }

    fn invalid(&self) -> Value<'i> {
        Value {
            raw: iiv::RawValue::NULL,
            ty: self.ty.get_ty_invalid(),
        }
    }

    fn bind(&mut self, pattern: &Pattern<'i>, value: Value<'i>, no_match_block: Option<BlockRef>) {
        dbg!("BINDING");
        dbg!(value);
        match &pattern.body {
            PatternBody::Bind(BindPattern { binding_type, name }) => match binding_type {
                BindingType::Let => self.define(&name, Object::Value(value)),
                BindingType::Var => self.define(&name, Object::Place(value, vec![])),
                _ => unimplemented!(),
            },
            PatternBody::NarrowType(NarrowTypePattern { inner, ty }) => {
                let target_ty = self.check_type(ty);
                let narrowed = self.ensure_ty(&ty.span(), target_ty, value);
                self.bind(&inner, narrowed, no_match_block);
            }
            PatternBody::Variant(vairant) => {
                dbg!("B VARIANT");
                if let Type::Variant(elems) = *value.ty {
                    let i = elems.iter().enumerate().find_map(|(i, elem)| {
                        if elem.0 == vairant.name.value {
                            Some(i)
                        } else {
                            None
                        }
                    });
                    if let Some(i) = i {
                        let discriminant = self.iiv.discriminant(value);
                        let expected = self.iiv.int_lit(i as u32);
                        let cond = self.iiv.equals(discriminant, expected);
                        let next_block = self.iiv.create_block();
                        self.iiv.branch(cond, next_block, no_match_block.unwrap());
                        self.iiv.append_block(next_block);
                        self.iiv.select(next_block);
                        if let Some(inner_patter) = &vairant.inner {
                            let inner = self.iiv.get_prop(value, i as u8, elems[i].1);
                            self.bind(&inner_patter, inner, no_match_block);
                        }
                    } else {
                        self.msg(err!(
                            &pattern.span(),
                            "variant {} not found on type {}",
                            vairant.name.value,
                            value.ty
                        ));
                    }
                } else {
                    self.msg(err!(
                        &pattern.span(),
                        "expected a variant type, found {}",
                        value.ty
                    ));
                }
            }
            PatternBody::Literal(_) => unimplemented!(),
            PatternBody::Struct(struct_pattern) => {
                if let Type::Struct(_) = *value.ty {
                    for (name, prop_pattern) in &struct_pattern.inner {
                        let prop = self.get_prop(value.obj(), name);
                        let prop = self.get_val(&pattern.span(), prop);
                        self.bind(prop_pattern, prop, no_match_block);
                    }
                } else {
                    self.msg(err!(
                        &pattern.span(),
                        "expected a struct type, found {}",
                        value.ty
                    ));
                }
            }
            _ => unimplemented!(),
        }
    }

    fn check_statement(&mut self, item: &BlockItem<'i>) -> Value<'i> {
        match item {
            BlockItem::Expr(expr) => self.check_val(expr),
            BlockItem::Break(_break) => self.never(),
            BlockItem::Return(ret) => {
                let Some(return_type) = self.ret_type else {
                    self.msg(err!(&ret.span, "return is invalid within this context"));
                    return self.invalid();
                };

                let val = if let Some(e) = &ret.value {
                    self.check_val(&e)
                } else {
                    self.null()
                };

                self.ensure_ty(&ret.span, return_type, val);
                self.never()
            }
            BlockItem::Continue(Continue) => self.never(),
            BlockItem::Bind(binding) => {
                let initializer = self.check_val(&binding.value);
                self.bind(&binding.binding, initializer, None);

                self.null()
            }
        }
    }

    fn get_val(&mut self, span: &Span, obj: Object<'i>) -> Value<'i> {
        match obj {
            Object::Trait => {
                err!(span, "expected a value, found trait name");
                self.invalid()
            }
            Object::Module => {
                err!(span, "expected a value, found module");
                self.invalid()
            }
            Object::Value(val) => val,
            Object::Place(val, offsets) => {
                if offsets.is_empty() {
                    val
                } else {
                    self.iiv.get_deep_prop(val, offsets, val.ty)
                }
            }
            Object::Type(ty) => self.iiv.ty_expr(ty),
            Object::Condition(raw) => panic!("unexpected condition returned"),
            Object::Fun(_) => {
                err!(span, "expected a value, found function");
                self.invalid()
            }
        }
    }

    fn get_type(&mut self, span: &Span, obj: Object<'i>) -> TypeRef<'i> {
        match obj {
            Object::Trait => {
                err!(span, "expected a type, found trait name");
                self.ty.get_ty_invalid()
            }
            Object::Module => {
                err!(span, "expected a type, found module");
                self.ty.get_ty_invalid()
            }
            Object::Value(val) | Object::Place(val, _) => {
                if val.ty == self.ty.get_ty_type() {
                    self.ty.get_ty_constant(val.raw)
                } else {
                    err!(span, "expected a type, found constant of type {}", val.ty);
                    self.ty.get_ty_invalid()
                }
            }
            Object::Type(ty) => ty,
            Object::Condition(_) => panic!("unexpedted condition"),
            Object::Fun(_) => {
                err!(span, "expected a type, found function");
                self.ty.get_ty_invalid()
            }
        }
    }

    fn check_condition(&mut self, expr: &Expr<'i>, false_block: BlockRef) -> usize {
        match self.check(expr, Some(false_block)) {
            Object::Condition(scopes) => scopes,
            obj => {
                let val = self.get_val(&expr.span(), obj);
                let val = self.ensure_ty(&expr.span(), self.ty.get_ty_bool(), val);
                let next_block = self.iiv.create_block();
                self.iiv.branch(val, next_block, false_block);
                self.iiv.append_block(next_block);
                self.iiv.select(next_block);
                0
            }
        }
    }

    fn get_prop(&mut self, value: Object<'i>, prop: &Ident<'i>) -> Object<'i> {
        match value {
            Object::Place(mut place, mut offsets) => {
                if let Some((i, prop_ty)) = place.ty.prop(prop.value) {
                    offsets.push(i);
                    place.ty = prop_ty;
                    Object::Place(place, offsets)
                } else {
                    self.msg(err!(
                        &prop.span(),
                        "property {} does not exist on type {}",
                        prop.value,
                        place.ty
                    ));
                    Object::Place(self.invalid(), vec![])
                }
            }
            Object::Value(value) => {
                if let Some((i, prop_ty)) = value.ty.prop(prop.value) {
                    self.iiv.get_prop(value, i, prop_ty).obj()
                } else {
                    self.msg(err!(
                        &prop.span(),
                        "property {} does not exist on type {}",
                        prop.value,
                        value.ty
                    ));
                    Object::Value(self.invalid())
                }
            }
            _ => {
                self.msg(err!(&prop.span(), "no such property"));
                Object::Place(self.invalid(), vec![])
            }
        }
    }

    fn resolve_props(&mut self, props: &PropsTy<'i>) -> Vec<PropRef<'i>> {
        props
            .props
            .iter()
            .map(|prop| {
                let ty = if let Some(ty_name) = &prop.ty {
                    self.check_type(&ty_name)
                } else {
                    self.ty.get_null()
                };
                self.ty.get_prop(prop.name.value, ty)
            })
            .collect()
    }

    fn check(&mut self, expr: &Expr<'i>, false_block: Option<BlockRef>) -> Object<'i> {
        match expr {
            Expr::Variable(ident) => self.resolve(ident),
            Expr::Block(block) => {
                self.begin_scope();
                let mut last: Option<Value<'i>> = None;
                for item in &block.items {
                    last = Some(self.check_statement(item));
                }
                self.end_scope();
                if let (Some(value), true) = (last, block.has_trailing_expression) {
                    value.obj()
                } else {
                    self.null().obj()
                }
            }
            Expr::Int(int) => self.iiv.int_lit(int.value).obj(),
            Expr::Bool(boolean) => self.iiv.bool_lit(boolean.value).obj(),
            Expr::Float(Float) => unimplemented!(),
            Expr::String(StringLit) => unimplemented!(),
            Expr::Char(Char) => unimplemented!(),
            Expr::Tuple(tuple) => {
                let items: Vec<_> = tuple.fields.iter().map(|e| self.check_val(e)).collect();
                self.iiv.make_tuple(&items).obj()
            }
            Expr::Struct(structure) => {
                let (props, vals): (Vec<_>, Vec<_>) = structure
                    .props
                    .iter()
                    .map(|prop| {
                        let val = if let Some(expr) = &prop.value {
                            self.check_val(&expr)
                        } else {
                            self.resolve_val(&ast::Ident {
                                span: prop.span(),
                                value: prop.name.value,
                            })
                        };
                        (
                            self.ty.get_prop(prop.name.value, val.ty),
                            (prop.name.value, val),
                        )
                    })
                    .unzip();
                let ty = self.ty.get_struct(props);
                let iiv::ty::Type::Struct(prop_list) = *ty else {
                    unreachable!()
                };
                let vals: Vec<_> = prop_list
                    .iter()
                    .map(|prop| {
                        *vals
                            .iter()
                            .find_map(|(name, val)| if *name == prop.0 { Some(val) } else { None })
                            .unwrap()
                    })
                    .collect();
                self.iiv.make_struct(&vals, ty).obj()
            }
            Expr::If(if_expr) => {
                let no_block = self.iiv.create_block();
                let after_block = self.iiv.create_block();

                let scopes = self.check_condition(&*if_expr.condition, no_block);

                let yes_block = self.iiv.get_current_block();
                let yes = self.check_val(&if_expr.yes);
                let yes_cp = self.iiv.set_up_patch();

                for _ in 0..scopes {
                    self.end_scope();
                }

                self.iiv.append_block(no_block);
                self.iiv.select(no_block);
                let no = if let Some(ref expr) = if_expr.no {
                    self.check_val(expr)
                } else {
                    self.null()
                };

                let result_ty = self.make_type_union(&if_expr.span, &[no.ty, yes.ty]);

                let no = self.ensure_ty(&if_expr.span(), result_ty, no);
                self.iiv.jump(after_block, &[no]);

                self.iiv.select(yes_block);
                let cp2 = self.iiv.start_patch(yes_cp);
                let yes = self.ensure_ty(&if_expr.yes.span(), result_ty, yes);
                self.iiv.jump(after_block, &[yes]);
                self.iiv.end_patch(yes_cp, cp2);

                self.iiv.append_block(after_block);
                self.iiv.select(after_block);
                dbg!(self.iiv.block_param(result_ty)).obj()
            }
            Expr::While(while_expr) => {
                let cond_block = self.iiv.create_block();
                self.iiv.select(cond_block);

                let body = self.iiv.create_block();
                let after = self.iiv.create_block();

                let scopes = self.check_condition(&while_expr.condition, after);

                self.iiv.jump(body, &[]);
                self.iiv.select(body);
                self.check_val(&*while_expr.body);

                self.iiv.select(after);
                self.null().obj()
            }
            Expr::Match(Match) => unimplemented!(),
            Expr::Call(call) => {
                if let Object::Fun(fun_ref) = self.check(&call.lhs, None) {
                    let fun = fun_ref.borrow();

                    if fun.sig.params.len() != call.args.len() {
                        self.msg(err!(
                            &call.span(),
                            "invalid number of arguments in this call"
                        ));
                    }

                    let mut args: Vec<_> =
                        call.args.iter().map(|arg| self.check_val(arg)).collect();

                    for (i, (arg, &param)) in args.iter_mut().zip(fun.sig.params.iter()).enumerate()
                    {
                        *arg = self.ensure_ty(&call.args[i].span(), param, *arg);
                    }

                    self.iiv.call(fun_ref, &args).obj()
                } else {
                    self.msg(err!(&call.lhs.span(), "expected a callable"));
                    self.invalid().obj()
                }
            }
            Expr::Prop(prop) => {
                let lhs = self.check(&prop.lhs, false_block);
                self.get_prop(lhs, &prop.prop)
            }
            Expr::Field(Field) => unimplemented!(),
            Expr::Index(Index) => unimplemented!(),
            Expr::Cast(Cast) => unimplemented!(),
            Expr::RefTo(ref_to) => {
                let checked = self.check(&ref_to.rhs, false_block);
                match checked {
                    Object::Place(place, offsets) => {
                        let r = self.iiv.get_prop_ref(place, offsets, place.ty);
                        dbg!(r.ty);
                        r.obj()
                    }
                    _ => {
                        self.msg(err!(&ref_to.rhs.span(), "expected an assignable l-value"));
                        self.invalid().obj()
                    }
                }
            }
            Expr::Add(add) => {
                let lhs = self.check_val(&add.lhs);
                let rhs = self.check_val(&add.rhs);
                self.iiv.add(lhs, rhs).obj()
            }
            Expr::Mul(Mul) => unimplemented!(),
            Expr::Eq(equals) => {
                let lhs = self.check_val(&equals.lhs);
                let rhs = self.check_val(&equals.rhs);
                self.iiv.equals(lhs, rhs).obj()
            }
            Expr::Neq(NotEquals) => unimplemented!(),
            Expr::And(And) => unimplemented!(),
            Expr::Or(Or) => unimplemented!(),
            Expr::Geq(GreaterEq) => unimplemented!(),
            Expr::Leq(LessEq) => unimplemented!(),
            Expr::Lt(Less) => unimplemented!(),
            Expr::Gt(Greater) => unimplemented!(),
            Expr::Assign(assign) => {
                let rhs = self.check_val(&assign.rhs);
                let lhs = self.check(&assign.lhs, false_block);
                match lhs {
                    Object::Place(place, offsets) => {
                        let rhs = self.ensure_ty(&assign.rhs.span(), place.ty, rhs);
                        let prop_ref = self.iiv.get_prop_ref(place, offsets, place.ty);
                        self.iiv.assign(prop_ref, rhs).obj()
                    }
                    other => {
                        let lhs = self.get_val(&assign.lhs.span(), other);
                        if let Type::Ref(inner) = *lhs.ty {
                            let rhs = self.ensure_ty(&assign.rhs.span(), inner, rhs);
                            self.iiv.assign(lhs, rhs).obj()
                        } else {
                            self.msg(err!(
                                &assign.lhs.span(),
                                "expected an assignable l-value, got {}",
                                lhs.ty
                            ));
                            self.invalid().obj()
                        }
                    }
                }
            }
            Expr::Div(Div) => unimplemented!(),
            Expr::Sub(Sub) => unimplemented!(),
            Expr::Neg(Neg) => unimplemented!(),
            Expr::Not(Not) => unimplemented!(),
            Expr::Vec(Vector) => unimplemented!(),
            Expr::Variant(variant) => {
                let value = if let Some(val) = &variant.value {
                    self.check_val(&val)
                } else {
                    self.null()
                };
                let variant_ty = self
                    .ty
                    .get_variant(vec![self.ty.get_prop(variant.variant.value, value.ty)]);
                self.iiv.variant(variant_ty, 0, value).obj()
            }
            Expr::AddAssign(AddAssign) => unimplemented!(),
            Expr::Is(is_expr) => {
                dbg!("ISSS");
                let lhs = dbg!(self.check_val(&is_expr.lhs));
                self.begin_scope();
                if let Some(false_block) = false_block {
                    self.bind(&is_expr.rhs, lhs, Some(false_block));
                    Object::Condition(1)
                } else {
                    let false_block = self.iiv.create_block();
                    let after_block = self.iiv.create_block();
                    self.bind(&is_expr.rhs, lhs, Some(false_block));
                    let true_val = self.iiv.bool_lit(true);
                    self.iiv.jump(after_block, &[true_val]);
                    self.iiv.append_block(false_block);
                    self.iiv.select(false_block);
                    let false_val = self.iiv.bool_lit(false);
                    self.iiv.jump(after_block, &[false_val]);
                    self.iiv.append_block(after_block);
                    self.iiv.select(after_block);
                    let value = self.iiv.block_param(self.ty.get_ty_bool());
                    self.end_scope();
                    Object::Value(value)
                }
            }
            Expr::VariantTy(props) => {
                let props = self.resolve_props(props);
                self.ty.get_variant(props).obj()
            }
            Expr::StructTy(props) => {
                let props = self.resolve_props(props);
                self.ty.get_struct(props).obj()
            }
        }
    }
}

impl<'i, 'g> FunctionGenerator<'i, 'g> {
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &Ident<'i>, object: Object<'i>) {
        self.scopes.last_mut().unwrap().insert(name.value, object);
    }

    fn resolve(&mut self, name: &Ident) -> Object<'i> {
        for scope in &self.scopes {
            if let Some(obj) = scope.get(&name.value) {
                return obj.clone();
            }
        }

        if let Some(obj) = self.global_scope.get(&name.value) {
            return obj.clone();
        }

        self.msg(err!(
            &name.span,
            "\"{}\" is not defined in this context",
            name.value
        ));

        self.invalid().obj()
    }

    fn resolve_val(&mut self, name: &Ident) -> Value<'i> {
        let obj = self.resolve(name);
        self.get_val(&name.span, obj)
    }
}

trait ObjContent<'i> {
    fn obj(self) -> Object<'i>;
}

impl<'i> ObjContent<'i> for Value<'i> {
    fn obj(self) -> Object<'i> {
        Object::Value(self)
    }
}

impl<'i> ObjContent<'i> for TypeRef<'i> {
    fn obj(self) -> Object<'i> {
        Object::Type(self)
    }
}

#[derive(Clone, Debug)]
enum Object<'i> {
    Trait,
    Module,
    Value(Value<'i>),
    Place(Value<'i>, Vec<u8>),
    Type(TypeRef<'i>),
    Fun(FuncRef<'i>),
    Condition(usize),
}
