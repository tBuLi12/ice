use std::{collections::HashMap, ops::Deref};

use ast::{
    BindPattern, BindingType, BlockItem, Expr, Ident, Module, NarrowTypePattern, Pattern,
    PatternBody, PropsTy, Spanned,
};
use iiv::{
    builder::BlockRef,
    diagnostics, err,
    fun::{Function, Signature},
    move_check,
    pool::FuncRef,
    str::Str,
    ty::{PropRef, Type, TypeOverlap, TypeRef},
    Ctx, Package, Prop, Span, Value,
};

mod ty;

pub struct Generator<'i> {
    global_scope: HashMap<Str<'i>, Object<'i>>,
    ty: &'i iiv::ty::Pool<'i>,
    fun_pool: &'i iiv::pool::FunPool<'i>,
    messages: &'i iiv::diagnostics::Diagnostics,
}

struct Scope<'i> {
    index: HashMap<Str<'i>, Object<'i>>,
    values: Vec<Value<'i>>,
}

pub struct FunctionGenerator<'i, 'g> {
    global_scope: &'g HashMap<Str<'i>, Object<'i>>,
    scopes: Vec<Scope<'i>>,
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
                value_count: 0,
            });

            let name = fun_node.signature.name.value;

            self.global_scope.insert(name, Object::Fun(fun));
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
        self.begin_scope();

        for (&ty, param) in fun.sig.params.iter().zip(&fun_node.signature.params) {
            let param_value = self.iiv.param(ty);
            self.define(param.name.value, Object::Place(param_value, ty, vec![]));
        }

        self.ret_type = Some(fun.sig.ret_ty);

        let body = self.check_val(&fun_node.body);
        let body = self.ensure_ty(&fun_node.body.span(), fun.sig.ret_ty, body);
        let body = self.move_val(body);
        println!("exiting func {}", &*fun.sig.name);
        self.end_scope();
        self.iiv.ret(body);

        if self.messages.ok() {
            let (blocks, inst_count) = self.iiv.build();
            fun.body = blocks;
            fun.value_count = inst_count;
        }
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

    fn null(&mut self) -> Value<'i> {
        self.iiv.null()
    }

    fn never(&mut self) -> Value<'i> {
        Value {
            raw: self.iiv.null().raw,
            ty: self.ty.get_ty_never(),
        }
    }

    fn invalid(&mut self) -> Value<'i> {
        Value {
            raw: self.iiv.null().raw,
            ty: self.ty.get_ty_invalid(),
        }
    }

    fn bind(
        &mut self,
        pattern: &Pattern<'i>,
        value: Value<'i>,
        no_match_block: Option<BlockRef>,
        match_block: Option<BlockRef>,
    ) {
        match &pattern.body {
            PatternBody::Bind(BindPattern { binding_type, name }) => match binding_type {
                BindingType::Var => self.define(name.value, Object::Place(value, value.ty, vec![])),
                BindingType::Let => self.define(name.value, Object::Place(value, value.ty, vec![])),
                _ => unimplemented!(),
            },
            PatternBody::NarrowType(NarrowTypePattern { inner, ty }) => {
                let target_ty = self.check_type(ty);
                let narrowed = self.ensure_ty(&ty.span(), target_ty, value);
                self.bind(&inner, narrowed, no_match_block, match_block);
            }
            PatternBody::Variant(vairant) => {
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
                        let no_match = no_match_block.unwrap();
                        self.iiv.branch(cond, next_block, no_match);
                        self.iiv.select(next_block);
                        let inner = self.iiv.move_prop(value, i as u8, value.ty, elems[i].1);
                        if let Some(inner_patter) = &vairant.inner {
                            self.bind(&inner_patter, inner, no_match_block, match_block);
                        } else {
                            self.iiv.select(match_block.unwrap());
                            self.drop(inner);
                            self.iiv.select(next_block);
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
                if let Type::Struct(props) = *value.ty {
                    for (name, prop_pattern) in &struct_pattern.inner {
                        let prop = if let Some((i, prop_ty)) = value.ty.prop(name.value) {
                            self.iiv.move_prop(value, i, value.ty, prop_ty)
                        } else {
                            self.msg(err!(
                                &name.span(),
                                "property {} does not exist on type {}",
                                name.value,
                                value.ty
                            ));
                            self.invalid()
                        };
                        self.bind(prop_pattern, prop, no_match_block, match_block);
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
            BlockItem::Expr(expr) => match self.check(expr, None) {
                Object::Place(val, _, offsets) => {
                    if offsets.is_empty() {
                        val
                    } else {
                        self.iiv.get_deep_prop(val, offsets, val.ty)
                    }
                }
                Object::Value(val) => val,
                Object::Condition(_) => panic!("unexpected condtion!"),
                _ => self.null(),
            },
            BlockItem::Bind(binding) => {
                let initializer = self.check_val(&binding.value);
                self.bind(
                    &binding.binding,
                    initializer,
                    None,
                    Some(self.iiv.get_current_block()),
                );

                self.null()
            }
            _ => unimplemented!(),
            // BlockItem::Break(_break) => self.never(),
            // BlockItem::Return(ret) => {
            //     let Some(return_type) = self.ret_type else {
            //         self.msg(err!(&ret.span, "return is invalid within this context"));
            //         return self.invalid();
            //     };

            //     let val = if let Some(e) = &ret.value {
            //         self.check_val(&e)
            //     } else {
            //         self.null()
            //     };

            //     self.ensure_ty(&ret.span, return_type, val);
            //     self.never()
            // }
            // BlockItem::Continue(_continue) => self.never(),
        }
    }

    fn get_val(&mut self, span: &Span, obj: Object<'i>) -> Value<'i> {
        match obj {
            // Object::Trait => {
            //     err!(span, "expected a value, found trait name");
            //     self.invalid()
            // }
            // Object::Module => {
            //     err!(span, "expected a value, found module");
            //     self.invalid()
            // }
            Object::Value(val) => val,
            Object::Place(val, _, offsets) => {
                let result = if offsets.is_empty() {
                    val
                } else {
                    self.iiv.get_deep_prop(val, offsets, val.ty)
                };
                self.copy(result)
            }
            Object::Type(ty) => self.iiv.ty_expr(ty),
            Object::Condition(_raw) => panic!("unexpected condition returned"),
            Object::Fun(_) => {
                err!(span, "expected a value, found function");
                self.invalid()
            }
        }
    }

    fn get_type(&mut self, span: &Span, obj: Object<'i>) -> TypeRef<'i> {
        match obj {
            // Object::Trait => {
            //     err!(span, "expected a type, found trait name");
            //     self.ty.get_ty_invalid()
            // }
            // Object::Module => {
            //     err!(span, "expected a type, found module");
            //     self.ty.get_ty_invalid()
            // }
            Object::Value(val) | Object::Place(val, _, _) => {
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
                self.iiv.select(next_block);
                0
            }
        }
    }

    fn get_prop(&mut self, value: Object<'i>, prop: &Ident<'i>) -> Object<'i> {
        match value {
            Object::Place(mut place, raw_ty, mut offsets) => {
                if let Some((i, prop_ty)) = place.ty.prop(prop.value) {
                    offsets.push(i);
                    place.ty = prop_ty;
                    Object::Place(place, raw_ty, offsets)
                } else {
                    self.msg(err!(
                        &prop.span(),
                        "property {} does not exist on type {}",
                        prop.value,
                        place.ty
                    ));
                    Object::Place(self.invalid(), self.ty.get_ty_invalid(), vec![])
                }
            }
            Object::Value(value) => {
                if let Some((i, prop_ty)) = value.ty.prop(prop.value) {
                    let prop = self.iiv.get_prop(value, i, prop_ty).obj();
                    self.drop(value);
                    prop
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
                Object::Place(self.invalid(), self.ty.get_ty_invalid(), vec![])
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
        let object = match expr {
            Expr::Variable(ident) => self.resolve(ident),
            Expr::Block(block) => {
                self.begin_scope();
                for item in &block.items[..(block.items.len() - 1)] {
                    let value = self.check_statement(item);
                    self.drop(value);
                }
                let result = if let (Some(value), true) =
                    (block.items.last(), block.has_trailing_expression)
                {
                    self.check_statement(value)
                } else {
                    // let last = self.check_statement(value);
                    // self.drop(last);

                    self.null()
                };
                let result = self.move_val(result);
                self.end_scope();
                result.obj()
            }
            Expr::Int(int) => self.iiv.int_lit(int.value).obj(),
            Expr::Bool(boolean) => self.iiv.bool_lit(boolean.value).obj(),
            Expr::Float(_float) => unimplemented!(),
            Expr::String(_string_lit) => unimplemented!(),
            Expr::Char(_char) => unimplemented!(),
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
                let yes = self.move_val(yes);

                for _ in 0..scopes {
                    self.end_scope();
                }

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
                let yes = self.ensure_ty(&if_expr.yes.span(), result_ty, yes);
                self.iiv.jump(after_block, &[yes]);

                self.iiv.select(after_block);
                self.iiv.block_param(result_ty).obj()
            }
            Expr::While(while_expr) => {
                let cond_block = self.iiv.create_block();
                self.iiv.select(cond_block);

                let body = self.iiv.create_block();
                let after = self.iiv.create_block();

                let _scopes = self.check_condition(&while_expr.condition, after);

                self.iiv.jump(body, &[]);
                self.iiv.select(body);
                self.check_val(&*while_expr.body);

                self.iiv.select(after);
                self.null().obj();
                unimplemented!();
            }
            Expr::Match(_match) => unimplemented!(),
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
            Expr::Field(_field) => unimplemented!(),
            Expr::Index(_index) => unimplemented!(),
            Expr::Cast(_cast) => unimplemented!(),
            Expr::RefTo(ref_to) => {
                let checked = self.check(&ref_to.rhs, false_block);
                match checked {
                    Object::Place(place, _, offsets) => {
                        let r = self.iiv.get_prop_ref(place, offsets, place.ty);
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
            Expr::Mul(_mul) => unimplemented!(),
            Expr::Eq(equals) => {
                let lhs = self.check_val(&equals.lhs);
                let rhs = self.check_val(&equals.rhs);
                self.iiv.equals(lhs, rhs).obj()
            }
            Expr::Neq(_not_equals) => unimplemented!(),
            Expr::And(_and) => unimplemented!(),
            Expr::Or(_or) => unimplemented!(),
            Expr::Geq(_greater_eq) => unimplemented!(),
            Expr::Leq(_less_eq) => unimplemented!(),
            Expr::Lt(_less) => unimplemented!(),
            Expr::Gt(_greater) => unimplemented!(),
            Expr::Assign(assign) => {
                let rhs = self.check_val(&assign.rhs);
                let lhs = self.check(&assign.lhs, None);
                match lhs {
                    Object::Place(place, raw_ty, offsets) => {
                        let rhs = self.ensure_ty(&assign.rhs.span(), place.ty, rhs);
                        let elems = offsets
                            .into_iter()
                            .map(|offset| iiv::Elem::Prop(Prop(offset)))
                            .collect();
                        self.iiv.assign(place, raw_ty, elems, rhs).obj()
                    }
                    other => {
                        self.msg(err!(&assign.lhs.span(), "expected an assignable l-value"));
                        self.invalid().obj()
                        // let lhs = self.get_val(&assign.lhs.span(), other);
                        // if let Type::Ref(inner) = *lhs.ty {
                        //     let rhs = self.ensure_ty(&assign.rhs.span(), inner, rhs);
                        //     self.iiv.assign(lhs, rhs).obj()
                        // } else {
                        //     self.msg(err!(
                        //         &assign.lhs.span(),
                        //         "expected an assignable l-value, got {}",
                        //         lhs.ty
                        //     ));
                        //
                        // }
                    }
                }
            }
            Expr::Div(_div) => unimplemented!(),
            Expr::Sub(_sub) => unimplemented!(),
            Expr::Neg(_neg) => unimplemented!(),
            Expr::Not(_not) => unimplemented!(),
            Expr::Vec(_vector) => unimplemented!(),
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
            Expr::AddAssign(_add_assign) => unimplemented!(),
            Expr::Is(is_expr) => {
                let lhs = self.check_val(&is_expr.lhs);
                self.begin_scope();
                if let Some(false_block) = false_block {
                    let match_block = self.iiv.create_block();
                    self.bind(&is_expr.rhs, lhs, Some(false_block), Some(match_block));
                    self.iiv.jump(match_block, &[]);
                    self.iiv.select(false_block);
                    self.drop(lhs);
                    self.iiv.select(match_block);
                    Object::Condition(1)
                } else {
                    let false_block = self.iiv.create_block();
                    let true_block = self.iiv.create_block();
                    let after_block = self.iiv.create_block();
                    self.bind(&is_expr.rhs, lhs, Some(false_block), Some(true_block));
                    self.iiv.jump(true_block, &[]);
                    self.iiv.select(true_block);
                    let true_val = self.iiv.bool_lit(true);
                    self.iiv.jump(after_block, &[true_val]);
                    self.iiv.select(false_block);
                    self.drop(lhs);
                    let false_val = self.iiv.bool_lit(false);
                    self.iiv.jump(after_block, &[false_val]);
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
        };
        object
    }

    fn drop(&mut self, val: Value<'i>) {
        self.iiv.drop(val);
    }

    fn copy(&mut self, val: Value<'i>) -> Value<'i> {
        self.iiv.copy(val)
    }

    fn move_val(&mut self, val: Value<'i>) -> Value<'i> {
        self.iiv.move_prop_deep(val, vec![], val.ty, val.ty)
    }
}

impl<'i, 'g> FunctionGenerator<'i, 'g> {
    fn begin_scope(&mut self) {
        self.scopes.push(Scope {
            index: HashMap::new(),
            values: vec![],
        });
    }

    fn end_scope(&mut self) {
        let Scope { values, .. } = self.scopes.pop().unwrap();
        for val in values {
            self.drop(val);
        }
    }

    fn define(&mut self, name: Str<'i>, object: Object<'i>) {
        let scope = self.scopes.last_mut().unwrap();
        if let Object::Place(val, _, _) = &object {
            scope.values.push(*val);
        }
        scope.index.insert(name, object);
    }

    fn resolve(&mut self, name: &Ident) -> Object<'i> {
        for scope in &self.scopes {
            if let Some(obj) = scope.index.get(&name.value) {
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
    // Trait,
    // Module,
    Value(Value<'i>),
    Place(Value<'i>, TypeRef<'i>, Vec<u8>),
    Type(TypeRef<'i>),
    Fun(FuncRef<'i>),
    Condition(usize),
}
