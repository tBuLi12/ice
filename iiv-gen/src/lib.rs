use std::collections::HashMap;

use ast::{BlockItem, Expr, Ident, Module, Spanned};
use iiv::{
    diagnostics, err,
    str::Str,
    ty::{TypeOverlap, TypeRef},
    Ctx, Package, Span, Value,
};

mod ty;

pub struct Generator<'i> {
    global_scope: HashMap<Str<'i>, Object<'i>>,
    ty: &'i iiv::ty::Pool<'i>,
    messages: &'i iiv::diagnostics::Diagnostics,
}

pub struct FunctionGenerator<'i, 'g> {
    global_scope: &'g HashMap<Str<'i>, Object<'i>>,
    scopes: Vec<HashMap<Str<'i>, Object<'i>>>,
    ty: &'i iiv::ty::Pool<'i>,
    messages: &'i iiv::diagnostics::Diagnostics,
    constant_depth: usize,
    current_return_ty: TypeRef<'i>,
    iiv: iiv::builder::FunctionBuilder<'i>,
}

impl<'i> Generator<'i> {
    pub fn new(ctx: &'i Ctx<'i>) -> Self {
        Generator {
            global_scope: HashMap::new(),
            ty: &ctx.type_pool,
            messages: &ctx.diagnostcs,
        }
    }

    pub fn emit_iiv(&mut self, modules: &[Module<'i>]) -> Package<'i> {
        self.global_scope
            .insert(self.ty.str_pool.get("int"), Object::Type(self.ty.get_int()));

        Package {
            funcs: modules[0]
                .functions
                .iter()
                .map(|f| FunctionGenerator::new(&self, f.signature.name.value).emit_function(f))
                .collect(),
        }
    }
}

impl<'i, 'g> FunctionGenerator<'i, 'g> {
    pub fn new(parent: &'g Generator<'i>, name: Str<'i>) -> Self {
        Self {
            global_scope: &parent.global_scope,
            scopes: Vec::new(),
            ty: &parent.ty,
            messages: &parent.messages,
            constant_depth: 0,
            current_return_ty: parent.ty.get_null(),
            iiv: iiv::builder::FunctionBuilder::new(parent.ty, name),
        }
    }

    fn emit_function(mut self, fun: &ast::Function<'i>) -> iiv::fun::Function<'i> {
        let (ret_ty, span) = fun
            .signature
            .return_ty
            .as_ref()
            .map(|ty| (self.check_type(ty), ty.span()))
            .unwrap_or_else(|| (self.ty.get_null(), fun.signature.span()));

        self.scopes.push(HashMap::new());

        for param in &fun.signature.params {
            let ty = self.check_type(&param.ty);
            self.scopes
                .last_mut()
                .unwrap()
                .insert(param.name.value, Object::Value(self.iiv.parameter(ty)));
        }

        self.current_return_ty = ret_ty;
        self.iiv.ret_ty = Some(ret_ty);

        let body = self.check_val(&fun.body);
        let body = self.ensure_ty(&fun.body.span(), ret_ty, body);
        self.iiv.ret(body);
        self.iiv.build()
    }

    fn check_val(&mut self, expr: &Expr) -> Value<'i> {
        let obj = self.check(expr);
        self.get_val(&expr.span(), obj)
    }

    fn in_const_ctx<T>(&mut self, fun: impl FnOnce(&mut Self) -> T) -> T {
        self.constant_depth += 1;
        let res = fun(self);
        self.constant_depth -= 1;
        res
    }

    fn check_type(&mut self, expr: &Expr) -> TypeRef<'i> {
        self.in_const_ctx(|g| {
            let obj = g.check(expr);
            g.get_type(&expr.span(), obj)
        })
    }

    fn msg(&mut self, message: diagnostics::Diagnostic) {
        println!("adding error");
        self.messages.add(message);
    }

    fn ensure_ty(&mut self, span: &Span, ty: TypeRef<'i>, value: Value<'i>) -> Value<'i> {
        if value.ty != ty {
            self.msg(err!(span, "expected type {}, got {} instead", ty, value.ty));
        }
        Value { raw: value.raw, ty }
    }

    fn make_type_union(&'i mut self, span: &Span, types: &[TypeRef<'i>]) -> TypeRef<'i> {
        let mut unqiue = vec![];

        'outer: for &ty in types {
            for &member in unqiue.iter() {
                match ty.intersects(member) {
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

    fn check_statement(&mut self, item: &BlockItem) -> Value<'i> {
        match item {
            BlockItem::Expr(expr) => self.check_val(expr),
            BlockItem::Break(_break) => self.never(),
            BlockItem::Return(ret) => {
                let val = if let Some(e) = &ret.value {
                    self.check_val(&e)
                } else {
                    self.null()
                };
                // let val = ret
                //     .value
                //     .as_ref()
                //     .map(|e| self.check_val(e))
                //     .unwrap_or(self.null());

                self.ensure_ty(&ret.span, self.current_return_ty, val);
                self.never()
            }
            BlockItem::Continue(Continue) => self.never(),
            BlockItem::Bind(binding) => self.null(),
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
            Object::Value(val) | Object::Place(val) => val,
            Object::Type(ty) => self.iiv.ty_expr(ty),
            Object::IsResult(raw) => Value {
                raw,
                ty: self.ty.get_ty_bool(),
            },
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
            Object::Value(val) | Object::Place(val) => {
                if val.ty == self.ty.get_ty_type() {
                    self.ty.get_ty_constant(val.raw)
                } else {
                    err!(span, "expected a type, found constant of type {}", val.ty);
                    self.ty.get_ty_invalid()
                }
            }
            Object::Type(ty) => ty,
            Object::IsResult(raw) => {
                err!(span, "expected a type, found bool");
                self.ty.get_ty_invalid()
            }
        }
    }

    fn check_condition(&mut self, expr: &Expr) -> (Option<()>, Value<'i>) {
        match self.check(expr) {
            Object::IsResult(raw_val) => {
                let val = Value {
                    raw: raw_val,
                    ty: self.ty.get_ty_bool(),
                };
                (Some(()), val)
                // fine, but close the scope later
            }
            obj => {
                let val = self.get_val(&expr.span(), obj);
                let val = self.ensure_ty(&expr.span(), self.ty.get_ty_bool(), val);
                (None, val)
            }
        }
    }

    fn check(&mut self, expr: &Expr) -> Object<'i> {
        match expr {
            Expr::Variable(ident) => self.resolve(ident),
            Expr::Block(block) => {
                let mut last: Option<Value<'i>> = None;
                for item in &block.items {
                    last = Some(self.check_statement(item));
                }
                if let (Some(value), true) = (last, block.has_trailing_expression) {
                    value.obj()
                } else {
                    self.null().obj()
                }
            }
            Expr::Int(int) => self.iiv.int_lit(int.value).obj(),
            Expr::Float(Float) => unimplemented!(),
            Expr::String(StringLit) => unimplemented!(),
            Expr::Char(Char) => unimplemented!(),
            Expr::Tuple(tuple) => {
                let items: Vec<_> = tuple.fields.iter().map(|e| self.check_val(e)).collect();
                self.iiv.make_tuple(&items).obj()
            }
            Expr::Struct(structure) => {
                let mut is_const = true;
                let props: Vec<_> = structure
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
                        (prop.name.value, val)
                    })
                    .collect();
                self.iiv.make_struct(&props).obj()
            }
            Expr::If(if_expr) => {
                let (scope, cond) = self.check_condition(&*if_expr.condition);

                let yes_block = self.iiv.create_block();
                let no_block = self.iiv.create_block();
                let after_block = self.iiv.create_block();

                self.iiv.branch(cond, yes_block, no_block);

                self.iiv.select(yes_block);
                let yes = self.check_val(&if_expr.yes);
                drop(scope);
                self.iiv.jump(after_block);

                self.iiv.select(no_block);
                let no = if let Some(ref expr) = if_expr.no {
                    self.check_val(expr)
                } else {
                    self.null()
                };
                self.iiv.jump(after_block);

                self.iiv.select(after_block);
                self.iiv.phi(&[(yes_block, yes), (no_block, no)]).obj()
            }
            Expr::While(while_expr) => {
                let cond_block = self.iiv.create_block();
                self.iiv.select(cond_block);

                let (scope, cond) = self.check_condition(&while_expr.condition);

                let body = self.iiv.create_block();
                let after = self.iiv.create_block();

                self.iiv.jump(body);
                self.iiv.select(body);
                self.check_val(&*while_expr.body);
                self.iiv.branch(cond, cond_block, after);

                self.iiv.select(after);
                self.null().obj()
            }
            Expr::Match(Match) => unimplemented!(),
            Expr::Call(Call) => unimplemented!(),
            Expr::Prop(prop) => {
                let lhs = self.check_val(&prop.lhs);
                self.iiv.get_prop(lhs, prop.prop.value).obj()
            }
            Expr::Field(Field) => unimplemented!(),
            Expr::Index(Index) => unimplemented!(),
            Expr::Cast(Cast) => unimplemented!(),
            Expr::Add(add) => {
                let lhs = self.check_val(&add.lhs);
                let rhs = self.check_val(&add.rhs);
                self.iiv.add(lhs, rhs).obj()
            }
            Expr::Mul(Mul) => unimplemented!(),
            Expr::Eq(Equals) => unimplemented!(),
            Expr::Neq(NotEquals) => unimplemented!(),
            Expr::And(And) => unimplemented!(),
            Expr::Or(Or) => unimplemented!(),
            Expr::Geq(GreaterEq) => unimplemented!(),
            Expr::Leq(LessEq) => unimplemented!(),
            Expr::Lt(Less) => unimplemented!(),
            Expr::Gt(Greater) => unimplemented!(),
            Expr::Assign(Assign) => unimplemented!(),
            Expr::Div(Div) => unimplemented!(),
            Expr::Sub(Sub) => unimplemented!(),
            Expr::Neg(Neg) => unimplemented!(),
            Expr::Not(Not) => unimplemented!(),
            Expr::Vec(Vector) => unimplemented!(),
            Expr::Variant(Variant) => unimplemented!(),
            Expr::AddAssign(AddAssign) => unimplemented!(),
            Expr::Is(AddAssign) => unimplemented!(),
        }
    }
}

impl<'i, 'g> FunctionGenerator<'i, 'g> {
    fn resolve(&mut self, name: &Ident) -> Object<'i> {
        for scope in &self.scopes {
            if let Some(obj) = scope.get(&name.value) {
                return *obj;
            }
        }

        if let Some(obj) = self.global_scope.get(&name.value) {
            return *obj;
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

#[derive(Clone, Copy, Debug)]
enum Object<'i> {
    Trait,
    Module,
    Value(Value<'i>),
    Place(Value<'i>),
    Type(TypeRef<'i>),
    IsResult(iiv::RawValue),
}
