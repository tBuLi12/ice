use ast::{BlockItem, Expr, Ident};
use iiv::{
    diagnostics, err,
    ty::{TypeOverlap, TypeRef},
    Span, Value,
};

mod ty;

struct Checker<'i> {
    ty: iiv::ty::Pool<'i>,
    int_pool: iiv::pool::Pool<'i, u32>,
    constant_depth: usize,
    fun: iiv::fun::Function<'i>,
    iiv: iiv::builder::Builder<'i>,
}

impl<'i> Checker<'i> {
    fn msg(&mut self, message: diagnostics::Diagnostic) {}

    fn ensure_ty(&mut self, span: &Span, ty: TypeRef<'i>, value: Value<'i>) -> Value<'i> {
        if value.ty != ty {
            self.msg(err!(span, "expected type {}, got {} instead", ty, value.ty));
        }
        Value { raw: value.raw, ty }
    }

    fn make_type_union(&mut self, span: &Span, types: &[TypeRef<'i>]) -> TypeRef<'i> {
        let unqiue = vec![];

        'outer: for &ty in types {
            for member in unqiue {
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
                    TypeOverlap::None => unqiue.push(ty),
                }
            }
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

    fn null(&self) -> Value<'_> {
        Value {
            raw: iiv::RawValue::NULL,
            ty: self.ty.get_null(),
        }
    }

    fn never(&self) -> Value<'_> {
        Value {
            raw: iiv::RawValue::NULL,
            ty: self.ty.get_ty_never(),
        }
    }

    fn invalid(&self) -> Value<'_> {
        Value {
            raw: iiv::RawValue::NULL,
            ty: self.ty.get_ty_invalid(),
        }
    }

    fn check_statement(&mut self, item: &BlockItem) -> Value<'_> {
        match item {
            BlockItem::Expr(expr) => self.check_val(expr),
            BlockItem::Break(_break) => self.never(),
            BlockItem::Return(ret) => {
                self.ensure_ty(
                    &ret.span,
                    self.fun.sig.ret_ty,
                    ret.value
                        .map(|ref e| self.check_val(e))
                        .unwrap_or(self.null()),
                );
                self.never()
            }
            BlockItem::Continue(Continue) => self.never(),
            BlockItem::Bind(binding) => self.null(),
        }
    }

    fn get_val(&mut self, span: &Span, obj: Object<'i>) -> Value {
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

    fn check_val(&mut self, expr: &Expr) -> Value {
        self.get_val(expr.span(), self.check(expr))
    }

    fn check_condition(&mut self, expr: &Expr) -> (Option<()>, Value) {
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
                let val = self.get_val(expr.span(), obj);
                let val = self.ensure_ty(expr.span(), self.ty.get_ty_bool(), val);
                (None, val)
            }
        }
    }

    fn check(&mut self, expr: &Expr) -> Object {
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
                self.iiv.aggregate(&items).obj()
            }
            Expr::Struct(structure) => {
                let mut is_const = true;
                let props = structure
                    .props
                    .iter()
                    .map(|prop| {
                        let val = if let Some(expr) = prop.value {
                            self.check(&expr)
                        } else {
                            self.resolve(&ast::Ident {
                                span: prop.span,
                                value: prop.name,
                            })
                        };
                    })
                    .collect();
                Object::Value(self.ty.get_struct(props))
            }
            Expr::If(if_expr) => {
                let (scope, cond) = self.check_condition(&*if_expr.condition);

                let yes_block = self.iiv.create_block();
                let no_block = self.iiv.create_block();

                self.iiv.branch(cond, yes_block, no_block);

                self.iiv.select(yes_block);
                let yes = self.check_val(&*if_expr.yes);
                drop(scope);

                self.iiv.select(no_block);
                let no = self.check_val(&*if_expr.no);

                Object::Value(Value {
                    raw: self.iiv.phi(&[(yes.raw, yes_block), (no.raw, no_block)]),
                    ty: self.ty_union(&[yes, no]),
                })
            }
            Expr::While(while_expr) => {
                let cond_block = self.iiv.create_block();
                self.iiv.select(cond_block);

                let (scope, cond) = self.check_condition(&*while_expr.condition);

                let body = self.iiv.create_block();
                let after = self.iiv.create_block();

                self.iiv.jump(body);
                self.iiv.select(body);
                self.check_val(&*while_expr.body);
                self.iiv.branch(cond, cond_block, after);

                self.iiv.select(after);
            }
            Expr::Match(Match) => {}
            Expr::Call(Call) => {}
            Expr::Prop(prop) => {
                let lhs = self.check(&prop.lhs);
                match lhs {
                    Object::Place(ty) => Object::Place(ty.prop(prop.prop)),
                    Object::Value(ty) => Object::Value(ty.prop(prop.prop)),
                    _ => panic!(), // Object::Type()
                                   // Object::Module
                                   // Object::Trait
                }
            }
            Expr::Field(Field) => unimplemented!(),
            Expr::Index(Index) => unimplemented!(),
            Expr::Cast(Cast) => unimplemented!(),
            Expr::Add(Add) => unimplemented!(),
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
            Expr::Bs(Bs) => unimplemented!(),
            Expr::Vec(Vector) => unimplemented!(),
            Expr::Type(Type) => unimplemented!(),
            Expr::Variant(Variant) => unimplemented!(),
            Expr::AddAssign(AddAssign) => unimplemented!(),
            Expr::Is(AddAssign) => unimplemented!(),
        }
    }
}

impl<'i> Checker<'i> {
    fn resolve(&mut self, name: &Ident) -> Object<'_> {
        unimplemented!()
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

enum Object<'i> {
    Trait,
    Module,
    Value(Value<'i>),
    Place(Value<'i>),
    Type(TypeRef<'i>),
    IsResult(iiv::RawValue),
}
