use ast::{BindingType, BlockItem, Expr, Ident, Pattern, PatternBody};
use iiv::ty::TypeRef;

mod ty;

struct Checker<'i> {
    ty: iiv::ty::Pool<'i>,
}

impl<'i> Checker<'i> {
    fn try_returning(&mut self, value: Object) {}

    fn read_rt_value(&mut self, value: Object) -> TypeRef<'_> {
        match value {
            Object::Constant(ty) | Object::Value(ty) | Object::Place(ty) => {
                return ty;
            }
            _ => panic!(),
        }
    }

    fn read_ct_value(&mut self, value: Object) -> TypeRef<'_> {
        match value {
            Object::Constant(ty) => {
                return ty;
            }
            Object::Type(ty) => {
                return;
            }
            _ => panic!(),
        }
    }

    fn bind(&mut self, pattern: &Pattern, value: Object) {
        match (pattern.body, value) {
            (PatternBody::Bind(BindingType::Let, ident), value) => {
                self.read_rt_value(value);
            }
            (PatternBody::Bind(BindingType::Var, ident), value) => {
                self.read_rt_value(value);
            }
            (PatternBody::Bind(BindingType::Const, ident), value) => {
                self.read_ct_value(value);
            }
            _ => panic!(),
        }
    }

    fn check_statement(&mut self, item: &BlockItem) -> Object {
        match item {
            BlockItem::Expr(expr) => self.check(expr),
            BlockItem::Break(Break) => {}
            BlockItem::Return(ret) => {
                self.try_returning(ret.value.map(|ref e| self.check(e)).unwrap_or(self.null()))
            }
            BlockItem::Continue(Continue) => {}
            BlockItem::Bind(binding) => self.bind(&binding.binding, self.check(&binding.value)),
        }
    }

    fn check(&mut self, expr: &Expr) -> Object {
        match expr {
            Expr::Variable(ident) => self.resolve(ident),
            Expr::Block(block) => {
                let mut last: Option<Object> = None;
                for item in &block.items {
                    last = Some(self.check_statement(item));
                }
                if let (Some(value), true) = (last, block.has_trailing_expression) {
                    value
                } else {
                    self.ty_null()
                }
            }
            Expr::Int(int) => Object::Constant(self.ty.get_int()),
            Expr::Float(Float) => {}
            Expr::String(StringLit) => {}
            Expr::Char(Char) => {}
            Expr::Tuple(tuple) => {
                let mut is_const = true;
                let items = tuple
                    .fields
                    .iter()
                    .map(|e| match self.check(e) {
                        Object::Value(ty) => {
                            is_const = false;
                            ty
                        }
                        Object::Constant(ty) | Object::Place(ty) => ty,
                        _ => self.ty.get_ty_invalid(),
                    })
                    .collect();
                if is_const {
                    Object::Constant(self.ty.get_tuple(items))
                } else {
                    Object::Value(self.ty.get_tuple(items))
                }
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
                            self.resolve(&prop.name)
                        };
                        let ty = match val {
                            Object::Value(ty) => {
                                is_const = false;
                                ty
                            }
                            Object::Constant(ty) | Object::Place(ty) => ty,
                            _ => self.ty.get_ty_invalid(),
                        };
                        self.ty.get_prop(prop.name, ty)
                    })
                    .collect();
                if is_const {
                    Object::Constant(self.ty.get_struct(props))
                } else {
                    Object::Value(self.ty.get_struct(props))
                }
            }
            Expr::If(if_expr) => {
                let cond = self.check(if_expr.condition);
            }
            Expr::While(While) => {}
            Expr::Match(Match) => {}
            Expr::Call(Call) => {}
            Expr::Prop(prop) => {
                let lhs = self.check(&prop.lhs);
                match lhs {
                    Object::Constant(ty) => Object::Constant(ty.prop(prop.prop)), 
                    Object::Place(ty) => Object::Place(ty.prop(prop.prop)),
                    Object::Value(ty) => Object::Value(ty.prop(prop.prop)),
                    Object::Type()
                    Object::Module
                    Object::Trait
                }
            }
            Expr::Field(Field) => {}
            Expr::Index(Index) => {}
            Expr::Cast(Cast) => {}
            Expr::Add(Add) => {}
            Expr::Mul(Mul) => {}
            Expr::Eq(Equals) => {}
            Expr::Neq(NotEquals) => {}
            Expr::And(And) => {}
            Expr::Or(Or) => {}
            Expr::Geq(GreaterEq) => {}
            Expr::Leq(LessEq) => {}
            Expr::Lt(Less) => {}
            Expr::Gt(Greater) => {}
            Expr::Assign(Assign) => {}
            Expr::Div(Div) => {}
            Expr::Sub(Sub) => {}
            Expr::Neg(Neg) => {}
            Expr::Not(Not) => {}
            Expr::Bs(Bs) => {}
            Expr::Vec(Vector) => {}
            Expr::Type(Type) => {}
            Expr::Variant(Variant) => {}
            Expr::AddAssign(AddAssign) => {}
        }
    }
}

impl<'i> Checker<'i> {
    fn resolve(&mut self, name: &Ident) -> Object<'_> {}

    fn ty_null(&mut self) -> Object {
        Object::Tuple(vec![])
    }
}

enum Object<'i> {
    Trait,
    Module,
    Value(TypeRef<'i>),
    Place(TypeRef<'i>),
    Constant(TypeRef<'i>),
    Type(TypeRef<'i>),
}
