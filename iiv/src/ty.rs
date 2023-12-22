use std::{fmt, ops::Deref};

use crate::{
    pool::{self, List, TyDeclRef},
    str::{Str, StrPool},
    RawValue,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct TypeRef<'i>(pool::Ref<'i, Type<'i>>);

impl<'i> Deref for TypeRef<'i> {
    type Target = Type<'i>;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct ShapeRef<'i>(pool::Ref<'i, Shape<'i>>);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PropRef<'i>(pool::Ref<'i, Prop<'i>>);

impl<'i> Deref for PropRef<'i> {
    type Target = Prop<'i>;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<'i> PartialOrd for PropRef<'i> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match Str::partial_cmp(&self.0 .0, &other.0 .0) {
            Some(std::cmp::Ordering::Equal) => TypeRef::partial_cmp(&self.0 .1, &other.0 .1),
            ord => ord,
        }
    }
}

impl<'i> Ord for PropRef<'i> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match Str::cmp(&self.0 .0, &other.0 .0) {
            std::cmp::Ordering::Equal => TypeRef::cmp(&self.0 .1, &other.0 .1),
            ord => ord,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Named<'i> {
    pub proto: &'i Type<'i>,
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum BuiltinType {
    Null,
    Int,
    Bool,
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Type<'i> {
    Tuple(List<'i, TypeRef<'i>>),
    Struct(List<'i, PropRef<'i>>),
    Vector(TypeRef<'i>),
    Union(List<'i, TypeRef<'i>>),
    Variant(List<'i, PropRef<'i>>),
    Ref(TypeRef<'i>),
    Named(TyDeclRef<'i>, List<'i, TypeRef<'i>>, TypeRef<'i>),
    Type(ShapeRef<'i>),
    Constant(usize),
    InferenceVar(usize),
    Builtin(BuiltinType), // Named(Named<'i>),
    Invalid,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Shape<'i> {
    Any,
    Tuple(ShapeRef<'i>),
    Struct(ShapeRef<'i>),
    Variant(ShapeRef<'i>),
    Union(ShapeRef<'i>),
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Prop<'i>(pub Str<'i>, pub TypeRef<'i>);

pub struct Pool<'i> {
    ty_pool: pool::Pool<'i, Type<'i>>,
    ty_list_pool: pool::ListPool<'i, TypeRef<'i>>,
    prop_pool: pool::Pool<'i, Prop<'i>>,
    prop_list_pool: pool::ListPool<'i, PropRef<'i>>,
    shape_pool: pool::Pool<'i, Shape<'i>>,
    pub str_pool: StrPool<'i>,
}

impl<'i> Pool<'i> {
    pub fn new() -> Self {
        Pool {
            ty_pool: pool::Pool::new(),
            ty_list_pool: pool::ListPool::new(),
            prop_pool: pool::Pool::new(),
            prop_list_pool: pool::ListPool::new(),
            shape_pool: pool::Pool::new(),
            str_pool: crate::str::StrPool::new(),
        }
    }

    pub fn index_of(&'i self, value: TypeRef<'i>) -> usize {
        self.ty_pool.index_of(value.0)
    }

    pub fn len(&'i self) -> usize {
        self.ty_pool.len()
    }

    pub fn get_tuple(&'i self, types: Vec<TypeRef<'i>>) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Tuple(self.get_ty_list(types))))
    }

    pub fn get_ty_list(&'i self, types: Vec<TypeRef<'i>>) -> List<'i, TypeRef<'i>> {
        self.ty_list_pool.get(types)
    }

    fn get_ty_set(&'i self, types: Vec<TypeRef<'i>>) -> List<'i, TypeRef<'i>> {
        self.ty_list_pool.get_set(types)
    }

    pub fn get_prop_set(&'i self, props: Vec<PropRef<'i>>) -> List<'i, PropRef<'i>> {
        self.prop_list_pool.get_set(props)
    }

    pub fn get_struct(&'i self, props: Vec<PropRef<'i>>) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Struct(self.get_prop_set(props))))
    }

    pub fn get_vec(&'i self, elem_ty: TypeRef<'i>) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Vector(elem_ty)))
    }

    pub fn get_ref(&'i self, elem_ty: TypeRef<'i>) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Ref(elem_ty)))
    }

    pub fn get_union(&'i self, types: Vec<TypeRef<'i>>) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Union(self.get_ty_set(types))))
    }

    pub fn get_variant(&'i self, props: Vec<PropRef<'i>>) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Variant(self.get_prop_set(props))))
    }

    pub fn get_prop(&'i self, name: Str<'i>, ty: TypeRef<'i>) -> PropRef<'i> {
        PropRef(self.prop_pool.get(Prop(name, ty)))
    }

    pub fn get_int(&'i self) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Builtin(BuiltinType::Int)))
    }

    pub fn get_null(&'i self) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Builtin(BuiltinType::Null)))
    }

    pub fn get_ty_invalid(&'i self) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Invalid))
    }

    pub fn get_ty_never(&'i self) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Union(self.get_ty_set(vec![]))))
    }

    pub fn get_ty_bool(&'i self) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Builtin(BuiltinType::Bool)))
    }

    pub fn get_ty_type(&'i self) -> TypeRef<'i> {
        TypeRef(
            self.ty_pool
                .get(Type::Type(ShapeRef(self.shape_pool.get(Shape::Any)))),
        )
    }

    pub fn get_ty_named(&'i self, decl: TyDeclRef<'i>, args: Vec<TypeRef<'i>>) -> TypeRef<'i> {
        let proto = self.resolve_ty_args(decl.proto, &args);
        TypeRef(
            self.ty_pool
                .get(Type::Named(decl, self.ty_list_pool.get(args), proto)),
        )
    }

    pub fn get_ty_constant(&'i self, idx: usize) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Constant(idx)))
    }

    pub fn get_ty_inference_var(&'i self, idx: usize) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::InferenceVar(idx)))
    }

    pub fn resolve_ty_args(&'i self, ty: TypeRef<'i>, args: &[TypeRef<'i>]) -> TypeRef<'i> {
        match &*ty {
            Type::Invalid | Type::Builtin(_) => ty,
            Type::Tuple(fields) => self.get_tuple(
                fields
                    .iter()
                    .map(|&ty| self.resolve_ty_args(ty, args))
                    .collect(),
            ),
            Type::Struct(props) => self.get_struct(
                props
                    .iter()
                    .map(|&prop| self.get_prop(prop.0 .0, self.resolve_ty_args(prop.1, args)))
                    .collect(),
            ),
            Type::Vector(elem) => unimplemented!(),
            Type::Union(fields) => self.get_union(
                fields
                    .iter()
                    .map(|&ty| self.resolve_ty_args(ty, args))
                    .collect(),
            ),
            Type::Variant(props) => self.get_variant(
                props
                    .iter()
                    .map(|&prop| self.get_prop(prop.0 .0, self.resolve_ty_args(prop.1, args)))
                    .collect(),
            ),
            Type::Ref(pointee) => self.get_ref(self.resolve_ty_args(*pointee, args)),
            Type::Named(decl, ty_args, proto) => self.get_ty_named(
                *decl,
                ty_args
                    .iter()
                    .map(|&ty| self.resolve_ty_args(ty, args))
                    .collect(),
            ),
            Type::Type(_ty) => unimplemented!(),
            Type::Constant(idx) => args[*idx],
            Type::InferenceVar(val) => panic!("type variable out of context"),
        }
    }
}

impl<'i> TypeRef<'i> {
    pub fn elem(&self, i: u8) -> Option<TypeRef<'i>> {
        dbg!(&*self.0, i);
        match *self.0 {
            Type::Struct(props) => Some(props[i as usize].1),
            Type::Variant(elems) => Some(elems[i as usize].1),
            _ => None,
        }
    }

    pub fn prop(&self, name: Str<'i>) -> Option<(u8, TypeRef<'i>)> {
        match *self.0 {
            Type::Struct(props) | Type::Variant(props) => {
                Some(props.into_iter().enumerate().find_map(|(i, &prop)| {
                    if prop.0 .0 == name {
                        Some((i as u8, prop.0 .1))
                    } else {
                        None
                    }
                })?)
            }
            _ => None,
        }
    }

    pub fn has_primitive_repr(self) -> bool {
        match *self {
            Type::Ref(_) | Type::Constant(_) | Type::Builtin(_) => true,
            _ => false,
        }
    }
}

impl<'i> fmt::Display for TypeRef<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.0 {
            Type::Invalid => write!(f, "{{unknown}}"),
            Type::Tuple(fields) => {
                write!(f, "(")?;
                for field in fields.iter() {
                    write!(f, "{}, ", field)?;
                }
                write!(f, ")")
            }
            Type::Struct(props) => {
                write!(f, "{{")?;
                for prop in props.iter() {
                    write!(f, "{}: {}, ", prop.0 .0, prop.0 .1)?;
                }
                write!(f, "}}")
            }
            Type::Vector(elem) => write!(f, "[{}]", elem),
            Type::Union(fields) => {
                write!(f, "(")?;
                for field in fields.iter() {
                    write!(f, "{} | ", field)?;
                }
                write!(f, ")")
            }
            Type::Variant(props) => {
                write!(f, "{{")?;
                for prop in props.iter() {
                    write!(f, "{}: {} | ", prop.0 .0, prop.0 .1)?;
                }
                write!(f, "}}")
            }
            Type::Ref(pointee) => write!(f, "ref {}", pointee),
            Type::Named(decl, args, _) => {
                write!(
                    f,
                    "{}[{}]",
                    decl.name,
                    crate::diagnostics::fmt::List(args.iter())
                )
            }
            Type::Type(_ty) => write!(f, "type"),
            Type::Constant(val) => write!(f, "T{}", val),
            Type::InferenceVar(val) => write!(f, "InfVar{}", val),
            Type::Builtin(BuiltinType::Bool) => write!(f, "bool"),
            Type::Builtin(BuiltinType::Int) => write!(f, "int"),
            Type::Builtin(BuiltinType::Null) => write!(f, "null"),
        }
    }
}
