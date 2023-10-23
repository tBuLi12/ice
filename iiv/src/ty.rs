use std::ops::BitAnd;

use crate::{
    pool::{self, List},
    str::{Str, StrPool},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeRef<'i>(pool::Ref<'i, Type<'i>>);

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ShapeRef<'i>(pool::Ref<'i, Shape<'i>>);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PropRef<'i>(pool::Ref<'i, Prop<'i>>);

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

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinType {
    Null,
    Int,
    Bool,
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'i> {
    Tuple(List<'i, TypeRef<'i>>),
    Struct(List<'i, PropRef<'i>>),
    Vector(TypeRef<'i>),
    Union(List<'i, TypeRef<'i>>),
    Variant(List<'i, PropRef<'i>>),
    Ref(TypeRef<'i>),
    // Named(Named<'i>),
    Type(ShapeRef<'i>),
    Constant(/* sth goes here I guess */),
    Builtin(BuiltinType), // Named(Named<'i>),
    Invalid,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Shape<'i> {
    Any,
    Tuple(ShapeRef<'i>),
    Struct(ShapeRef<'i>),
    Variant(ShapeRef<'i>),
    Union(ShapeRef<'i>),
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Prop<'i>(Str<'i>, TypeRef<'i>);

pub struct Pool<'i> {
    ty_pool: pool::Pool<'i, Type<'i>>,
    ty_list_pool: pool::ListPool<'i, TypeRef<'i>>,
    prop_pool: pool::Pool<'i, Prop<'i>>,
    prop_list_pool: pool::ListPool<'i, PropRef<'i>>,
    str_pool: &'i StrPool<'i>,
}

impl<'i> Pool<'i> {
    pub fn get_tuple(&'i self, types: Vec<TypeRef<'i>>) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Tuple(self.get_ty_list(types))))
    }

    fn get_ty_list(&'i self, types: Vec<TypeRef<'i>>) -> List<'i, TypeRef<'i>> {
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
}

impl<'i> TypeRef<'i> {
    pub fn prop(&self, name: Str<'i>) -> TypeRef<'i> {
        match *self.0 {
            Type::Struct(props) => {
                props
                    .into_iter()
                    .find(|&&prop| prop.0 .0 == name)
                    .unwrap()
                    .0
                     .1
            }
            _ => panic!(),
        }
    }

    pub fn intersects(self, other: TypeRef<'i>) -> TypeOverlap {
        match (*self.0, *other.0) {
            (Type::Tuple(types), Type::Tuple(types2)) => overlap_list(types, types2),
            (Type::Struct(props), Type::Struct(props2)) => overlap_props(props, props2),
            (Type::Vector(ty), Type::Vector(ty2)) => ty.intersects(ty2),
            (Type::Union(ty), Type::Union(ty2)) => overlap_list(ty, ty2),
            (Type::Variant(props), Type::Variant(props2)) => overlap_props(props, props2),
            (Type::Ref(ty), Type::Ref(ty2)) => ty.intersects(ty2),
            (Type::Type(s1), Type::Type(s2)) => if s1 == s2 { TypeOverlap::Complete } else { TypeOverlap::None },
            (Type::Constant(/* sth goes here I guess */), Type::Constant(/* sth goes here I guess */)) => TypeOverlap::Partial,
            // the rest is listed explicitly to cause errors when more kinds are added, instead of silently returning None
            (Type::Invalid, _) |
            (Type::Builtin(_), _) |
            (Type::Tuple(_), _) |
            (Type::Struct(_), _) |
            (Type::Vector(_), _) |
            (Type::Union(_), _) |
            (Type::Variant(_), _) |
            (Type::Ref(_), _) |
            (Type::Type(_), _) |
            (Type::Constant(), _) |
            (Type::Builtin(_), _) => TypeOverlap::None,
        }
    }
}

fn overlap_props(p1: List<'_, PropRef<'_>>, p2: List<'_, PropRef<'_>>) -> TypeOverlap {
    if p1.len() != p2.len() {
        return TypeOverlap::None;
    }
    p1.iter()
        .zip(p2.iter())
        .map(|(&p1, &p2)| {
            if p1.0 .0 != p2.0 .0 {
                return TypeOverlap::None;
            }
            p1.0 .1.intersects(p2.0 .1)
        })
        .fold(TypeOverlap::Complete, std::cmp::min)
}

fn overlap_list(t1: List<'_, TypeRef<'_>>, t2: List<'_, TypeRef<'_>>) -> TypeOverlap {
    if t1.len() != t2.len() {
        return TypeOverlap::None;
    }
    t1.iter()
        .zip(t2.iter())
        .map(|(&p1, &p2)| p1.intersects(p2))
        .fold(TypeOverlap::Complete, std::cmp::min)
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeOverlap {
    None,
    Partial,
    Complete,
}
