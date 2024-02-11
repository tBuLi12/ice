use std::{fmt, ops::Deref};

use crate::{
    fun::Bound,
    pool::{self, List, TraitDeclRef, TyDeclRef},
    str::{Str, StrPool},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct TraitRef<'i>(pub TraitDeclRef<'i>, pub List<'i, TypeRef<'i>>);

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
    Ptr(TypeRef<'i>),
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

    pub fn get_trait_method_ty_args(
        &'i self,
        this: TypeRef<'i>,
        trait_args: Vec<TypeRef<'i>>,
        fun_args: Vec<TypeRef<'i>>,
    ) -> List<'i, TypeRef<'i>> {
        self.ty_list_pool.get(
            std::iter::once(this)
                .chain(trait_args)
                .chain(fun_args)
                .collect(),
        )
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

    pub fn get_ptr(&'i self, elem_ty: TypeRef<'i>) -> TypeRef<'i> {
        TypeRef(self.ty_pool.get(Type::Ptr(elem_ty)))
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

    pub fn resolve_ty_list_args(
        &'i self,
        ty_list: List<'i, TypeRef<'i>>,
        args: &[TypeRef<'i>],
    ) -> List<'i, TypeRef<'i>> {
        self.get_ty_list(
            ty_list
                .iter()
                .map(|&ty| self.resolve_ty_args(ty, args))
                .collect(),
        )
    }

    pub fn resolve_bound(&'i self, mut bound: Bound<'i>, args: &[TypeRef<'i>]) -> Bound<'i> {
        bound.ty = self.resolve_ty_args(bound.ty, &args);
        bound.tr.1 = self.resolve_ty_list_args(bound.tr.1, &args);
        bound
    }

    // pub fn resolve_trait_args(&'i self, ty: TraitRef<'i>, args: &[TypeRef<'i>]) -> TraitRef<'i> {

    // }

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
            Type::Vector(_) => unimplemented!(),
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
            Type::Ptr(pointee) => self.get_ptr(self.resolve_ty_args(*pointee, args)),
            Type::Named(decl, ty_args, _) => self.get_ty_named(
                *decl,
                ty_args
                    .iter()
                    .map(|&ty| self.resolve_ty_args(ty, args))
                    .collect(),
            ),
            Type::Type(_ty) => unimplemented!(),
            Type::Constant(idx) => args[*idx],
            Type::InferenceVar(_) => panic!("type variable out of context"),
        }
    }
}

impl<'i> TypeRef<'i> {
    pub fn visit(self, mut fun: impl FnMut(TypeRef<'i>)) {
        fun(self);
        match &*self {
            Type::Tuple(elems) | Type::Union(elems) | Type::Named(_, elems, _) => {
                elems.iter().for_each(|ty| fun(*ty));
            }
            Type::Struct(elems) | Type::Variant(elems) => {
                elems.iter().for_each(|prop| fun(prop.1));
            }
            Type::Ref(ty) | Type::Ptr(ty) => {
                fun(*ty);
            }
            _ => {}
        }
    }

    pub fn elem(&self, i: u8) -> Option<TypeRef<'i>> {
        match *self.0 {
            Type::Struct(props) => Some(props[i as usize].1),
            Type::Variant(elems) => Some(elems[i as usize].1),
            _ => None,
        }
    }

    pub fn has_primitive_repr(self) -> bool {
        match *self {
            Type::Ref(_) | Type::Ptr(_) | Type::Constant(_) | Type::Builtin(_) => true,
            _ => false,
        }
    }

    pub fn contains(&self, mut fun: impl FnMut(Self) -> bool) -> bool {
        fun(*self)
            || match &**self {
                Type::Builtin(_) | Type::Constant(_) | Type::Invalid => false,
                Type::Vector(_) => unimplemented!(),
                Type::Union(types) | Type::Tuple(types) | Type::Named(_, types, _) => {
                    types.iter().any(|&ty| fun(ty))
                }

                Type::Variant(props) | Type::Struct(props) => props.iter().any(|prop| fun(prop.1)),
                Type::Ref(ty) | Type::Ptr(ty) => fun(*ty),
                Type::InferenceVar(_) => false,
                Type::Type(_) => {
                    unimplemented!()
                }
            }
    }
}

impl<'i> fmt::Display for TraitRef<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}",
            self.0.borrow().name,
            crate::diagnostics::fmt::List(self.1.iter())
        )
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
            Type::Ref(pointee) => write!(f, "&{}", pointee),
            Type::Ptr(pointee) => write!(f, "*{}", pointee),
            Type::Named(decl, args, _) => {
                write!(
                    f,
                    "{}{}",
                    decl.name,
                    crate::diagnostics::fmt::List(args.iter())
                )
            }
            Type::Type(_ty) => write!(f, "type"),
            Type::Constant(val) => write!(f, "T{}", val),
            Type::InferenceVar(_) => write!(f, "_"),
            Type::Builtin(BuiltinType::Bool) => write!(f, "bool"),
            Type::Builtin(BuiltinType::Int) => write!(f, "int"),
            Type::Builtin(BuiltinType::Null) => write!(f, "null"),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum TypeOverlap {
    None,
    Partial,
    Complete,
}

impl<'i> TypeRef<'i> {
    pub fn get_intersection(self, other: TypeRef<'i>) -> TypeOverlap {
        match (&*self, &*other) {
            (Type::Builtin(types), Type::Builtin(types2)) => {
                if types == types2 {
                    TypeOverlap::Complete
                } else {
                    TypeOverlap::None
                }
            }
            (Type::Vector(ty), Type::Vector(ty2)) => ty.get_intersection(*ty2),
            (Type::Tuple(t1), Type::Tuple(t2)) | (Type::Union(t1), Type::Union(t2)) => {
                if t1.len() != t2.len() {
                    return TypeOverlap::None;
                }
                t1.iter()
                    .zip(t2.iter())
                    .map(|(&p1, &p2)| p1.get_intersection(p2))
                    .fold(TypeOverlap::Complete, std::cmp::min)
            }
            (Type::Struct(props), Type::Struct(props2))
            | (Type::Variant(props), Type::Variant(props2)) => {
                if props.len() != props2.len() {
                    return TypeOverlap::None;
                }
                props
                    .iter()
                    .zip(props2.iter())
                    .map(|(&p1, &p2)| {
                        if p1.0 != p2.0 {
                            return TypeOverlap::None;
                        }
                        p1.1.get_intersection(p2.1)
                    })
                    .fold(TypeOverlap::Complete, std::cmp::min)
            }
            (Type::Ref(ty), Type::Ref(ty2)) | (Type::Ptr(ty), Type::Ptr(ty2)) => {
                ty.get_intersection(*ty2)
            }
            (Type::Named(decl1, args1, _), Type::Named(decl2, args2, _)) => {
                if decl1 != decl2 || args1.len() != args2.len() {
                    TypeOverlap::None
                } else {
                    args1
                        .iter()
                        .zip(args2.iter())
                        .map(|(&t1, &t2)| t1.get_intersection(t2))
                        .fold(TypeOverlap::Complete, std::cmp::min)
                }
            }
            (Type::Type(s1), Type::Type(s2)) => {
                if s1 == s2 {
                    TypeOverlap::Complete
                } else {
                    TypeOverlap::None
                }
            }
            (Type::Constant(val), Type::Constant(val2)) => {
                if val == val2 {
                    TypeOverlap::Complete
                } else {
                    TypeOverlap::Partial
                }
            }
            (_, Type::Constant(_)) | (Type::Constant(_), _) => TypeOverlap::Partial,
            (_, Type::InferenceVar(_)) | (Type::InferenceVar(_), _) => TypeOverlap::Partial,
            // the rest is listed explicitly to cause errors when more kinds are added, instead of silently returning None
            (Type::Invalid, _)
            | (Type::Tuple(_), _)
            | (Type::Struct(_), _)
            | (Type::Vector(_), _)
            | (Type::Union(_), _)
            | (Type::Named(_, _, _), _)
            | (Type::Variant(_), _)
            | (Type::Ref(_), _)
            | (Type::Ptr(_), _)
            | (Type::Type(_), _)
            | (Type::Builtin(_), _) => TypeOverlap::None,
        }
    }
}
