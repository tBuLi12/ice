use crate::{
    pool::{self, List},
    str::{Str, StrPool},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeRef<'i>(pool::Ref<'i, Type<'i>>);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PropRef<'i>(pool::Ref<'i, (Str<'i>, TypeRef<'i>)>);

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
pub enum Type<'i> {
    Tuple(List<'i, TypeRef<'i>>),
    Struct(List<'i, PropRef<'i>>),
    Vector(TypeRef<'i>),
    Union(List<'i, TypeRef<'i>>),
    Variant(List<'i, PropRef<'i>>),
    Ref(TypeRef<'i>),
    // Named(Named<'i>),
}

pub struct Pool<'i> {
    ty_pool: pool::Pool<'i, Type<'i>>,
    ty_list_pool: pool::ListPool<'i, TypeRef<'i>>,
    prop_pool: pool::Pool<'i, (Str<'i>, TypeRef<'i>)>,
    prop_list_pool: pool::ListPool<'i, PropRef<'i>>,
    str_pool: StrPool<'i>,
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
}
