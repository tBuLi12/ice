use crate::{
    fun::{Bound, Method},
    str::Str,
    ty::{TraitRef, TypeRef},
};

#[derive(Hash, Debug)]
pub struct TypeDecl<'i> {
    pub name: Str<'i>,
    pub is_copy: bool,
    pub ty_params: Vec<()>,
    pub proto: TypeRef<'i>,
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TraitDecl<'i> {
    pub name: Str<'i>,
    pub ty_params: Vec<()>,
    pub trait_bounds: Vec<Bound<'i>>,
    pub signatures: Vec<Method<'i>>,
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct TraitImpl<'i> {
    pub ty: TypeRef<'i>,
    pub tr: TraitRef<'i>,
    pub ty_params: Vec<()>,
    pub trait_bounds: Vec<Bound<'i>>,
    pub functions: Vec<Method<'i>>,
}
