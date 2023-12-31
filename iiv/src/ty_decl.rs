use crate::{fun::Method, str::Str, ty::TypeRef};

#[derive(Hash, Debug)]
pub struct TypeDecl<'i> {
    pub name: Str<'i>,
    pub ty_params: Vec<()>,
    pub proto: TypeRef<'i>,
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TraitDecl<'i> {
    pub name: Str<'i>,
    pub ty_params: Vec<()>,
    pub signatures: Vec<Method<'i>>,
}
