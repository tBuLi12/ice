use crate::{str::Str, ty::TypeRef};

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TypeDecl<'i> {
    pub name: Str<'i>,
    pub ty_params: Vec<()>,
    pub proto: TypeRef<'i>,
}
