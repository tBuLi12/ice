use crate::{pool, str::Str, ty::TypeRef, Instruction};

pub struct Signature<'i> {
    pub name: Str<'i>,
    pub params: pool::List<'i, TypeRef<'i>>,
    pub ret_ty: TypeRef<'i>,
}

pub struct Function<'i> {
    pub sig: Signature<'i>,
    pub body: Vec<Vec<Instruction<'i>>>,
}
