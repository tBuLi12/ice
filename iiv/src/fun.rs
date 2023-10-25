use crate::{ty::TypeRef, Instruction};

pub struct Signature<'i> {
    pub ret_ty: TypeRef<'i>,
}

pub struct Function<'i> {
    pub sig: Signature<'i>,
    pub body: Vec<Vec<Instruction<'i>>>,
}
