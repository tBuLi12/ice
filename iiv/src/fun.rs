use crate::ty::TypeRef;

pub struct Signature<'i> {
    pub ret_ty: TypeRef<'i>,
}

pub struct Function<'i> {
    pub sig: Signature<'i>,
}
