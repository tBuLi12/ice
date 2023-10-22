use crate::ty::TypeRef;

pub struct Signature<'i> {
    ret_ty: TypeRef<'i>,
}

pub struct Function<'i> {
    sig: Signature<'i>,
}
