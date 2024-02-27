use crate::diagnostics::Diagnostics;

pub trait IceError {
    fn emit(&self, diagnostics: &Diagnostics);
}

impl<'i, T: IceError + 'i> From<T> for AnyError<'i> {
    fn from(value: T) -> Self {
        Box::new(value)
    }
}
pub type AnyError<'i> = Box<dyn IceError + 'i>;
