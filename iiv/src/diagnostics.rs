use crate::{ty::TypeRef, Span};

// pub trait Error {
//     fn span(&self) -> ast::Span;
// }

// pub struct AnyError(Box<dyn Error>);

enum Level {
    Error,
    Warn,
    Note,
}

pub struct Diagnostic {
    message: String,
    span: Span,
    level: Level,
}

pub fn error(span: &Span, message: String) -> Diagnostic {
    Diagnostic {
        message,
        span: *span,
        level: Level::Error,
    }
}

#[macro_export]
macro_rules! err {
    ($span:expr, $fmt:literal $(, $val:expr)*) => {
        ::iiv::diagnostics::error($span, format!($fmt $(,$val)*))
    };
}
