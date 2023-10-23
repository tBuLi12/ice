use crate::ty::TypeRef;

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
    span: ast::Span,
    level: Level,
}

fn error(span: ast::Span, message: String) -> Diagnostic {
    Diagnostic {
        message,
        span,
        level: Level::Error,
    }
}

#[macro_export]
macro_rules! err {
    ($span:expr, $fmt:literal, $($val:expr),*) => {
        error($span, format!($fmt $(,$val)*))
    };
}
