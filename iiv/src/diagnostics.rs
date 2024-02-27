use std::{cell::UnsafeCell, error::Error};

use crate::{Source, Span};

#[derive(Debug)]
pub enum Level {
    Error,
    _Warn,
    _Note,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub message: String,
    pub span: Span,
    #[allow(dead_code)]
    pub level: Level,
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
        $crate::diagnostics::error($span, format!($fmt $(,$val)*))
    };
}

pub struct Diagnostics(UnsafeCell<Vec<Diagnostic>>);

pub mod fmt {
    pub mod color {
        pub const RED: &'static str = "\u{001b}[31;1m";
        pub const BLUE: &'static str = "\u{001b}[36;1m";
        pub const RESET: &'static str = "\u{001b}[0m";
    }

    use std::fmt::Display;

    use crate::Span;

    pub fn n_of_digits(n: u32) -> u32 {
        if n > 1 {
            n.ilog10() + 1
        } else {
            1
        }
    }

    #[cfg(test)]
    mod test {
        use crate::diagnostics::fmt::n_of_digits;

        #[test]
        fn test_n_of_digits() {
            assert_eq!(n_of_digits(0), 1);
            assert_eq!(n_of_digits(1), 1);
            assert_eq!(n_of_digits(9), 1);
            assert_eq!(n_of_digits(10), 2);
            assert_eq!(n_of_digits(99), 2);
            assert_eq!(n_of_digits(100), 3);
        }
    }

    pub struct List<I>(pub I);
    impl<I: Clone + Iterator> Display for List<I>
    where
        I::Item: Display,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let mut this = self.0.clone();
            let Some(first) = this.next() else {
                return Ok(());
            };
            write!(f, "[{}", first)?;
            for item in this {
                write!(f, ",{}", item)?;
            }
            write!(f, "]")
        }
    }

    #[derive(Clone, Copy)]
    pub struct Repeat(pub u32, pub char);

    impl Display for Repeat {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for _ in 0..self.0 {
                write!(f, "{}", self.1)?;
            }
            Ok(())
        }
    }

    #[derive(Clone, Copy)]
    pub struct Margin(pub u32);

    impl Display for Margin {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}| ", Repeat(self.0, ' '))
        }
    }

    impl Margin {
        pub fn with_number(self, n: u32) -> SideNumber {
            SideNumber { margin: self.0, n }
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub struct SideNumber {
        margin: u32,
        n: u32,
    }

    impl Display for SideNumber {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}", self.n, Margin(self.margin - n_of_digits(self.n)))
        }
    }

    #[derive(Clone, Copy)]
    pub struct Squiggles(pub Span);

    impl Display for Squiggles {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}{}{}{}",
                Repeat(self.0.left.column, ' '),
                color::RED,
                Repeat(self.0.right.column - self.0.left.column, '^'),
                color::RESET
            )
        }
    }
}

impl Diagnostics {
    pub fn new() -> Self {
        Diagnostics(UnsafeCell::new(vec![]))
    }

    pub fn add_plain_err(&self, error: &dyn Error) {
        self.add(err!(&Span::null(), "{}", error));
    }

    pub fn add(&self, message: Diagnostic) {
        let messages = unsafe { &mut *self.0.get() };
        messages.push(message);
    }

    pub fn ok(&self) -> bool {
        let messages = unsafe { &*self.0.get() };
        messages.is_empty()
    }

    pub fn take_all(&self) -> Vec<Diagnostic> {
        let messages = unsafe { &mut *self.0.get() };
        std::mem::replace(messages, vec![])
    }

    pub fn print_all(&self, source: &mut impl Source) -> bool {
        match self.try_print_all(source) {
            Ok(errors) => errors,
            Err(error) => {
                self.add(err!(&Span::null(), "{}", error));
                true
            }
        }
    }

    pub fn try_print_all(&self, source: &mut impl Source) -> Result<bool, Box<dyn Error>> {
        let messages = unsafe { &mut *self.0.get() };
        let has_errors = messages.len() != 0;
        for Diagnostic { message, span, .. } in messages {
            let margin = fmt::Margin(fmt::n_of_digits(span.right.line + 1));
            eprintln!("{}", message);
            eprintln!("{}", margin);

            source.seek(span.left);
            let mut current_line = span.left.line;
            while current_line <= span.right.line {
                eprint!("{}", fmt::color::RESET);
                eprint!("{}", margin.with_number(current_line + 1));
                if current_line > span.left.line {
                    eprint!("{}", fmt::color::RED);
                }

                let mut column = 0;
                while let Some(byte) = source.next()? {
                    if byte == '\n' {
                        break;
                    }

                    if current_line == span.left.line && column == span.left.column {
                        eprint!("{}", fmt::color::RED);
                    } else if current_line == span.right.line && column == span.right.column {
                        eprint!("{}", fmt::color::RESET);
                    }

                    eprint!("{}", byte);
                    column += 1;
                }

                eprint!("\n");
                current_line += 1;
            }

            eprint!("{}{}", fmt::color::RESET, margin);

            if span.left.line == span.right.line {
                eprint!("{}", fmt::Squiggles(*span));
            }

            eprintln!("");
            eprintln!(
                "{}@ {}:{}:{}",
                fmt::Repeat(margin.0, ' '),
                source.name(),
                span.left.line + 1,
                span.left.column + 1
            );
            eprintln!("");
        }
        Ok(has_errors)
    }
}
