use std::{
    cell::UnsafeCell,
    io::{BufReader, Read, Seek, SeekFrom},
};

use crate::{Source, Span};

// pub trait Error {
//     fn span(&self) -> ast::Span;
// }

// pub struct AnyError(Box<dyn Error>);

#[derive(Debug)]
enum Level {
    Error,
    Warn,
    Note,
}

#[derive(Debug)]
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

pub struct Diagnostics(UnsafeCell<Vec<Diagnostic>>);

mod fmt {
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

    #[derive(Clone, Copy)]
    pub struct SideNumber {
        margin: u32,
        n: u32,
    }

    impl Display for SideNumber {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}{}",
                self.n,
                Margin(self.margin - n_of_digits(self.n + 1))
            )
        }
    }

    #[derive(Clone, Copy)]
    pub struct Squiggles(pub Span);

    impl Display for Squiggles {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}{}{}{}",
                Repeat(self.0.begin_highlight_offset, ' '),
                color::RED,
                Repeat(
                    self.0.end_highlight_offset - self.0.begin_highlight_offset,
                    '^'
                ),
                color::RESET
            )
        }
    }
}

impl Diagnostics {
    pub fn new() -> Self {
        Diagnostics(UnsafeCell::new(vec![]))
    }

    pub fn add(&self, message: Diagnostic) {
        let messages = unsafe { &mut *self.0.get() };
        messages.push(message);
    }

    pub fn print_all(&self, source: &Source) -> bool {
        let messages = unsafe { &mut *self.0.get() };
        let has_errors = messages.len() != 0;
        for Diagnostic {
            message,
            span,
            level,
        } in messages
        {
            println!("printing: {:?}", span);
            let margin = fmt::Margin(fmt::n_of_digits(span.last_line + 1));
            eprintln!("{}", message);
            eprintln!("{}", margin);

            Seek::seek(&mut &source.file, SeekFrom::Start(span.begin_offset as u64)).unwrap();
            let mut bytes = BufReader::new(&source.file).bytes();
            let mut current_line = span.first_line;
            while current_line <= span.last_line {
                eprint!("{}", fmt::color::RESET);
                eprint!("{}", margin.with_number(current_line + 1));
                if current_line > span.first_line {
                    eprint!("{}", fmt::color::RED);
                }

                let mut column = 0;
                while let Some(byte) = bytes.next() {
                    let byte = char::from(byte.unwrap());
                    if byte == '\n' {
                        print!("newline {}", column);
                        break;
                    }

                    if current_line == span.first_line && column == span.begin_highlight_offset {
                        eprint!("{}", fmt::color::RED);
                    } else if current_line == span.last_line && column == span.end_highlight_offset
                    {
                        eprint!("{}", fmt::color::RESET);
                    }

                    eprint!("{}", byte);
                    column += 1;
                }

                eprint!("\n");
                current_line += 1;
            }

            eprint!("{}{}", fmt::color::RESET, margin);

            if span.first_line == span.last_line {
                eprint!("{}", fmt::Squiggles(*span));
            }

            eprintln!("");
            eprintln!(
                "{}@ {}:{}:{}",
                fmt::Repeat(margin.0, ' '),
                &source.name,
                span.first_line + 1,
                span.begin_highlight_offset + 1
            );
            eprintln!("");
        }
        has_errors
    }
}
