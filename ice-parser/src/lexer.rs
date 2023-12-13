use std::io;

use ast::{Ident, Int, StringLit};
use iiv::{err, str::StrPool, Span};

pub struct Lexer<'i, R> {
    str_pool: &'i StrPool<'i>,
    messages: &'i iiv::diagnostics::Diagnostics,
    source: io::Bytes<R>,
    current: Option<char>,
    offset: u32,
    column: u32,
    line: u32,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Keyword {
    Fun,
    If,
    Else,
    Let,
    Var,
    Const,
    Is,
    True,
    False,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Punctuation {
    Plus,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Period,
    Semicolon,
    ThinArrow,
    Minus,
    Eq,
    DoubleEq,
    Pipe,
    Et,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Token<'i> {
    Ident(Ident<'i>),
    Int(Int),
    String(StringLit<'i>),
    Keyword(Keyword, Span),
    Punctuation(Punctuation, Span),
    Eof(Span),
}

impl<'i> Token<'i> {
    pub fn span(&self) -> Span {
        match self {
            Self::Ident(v) => v.span,
            Self::Int(v) => v.span,
            Self::String(v) => v.span,
            Self::Keyword(_, s) => *s,
            Self::Punctuation(_, s) => *s,
            Self::Eof(s) => *s,
        }
    }
}

impl<'i, R: io::Read> Lexer<'i, R> {
    pub fn new(ctx: &'i iiv::Ctx<'i>, source: R) -> Self {
        let mut bytes = source.bytes();
        let first = bytes.next().map(|o| o.unwrap().into());
        Lexer {
            str_pool: &ctx.type_pool.str_pool,
            messages: &ctx.diagnostcs,
            source: bytes,
            current: first,
            offset: 0,
            column: 0,
            line: 0,
        }
    }

    pub fn next(&mut self) -> Token<'i> {
        loop {
            match self.current {
                Some(c) if c.is_whitespace() => {
                    self.read_char();
                }
                _ => break,
            }
        }

        self.read_ident_or_keyword()
            .or_else(|| self.read_punctuation())
            .or_else(|| self.read_numeric_literal())
            .or_else(|| self.read_string_literal())
            .unwrap_or(Token::Eof(self.current_span()))
    }

    fn current_span(&self) -> Span {
        Span {
            first_line: self.line,
            last_line: self.line,
            begin_offset: self.offset - self.column,
            begin_highlight_offset: self.column,
            end_highlight_offset: self.column,
        }
    }

    fn read_ident_or_keyword(&mut self) -> Option<Token<'i>> {
        if !self.current?.is_alphabetic() {
            return None;
        }

        let mut ident = String::from(self.read_char()?);

        while let Some(char) = self.current {
            if !char.is_alphanumeric() {
                break;
            }
            ident.push(char);
            self.read_char();
        }

        let span = self.current_span().extend_back(ident.len() as u32);

        Some(match &ident[..] {
            "fun" => Token::Keyword(Keyword::Fun, span),
            "if" => Token::Keyword(Keyword::If, span),
            "else" => Token::Keyword(Keyword::Else, span),
            "let" => Token::Keyword(Keyword::Let, span),
            "var" => Token::Keyword(Keyword::Var, span),
            "const" => Token::Keyword(Keyword::Const, span),
            "is" => Token::Keyword(Keyword::Is, span),
            "true" => Token::Keyword(Keyword::True, span),
            "false" => Token::Keyword(Keyword::False, span),
            _ => Token::Ident(Ident {
                span,
                value: self.str_pool.get(&ident),
            }),
        })
    }

    fn read_punctuation(&mut self) -> Option<Token<'i>> {
        if !self.current?.is_ascii_punctuation() {
            return None;
        }

        macro_rules! punct {
            ($name:ident, $len:expr) => {
                Some(Token::Punctuation(
                    Punctuation::$name,
                    self.current_span().extend_back($len),
                ))
            };
        }

        macro_rules! punct_impl {
            ([$char:literal => $name:ident , $($rest:tt)*] [$($out:tt)*] $none:expr, $depth:expr) => {
                punct_impl!([$($rest)*] [$($out)*
                    Some($char) => {
                        self.read_char();
                        punct!($name, $depth)
                    },
                ] $none, $depth)
            };

            ([$char:literal => $name:ident { $($nested:tt)* } , $($rest:tt)*] [$($out:tt)*] $none:expr, $depth:expr) => {
                punct_impl!([$($rest)*] [$($out)*
                    Some($char) => {
                        self.read_char();
                        punct_impl!([$($nested)*] [] Some(Token::Punctuation(
                            Punctuation::$name,
                            self.current_span().extend_back($depth),
                        )), $depth + 1)
                    },
                ] $none, $depth)
            };

            ([] [$($out:tt)*] $none:expr, $depth:expr) => {
                match self.current {
                    $($out)*
                    _ => $none,
                }
            };
        }

        macro_rules! puncts {
            ($($rest:tt)*) => {
                punct_impl!([$($rest)*] [] None, 1)
            };
        }

        let punct = puncts! {
            '+' => Plus,
            '(' => LParen,
            ')' => RParen,
            '{' => LBrace,
            '}' => RBrace,
            ':' => Colon,
            ';' => Semicolon,
            ',' => Comma,
            '.' => Period,
            '|' => Pipe,
            '&' => Et,
            '-' => Minus {
                '>' => ThinArrow,
            },
            '=' => Eq {
                '=' => DoubleEq,
            },
        };
        if punct.is_none() {
            self.read_char();
            self.messages.add(err!(
                &self.current_span().extend_back(1),
                "unknown punctuation"
            ));
            return Some(self.next());
        }
        punct
    }

    fn read_numeric_literal(&mut self) -> Option<Token<'i>> {
        if !self.current?.is_digit(10) {
            return None;
        }

        let mut value = 0;
        let mut len = 0;
        while let Some(Some(digit)) = self.current.map(|c| c.to_digit(10)) {
            self.read_char();
            len += 1;
            value *= 10;
            value += digit;
        }

        Some(Token::Int(Int {
            span: self.current_span().extend_back(len),
            value,
        }))
    }

    fn read_string_literal(&mut self) -> Option<Token<'i>> {
        if self.current != Some('"') {
            return None;
        }

        self.read_char();
        let mut value = String::new();

        loop {
            match self.read_char() {
                Some('"') | None => {
                    return Some(Token::String(StringLit {
                        value: self.str_pool.get(&value),
                        span: self.current_span().extend_back(value.len() as u32),
                    }));
                }
                Some(character) => value.push(character),
            }
        }
    }

    fn read_char(&mut self) -> Option<char> {
        let old = self.current;

        if let Some(char) = old {
            self.offset += 1;
            if char == '\n' {
                self.column = 0;
                self.line += 1;
            } else {
                self.column += 1;
            }
        }

        self.current = match self.source.next() {
            None => None,
            Some(Err(e)) => panic!("{}", e),
            Some(Ok(byte)) => Some(byte.into()),
        };

        old
    }
}
