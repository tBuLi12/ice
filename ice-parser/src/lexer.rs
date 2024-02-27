use std::{error::Error, io};

use ast::{Ident, Int, StringLit};
use iiv::{err, str::StrPool, Position, Span};

pub struct Lexer<'i, S> {
    str_pool: &'i StrPool<'i>,
    messages: &'i iiv::diagnostics::Diagnostics,
    pub cursor_position: Option<Position>,
    pub completion_token: Option<Token<'i>>,
    source: S,
    current: Option<char>,
    offset: u32,
    column: u32,
    line: u32,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Keyword {
    Type,
    Data,
    Trait,
    Def,
    Fun,
    As,
    If,
    Else,
    While,
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
    Asterisk,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Period,
    Semicolon,
    ThinArrow,
    Minus,
    Eq,
    DoubleEq,
    NotEq,
    Less,
    Bang,
    LessEq,
    Greater,
    GreaterEq,
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

impl<'i, S: iiv::Source> Lexer<'i, S> {
    pub fn new(ctx: &'i iiv::Ctx<'i>, mut source: S) -> Self {
        let first = match source.next() {
            Err(error) => {
                ctx.diagnostcs.add_plain_err(&*error);
                None
            }
            Ok(first) => first,
        };
        Lexer {
            str_pool: &ctx.type_pool.str_pool,
            messages: &ctx.diagnostcs,
            source,
            current: first,
            offset: 0,
            column: 0,
            line: 0,
            cursor_position: None,
            completion_token: None,
        }
    }

    pub fn next(&mut self) -> Token<'i> {
        loop {
            match self.current {
                Some(c) if c.is_whitespace() => {
                    let update = if let Some(Position { line, column }) = self.cursor_position {
                        line == self.line && column == self.column
                    } else {
                        false
                    };
                    self.read_char();
                    if update {
                        self.cursor_position = Some(Position {
                            line: self.line,
                            column: self.column,
                        });
                    }
                }
                _ => break,
            }
        }

        if self.current.is_none() {
            return Token::Eof(self.current_position().extend_back(1));
        }

        let token = self
            .read_ident_or_keyword()
            .or_else(|| self.read_string_literal())
            .or_else(|| self.read_punctuation())
            .or_else(|| self.read_numeric_literal())
            .unwrap_or_else(|| {
                self.messages.add(err!(
                    &self.current_position().extend_back(1),
                    "unexpected character"
                ));
                self.read_char();
                self.next()
            });

        if let Some(Position { line, column }) = self.cursor_position {
            let span = token.span();
            if span.contains(Position { line, column }) {
                self.completion_token = Some(token);
            }
        }
        token
    }

    fn current_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
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

        let span = self.current_position().extend_back(ident.len() as u32);

        Some(match &ident[..] {
            "data" => Token::Keyword(Keyword::Data, span),
            "type" => Token::Keyword(Keyword::Type, span),
            "trait" => Token::Keyword(Keyword::Trait, span),
            "def" => Token::Keyword(Keyword::Def, span),
            "as" => Token::Keyword(Keyword::As, span),
            "fun" => Token::Keyword(Keyword::Fun, span),
            "if" => Token::Keyword(Keyword::If, span),
            "else" => Token::Keyword(Keyword::Else, span),
            "while" => Token::Keyword(Keyword::While, span),
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
                    self.current_position().extend_back($len),
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
                            self.current_position().extend_back($depth),
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
            '*' => Asterisk,
            '(' => LParen,
            ')' => RParen,
            '{' => LBrace,
            '}' => RBrace,
            '[' => LBracket,
            ']' => RBracket,
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
            '!' => Bang {
                '=' => NotEq,
            },
            '<' => Less {
                '=' => LessEq,
            },
            '>' => Greater {
                '=' => GreaterEq,
            },
        };
        if punct.is_none() {
            self.read_char();
            self.messages.add(err!(
                &self.current_position().extend_back(1),
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
            span: self.current_position().extend_back(len),
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
                        span: self.current_position().extend_back(value.len() as u32),
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
            Err(error) => {
                self.messages.add_plain_err(&*error);
                None
            }
            Ok(character) => character,
        };

        old
    }
}
