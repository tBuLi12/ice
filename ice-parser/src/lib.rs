use std::{io, mem};

use ast::*;
use iiv::{diagnostics, Ctx, Span};
use lexer::{Keyword, Lexer, Punctuation, Token};

mod lexer;

pub struct Parser<'i, R> {
    lexer: Lexer<'i, R>,
    messages: &'i iiv::diagnostics::Diagnostics,
    current: Token<'i>,
}

trait AsSyntaxError {
    fn expected(self, message: &'static str) -> Self;
}

#[derive(Debug)]
enum ParseError {
    NoMatch,
    InvalidSyntax(&'static str),
}

type Parsed<T> = Result<T, ParseError>;

impl<T> AsSyntaxError for Parsed<T> {
    fn expected(self, message: &'static str) -> Self {
        self.map_err(|e| match e {
            ParseError::InvalidSyntax(msg) => ParseError::InvalidSyntax(msg),
            ParseError::NoMatch => ParseError::InvalidSyntax(message),
        })
    }
}

macro_rules! get_token {
    ($name:ident.$tok:ident) => {
        match $name.current {
            Token::$tok(value) => {
                $name.next_token();
                Ok(value)
            }
            _ => Err(ParseError::NoMatch),
        }
    };
}

impl<'i, R: io::Read> Parser<'i, R> {
    pub fn new(ctx: &'i Ctx<'i>, source: R) -> Self {
        let mut parser = Parser {
            lexer: Lexer::new(ctx, source),
            messages: &ctx.diagnostcs,
            current: Token::Eof(Span {
                first_line: 0,
                last_line: 0,
                begin_offset: 0,
                begin_highlight_offset: 0,
                end_highlight_offset: 0,
            }),
        };
        parser.next_token();
        parser
    }

    pub fn parse_program(&mut self) -> Module<'i> {
        let mut funs = vec![];
        loop {
            match self.parse_function() {
                Ok(fun) => funs.push(fun),
                Err(ParseError::NoMatch) => break,
                Err(ParseError::InvalidSyntax(msg)) => self
                    .messages
                    .add(diagnostics::error(&self.current.span(), msg.to_string())),
            }
        }

        Module {
            imports: vec![],
            functions: funs,
            types: vec![],
            traits: vec![],
            impls: vec![],
            trait_impls: vec![],
        }
    }

    fn next_token(&mut self) -> Token<'i> {
        mem::replace(&mut self.current, self.lexer.next())
    }

    fn eat_kw(&mut self, keyword: Keyword) -> Parsed<Span> {
        match self.current {
            Token::Keyword(kw, span) if kw == keyword => {
                self.next_token();
                Ok(span)
            }
            _ => Err(ParseError::NoMatch),
        }
    }

    fn eat_punct(&mut self, punctuation: Punctuation) -> Parsed<Span> {
        match self.current {
            Token::Punctuation(punct, span) if punct == punctuation => {
                self.next_token();
                Ok(span)
            }
            _ => Err(ParseError::NoMatch),
        }
    }

    fn ident(&mut self) -> Parsed<Ident<'i>> {
        get_token!(self.Ident)
    }

    fn int_lit(&mut self) -> Parsed<Int> {
        get_token!(self.Int)
    }

    fn str_lit(&mut self) -> Parsed<StringLit<'i>> {
        get_token!(self.String)
    }

    fn list<T>(
        &mut self,
        separator: Punctuation,
        mut fun: impl FnMut(&mut Self) -> Parsed<T>,
    ) -> Parsed<(Vec<T>, bool)> {
        let mut items = vec![];

        loop {
            match fun(self) {
                Ok(inner) => items.push(inner),
                Err(ParseError::NoMatch) => {
                    let len = items.len() > 0;
                    return Ok((items, len));
                }
                Err(err) => return Err(err),
            }
            if self.eat_punct(separator).is_err() {
                return Ok((items, false));
            }
        }
    }

    fn any_parens<T>(
        &mut self,
        left: Punctuation,
        right: Punctuation,
        closing: &'static str,
        fun: impl FnOnce(&mut Self) -> Parsed<T>,
    ) -> Parsed<(T, Span)> {
        let lspan = self.eat_punct(left)?;
        let inner = fun(self)?;
        let rspan = self.eat_punct(right).expected(")")?;
        Ok((inner, lspan.to(rspan)))
    }

    fn parens<T>(&mut self, fun: impl FnOnce(&mut Self) -> Parsed<T>) -> Parsed<(T, Span)> {
        self.any_parens(Punctuation::LParen, Punctuation::RParen, ")", fun)
    }

    fn braces<T>(&mut self, fun: impl FnOnce(&mut Self) -> Parsed<T>) -> Parsed<(T, Span)> {
        self.any_parens(Punctuation::LBrace, Punctuation::RBrace, "}", fun)
    }

    fn opt<T>(&mut self, fun: impl FnOnce(&mut Self) -> Parsed<T>) -> Parsed<Option<T>> {
        match fun(self) {
            Ok(val) => Ok(Some(val)),
            Err(ParseError::NoMatch) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn parse_signature(&mut self) -> Parsed<Signature<'i>> {
        let fun = self.eat_kw(Keyword::Fun)?;
        let name = self.ident().expected("function name")?;
        let ((params, _), _) = self
            .parens(|p| {
                p.list(Punctuation::Comma, |p| {
                    let name = p.ident().expected("argument name")?;
                    p.eat_punct(Punctuation::Colon).expected(":")?;
                    let ty = p.parse_ty_name().expected("argument type")?;
                    Ok(Parameter { name, ty })
                })
            })
            .expected("function arguments")?;

        let return_ty = self.opt(|p| {
            p.eat_punct(Punctuation::Colon)?;
            p.parse_ty_name()
        })?;

        Ok(Signature {
            span: fun,
            name,
            is_mut: false,
            params,
            return_ty,
            visibility: Visibility::Public,
            type_params: vec![],
        })
    }

    fn parse_function(&mut self) -> Parsed<Function<'i>> {
        let signature = self.parse_signature()?;
        let body = if self.eat_punct(Punctuation::ThinArrow).is_ok() {
            self.parse_expr().expected("an expression")?
        } else {
            dbg!(self.current);
            self.parse_block().expected("-> or a block")?
        };

        Ok(Function { signature, body })
    }

    fn parse_block(&mut self) -> Parsed<Expr<'i>> {
        let ((exprs, trailing_separator), span) =
            self.braces(|p| p.list(Punctuation::Colon, |p| p.parse_block_item()))?;

        let len = exprs.len();
        Ok(Expr::Block(Block {
            span,
            items: exprs,
            has_trailing_expression: len > 0 && !trailing_separator,
        }))
    }

    fn parse_block_item(&mut self) -> Parsed<BlockItem<'i>> {
        Ok(BlockItem::Expr(self.parse_expr()?))
    }

    fn parse_ty_name(&mut self) -> Parsed<Expr<'i>> {
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> Parsed<Expr<'i>> {
        let mut expr = self
            .ident()
            .map(Expr::Variable)
            .or_else(|_| self.int_lit().map(Expr::Int))
            .or_else(|_| self.str_lit().map(Expr::String))?;

        loop {
            if self.eat_punct(Punctuation::Plus).is_ok() {
                let rhs = self.parse_expr().expected("an expression")?;
                expr = Expr::Add(Add {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                })
            } else {
                return Ok(expr);
            }
        }
    }
}
