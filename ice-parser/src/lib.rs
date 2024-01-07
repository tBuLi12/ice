use std::{io, mem};

use ast::*;
use iiv::{diagnostics, err, Ctx, Span};
use lexer::{Keyword, Lexer, Punctuation, Token};

mod lexer;

pub struct Parser<'i, R> {
    lexer: Lexer<'i, R>,
    messages: &'i iiv::diagnostics::Diagnostics,
    current: Token<'i>,
    default_binding: Option<BindingType>,
}

trait AsSyntaxError: Sized {
    fn expected(self, message: &'static str) -> Self;
    fn invalid(self) -> Parsed<Self>;
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
    fn invalid(self) -> Parsed<Self> {
        match self {
            Ok(val) => Ok(Ok(val)),
            Err(ParseError::NoMatch) => Ok(Err(ParseError::NoMatch)),
            Err(e) => Err(e),
        }
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
            default_binding: None,
        };
        parser.next_token();
        parser
    }

    pub fn parse_program(&mut self) -> Module<'i> {
        let mut module = Module {
            imports: vec![],
            functions: vec![],
            types: vec![],
            traits: vec![],
            impls: vec![],
            trait_impls: vec![],
        };
        loop {
            match self.parse_declaration_into(&mut module) {
                Ok(()) => {}
                Err(ParseError::NoMatch) => break,
                Err(ParseError::InvalidSyntax(msg)) => self
                    .messages
                    .add(diagnostics::error(&self.current.span(), msg.to_string())),
            }
        }
        if !matches!(self.current, Token::Eof(_)) {
            self.messages
                .add(err!(&self.current.span(), "unexpected token"))
        }
        module
    }

    fn parse_declaration_into(&mut self, module: &mut Module<'i>) -> Parsed<()> {
        if let Ok(fun) = self.parse_function().invalid()? {
            module.functions.push(fun);
            return Ok(());
        }
        if let Ok(ty_decl) = self.parse_type_decl().invalid()? {
            module.types.push(ty_decl);
            return Ok(());
        }
        if let Ok(tr) = self.parse_trait_decl().invalid()? {
            module.traits.push(tr);
            return Ok(());
        }
        if let Ok(tr_impl) = self.parse_trait_impl().invalid()? {
            module.trait_impls.push(tr_impl);
            return Ok(());
        }
        Err(ParseError::NoMatch)
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

    fn bool_lit(&mut self) -> Parsed<Bool> {
        if let Ok(span) = self.eat_kw(Keyword::True) {
            Ok(Bool { span, value: true })
        } else if let Ok(span) = self.eat_kw(Keyword::False) {
            Ok(Bool { span, value: false })
        } else {
            Err(ParseError::NoMatch)
        }
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
        let rspan = self.eat_punct(right).expected(closing)?;
        Ok((inner, lspan.to(rspan)))
    }

    fn parens<T>(&mut self, fun: impl FnOnce(&mut Self) -> Parsed<T>) -> Parsed<(T, Span)> {
        self.any_parens(Punctuation::LParen, Punctuation::RParen, ")", fun)
    }

    fn braces<T>(&mut self, fun: impl FnOnce(&mut Self) -> Parsed<T>) -> Parsed<(T, Span)> {
        self.any_parens(Punctuation::LBrace, Punctuation::RBrace, "}", fun)
    }

    fn brackets<T>(&mut self, fun: impl FnOnce(&mut Self) -> Parsed<T>) -> Parsed<(T, Span)> {
        self.any_parens(Punctuation::LBracket, Punctuation::RBracket, "]", fun)
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
        let type_params = self.parse_type_params()?;
        let ((params, _), span) = self
            .parens(|p| {
                p.list(Punctuation::Comma, |p| {
                    let name = p.ident()?;
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
            span: if let Some(ty) = &return_ty {
                fun.to(ty.span())
            } else {
                fun.to(span)
            },
            name,
            is_mut: false,
            params,
            return_ty,
            visibility: Visibility::Public,
            type_params,
        })
    }

    fn parse_function(&mut self) -> Parsed<Function<'i>> {
        let signature = self.parse_signature()?;
        let body = if self.eat_punct(Punctuation::ThinArrow).is_ok() {
            self.parse_expr().expected("an expression")?
        } else {
            self.parse_block().expected("-> or a block")?
        };

        Ok(Function { signature, body })
    }

    fn parse_type_params(&mut self) -> Parsed<Vec<TypeParam<'i>>> {
        let type_params = self
            .opt(|p| {
                p.brackets(|p| {
                    p.list(Punctuation::Comma, |p| {
                        let name = p.ident()?;
                        Ok(TypeParam {
                            name,
                            trait_bounds: if p.eat_punct(Punctuation::Colon).is_ok() {
                                vec![p.parse_ty_name().expected("a trait bound")?]
                            } else {
                                vec![]
                            },
                        })
                    })
                    .map(|(params, _)| params)
                })
            })?
            .map(|(params, _)| params)
            .unwrap_or(vec![]);
        Ok(type_params)
    }

    fn parse_type_decl(&mut self) -> Parsed<TypeDecl<'i>> {
        let (is_data, span) = if let Ok(span) = self.eat_kw(Keyword::Data) {
            (true, span)
        } else if let Ok(span) = self.eat_kw(Keyword::Type) {
            (false, span)
        } else {
            return Err(ParseError::NoMatch);
        };
        let name = self.ident().expected("a name")?;

        let type_params = self.parse_type_params()?;

        let proto = self.parse_ty_name().expected("a prototype")?;

        let span = span.to(proto.span());
        Ok(TypeDecl {
            name,
            proto,
            proto_visibility: Visibility::Public,
            is_data,
            span,
            type_params,
            visibility: Visibility::Public,
        })
    }

    fn parse_trait_decl(&mut self) -> Parsed<TraitDecl<'i>> {
        let span = self.eat_kw(Keyword::Trait)?;
        let name = self.ident().expected("a name")?;
        let type_params = self.parse_type_params()?;

        let ((signatures, _), block_span) = self
            .braces(|p| p.list(Punctuation::Semicolon, |p| p.parse_signature()))
            .expected("signature block")?;

        let span = span.to(block_span);
        Ok(TraitDecl {
            name,
            span,
            type_params,
            visibility: Visibility::Public,
            signatures,
        })
    }

    fn parse_trait_impl(&mut self) -> Parsed<TraitImpl<'i>> {
        let span = self.eat_kw(Keyword::Def)?;
        let type_params = self.parse_type_params()?;
        let ty = self.parse_ty_name().expected("a type name")?;
        self.eat_kw(Keyword::As).expected("as")?;
        let tr = self.parse_ty_name().expected("a trait name")?;

        let ((functions, _), block_span) = self
            .braces(|p| p.list(Punctuation::Semicolon, |p| p.parse_function()))
            .expected("signature block")?;

        let span = span.to(block_span);
        Ok(TraitImpl {
            span: span.to(block_span),
            type_params,
            tr,
            ty,
            functions,
        })
    }

    fn parse_block(&mut self) -> Parsed<Expr<'i>> {
        let ((exprs, trailing_separator), span) =
            self.braces(|p| p.list(Punctuation::Semicolon, |p| p.parse_block_item()))?;

        let len = exprs.len();
        Ok(Expr::Block(Block {
            span,
            items: exprs,
            has_trailing_expression: len > 0 && !trailing_separator,
        }))
    }

    fn pattern_ident(&mut self, name: Ident<'i>) -> Pattern<'i> {
        if let Some(binding_type) = self.default_binding {
            Pattern {
                body: PatternBody::Bind(BindPattern { binding_type, name }),
                guard: None,
            }
        } else {
            Pattern {
                body: PatternBody::Bind(BindPattern {
                    binding_type: BindingType::Let,
                    name,
                }),
                guard: None,
            }
        }
    }

    fn parse_untyped_pattern(&mut self) -> Parsed<Pattern<'i>> {
        if let Ok(name) = self.ident() {
            return Ok(self.pattern_ident(name));
        }

        if let Ok(span) = self.eat_punct(Punctuation::Period) {
            let name = self.ident().expected("a variant name")?;

            let (span, inner) = if let Ok((pattern, paren_span)) = self
                .parens(|p| p.parse_pattern().expected("a pattern"))
                .invalid()?
            {
                (span.to(paren_span.span()), Some(Box::new(pattern)))
            } else {
                (span.to(name.span), None)
            };

            return Ok(Pattern {
                body: PatternBody::Variant(VariantPattern { span, name, inner }),
                guard: None,
            });
        }

        let ((struct_pattern, _), span) = self
            .braces(|p| {
                p.list(Punctuation::Comma, |p| {
                    let name = p.ident().expected("a property name")?;
                    Ok((name, p.pattern_ident(name)))
                })
            })
            .expected("a pattern")?;
        Ok(Pattern {
            guard: None,
            body: PatternBody::Struct(StructPattern {
                span,
                inner: struct_pattern,
            }),
        })
    }

    fn parse_pattern(&mut self) -> Parsed<Pattern<'i>> {
        let mut pattern = self.parse_untyped_pattern()?;
        while self.eat_punct(Punctuation::Colon).is_ok() {
            let ty = self.parse_ty_name().expected("a type")?;
            pattern = Pattern {
                guard: None,
                body: PatternBody::NarrowType(NarrowTypePattern {
                    inner: Box::new(pattern),
                    ty,
                }),
            };
        }
        Ok(pattern)
    }

    fn parse_block_item(&mut self) -> Parsed<BlockItem<'i>> {
        let binding_type = if let Ok(span) = self.eat_kw(Keyword::Let) {
            Some((span, BindingType::Let))
        } else if let Ok(span) = self.eat_kw(Keyword::Var) {
            Some((span, BindingType::Var))
        } else {
            None
        };

        if let Some((span, binding_type)) = binding_type {
            self.default_binding = Some(binding_type);
            let binding = self.parse_pattern().expected("a pattern")?;
            self.default_binding = None;

            self.eat_punct(Punctuation::Eq).expected("=")?;
            let value = self.parse_expr().expected("an initializer")?;

            return Ok(BlockItem::Bind(Binding {
                binding_type,
                span: span.to(value.span()),
                binding,
                value,
            }));
        }

        Ok(BlockItem::Expr(self.parse_expr()?))
    }

    fn parse_ty_prop(&mut self) -> Parsed<TyProp<'i>> {
        let name = self.ident()?;
        let ty = if self.eat_punct(Punctuation::Colon).is_ok() {
            let ty = self.parse_ty_name().expected("a type name")?;
            Some(ty)
        } else {
            None
        };
        Ok(TyProp { name, ty })
    }

    fn parse_struct_or_variant_ty(&mut self) -> Parsed<Expr<'i>> {
        let ((props, is_variant), span) = self.braces(|p| {
            if p.eat_punct(Punctuation::Pipe).is_ok() {
                return Ok((p.list(Punctuation::Pipe, |p| p.parse_ty_prop())?.0, true));
            }
            let first = p.parse_ty_prop()?;
            if p.eat_punct(Punctuation::Pipe).is_ok() {
                let mut prop_list = p.list(Punctuation::Pipe, |p| p.parse_ty_prop())?.0;
                prop_list.insert(0, first);
                return Ok((prop_list, true));
            }
            if p.eat_punct(Punctuation::Comma).is_ok() {
                let mut prop_list = p.list(Punctuation::Comma, |p| p.parse_ty_prop())?.0;
                prop_list.insert(0, first);
                return Ok((prop_list, false));
            }
            Ok((vec![first], false))
        })?;
        if is_variant {
            Ok(Expr::VariantTy(PropsTy { span, props }))
        } else {
            Ok(Expr::StructTy(PropsTy { span, props }))
        }
    }

    fn parse_ty_name(&mut self) -> Parsed<Expr<'i>> {
        if let Ok(ident) = self.ident() {
            let expr = Expr::Variable(ident);
            if let Ok((span, args)) = self.parse_ty_args().invalid()? {
                Ok(Expr::TyArgApply(TyArgApply {
                    span: ident.span.to(span),
                    lhs: Box::new(expr),
                    args,
                }))
            } else {
                Ok(expr)
            }
        } else if let Ok(et) = self.eat_punct(Punctuation::Et) {
            let inner = self.parse_ty_name().expected("a type name")?;
            Ok(Expr::RefTy(RefTo {
                span: et.to(inner.span()),
                rhs: Box::new(inner),
            }))
        } else if let Ok(asterisk) = self.eat_punct(Punctuation::Asterisk) {
            let inner = self.parse_ty_name().expected("a type name")?;
            Ok(Expr::PtrTy(RefTo {
                span: asterisk.to(inner.span()),
                rhs: Box::new(inner),
            }))
        } else {
            self.parse_struct_or_variant_ty()
        }
    }

    fn parse_struct_or_block(&mut self) -> Parsed<Expr<'i>> {
        enum StrucOrBlock<'i> {
            Struct(Vec<StructProp<'i>>),
            Block(Vec<BlockItem<'i>>, bool),
        }

        let (inner, span) = self.braces(|p| {
            if let Ok(ident) = p.ident() {
                if p.eat_punct(Punctuation::Colon).is_ok() {
                    let first_val = p.parse_expr().expected("an expression")?;
                    let _ = p.eat_punct(Punctuation::Comma);
                    let (mut props, _) = p.list(Punctuation::Comma, |p| {
                        let name = p.ident()?;
                        if p.eat_punct(Punctuation::Colon).is_ok() {
                            let value = p.parse_expr().expected("an expression")?;
                            return Ok(StructProp {
                                name,
                                value: Some(value),
                            });
                        }
                        Ok(StructProp { name, value: None })
                    })?;
                    props.insert(
                        0,
                        StructProp {
                            name: ident,
                            value: Some(first_val),
                        },
                    );
                    Ok(StrucOrBlock::Struct(props))
                } else {
                    let first_expr = p.parse_rhs(Expr::Variable(ident))?;
                    let semi = p.eat_punct(Punctuation::Semicolon).is_ok();
                    let (mut exprs, trailing_semi) =
                        p.list(Punctuation::Semicolon, |p| p.parse_block_item())?;
                    exprs.insert(0, BlockItem::Expr(first_expr));
                    let len = exprs.len();
                    Ok(StrucOrBlock::Block(
                        exprs,
                        !trailing_semi || (len == 1 && !semi),
                    ))
                }
            } else {
                let (exprs, trailing_semi) =
                    p.list(Punctuation::Semicolon, |p| p.parse_block_item())?;
                let len = exprs.len();
                Ok(StrucOrBlock::Block(exprs, !trailing_semi && len > 0))
            }
        })?;

        Ok(match inner {
            StrucOrBlock::Block(items, has_trailing_expression) => Expr::Block(Block {
                span,
                items,
                has_trailing_expression,
            }),
            StrucOrBlock::Struct(props) => Expr::Struct(Struct {
                span,
                props,
                ty: None,
            }),
        })
    }

    fn parse_ty_args(&mut self) -> Parsed<(Span, Vec<Expr<'i>>)> {
        self.brackets(|p| p.list(Punctuation::Comma, |p| p.parse_ty_name()))
            .invalid()?
            .map(|((args, _), span)| (span, args))
    }

    fn parse_rhs(&mut self, mut lhs: Expr<'i>) -> Parsed<Expr<'i>> {
        let mut lhs = loop {
            if self.eat_punct(Punctuation::Plus).is_ok() {
                let rhs = self.parse_expr().expected("an expression")?;
                lhs = Expr::Add(Add {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            } else if self.eat_punct(Punctuation::DoubleEq).is_ok() {
                let rhs = self.parse_expr().expected("an expression")?;
                lhs = Expr::Eq(Equals {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            } else if let Ok(((args, _), span)) = self
                .parens(|p| p.list(Punctuation::Comma, |p| p.parse_expr()))
                .invalid()?
            {
                lhs = Expr::Call(Call {
                    span: lhs.span().to(span),
                    lhs: Box::new(lhs),
                    args,
                });
            } else if let Ok((span, args)) = self.parse_ty_args().invalid()? {
                lhs = Expr::TyArgApply(TyArgApply {
                    span: lhs.span().to(span),
                    lhs: Box::new(lhs),
                    args,
                });
            } else if self.eat_punct(Punctuation::Period).is_ok() {
                if let Ok(span) = self.eat_punct(Punctuation::Asterisk) {
                    lhs = Expr::Deref(Deref {
                        span: lhs.span().to(span),
                        lhs: Box::new(lhs),
                    });
                } else {
                    let prop = self.ident().expected("property name")?;
                    lhs = Expr::Prop(Prop {
                        lhs: Box::new(lhs),
                        prop,
                        tr: None,
                    });
                }
            } else if self.eat_kw(Keyword::Is).is_ok() {
                let pattern = self.parse_pattern().expected("a pattern")?;
                lhs = Expr::Is(Is {
                    lhs: Box::new(lhs),
                    rhs: Box::new(pattern),
                })
            } else {
                break lhs;
            }
        };

        while self.eat_punct(Punctuation::Eq).is_ok() {
            let rhs = self.parse_expr().expected("an expression")?;
            lhs = Expr::Assign(Assign {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
        }

        Ok(lhs)
    }

    fn parse_if(&mut self) -> Parsed<Expr<'i>> {
        let if_span = self.eat_kw(Keyword::If)?;
        let (condition, _) = self.parens(|p| p.parse_expr()).expected("a condition")?;
        let yes = self.parse_expr().expected("an expression")?;
        let no = if self.eat_kw(Keyword::Else).is_ok() {
            let no = self.parse_expr().expected("an expression")?;
            Some(no)
        } else {
            None
        };
        Ok(Expr::If(If {
            span: if_span.to(no.as_ref().map(|e| e.span()).unwrap_or_else(|| yes.span())),
            condition: Box::new(condition),
            yes: Box::new(yes),
            no: no.map(Box::new),
        }))
    }

    fn parse_variant_lit(&mut self) -> Parsed<Expr<'i>> {
        let span = self.eat_punct(Punctuation::Period)?;
        let name = self.ident().expected("variant name")?;
        if let Ok((expr, paren_span)) = self.parens(|p| p.parse_expr()).invalid()? {
            Ok(Expr::Variant(Variant {
                span: span.to(paren_span),
                variant: name,
                value: Some(Box::new(expr)),
            }))
        } else {
            Ok(Expr::Variant(Variant {
                span: span.to(name.span()),
                variant: name,
                value: None,
            }))
        }
    }

    fn parse_ref_to(&mut self) -> Parsed<Expr<'i>> {
        let span = self.eat_punct(Punctuation::Et)?;
        let expr = self.parse_expr().expected("an l-value")?;
        Ok(Expr::RefTo(RefTo {
            span: span.to(expr.span()),
            rhs: Box::new(expr),
        }))
    }

    fn parse_expr(&mut self) -> Parsed<Expr<'i>> {
        let mut expr = self
            .ident()
            .map(Expr::Variable)
            .or_else(|_| self.int_lit().map(Expr::Int))
            .or_else(|_| self.str_lit().map(Expr::String))
            .or_else(|_| self.bool_lit().map(Expr::Bool))
            .or_else(|_| self.parse_struct_or_block())
            .invalid()?
            .or_else(|_| self.parse_variant_lit())
            .invalid()?
            .or_else(|_| self.parse_ref_to())
            .invalid()?
            .or_else(|_| self.parse_if())?;

        expr = self.parse_rhs(expr)?;

        Ok(expr)
    }
}
