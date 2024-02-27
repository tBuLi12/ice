use std::mem;

use ast::*;
use iiv::{Ctx, Span};
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
                Some(value)
            }
            _ => None,
        }
    };
}

trait Parse<'i>: Sized + Copy {
    type Output;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output>;
    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output>;
    fn invalid(self, span: Span) -> Self::Output;

    fn continue_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
        out: &mut Self::Output,
    ) -> bool {
        false
    }

    fn and<Other: Parse<'i>>(self, other: Other) -> AndRule<Self, Other> {
        AndRule(self, other)
    }

    fn or<Other: Parse<'i>>(self, other: Other) -> OrRule<Self, Other> {
        OrRule(self, other)
    }

    fn map<O, F: FnOnce(Self::Output) -> O>(self, fun: F) -> Map<Self, F> {
        Map { rule: self, fun }
    }

    fn separated_with<Separator: Parse<'i, Output = Span>>(
        self,
        separator: Separator,
    ) -> TrailingListRule<Self, Separator> {
        TrailingListRule {
            item_rule: self,
            separator_rule: separator,
        }
    }

    fn binary<
        Op: Parse<'i, Output = Span>,
        F: FnOnce(Self::Output, Span, Self::Output) -> Self::Output,
    >(
        self,
        op: Op,
        fun: F,
    ) -> Binary<Self, Op, F> {
        Binary {
            expr: self,
            operator: op,
            fun,
        }
    }
}

impl<'i> Parse<'i> for Punctuation {
    type Output = Span;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        parser.eat_punct(self)
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        parser.eat_punct(self)
    }

    fn invalid(self, span: Span) -> Self::Output {
        span
    }
}

impl<'i> Parse<'i> for Keyword {
    type Output = Span;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        parser.eat_kw(self)
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        parser.eat_kw(self)
    }

    fn invalid(self, span: Span) -> Self::Output {
        span
    }
}

#[derive(Clone, Copy)]
struct IdentRule;

#[derive(Clone, Copy)]
struct IntRule;

#[derive(Clone, Copy)]
struct StringRule;

impl<'i> Parse<'i> for IdentRule {
    type Output = Ident<'i>;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        parser.ident()
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        parser.ident()
    }

    fn invalid(self, span: Span) -> Self::Output {
        Ident { span, value: None }
    }
}

impl<'i> Parse<'i> for IntRule {
    type Output = Ident<'i>;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        parser.ident()
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        parser.ident()
    }

    fn invalid(self, span: Span) -> Self::Output {
        Ident { span, value: None }
    }
}

impl<'i> Parse<'i> for StringRule {
    type Output = Ident<'i>;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        parser.ident()
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        parser.ident()
    }

    fn invalid(self, span: Span) -> Self::Output {
        Ident { span, value: None }
    }
}

#[derive(Clone, Copy)]
struct OrRule<L, R>(L, R);

enum Or<L, R> {
    Left(L),
    Right(R),
}

impl<'i, L: Parse<'i>, R: Parse<'i>> OrRule<L, R> {
    fn choice(
        self,
    ) -> impl Parse<'i, Output = Or<<L as Parse<'i>>::Output, <R as Parse<'i>>::Output>> {
        OrRule(self.0.map(Or::Left), self.1.map(Or::Right))
    }
}

impl<'i, O, L: Parse<'i, Output = O>, R: Parse<'i, Output = O>> Parse<'i> for OrRule<L, R> {
    type Output = O;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        self.0
            .try_parse(parser)
            .or_else(|| self.1.try_parse(parser))
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        self.0
            .try_parse_with_errors(parser)
            .or_else(|| self.1.try_parse_with_errors(parser))
    }

    fn continue_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
        out: &mut Self::Output,
    ) -> bool {
        self.0.continue_with_errors(parser, out) || self.1.continue_with_errors(parser, out)
    }

    fn invalid(self, span: Span) -> Self::Output {
        self.0.invalid(span)
    }
}

#[derive(Clone, Copy)]
struct AndRule<L, R>(L, R);

impl<'i, L: Parse<'i>, R: Parse<'i>> Parse<'i> for AndRule<L, R> {
    type Output = (L::Output, R::Output);

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        let mut left = self.0.try_parse(parser)?;

        if let Some(right) = self.1.try_parse(parser) {
            return Some((left, right));
        }

        if self.0.continue_with_errors(parser, &mut left) {
            if let Some(right) = self.1.try_parse(parser) {
                return Some((left, right));
            }
        }

        let right = self
            .1
            .try_parse_with_errors(parser)
            .unwrap_or_else(|| self.1.invalid(parser.current_zero_span()));

        Some((left, right))
    }

    fn continue_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
        out: &mut Self::Output,
    ) -> bool {
        self.1.continue_with_errors(parser, &mut out.1)
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        let mut left = self.0.try_parse_with_errors(parser)?;

        let right = self
            .1
            .try_parse_with_errors(parser)
            .unwrap_or_else(|| self.1.invalid(parser.current_zero_span()));

        Some((left, right))
    }

    fn invalid(self, span: Span) -> Self::Output {
        (self.0.invalid(span), self.1.invalid(span))
    }
}

#[derive(Clone, Copy)]
struct Map<R, F> {
    rule: R,
    fun: F,
}

impl<'i, R: Parse<'i>, O, F: FnOnce(R::Output) -> O + Copy> Parse<'i> for Map<R, F> {
    type Output = O;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        self.rule.try_parse(parser).map(self.fun)
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        self.rule.try_parse_with_errors(parser).map(self.fun)
    }

    fn invalid(self, span: Span) -> Self::Output {
        (self.fun)(self.rule.invalid(span))
    }
}

#[derive(Clone, Copy)]
struct Maybe<R>(R);

impl<'i, R: Parse<'i>> Parse<'i> for Maybe<R> {
    type Output = Option<R::Output>;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        Some(self.0.try_parse(parser))
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        Some(self.0.try_parse_with_errors(parser))
    }

    fn invalid(self, span: Span) -> Self::Output {
        None
    }
}

#[derive(Clone, Copy)]
struct TrailingListRule<R, S> {
    item_rule: R,
    separator_rule: S,
}

impl<'i, R: Parse<'i>, S: Parse<'i, Output = Span>> Parse<'i> for TrailingListRule<R, S> {
    type Output = TrailingList<R::Output>;

    fn try_parse<Src: iiv::Source>(self, parser: &mut Parser<'i, Src>) -> Option<Self::Output> {
        let mut items = TrailingList::new();

        loop {
            let item = self.item_rule.try_parse(parser);
            if let Some(item) = item {
                items.add_item(item);
            } else {
                return Some(items);
            }

            let Some(seprarator) = self.separator_rule.try_parse(parser) else {
                return Some(items);
            };

            items.add_separator(seprarator);
        }
    }

    fn try_parse_with_errors<Src: iiv::Source>(
        self,
        parser: &mut Parser<'i, Src>,
    ) -> Option<Self::Output> {
        let mut items = TrailingList::new();
        if self.continue_with_errors(parser, &mut items) {
            Some(items)
        } else {
            None
        }
    }

    fn continue_with_errors<Src: iiv::Source>(
        self,
        parser: &mut Parser<'i, Src>,
        items: &mut Self::Output,
    ) -> bool {
        if items.separators().len() == items.items().len() {
            let Some(separator) = self.separator_rule.try_parse_with_errors(parser) else {
                return false;
            };
            items.add_separator(separator);
        }

        loop {
            let item = self.item_rule.try_parse_with_errors(parser);
            if let Some(item) = item {
                items.add_item(item);
            } else {
                return true;
            }

            let Some(seprarator) = self.separator_rule.try_parse_with_errors(parser) else {
                return true;
            };

            items.add_separator(seprarator);
        }
    }

    fn invalid(self, span: Span) -> Self::Output {
        TrailingList::new()
    }
}

#[derive(Clone, Copy)]
struct Repeated<R>(R);

impl<'i, R: Parse<'i>> Parse<'i> for Repeated<R> {
    type Output = Vec<R::Output>;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        let mut items = Vec::new();

        while let Some(item) = self.0.try_parse(parser) {
            items.push(item);
        }

        Some(items)
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        let mut items = Vec::new();
        if self.continue_with_errors(parser, &mut items) {
            Some(items)
        } else {
            None
        }
    }

    fn continue_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
        items: &mut Self::Output,
    ) -> bool {
        let len = items.len();

        while let Some(item) = self.0.try_parse(parser) {
            items.push(item);
        }

        len != items.len()
    }

    fn invalid(self, span: Span) -> Self::Output {
        Vec::new()
    }
}

#[derive(Clone, Copy)]
struct Binary<E, Op, F> {
    expr: E,
    operator: Op,
    fun: F,
}

impl<
        'i,
        E: Parse<'i>,
        Op: Parse<'i, Output = Span>,
        F: FnOnce(E::Output, Span, E::Output) -> E::Output + Copy,
    > Parse<'i> for Binary<E, Op, F>
{
    type Output = E::Output;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        let mut lhs = self.expr.try_parse(parser)?;

        while let Some((op, rhs)) = self.operator.and(self.expr).try_parse(parser) {
            lhs = (self.fun)(lhs, op, rhs);
        }

        Some(lhs)
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        let mut lhs = self.expr.try_parse_with_errors(parser)?;
        self.continue_with_errors(parser, &mut lhs);
        Some(lhs)
    }

    fn continue_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
        lhs: &mut Self::Output,
    ) -> bool {
        let mut parsed = false;
        while let Some((op, rhs)) = self.operator.and(self.expr).try_parse_with_errors(parser) {
            parsed = true;
            let old_lhs = std::mem::replace(lhs, self.expr.invalid(Span::null()));
            *lhs = (self.fun)(old_lhs, op, rhs);
        }
        parsed
    }

    fn invalid(self, span: Span) -> Self::Output {
        self.expr.invalid(span)
    }
}

trait DispatchParse<'i> {
    type Output;

    fn get_rule(self) -> impl Parse<'i, Output = Self::Output>;
}

impl<'i, T: DispatchParse<'i> + Copy> Parse<'i> for T {
    type Output = T::Output;

    fn try_parse<S: iiv::Source>(self, parser: &mut Parser<'i, S>) -> Option<Self::Output> {
        self.get_rule().try_parse(parser)
    }

    fn try_parse_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
    ) -> Option<Self::Output> {
        self.get_rule().try_parse_with_errors(parser)
    }

    fn invalid(self, span: Span) -> Self::Output {
        self.get_rule().invalid(span)
    }

    fn continue_with_errors<S: iiv::Source>(
        self,
        parser: &mut Parser<'i, S>,
        out: &mut Self::Output,
    ) -> bool {
        self.get_rule().continue_with_errors(parser, out)
    }
}

impl<'i, P: Parse<'i>, F: FnOnce() -> P> DispatchParse<'i> for F {
    type Output = P::Output;

    fn get_rule(self) -> impl Parse<'i, Output = Self::Output> {
        self()
    }
}

mod rule {
    use iiv::ty;

    use super::*;

    fn ident<'i>() -> impl Parse<'i, Output = Ident<'i>> {
        IdentRule
    }

    fn maybe<'i, R: Parse<'i>>(rule: R) -> Maybe<R> {
        Maybe(rule)
    }

    fn prop<'i>() -> impl Parse<'i, Output = StructProp<'i>> {
        Punctuation::Period
            .and(ident)
            .and(maybe(
                Punctuation::Eq
                    .and(expr)
                    .map(|(equals, value)| ExplicitProp { equals, value }),
            ))
            .map(|((period, name), value)| StructProp {
                period,
                name,
                value,
            })
    }

    fn block_item<'i>() -> impl Parse<'i, Output = BlockItem<'i>> {
        expr.map(BlockItem::Expr)
    }

    fn struct_or_block<'i>() -> impl Parse<'i, Output = Expr<'i>> {
        Punctuation::LBrace
            .and(
                prop.and(maybe(
                    Punctuation::Comma.and(prop.separated_with(Punctuation::Comma)),
                ))
                .or(block_item.separated_with(Punctuation::Semicolon))
                .choice(),
            )
            .and(Punctuation::RBrace)
            .map(|((l_brace, inner), r_brace)| match inner {
                Or::Left((first_prop, None)) => {
                    let mut props = TrailingList::new();
                    props.add_item(first_prop);
                    Expr::Struct(Struct {
                        ty: None,
                        l_brace,
                        props,
                        r_brace,
                    })
                }
                Or::Left((first_prop, Some((first_comma, mut props)))) => {
                    props.prepend_separator(first_comma);
                    props.prepend_item(first_prop);
                    Expr::Struct(Struct {
                        ty: None,
                        l_brace,
                        props,
                        r_brace,
                    })
                }
                Or::Right(items) => Expr::Block(Block {
                    l_brace,
                    items,
                    r_brace,
                }),
            })
    }

    fn primary_expr<'i>() -> impl Parse<'i, Output = Expr<'i>> {
        ident.map(Expr::Variable).or(struct_or_block)
    }

    fn expr<'i>() -> impl Parse<'i, Output = Expr<'i>> {
        primary_expr
            .binary(Punctuation::Plus, |lhs, plus, rhs| {
                Expr::Add(Add {
                    lhs: Box::new(lhs),
                    plus,
                    rhs: Box::new(rhs),
                })
            })
            .binary(Punctuation::Asterisk, |lhs, asterisk, rhs| {
                Expr::Mul(Mul {
                    lhs: Box::new(lhs),
                    asterisk,
                    rhs: Box::new(rhs),
                })
            })
    }

    fn type_name<'i>() -> impl Parse<'i, Output = Expr<'i>> {
        Punctuation::Comma.map(Expr::Invalid)
    }

    fn type_params<'i>() -> impl Parse<'i, Output = TypeParamList<'i>> {
        Punctuation::LBracket
            .and(
                ident
                    .and(maybe(
                        Punctuation::Colon.and(type_name.separated_with(Punctuation::Et)),
                    ))
                    .map(|(name, bounds)| TypeParam {
                        name,
                        trait_bounds: bounds
                            .map(|(colon, bounds)| InlineTraitBounds { colon, bounds }),
                    })
                    .separated_with(Punctuation::Comma),
            )
            .and(Punctuation::RBracket)
            .map(|((l_bracket, list), r_bracket)| TypeParamList {
                l_bracket,
                list,
                r_bracket,
            })
    }

    pub(super) fn function<'i>() -> impl Parse<'i, Output = Function<'i>> {
        Keyword::Fun
            .and(ident)
            .and(maybe(type_params))
            .and(Punctuation::LParen)
            .and(
                ident
                    .and(Punctuation::Colon)
                    .and(type_name)
                    .map(|((name, colon), ty)| Parameter { name, colon, ty })
                    .separated_with(Punctuation::Comma),
            )
            .and(Punctuation::RParen)
            .and(maybe(
                Punctuation::Colon
                    .and(type_name)
                    .map(|(colon, ty)| ReturnType { colon, ty }),
            ))
            .and(
                Punctuation::Semicolon
                    .map(FunctionBody::None)
                    .or(Punctuation::ThinArrow
                        .and(expr)
                        .and(Punctuation::Semicolon)
                        .map(|((arrow, body), semicolon)| {
                            FunctionBody::Inline(InlineBody {
                                arrow,
                                body,
                                semicolon,
                            })
                        })),
            )
            .map(
                |(
                    ((((((fun, name), type_params), l_paren), params), r_paren), return_ty),
                    body,
                )| Function {
                    visibility: None,
                    fun_decl_keyword: FunDeclKeyword::Fun(fun),
                    name,
                    type_params,
                    l_paren,
                    params,
                    r_paren,
                    return_ty,
                    body,
                },
            )
    }

    pub(super) fn type_decl<'i>() -> impl Parse<'i, Output = TypeDecl<'i>> {
        Keyword::Type
            .or(Keyword::Data)
            .choice()
            .map(|type_decl_keyword| match type_decl_keyword {
                Or::Left(data) => TypeDeclKeyword::Data(data),
                Or::Right(ty) => TypeDeclKeyword::Type(ty),
            })
            .and(ident)
            .and(maybe(type_params))
            .and(type_name)
            .map(|(((decl_keyword, name), type_params), proto)| TypeDecl {
                visibility: None,
                decl_keyword,
                name,
                type_params,
                proto_visibility: None,
                proto,
            })
    }
}

impl<'i, S: iiv::Source> Parser<'i, S> {
    pub fn new(ctx: &'i Ctx<'i>, source: S) -> Self {
        let mut parser = Parser {
            lexer: Lexer::new(ctx, source),
            messages: &ctx.diagnostcs,
            current: Token::Eof(Span::null()),
            default_binding: None,
        };
        parser.next_token();
        parser
    }

    fn eof_reached(&self) -> bool {
        matches!(self.current, Token::Eof(_))
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

        while !self.eof_reached() {
            if !self.parse_declaration_into(&mut module) {
                self.expected("a declaration");
                self.next_token();
            }
        }

        module
    }

    fn expected_at(&self, span: Span, what: &'static str) {
        self.messages
            .add(iiv::diagnostics::error(&span, format!("expected {}", what)));
    }

    fn expected(&self, what: &'static str) {
        self.expected_at(self.current.span(), what);
    }

    fn parse_declaration_into(&mut self, module: &mut Module<'i>) -> bool {
        if let Some(fun) = rule::function().try_parse(self) {
            module.functions.push(fun);
            return true;
        }
        // if let Some(ty_decl) = self.parse_type_decl() {
        //     module.types.push(ty_decl);
        //     return true;
        // }
        // if let Some(tr) = self.parse_trait_decl() {
        //     module.traits.push(tr);
        //     return true;
        // }
        // if let Some(tr_impl) = self.parse_trait_impl() {
        //     module.trait_impls.push(tr_impl);
        //     return true;
        // }

        false
    }

    fn next_token(&mut self) -> Token<'i> {
        mem::replace(&mut self.current, self.lexer.next())
    }

    fn eat_kw(&mut self, keyword: Keyword) -> Option<Span> {
        match self.current {
            Token::Keyword(kw, span) if kw == keyword => {
                self.next_token();
                Some(span)
            }
            _ => None,
        }
    }

    fn eat_punct(&mut self, punctuation: Punctuation) -> Option<Span> {
        match self.current {
            Token::Punctuation(punct, span) if punct == punctuation => {
                self.next_token();
                Some(span)
            }
            _ => None,
        }
    }

    // fn should_yield(&self) -> bool {
    //     for &token in &self.future_expected_tokens {
    //         match (self.current, token) {
    //             (Token::Ident(_), FutureToken::Ident)
    //             | (Token::Int(_), FutureToken::Int)
    //             | (Token::String(_), FutureToken::String)
    //             | (Token::Eof(_), FutureToken::Eof) => return true,
    //             (Token::Keyword(kw1, _), FutureToken::Keyword(kw2)) if kw1 == kw2 => return true,
    //             (Token::Punctuation(punct1, _), FutureToken::Punctuation(punct2))
    //                 if punct1 == punct2 =>
    //             {
    //                 return true
    //             }
    //             _ => {}
    //         }
    //     }

    //     self.eof_reached()
    // }

    fn ident(&mut self) -> Option<Ident<'i>> {
        get_token!(self.Ident)
    }

    // fn must<T>(
    //     &mut self,
    //     mut parse: impl FnMut(&mut Self) -> Option<T>,
    //     err_value: T,
    //     expected: &'static str,
    // ) -> T {
    //     loop {
    //         if let Some(value) = parse(self) {
    //             return value;
    //         }

    //         self.expected(expected);

    //         if self.should_yield() {
    //             return err_value;
    //         }

    //         self.next_token();
    //     }
    // }

    fn current_zero_span(&self) -> Span {
        self.current.span().left.extend_back(0)
    }

    fn must_ident(&mut self, expected: &'static str) -> Ident<'i> {
        if let Some(ident) = self.ident() {
            ident
        } else {
            self.expected(expected);
            Ident {
                span: self.current_zero_span(),
                value: None,
            }
        }
    }

    // fn must_punct(&mut self, punctuation: Punctuation, expected: &'static str) -> Span {
    //     self.must(
    //         |p| p.eat_punct(punctuation),
    //         self.current_zero_span(),
    //         expected,
    //     )
    // }

    fn bool_lit(&mut self) -> Option<Bool> {
        if let Some(span) = self.eat_kw(Keyword::True) {
            Some(Bool { span, value: true })
        } else if let Some(span) = self.eat_kw(Keyword::False) {
            Some(Bool { span, value: false })
        } else {
            None
        }
    }

    fn int_lit(&mut self) -> Option<Int> {
        get_token!(self.Int)
    }

    fn str_lit(&mut self) -> Option<StringLit<'i>> {
        get_token!(self.String)
    }

    fn non_separated_list<T>(&mut self, mut fun: impl FnMut(&mut Self) -> Option<T>) -> Vec<T> {
        let mut items = vec![];

        while let Some(item) = fun(self) {
            items.push(item);
        }

        items
    }

    fn list<T>(
        &mut self,
        separator: Punctuation,
        mut fun: impl FnMut(&mut Self) -> Option<T>,
    ) -> TrailingList<T> {
        let mut items = TrailingList::new();

        loop {
            let item = fun(self);

            let Some(item) = item else {
                return items;
            };

            items.add_item(item);

            let Some(span) = self.eat_punct(separator) else {
                return items;
            };

            items.add_separator(span);
        }
    }

    //     fn any_parens<T>(
    //         &mut self,
    //         left: Punctuation,
    //         right: Punctuation,
    //         closing: &'static str,
    //         fun: impl FnOnce(&mut Self) -> T,
    //     ) -> Option<(Span, T, Span)> {
    //         let lspan = self.eat_punct(left)?;
    //         let inner = fun(self);
    //         let rspan = self.must_punct(right, closing);
    //         Some((lspan, inner, rspan))
    //     }

    //     fn parens<T>(&mut self, fun: impl FnOnce(&mut Self) -> T) -> Option<(Span, T, Span)> {
    //         self.any_parens(Punctuation::LParen, Punctuation::RParen, ")", fun)
    //     }

    //     fn braces<T>(&mut self, fun: impl FnOnce(&mut Self) -> T) -> Option<(Span, T, Span)> {
    //         self.any_parens(Punctuation::LBrace, Punctuation::RBrace, "}", fun)
    //     }

    //     fn brackets<T>(&mut self, fun: impl FnOnce(&mut Self) -> T) -> Option<(Span, T, Span)> {
    //         self.any_parens(Punctuation::LBracket, Punctuation::RBracket, "]", fun)
    //     }

    //     fn must_any_parens<T>(
    //         &mut self,
    //         fun: impl FnOnce(&mut Self) -> T,
    //         left: Punctuation,
    //         right: Punctuation,
    //         expected_l: &'static str,
    //         expected_r: &'static str,
    //     ) -> (Span, T, Span) {
    //         let lspan = self.must_punct(left, expected_l);
    //         self.will_match_punct(right);
    //         let inner = fun(self);
    //         self.pop_would_match();
    //         let rspan = self.must_punct(right, expected_r);
    //         (lspan, inner, rspan)
    //     }

    //     fn must_parens<T>(&mut self, fun: impl FnOnce(&mut Self) -> T) -> (Span, T, Span) {
    //         self.must_any_parens(fun, Punctuation::LParen, Punctuation::RParen, "(", ")")
    //     }

    //     fn must_braces<T>(&mut self, fun: impl FnOnce(&mut Self) -> T) -> (Span, T, Span) {
    //         self.must_any_parens(fun, Punctuation::LBrace, Punctuation::RBrace, "{", "}")
    //     }

    //     fn parse_function(&mut self) -> Option<Function<'i>> {
    //         let fun = self.eat_kw(Keyword::Fun)?;
    //         let name = self.must_ident("a function name");
    //         let type_params = self.parse_type_params();
    //         let (l_paren, params, r_paren) = self.must_parens(|p| {
    //             p.list(Punctuation::Comma, |p| {
    //                 p.will_match_punct(Punctuation::Colon);
    //                 let name = p.ident()?;
    //                 p.pop_would_match();
    //                 let colon = p.must_punct(Punctuation::Colon, ":");
    //                 let ty = p.must_parse_ty_name();
    //                 Some(Parameter { name, colon, ty })
    //             })
    //         });
    //         let return_ty = self.eat_punct(Punctuation::Colon).map(|colon| {
    //             let ty = self.must_parse_ty_name();
    //             ReturnType { colon, ty }
    //         });
    //         self.pop_would_match();
    //         self.pop_would_match();
    //         self.pop_would_match();
    //         let body = if let Some(arrow) = self.eat_punct(Punctuation::ThinArrow) {
    //             self.will_match_punct(Punctuation::Semicolon);
    //             let body = self.must_parse_expr();
    //             self.pop_would_match();
    //             let semicolon = self.must_punct(Punctuation::Semicolon, ";");
    //             FunctionBody::Inline(InlineBody {
    //                 arrow,
    //                 body,
    //                 semicolon,
    //             })
    //         } else if let Some(block) = self.parse_block() {
    //             FunctionBody::Block(block)
    //         } else {
    //             let semicolon = self.must_punct(Punctuation::Semicolon, ";");
    //             FunctionBody::None(semicolon)
    //         };

    //         Some(Function {
    //             fun_decl_keyword: FunDeclKeyword::Fun(fun),
    //             visibility: None,
    //             name,
    //             type_params,
    //             l_paren,
    //             params,
    //             r_paren,
    //             return_ty,
    //             body,
    //         })
    //     }

    //     fn parse_params(&mut self) -> Option<(Span, TrailingList<Parameter<'i>>, Span)> {
    //         let l_paren = self.eat_punct(Punctuation::LParen)?;

    //         let mut params = TrailingList::new();

    //         loop {
    //             if let Some(r_paren) = self.eat_punct(Punctuation::RParen) {
    //                 return Some((l_paren, params, r_paren));
    //             }

    //             if let Some(name) = self.ident() {
    //                 let colon = self.eat_punct(Punctuation::Colon).unwrap_or_else(|| {
    //                     self.expected(":");
    //                     self.current_zero_span()
    //                 });
    //                 let ty = self.parse_ty_name().unwrap_or_else(|| {
    //                     self.expected("a type name");
    //                     Expr::Invalid(self.current_zero_span())
    //                 });
    //                 params.add_item(Parameter { name, colon, ty });

    //                 if let Some(comma) = self.eat_punct(Punctuation::Comma) {
    //                     params.add_separator(comma);
    //                 } else if let Some(r_paren) = self.eat_punct(Punctuation::RParen) {
    //                     return Some((l_paren, params, r_paren));
    //                 } else {
    //                     let r_paren = self.current_zero_span();
    //                     return Some((l_paren, params, r_paren));
    //                 }
    //                 continue;
    //             }

    //             self.expected("an identifier or )");
    //             let name = Ident {
    //                 span: self.current_zero_span(),
    //                 value: None,
    //             };

    //             if let Some(comma) = self.eat_punct(Punctuation::Comma) {
    //                 params.add_item(Parameter {
    //                     name,
    //                     colon: comma,
    //                     ty: Expr::Invalid(comma),
    //                 });
    //                 params.add_separator(comma);
    //             } else if let Some(colon) = self.eat_punct(Punctuation::Colon) {
    //                 let ty = self.parse_ty_name().unwrap_or_else(|| {
    //                     self.expected("a type name");
    //                     Expr::Invalid(self.current_zero_span())
    //                 });
    //                 params.add_item(Parameter { name, colon, ty });
    //                 if let Some(comma) = self.eat_punct(Punctuation::Comma) {
    //                     params.add_separator(comma);
    //                 } else if let Some(r_paren) = self.eat_punct(Punctuation::RParen) {
    //                     return Some((l_paren, params, r_paren));
    //                 } else {
    //                     let r_paren = self.current_zero_span();
    //                     return Some((l_paren, params, r_paren));
    //                 }
    //             } else {
    //                 let r_paren = self.current_zero_span();
    //                 return Some((l_paren, params, r_paren));
    //             }
    //         }
    //     }

    //     fn parse_type_params(&mut self) -> Option<TypeParamList<'i>> {
    //         self.brackets(|p| {
    //             p.list(Punctuation::Comma, |p| {
    //                 let name = p.ident()?;
    //                 Some(TypeParam {
    //                     name,
    //                     trait_bounds: p
    //                         .eat_punct(Punctuation::Colon)
    //                         .map(|colon| InlineTraitBounds {
    //                             colon,
    //                             bounds: p.list(Punctuation::Plus, |p| p.parse_expr()),
    //                         }),
    //                 })
    //             })
    //         })
    //         .map(|(l_bracket, list, r_bracket)| TypeParamList {
    //             l_bracket,
    //             list,
    //             r_bracket,
    //         })
    //     }

    //     fn parse_type_decl(&mut self) -> Option<TypeDecl<'i>> {
    //         let decl_keyword = if let Some(span) = self.eat_kw(Keyword::Data) {
    //             TypeDeclKeyword::Data(span)
    //         } else if let Some(span) = self.eat_kw(Keyword::Type) {
    //             TypeDeclKeyword::Type(span)
    //         } else {
    //             return None;
    //         };

    //         let name = self.must_ident("a name");

    //         let type_params = self.parse_type_params();

    //         let proto = self.must_parse_ty_name();

    //         Some(TypeDecl {
    //             visibility: None,
    //             decl_keyword,
    //             name,
    //             type_params,
    //             proto_visibility: None,
    //             proto,
    //         })
    //     }

    //     fn parse_trait_decl(&mut self) -> Option<TraitDecl<'i>> {
    //         let trait_keyword = self.eat_kw(Keyword::Trait)?;
    //         let name = self.must_ident("a name");
    //         let type_params = self.parse_type_params();

    //         let (l_brace, signatures, r_brace) =
    //             self.must_braces(|p| p.non_separated_list(|p| p.parse_function()));

    //         Some(TraitDecl {
    //             visibility: None,
    //             trait_keyword,
    //             name,
    //             type_params,
    //             l_brace,
    //             signatures,
    //             r_brace,
    //         })
    //     }

    //     fn parse_trait_impl(&mut self) -> Option<TraitImpl<'i>> {
    //         let def_keyword = self.eat_kw(Keyword::Def)?;
    //         let type_params = self.parse_type_params();
    //         let ty = self.must_parse_ty_name();
    //         self.must(Keyword::As);
    //         let tr = self.must_parse_ty_name();

    //         let ((functions, _), block_span) = self
    //             .braces(|p| p.list(Punctuation::Semicolon, |p| p.parse_function()))
    //             .expected("signature block")?;

    //         let span = span.to(block_span);
    //         Ok(TraitImpl {
    //             span: span.to(block_span),
    //             type_params,
    //             tr,
    //             ty,
    //             functions,
    //         })
    //     }

    //     fn parse_block(&mut self) -> Option<Block<'i>> {
    //         let ((exprs, trailing_separator), span) =
    //             self.braces(|p| p.list(Punctuation::Semicolon, |p| p.parse_block_item()))?;

    //         let len = exprs.len();
    //         Ok(Expr::Block(Block {
    //             span,
    //             items: exprs,
    //             has_trailing_expression: len > 0 && !trailing_separator,
    //         }))
    //     }

    //     fn pattern_ident(&mut self, name: Ident<'i>) -> Pattern<'i> {
    //         if let Some(binding_type) = self.default_binding {
    //             Pattern {
    //                 body: PatternBody::Bind(BindPattern { binding_type, name }),
    //                 guard: None,
    //             }
    //         } else {
    //             Pattern {
    //                 body: PatternBody::Bind(BindPattern {
    //                     binding_type: BindingType::Let,
    //                     name,
    //                 }),
    //                 guard: None,
    //             }
    //         }
    //     }

    //     fn parse_untyped_pattern(&mut self) -> Parsed<Pattern<'i>> {
    //         if let Ok(name) = self.ident() {
    //             return Ok(self.pattern_ident(name));
    //         }

    //         if let Ok(span) = self.eat_punct(Punctuation::Period) {
    //             let name = self.ident().expected("a variant name")?;

    //             let (span, inner) = if let Ok((pattern, paren_span)) = self
    //                 .parens(|p| p.parse_pattern().expected("a pattern"))
    //                 .invalid()?
    //             {
    //                 (span.to(paren_span.span()), Some(Box::new(pattern)))
    //             } else {
    //                 (span.to(name.span), None)
    //             };

    //             return Ok(Pattern {
    //                 body: PatternBody::Variant(VariantPattern { span, name, inner }),
    //                 guard: None,
    //             });
    //         }

    //         let ((struct_pattern, _), span) = self
    //             .braces(|p| {
    //                 p.list(Punctuation::Comma, |p| {
    //                     let name = p.ident().expected("a property name")?;
    //                     Ok((name, p.pattern_ident(name)))
    //                 })
    //             })
    //             .expected("a pattern")?;
    //         Ok(Pattern {
    //             guard: None,
    //             body: PatternBody::Struct(StructPattern {
    //                 span,
    //                 inner: struct_pattern,
    //             }),
    //         })
    //     }

    //     fn parse_pattern(&mut self) -> Parsed<Pattern<'i>> {
    //         let mut pattern = self.parse_untyped_pattern()?;
    //         while self.eat_punct(Punctuation::Colon).is_ok() {
    //             let ty = self.parse_ty_name().expected("a type")?;
    //             pattern = Pattern {
    //                 guard: None,
    //                 body: PatternBody::NarrowType(NarrowTypePattern {
    //                     inner: Box::new(pattern),
    //                     ty,
    //                 }),
    //             };
    //         }
    //         Ok(pattern)
    //     }

    //     fn parse_block_item(&mut self) -> Option<BlockItem<'i>> {
    //         let binding_type = if let Ok(span) = self.eat_kw(Keyword::Let) {
    //             Some((span, BindingType::Let))
    //         } else if let Ok(span) = self.eat_kw(Keyword::Var) {
    //             Some((span, BindingType::Var))
    //         } else {
    //             None
    //         };

    //         if let Some((span, binding_type)) = binding_type {
    //             self.default_binding = Some(binding_type);
    //             let binding = self.parse_pattern().expected("a pattern")?;
    //             self.default_binding = None;

    //             self.eat_punct(Punctuation::Eq).expected("=")?;
    //             let value = self.parse_expr().expected("an initializer")?;

    //             return Ok(BlockItem::Bind(Binding {
    //                 binding_type,
    //                 span: span.to(value.span()),
    //                 binding,
    //                 value,
    //             }));
    //         }

    //         Ok(BlockItem::Expr(self.parse_expr()?))
    //     }

    //     fn parse_ty_prop(&mut self) -> Parsed<TyProp<'i>> {
    //         let name = self.ident()?;
    //         let ty = if self.eat_punct(Punctuation::Colon).is_ok() {
    //             let ty = self.parse_ty_name().expected("a type name")?;
    //             Some(ty)
    //         } else {
    //             None
    //         };
    //         Ok(TyProp { name, ty })
    //     }

    //     fn parse_struct_or_variant_ty(&mut self) -> Parsed<Expr<'i>> {
    //         let ((props, is_variant), span) = self.braces(|p| {
    //             if p.eat_punct(Punctuation::Pipe).is_ok() {
    //                 return Ok((p.list(Punctuation::Pipe, |p| p.parse_ty_prop())?.0, true));
    //             }
    //             let first = p.parse_ty_prop()?;
    //             if p.eat_punct(Punctuation::Pipe).is_ok() {
    //                 let mut prop_list = p.list(Punctuation::Pipe, |p| p.parse_ty_prop())?.0;
    //                 prop_list.insert(0, first);
    //                 return Ok((prop_list, true));
    //             }
    //             if p.eat_punct(Punctuation::Comma).is_ok() {
    //                 let mut prop_list = p.list(Punctuation::Comma, |p| p.parse_ty_prop())?.0;
    //                 prop_list.insert(0, first);
    //                 return Ok((prop_list, false));
    //             }
    //             Ok((vec![first], false))
    //         })?;
    //         if is_variant {
    //             Ok(Expr::VariantTy(PropsTy { span, props }))
    //         } else {
    //             Ok(Expr::StructTy(PropsTy { span, props }))
    //         }
    //     }

    //     fn must_parse_ty_name(&mut self) -> Expr<'i> {
    //         self.must(
    //             |p| p.parse_ty_name(),
    //             Expr::Invalid(self.current_zero_span()),
    //             "a type name",
    //         )
    //     }

    //     fn parse_ty_name(&mut self) -> Option<Expr<'i>> {
    //         if let Ok(ident) = self.ident()? {
    //             let expr = Expr::Variable(ident);
    //             if let Ok((span, args)) = self.parse_ty_args().invalid()? {
    //                 Ok(Expr::TyArgApply(TyArgApply {
    //                     span: ident.span.to(span),
    //                     lhs: Box::new(expr),
    //                     args,
    //                 }))
    //             } else {
    //                 Ok(expr)
    //             }
    //         } else if let Ok(et) = self.eat_punct(Punctuation::Et) {
    //             let inner = self.parse_ty_name().expected("a type name")?;
    //             Ok(Expr::RefTy(RefTo {
    //                 span: et.to(inner.span()),
    //                 rhs: Box::new(inner),
    //             }))
    //         } else if let Ok(asterisk) = self.eat_punct(Punctuation::Asterisk) {
    //             let inner = self.parse_ty_name().expected("a type name")?;
    //             Ok(Expr::PtrTy(RefTo {
    //                 span: asterisk.to(inner.span()),
    //                 rhs: Box::new(inner),
    //             }))
    //         } else {
    //             self.parse_struct_or_variant_ty()
    //         }
    //     }

    //     fn parse_struct_or_block(&mut self) -> Option<Expr<'i>> {
    //         enum StructOrBlock<'i> {
    //             Struct(TrailingList<StructProp<'i>>),
    //             Block(TrailingList<BlockItem<'i>>),
    //         }

    //         let (l_brace, inner, r_brace) = self.braces(|p| {
    //             if let Some(ident) = p.ident() {
    //                 if let Some(colon) = p.eat_punct(Punctuation::Colon) {
    //                     let first_val = p.must_parse_expr();
    //                     let comma = p.must_punct(Punctuation::Comma, ":");
    //                     let mut props = p.list(Punctuation::Comma, |p| {
    //                         let name = p.ident()?;
    //                         if let Some(colon) = p.eat_punct(Punctuation::Colon) {
    //                             let value = p.must_parse_expr();
    //                             return Some(StructProp {
    //                                 name,
    //                                 value: Some(ExplicitProp { colon, value }),
    //                             });
    //                         }
    //                         Some(StructProp { name, value: None })
    //                     });
    //                     props.prepend_separator(comma);
    //                     props.prepend_item(StructProp {
    //                         name: ident,
    //                         value: Some(ExplicitProp {
    //                             colon,
    //                             value: first_val,
    //                         }),
    //                     });
    //                     StructOrBlock::Struct(props)
    //                 } else {
    //                     let first_expr = p.parse_primary_rhs(Expr::Variable(ident));
    //                     let mut exprs = if let Some(semi) = p.eat_punct(Punctuation::Semicolon) {
    //                         let mut exprs = p.list(Punctuation::Semicolon, |p| p.parse_block_item());
    //                         exprs.prepend_separator(semi);
    //                         exprs
    //                     } else {
    //                         TrailingList::new()
    //                     };
    //                     exprs.prepend_item(BlockItem::Expr(first_expr));
    //                     StructOrBlock::Block(exprs)
    //                 }
    //             } else {
    //                 let block_items = p.list(Punctuation::Semicolon, |p| p.parse_block_item());
    //                 StructOrBlock::Block(block_items)
    //             }
    //         })?;

    //         Some(match inner {
    //             StructOrBlock::Block(items) => Expr::Block(Block {
    //                 l_brace,
    //                 items,
    //                 r_brace,
    //             }),
    //             StructOrBlock::Struct(props) => Expr::Struct(Struct {
    //                 ty: None,
    //                 l_brace,
    //                 props,
    //                 r_brace,
    //             }),
    //         })
    //     }

    //     fn parse_ty_args(&mut self) -> Option<(Span, TrailingList<Expr<'i>>, Span)> {
    //         self.brackets(|p| p.list(Punctuation::Comma, |p| p.parse_ty_name()))
    //     }

    //     fn parse_multipications(&mut self) -> Option<Expr<'i>> {
    //         let mut lhs = self.parse_primary_expr()?;

    //         while self.eat_punct(Punctuation::Asterisk).is_ok() {
    //             let rhs = self.parse_primary_expr().expected("an expression")?;
    //             lhs = Expr::Mul(Mul {
    //                 lhs: Box::new(lhs),
    //                 rhs: Box::new(rhs),
    //             });
    //         }

    //         Ok(lhs)
    //     }

    //     fn parse_additions(&mut self) -> Option<Expr<'i>> {
    //         let mut lhs = self.parse_multipications()?;

    //         while self.eat_punct(Punctuation::Plus).is_ok() {
    //             let rhs = self.parse_multipications().expected("an expression")?;
    //             lhs = Expr::Add(Add {
    //                 lhs: Box::new(lhs),
    //                 rhs: Box::new(rhs),
    //             });
    //         }

    //         Ok(lhs)
    //     }

    //     fn must_parse_expr(&mut self) -> Expr<'i> {
    //         self.must(
    //             |p| p.parse_expr(),
    //             Expr::Invalid(self.current_zero_span()),
    //             "an expression",
    //         )
    //     }

    //     fn parse_expr(&mut self) -> Option<Expr<'i>> {
    //         let mut lhs = self.parse_additions()?;

    //         loop {
    //             lhs = if self.eat_punct(Punctuation::DoubleEq).is_ok() {
    //                 let rhs = self.parse_additions().expected("an expression")?;
    //                 Expr::Eq(Equals {
    //                     lhs: Box::new(lhs),
    //                     rhs: Box::new(rhs),
    //                 })
    //             } else if self.eat_punct(Punctuation::NotEq).is_ok() {
    //                 let rhs = self.parse_additions().expected("an expression")?;
    //                 Expr::Neq(NotEquals {
    //                     lhs: Box::new(lhs),
    //                     rhs: Box::new(rhs),
    //                 })
    //             } else if self.eat_punct(Punctuation::Less).is_ok() {
    //                 let rhs = self.parse_additions().expected("an expression")?;
    //                 Expr::Lt(Less {
    //                     lhs: Box::new(lhs),
    //                     rhs: Box::new(rhs),
    //                 })
    //             } else if self.eat_punct(Punctuation::Greater).is_ok() {
    //                 let rhs = self.parse_additions().expected("an expression")?;
    //                 Expr::Gt(Greater {
    //                     lhs: Box::new(lhs),
    //                     rhs: Box::new(rhs),
    //                 })
    //             } else if self.eat_punct(Punctuation::LessEq).is_ok() {
    //                 let rhs = self.parse_additions().expected("an expression")?;
    //                 Expr::Leq(LessEq {
    //                     lhs: Box::new(lhs),
    //                     rhs: Box::new(rhs),
    //                 })
    //             } else if self.eat_punct(Punctuation::GreaterEq).is_ok() {
    //                 let rhs = self.parse_additions().expected("an expression")?;
    //                 Expr::Geq(GreaterEq {
    //                     lhs: Box::new(lhs),
    //                     rhs: Box::new(rhs),
    //                 })
    //             } else {
    //                 break;
    //             };
    //         }

    //         if self.eat_punct(Punctuation::Eq).is_ok() {
    //             let rhs = self.parse_expr().expected("an expression")?;
    //             lhs = Expr::Assign(Assign {
    //                 lhs: Box::new(lhs),
    //                 rhs: Box::new(rhs),
    //             });
    //         }

    //         Ok(lhs)
    //     }

    //     fn parse_primary_rhs(&mut self, mut lhs: Expr<'i>) -> Expr<'i> {
    //         let lhs = loop {
    //             if let Ok(((args, _), span)) = self
    //                 .parens(|p| p.list(Punctuation::Comma, |p| p.parse_expr()))
    //                 .invalid()?
    //             {
    //                 lhs = Expr::Call(Call {
    //                     span: lhs.span().to(span),
    //                     lhs: Box::new(lhs),
    //                     args,
    //                 });
    //             } else if let Ok((span, args)) = self.parse_ty_args().invalid()? {
    //                 lhs = Expr::TyArgApply(TyArgApply {
    //                     span: lhs.span().to(span),
    //                     lhs: Box::new(lhs),
    //                     args,
    //                 });
    //             } else if self.eat_punct(Punctuation::Period).is_ok() {
    //                 if let Ok(span) = self.eat_punct(Punctuation::Asterisk) {
    //                     lhs = Expr::Deref(Deref {
    //                         span: lhs.span().to(span),
    //                         lhs: Box::new(lhs),
    //                     });
    //                 } else {
    //                     let prop = self.ident().expected("property name")?;
    //                     lhs = Expr::Prop(Prop {
    //                         lhs: Box::new(lhs),
    //                         prop,
    //                         tr: None,
    //                     });
    //                 }
    //             } else if self.eat_kw(Keyword::Is).is_ok() {
    //                 let pattern = self.parse_pattern().expected("a pattern")?;
    //                 lhs = Expr::Is(Is {
    //                     lhs: Box::new(lhs),
    //                     rhs: Box::new(pattern),
    //                 })
    //             } else {
    //                 break lhs;
    //             }
    //         };

    //         Ok(lhs)
    //     }

    //     fn parse_if(&mut self) -> Parsed<Expr<'i>> {
    //         let if_span = self.eat_kw(Keyword::If)?;
    //         let (condition, _) = self.parens(|p| p.parse_expr()).expected("a condition")?;
    //         let yes = self.parse_expr().expected("an expression")?;
    //         let no = if self.eat_kw(Keyword::Else).is_ok() {
    //             let no = self.parse_expr().expected("an expression")?;
    //             Some(no)
    //         } else {
    //             None
    //         };
    //         Ok(Expr::If(If {
    //             span: if_span.to(no.as_ref().map(|e| e.span()).unwrap_or_else(|| yes.span())),
    //             condition: Box::new(condition),
    //             yes: Box::new(yes),
    //             no: no.map(Box::new),
    //         }))
    //     }

    //     fn parse_while(&mut self) -> Parsed<Expr<'i>> {
    //         let while_span = self.eat_kw(Keyword::While)?;
    //         let (condition, _) = self.parens(|p| p.parse_expr()).expected("a condition")?;
    //         let body = self.parse_expr().expected("an expression")?;
    //         Ok(Expr::While(While {
    //             span: while_span.to(body.span()),
    //             condition: Box::new(condition),
    //             body: Box::new(body),
    //         }))
    //     }

    //     fn parse_variant_lit(&mut self) -> Parsed<Expr<'i>> {
    //         let span = self.eat_punct(Punctuation::Period)?;
    //         let name = self.ident().expected("variant name")?;
    //         if let Ok((expr, paren_span)) = self.parens(|p| p.parse_expr()).invalid()? {
    //             Ok(Expr::Variant(Variant {
    //                 span: span.to(paren_span),
    //                 variant: name,
    //                 value: Some(Box::new(expr)),
    //             }))
    //         } else {
    //             Ok(Expr::Variant(Variant {
    //                 span: span.to(name.span()),
    //                 variant: name,
    //                 value: None,
    //             }))
    //         }
    //     }

    //     fn parse_ref_to(&mut self) -> Parsed<Expr<'i>> {
    //         let span = self.eat_punct(Punctuation::Et)?;
    //         let expr = self.parse_expr().expected("an l-value")?;
    //         Ok(Expr::RefTo(RefTo {
    //             span: span.to(expr.span()),
    //             rhs: Box::new(expr),
    //         }))
    //     }

    //     fn parse_primary_expr(&mut self) -> Option<Expr<'i>> {
    //         let mut expr = self
    //             .ident()
    //             .map(Expr::Variable)
    //             .or_else(|| self.int_lit().map(Expr::Int))
    //             .or_else(|| self.str_lit().map(Expr::String))
    //             .or_else(|| self.bool_lit().map(Expr::Bool))
    //             .or_else(|| self.parse_struct_or_block())
    //             .or_else(|| self.parse_variant_lit())
    //             .or_else(|| self.parse_ref_to())
    //             .or_else(|| self.parse_if())
    //             .or_else(|| self.parse_while())?;

    //         expr = self.parse_primary_rhs(expr);

    //         Ok(expr)
    //     }
}

#[cfg(test)]
mod test {
    use iiv::str_source::StrSource;

    use super::*;

    #[test]
    fn dummy() {
        let ctx = iiv::Ctx::new();
        ctx.init();

        let mut source = StrSource::new(
            "test".to_string(),
            "fun name(arg: ty, , arg2: ty2) -> res + some + yes",
        );

        let mut parser = Parser::new(&ctx, &mut source);

        let module = parser.parse_program();

        eprintln!("{:#?}", &module);
        assert_eq!(1, 0);
    }
}
