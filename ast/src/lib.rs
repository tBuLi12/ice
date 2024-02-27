use iiv::{str::Str, Position, Span};

macro_rules! spanned_enum {
    (
        pub enum $name:ident$(<$lf:lifetime>)? {
            $(
                $vname:ident($vtype:ty)
            ),*$(,)?
        }
    ) => {
        #[derive(Debug)]
        pub enum $name$(<$lf>)* {
            $(
                $vname($vtype)
            ),*
        }

        impl$(<$lf>)* Spanned for $name$(<$lf>)* {
            fn left(&self) -> Position {
                match self {
                    $(
                        Self::$vname(v) => Spanned::left(v)
                    ),*
                }
            }
            fn right(&self) -> Position {
                match self {
                    $(
                        Self::$vname(v) => Spanned::right(v)
                    ),*
                }
            }
        }
    };
}

mod punctuated_lists {
    use super::*;

    #[derive(Debug)]
    pub enum Element<T> {
        Item(T),
        Separator(Span),
    }

    pub struct PunctuatedIter<T, Items: Iterator<Item = T>, Separators: Iterator<Item = Span>> {
        items: Items,
        separators: Separators,
        item_next: bool,
    }

    #[derive(Debug)]
    pub struct TrailingList<T> {
        items: Vec<T>,
        separators: Vec<Span>,
    }

    impl<T> TrailingList<T> {
        pub fn new() -> Self {
            Self {
                items: vec![],
                separators: vec![],
            }
        }

        pub fn items(&self) -> &[T] {
            &self.items
        }

        pub fn separators(&self) -> &[Span] {
            &self.separators
        }

        pub fn interspersed(&self) -> impl Iterator<Item = Element<&T>> {
            PunctuatedIter {
                items: self.items.iter(),
                separators: self.separators.iter().copied(),
                item_next: true,
            }
        }

        pub fn last(&self) -> Option<Element<&T>> {
            if self.separators.len() > self.items.len() {
                self.separators.last().copied().map(Element::Separator)
            } else {
                self.items.last().map(Element::Item)
            }
        }

        pub fn add_item(&mut self, item: T) {
            self.items.push(item);
        }

        pub fn add_separator(&mut self, span: Span) {
            self.separators.push(span);
        }

        pub fn prepend_item(&mut self, item: T) {
            self.items.insert(0, item);
        }

        pub fn prepend_separator(&mut self, separator: Span) {
            self.separators.insert(0, separator);
        }
    }

    impl<T, Items, Separators> Iterator for PunctuatedIter<T, Items, Separators>
    where
        Items: Iterator<Item = T>,
        Separators: Iterator<Item = Span>,
    {
        type Item = Element<T>;
        fn next(&mut self) -> Option<Self::Item> {
            if self.item_next {
                if let Some(item) = self.items.next() {
                    self.item_next = false;
                    return Some(Element::Item(item));
                }
            } else {
                if let Some(separator) = self.separators.next() {
                    self.item_next = true;
                    return Some(Element::Separator(separator));
                }
            }

            None
        }
    }
}

pub use punctuated_lists::{Element, TrailingList};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Ident<'i> {
    pub span: Span,
    pub value: Option<iiv::str::Str<'i>>,
}

#[derive(Debug)]
pub struct InlineTraitBounds<'i> {
    pub colon: Span,
    pub bounds: TrailingList<Expr<'i>>,
}

#[derive(Debug)]
pub struct TypeParam<'i> {
    pub name: Ident<'i>,
    pub trait_bounds: Option<InlineTraitBounds<'i>>,
}

impl<'i> TypeParam<'i> {
    pub fn bounds(&self) -> &[Expr<'i>] {
        match &self.trait_bounds {
            Some(bounds) => bounds.bounds.items(),
            None => &[],
        }
    }
}

spanned_enum! {
    pub enum Visibility {
        Private(Span),
        Internal(Span),
        Public(Span),
    }
}

spanned_enum! {
    pub enum TypeDeclKeyword {
        Type(Span),
        Data(Span),
    }
}

#[derive(Debug)]
pub struct TypeParamList<'i> {
    pub l_bracket: Span,
    pub list: TrailingList<TypeParam<'i>>,
    pub r_bracket: Span,
}

#[derive(Debug)]
pub struct TypeDecl<'i> {
    pub visibility: Option<Visibility>,
    pub decl_keyword: TypeDeclKeyword,
    pub name: Ident<'i>,
    pub type_params: Option<TypeParamList<'i>>,
    pub proto_visibility: Option<Visibility>,
    pub proto: Expr<'i>,
}

impl<'i> TypeDecl<'i> {
    pub fn ty_params(&self) -> &[TypeParam<'i>] {
        match &self.type_params {
            Some(params) => params.list.items(),
            None => &[],
        }
    }

    pub fn is_data(&self) -> bool {
        matches!(self.decl_keyword, TypeDeclKeyword::Data(_))
    }
}

#[derive(Debug)]
pub struct TraitDecl<'i> {
    pub visibility: Option<Visibility>,
    pub trait_keyword: Span,
    pub name: Ident<'i>,
    pub type_params: Option<TypeParamList<'i>>,
    pub l_brace: Span,
    pub signatures: Vec<Function<'i>>,
    pub r_brace: Span,
}

impl<'i> TraitDecl<'i> {
    pub fn ty_params(&self) -> &[TypeParam<'i>] {
        match &self.type_params {
            Some(params) => params.list.items(),
            None => &[],
        }
    }
}

#[derive(Debug)]
pub struct Parameter<'i> {
    pub name: Ident<'i>,
    pub colon: Span,
    pub ty: Expr<'i>,
}

spanned_enum! {
    pub enum FunDeclKeyword {
        Fun(Span),
        Mut(Span),
    }
}

#[derive(Debug)]
pub struct ReturnType<'i> {
    pub colon: Span,
    pub ty: Expr<'i>,
}

#[derive(Debug)]
pub struct Function<'i> {
    pub visibility: Option<Visibility>,
    pub fun_decl_keyword: FunDeclKeyword,
    pub name: Ident<'i>,
    pub type_params: Option<TypeParamList<'i>>,
    pub l_paren: Span,
    pub params: TrailingList<Parameter<'i>>,
    pub r_paren: Span,
    pub return_ty: Option<ReturnType<'i>>,
    pub body: FunctionBody<'i>,
}

impl<'i> Function<'i> {
    pub fn is_mut(&self) -> bool {
        matches!(self.fun_decl_keyword, FunDeclKeyword::Mut(_))
    }

    pub fn ty_params(&self) -> &[TypeParam<'i>] {
        match &self.type_params {
            Some(params) => params.list.items(),
            None => &[],
        }
    }
}

#[derive(Debug)]
pub struct InlineBody<'i> {
    pub arrow: Span,
    pub body: Expr<'i>,
    pub semicolon: Span,
}

spanned_enum! {
    pub enum FunctionBody<'i> {
        Block(Block<'i>),
        Inline(InlineBody<'i>),
        None(Span),
    }
}

impl<'i> FunctionBody<'i> {
    pub fn is_not_empty(&self) -> bool {
        !matches!(self, FunctionBody::None(_))
    }
}

#[derive(Debug)]
pub struct TraitBound<'i> {
    pub ty: Expr<'i>,
    pub colon: Span,
    pub tr: Expr<'i>,
}

#[derive(Debug)]
pub struct Promotion<'i> {
    pub span: Span,
    pub prop: Ident<'i>,
}

#[derive(Debug)]
pub struct Impl<'i> {
    pub def_keyword: Span,
    pub type_params: Option<TypeParamList<'i>>,
    pub ty: Expr<'i>,
    pub l_brace: Span,
    pub functions: Vec<Function<'i>>,
    pub r_brace: Span,
}

#[derive(Debug)]
pub struct TraitImpl<'i> {
    pub def_keyword: Span,
    pub type_params: Option<TypeParamList<'i>>,
    pub ty: Expr<'i>,
    pub as_keyword: Span,
    pub tr: Expr<'i>,
    pub l_brace: Span,
    pub functions: Vec<Function<'i>>,
    pub r_brace: Span,
}

impl<'i> TraitImpl<'i> {
    pub fn ty_params(&self) -> &[TypeParam<'i>] {
        match &self.type_params {
            Some(params) => params.list.items(),
            None => &[],
        }
    }
}

#[derive(Debug)]
pub struct Import<'i> {
    pub visibility: Option<Visibility>,
    pub import_keyword: Span,
    pub path: TrailingList<Ident<'i>>,
    pub semicolon: Span,
}

#[derive(Debug)]
pub struct Module<'i> {
    pub imports: Vec<Import<'i>>,
    pub functions: Vec<Function<'i>>,
    pub types: Vec<TypeDecl<'i>>,
    pub traits: Vec<TraitDecl<'i>>,
    pub impls: Vec<Impl<'i>>,
    pub trait_impls: Vec<TraitImpl<'i>>,
}

#[derive(Debug)]
pub struct Block<'i> {
    pub l_brace: Span,
    pub items: TrailingList<BlockItem<'i>>,
    pub r_brace: Span,
}

spanned_enum! {
    pub enum BlockItem<'i> {
        Expr(Expr<'i>),
        Break(Break<'i>),
        Return(Return<'i>),
        Continue(Continue<'i>),
        Bind(Binding<'i>),
    }
}

#[derive(Debug)]
pub struct Break<'i> {
    pub break_keyword: Span,
    pub value: Option<Expr<'i>>,
}

#[derive(Debug)]
pub struct Return<'i> {
    pub return_keyword: Span,
    pub value: Option<Expr<'i>>,
}

#[derive(Debug)]
pub struct Continue<'i> {
    pub continue_keyword: Span,
    pub value: Option<Expr<'i>>,
}

spanned_enum! {
    pub enum BindingType {
        Let(Span),
        Var(Span),
        Const(Span),
    }
}

#[derive(Debug)]
pub struct Binding<'i> {
    pub binding_type: BindingType,
    pub binding: Pattern<'i>,
    pub equals: Span,
    pub value: Expr<'i>,
}

#[derive(Debug)]
pub struct NamedPattern<'i> {
    pub name: Ident<'i>,
    pub l_paren: Span,
    pub inner: Box<Pattern<'i>>,
    pub r_paren: Span,
}

#[derive(Debug)]
pub struct VariantPatternBody<'i> {
    pub l_paren: Span,
    pub inner: Box<Pattern<'i>>,
    pub r_paren: Span,
}

#[derive(Debug)]
pub struct VariantPattern<'i> {
    pub period: Span,
    pub name: Ident<'i>,
    pub inner: Option<VariantPatternBody<'i>>,
}

#[derive(Debug)]
pub struct VectorPattern<'i> {
    pub span: Span,
    pub patterns: Vec<Pattern<'i>>,
}

#[derive(Debug)]
pub struct TuplePattern<'i> {
    pub l_paren: Span,
    pub patterns: TrailingList<Pattern<'i>>,
    pub r_paren: Span,
}

#[derive(Debug)]
pub struct UnionPattern<'i> {
    pub span: Span,
    pub patterns: Vec<Pattern<'i>>,
}

#[derive(Debug)]
pub struct NamedTyPattern<'i> {
    pub span: Span,
    pub name: Ident<'i>,
    pub agrs: Vec<Pattern<'i>>,
}

#[derive(Debug)]
pub struct PropPattern<'i> {
    pub name: Ident<'i>,
    pub colon: Span,
    pub pattern: Pattern<'i>,
}

#[derive(Debug)]
pub struct StructPattern<'i> {
    pub l_brace: Span,
    pub inner: TrailingList<PropPattern<'i>>,
    pub r_brace: Span,
}

#[derive(Debug)]
pub struct VariantTyPattern<'i> {
    pub span: Span,
    pub inner: Vec<(Ident<'i>, Pattern<'i>)>,
}

#[derive(Debug)]
pub struct BindPattern<'i> {
    pub binding_type: BindingType,
    pub name: Ident<'i>,
}

#[derive(Debug)]
pub struct NarrowTypePattern<'i> {
    pub inner: Box<Pattern<'i>>,
    pub colon: Span,
    pub ty: Expr<'i>,
}

spanned_enum! {
    pub enum Pattern<'i> {
        Literal(Expr<'i>),
        Tuple(TuplePattern<'i>),
        Struct(StructPattern<'i>),
        Variant(VariantPattern<'i>),
        NarrowType(NarrowTypePattern<'i>),
        Named(NamedPattern<'i>),
        Vector(VectorPattern<'i>),
        UnionTy(UnionPattern<'i>),
        VariantTy(VariantTyPattern<'i>),
        NamedTy(NamedTyPattern<'i>),
        Type(Box<Pattern<'i>>),
        Bind(BindPattern<'i>),
    }
}

#[derive(Debug)]
pub enum DestructureProp<'i> {
    Explicit(DestructureExplicitProp<'i>),
    Implicit(DestructureImplicitProp<'i>),
}

#[derive(Debug)]
pub struct DestructureExplicitProp<'i> {
    pub span: Span,
    pub prop: Ident<'i>,
    pub pattern: Pattern<'i>,
}

#[derive(Debug)]
pub struct DestructureImplicitProp<'i> {
    pub span: Span,
    pub prop: Expr<'i>,
}

#[derive(Debug)]
pub struct DestructureStruct<'i> {
    pub span: Span,
    pub props: Vec<DestructureProp<'i>>,
    pub name: Option<Box<Expr<'i>>>,
}

#[derive(Debug)]
pub struct DestructureTuple<'i> {
    pub span: Span,
    pub fields: Vec<Pattern<'i>>,
    pub name: Option<Box<Expr<'i>>>,
}

#[derive(Debug)]
pub struct DestructureVector<'i> {
    pub span: Span,
    pub items: Vec<ElementPattern<'i>>,
}

#[derive(Debug)]
pub enum ElementPattern<'i> {
    Rest(Rest<'i>),
    Pattern(Pattern<'i>),
}

#[derive(Debug)]
pub struct Rest<'i> {
    pub span: Span,
    pub binding: Option<Ident<'i>>,
}

#[derive(Debug)]
pub struct DestructureUnion<'i> {
    pub span: Span,
    pub ty: Expr<'i>,
    pub pattern: Box<Pattern<'i>>,
}
#[derive(Debug)]
pub struct DestructureVariant<'i> {
    pub span: Span,
    pub name: Ident<'i>,
    pub pattern: Option<Box<Pattern<'i>>>,
}

#[derive(Debug)]
pub enum Destructure<'i> {
    Struct(DestructureStruct<'i>),
    Tuple(DestructureTuple<'i>),
    Vector(DestructureVector<'i>),
    Union(DestructureUnion<'i>),
    Variant(DestructureVariant<'i>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Int {
    pub span: Span,
    pub value: u32,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Bool {
    pub span: Span,
    pub value: bool,
}

#[derive(Debug)]
pub struct Float {
    pub span: Span,
    pub value: f64,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct StringLit<'i> {
    pub span: Span,
    pub value: Str<'i>,
}

#[derive(Debug)]
pub struct Char {
    pub span: Span,
    pub value: i8,
}

#[derive(Debug)]
pub struct Tuple<'i> {
    pub l_paren: Span,
    pub fields: TrailingList<Expr<'i>>,
    pub r_paren: Span,
}

#[derive(Debug)]
pub struct Struct<'i> {
    pub ty: Option<Box<Expr<'i>>>,
    pub l_brace: Span,
    pub props: TrailingList<StructProp<'i>>,
    pub r_brace: Span,
}

#[derive(Debug)]
pub struct ExplicitProp<'i> {
    pub equals: Span,
    pub value: Expr<'i>,
}

#[derive(Debug)]
pub struct StructProp<'i> {
    pub period: Span,
    pub name: Ident<'i>,
    pub value: Option<ExplicitProp<'i>>,
}

#[derive(Debug)]
pub struct VariantTypeAnnotation<'i> {
    pub colon: Span,
    pub ty: Expr<'i>,
}

#[derive(Debug)]
pub struct VariantTyProp<'i> {
    pub hash: Span,
    pub name: Ident<'i>,
    pub ty: Option<VariantTypeAnnotation<'i>>,
}

#[derive(Debug)]
pub struct StructTyProp<'i> {
    pub name: Ident<'i>,
    pub colon: Span,
    pub ty: Expr<'i>,
}

#[derive(Debug)]
pub struct StructTy<'i> {
    pub l_brace: Span,
    pub props: TrailingList<StructTyProp<'i>>,
    pub r_brace: Span,
}

#[derive(Debug)]
pub struct VariantTy<'i> {
    pub l_brace: Span,
    pub props: TrailingList<VariantTyProp<'i>>,
    pub r_brace: Span,
}

#[derive(Debug)]
pub struct ElseBranch<'i> {
    pub else_keyword: Span,
    pub value: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct If<'i> {
    pub if_keyword: Span,
    pub l_paren: Span,
    pub condition: Box<Expr<'i>>,
    pub r_paren: Span,
    pub yes: Box<Expr<'i>>,
    pub no: Option<ElseBranch<'i>>,
}

#[derive(Debug)]
pub struct While<'i> {
    pub while_keyword: Span,
    pub l_paren: Span,
    pub condition: Box<Expr<'i>>,
    pub r_paren: Span,
    pub body: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Match<'i> {
    pub match_keyword: Span,
    pub scrutinee: Box<Expr<'i>>,
    pub l_brace: Span,
    pub body: TrailingList<MatchCase<'i>>,
    pub r_brace: Span,
}

#[derive(Debug)]
pub struct MatchCase<'i> {
    pub pattern: Pattern<'i>,
    pub fat_arrow: Span,
    pub value: Expr<'i>,
}

#[derive(Debug)]
pub struct Call<'i> {
    pub lhs: Box<Expr<'i>>,
    pub l_paren: Span,
    pub args: TrailingList<Expr<'i>>,
    pub r_paren: Span,
}

#[derive(Debug)]
pub struct TraitSpecifier<'i> {
    pub l_bracket: Span,
    pub tr: Box<Expr<'i>>,
    pub r_bracket: Span,
}

#[derive(Debug)]
pub struct Prop<'i> {
    pub lhs: Box<Expr<'i>>,
    pub period: Span,
    pub prop: Ident<'i>,
    pub tr: Option<TraitSpecifier<'i>>,
}

#[derive(Debug)]
pub struct Deref<'i> {
    pub lhs: Box<Expr<'i>>,
    pub dot_asterisk: Span,
}

#[derive(Debug)]
pub struct Field<'i> {
    pub lhs: Box<Expr<'i>>,
    pub period: Span,
    pub prop: Int,
}

#[derive(Debug)]
pub struct TyArgApply<'i> {
    pub lhs: Box<Expr<'i>>,
    pub l_bracket: Span,
    pub args: TrailingList<Expr<'i>>,
    pub r_bracket: Span,
}

#[derive(Debug)]
pub struct Cast<'i> {
    pub lhs: Box<Expr<'i>>,
    pub as_keyword: Span,
    pub ty: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Add<'i> {
    pub lhs: Box<Expr<'i>>,
    pub plus: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Sub<'i> {
    pub lhs: Box<Expr<'i>>,
    pub minus: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Mul<'i> {
    pub lhs: Box<Expr<'i>>,
    pub asterisk: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Div<'i> {
    pub lhs: Box<Expr<'i>>,
    pub slash: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct And<'i> {
    pub lhs: Box<Expr<'i>>,
    pub ampersands: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Or<'i> {
    pub lhs: Box<Expr<'i>>,
    pub pipes: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Greater<'i> {
    pub lhs: Box<Expr<'i>>,
    pub greater: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Less<'i> {
    pub lhs: Box<Expr<'i>>,
    pub less: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct GreaterEq<'i> {
    pub lhs: Box<Expr<'i>>,
    pub greater_eq: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct LessEq<'i> {
    pub lhs: Box<Expr<'i>>,
    pub less_eq: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Equals<'i> {
    pub lhs: Box<Expr<'i>>,
    pub equals: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Is<'i> {
    pub lhs: Box<Expr<'i>>,
    pub is_keyword: Span,
    pub rhs: Box<Pattern<'i>>,
}

#[derive(Debug)]
pub struct NotEquals<'i> {
    pub lhs: Box<Expr<'i>>,
    pub not_eq: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Assign<'i> {
    pub lhs: Box<Expr<'i>>,
    pub equals: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Not<'i> {
    pub bang: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct RefTo<'i> {
    pub ampersand: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Neg<'i> {
    pub minus: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct AddAssign<'i> {
    pub lhs: Box<Expr<'i>>,
    pub plus_eq: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub enum VectorItem<'i> {
    Spread(Spread<'i>),
    Expr(Expr<'i>),
}

#[derive(Debug)]
pub struct Spread<'i> {
    pub span: Span,
    pub value: Expr<'i>,
}

#[derive(Debug)]
pub struct Vector<'i> {
    pub span: Span,
    pub body: Vec<Expr<'i>>,
}

#[derive(Debug)]
pub struct VariantValue<'i> {
    pub l_paren: Span,
    pub value: Box<Expr<'i>>,
    pub r_paren: Span,
}

#[derive(Debug)]
pub struct Variant<'i> {
    pub period: Span,
    pub variant: Ident<'i>,
    pub value: Option<VariantValue<'i>>,
}

spanned_enum! {
    pub enum Expr<'i> {
        Variable(Ident<'i>),
        Block(Block<'i>),
        Int(Int),
        Bool(Bool),
        Float(Float),
        String(StringLit<'i>),
        Char(Char),
        Tuple(Tuple<'i>),
        Struct(Struct<'i>),
        If(If<'i>),
        While(While<'i>),
        Match(Match<'i>),
        Call(Call<'i>),
        Prop(Prop<'i>),
        Field(Field<'i>),
        TyArgApply(TyArgApply<'i>),
        Cast(Cast<'i>),
        Add(Add<'i>),
        Mul(Mul<'i>),
        Eq(Equals<'i>),
        Neq(NotEquals<'i>),
        And(And<'i>),
        Or(Or<'i>),
        Geq(GreaterEq<'i>),
        Leq(LessEq<'i>),
        Lt(Less<'i>),
        Gt(Greater<'i>),
        Assign(Assign<'i>),
        Div(Div<'i>),
        Sub(Sub<'i>),
        Neg(Neg<'i>),
        Not(Not<'i>),
        RefTo(RefTo<'i>),
        Deref(Deref<'i>),
        Vec(Vector<'i>),
        Variant(Variant<'i>),
        AddAssign(AddAssign<'i>),
        Is(Is<'i>),
        VariantTy(VariantTy<'i>),
        StructTy(StructTy<'i>),
        RefTy(RefTo<'i>),
        PtrTy(RefTo<'i>),
        Invalid(Span),
    }
}

trait OptSpanned {
    fn opt_right(&self) -> Option<Position>;
}

impl<T: Spanned> OptSpanned for Vec<T> {
    fn opt_right(&self) -> Option<Position> {
        Some(self.last()?.right())
    }
}

impl<T: Spanned> OptSpanned for Option<T> {
    fn opt_right(&self) -> Option<Position> {
        self.as_ref().map(Spanned::right)
    }
}

impl<T: Spanned> OptSpanned for TrailingList<T> {
    fn opt_right(&self) -> Option<Position> {
        Some(self.last()?.right())
    }
}

impl<T: Spanned> Spanned for Box<T> {
    fn left(&self) -> Position {
        T::left(&self)
    }
    fn right(&self) -> Position {
        T::right(&self)
    }
}

impl<T: Spanned> Spanned for Element<T> {
    fn left(&self) -> Position {
        match self {
            Self::Item(item) => item.left(),
            Self::Separator(span) => span.left(),
        }
    }

    fn right(&self) -> Position {
        match self {
            Self::Item(item) => item.right(),
            Self::Separator(span) => span.right(),
        }
    }
}

impl<T: Spanned> Spanned for &T {
    fn left(&self) -> Position {
        T::left(self)
    }

    fn right(&self) -> Position {
        T::right(self)
    }
}

impl Spanned for Span {
    fn left(&self) -> Position {
        self.left
    }
    fn right(&self) -> Position {
        self.right
    }
}

impl<L: Spanned, R: Spanned> Spanned for (L, R) {
    fn left(&self) -> Position {
        self.0.left()
    }
    fn right(&self) -> Position {
        self.1.right()
    }
}

pub trait Spanned {
    fn span(&self) -> Span {
        let left = self.left();
        let right = self.right();
        left.to(right)
    }

    fn left(&self) -> Position;
    fn right(&self) -> Position;
}

macro_rules! spanned_impls {
    (@single $tyname:ident) => {
        impl<'i> Spanned for $tyname<'i> {
            fn span(&self) -> Span {
                self.span
            }
            fn left(&self) -> Position {
                self.span.left
            }
            fn right(&self) -> Position {
                self.span.right
            }
        }
    };

    (@single $tyname:ident : $(?$l_opts:ident)* $left:ident - $(?$opts:ident)* $last:ident) => {
        impl<'i> Spanned for $tyname<'i> {
            fn left(&self) -> Position {
                None
                    $(.or_else(|| self.$l_opts.opt_right()))*
                    .unwrap_or_else(|| self.$left.left())
            }
            fn right(&self) -> Position {
                None
                    $(.or_else(|| self.$opts.opt_right()))*
                    .unwrap_or_else(|| self.$last.right())
            }
        }
    };

    ($($tyname:ident $(:$(?$l_opts:ident)* $left:ident - $(?$opts:ident)* $last:ident)?),*$(,)?) => {
        $(
            spanned_impls!(@single $tyname $(: $(?$l_opts)* $left - $(?$opts)* $last)*);
        )*
    }
}

spanned_impls! {
    Ident,
    InlineTraitBounds: colon - ?bounds colon,
    TypeParam : name - ?trait_bounds name,
    Parameter : name - ty,
    TypeDecl: ?visibility decl_keyword - proto,
    Function: ?visibility fun_decl_keyword - body,
    InlineBody: arrow - semicolon,
    TraitBound: ty - tr,
    Impl: def_keyword - r_brace,
    TraitImpl: def_keyword - r_brace,
    Import: ?visibility import_keyword - semicolon,
    Block: l_brace - r_brace,
    Break: break_keyword - ?value break_keyword,
    Return: return_keyword - ?value return_keyword,
    Continue: continue_keyword - ?value continue_keyword,
    Binding: binding_type - value,
    StringLit,
    Tuple: l_paren - r_paren,
    Struct: l_brace - r_brace,
    StructProp: name - ?value name,
    If: if_keyword - ?no yes,
    ElseBranch: else_keyword - value,
    While: while_keyword - body,
    Match: match_keyword - r_brace,
    MatchCase: pattern - value,
    Call: lhs - r_paren,
    Prop: lhs - prop,
    Field: lhs - prop,
    TyArgApply: lhs - r_bracket,
    Cast: lhs - ty,
    Add: lhs - rhs,
    Sub: lhs - rhs,
    Mul: lhs - rhs,
    Div: lhs - rhs,
    And: lhs - rhs,
    Or: lhs - rhs,
    Greater: lhs - rhs,
    Less: lhs - rhs,
    GreaterEq: lhs - rhs,
    LessEq: lhs - rhs,
    Equals: lhs - rhs,
    Is: lhs - rhs,
    NotEquals: lhs - rhs,
    Assign: lhs - rhs,
    Not: bang - rhs,
    RefTo: ampersand - rhs,
    Deref: lhs - dot_asterisk,
    Neg: minus - rhs,
    AddAssign: lhs - rhs,
    Spread,
    Vector,
    Variant: period - ?value variant,
    NamedPattern: name - r_paren,
    VariantPattern: period - ?inner name,
    NamedTyPattern,
    BindPattern: name - name,
    TuplePattern: l_paren - r_paren,
    VectorPattern,
    UnionPattern,
    StructPattern: l_brace - r_brace,
    VariantTyPattern,
    NarrowTypePattern: inner - ty,
    ExplicitProp: equals - value,
    VariantPatternBody: l_paren - r_paren,
    VariantValue: l_paren - r_paren,
    VariantTyProp: hash - ?ty name,
    StructTyProp: name - ty,
    VariantTy: l_brace - r_brace,
    StructTy: l_brace - r_brace,
    VariantTypeAnnotation: colon - ty,
}

impl Spanned for Int {
    fn span(&self) -> Span {
        self.span
    }
    fn left(&self) -> Position {
        self.span.left()
    }
    fn right(&self) -> Position {
        self.span.right()
    }
}

impl Spanned for Float {
    fn span(&self) -> Span {
        self.span
    }
    fn left(&self) -> Position {
        self.span.left()
    }
    fn right(&self) -> Position {
        self.span.right()
    }
}

impl Spanned for Char {
    fn span(&self) -> Span {
        self.span
    }
    fn left(&self) -> Position {
        self.span.left()
    }
    fn right(&self) -> Position {
        self.span.right()
    }
}

impl Spanned for Bool {
    fn span(&self) -> Span {
        self.span
    }
    fn left(&self) -> Position {
        self.span.left()
    }
    fn right(&self) -> Position {
        self.span.right()
    }
}
