use iiv::{str::Str, LeftSpan, RightSpan, Span};

macro_rules! spanned_enum {
    (
        pub enum $name:ident<$lf:lifetime> {
            $(
                $vname:ident($vtype:ty)
            ),*$(,)?
        }
    ) => {
        #[derive(Debug)]
        pub enum $name<$lf> {
            $(
                $vname($vtype)
            ),*
        }

        impl<$lf> Spanned for $name<$lf> {
            fn left_span(&self) -> LeftSpan {
                match self {
                    $(
                        Self::$vname(v) => v.left_span()
                    ),*
                }
            }
            fn right_span(&self) -> RightSpan {
                match self {
                    $(
                        Self::$vname(v) => v.right_span()
                    ),*
                }
            }
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Ident<'i> {
    pub span: Span,
    pub value: iiv::str::Str<'i>,
}

#[derive(Debug)]
pub struct TypeParam<'i> {
    pub name: Ident<'i>,
    pub trait_bounds: Vec<Expr<'i>>,
}

#[derive(Debug)]
pub enum Visibility {
    Private,
    Internal,
    Public,
}

#[derive(Debug)]
pub struct TypeDecl<'i> {
    pub span: Span,
    pub name: Ident<'i>,
    pub is_data: bool,
    pub type_params: Vec<TypeParam<'i>>,
    pub visibility: Visibility,
    pub proto_visibility: Visibility,
    pub proto: Expr<'i>,
}

#[derive(Debug)]
pub struct TraitDecl<'i> {
    pub span: Span,
    pub name: Ident<'i>,
    pub type_params: Vec<TypeParam<'i>>,
    pub signatures: Vec<Signature<'i>>,
    pub visibility: Visibility,
}

#[derive(Debug)]
pub struct Parameter<'i> {
    pub name: Ident<'i>,
    pub ty: Expr<'i>,
}

#[derive(Debug)]
pub struct Signature<'i> {
    pub span: Span,
    pub name: Ident<'i>,
    pub is_mut: bool,
    pub type_params: Vec<TypeParam<'i>>,
    pub params: Vec<Parameter<'i>>,
    pub return_ty: Option<Expr<'i>>,
    pub visibility: Visibility,
}

#[derive(Debug)]
pub struct Function<'i> {
    pub signature: Signature<'i>,
    pub body: Option<Expr<'i>>,
}

#[derive(Debug)]
pub struct TraitBound<'i> {
    pub ty: Expr<'i>,
    pub tr: Expr<'i>,
}

#[derive(Debug)]
pub struct Promotion<'i> {
    pub span: Span,
    pub prop: Ident<'i>,
}

#[derive(Debug)]
pub struct Impl<'i> {
    pub span: Span,
    pub ty: Expr<'i>,
    pub type_params: Vec<TypeParam<'i>>,
    pub promotions: Vec<Promotion<'i>>,
    pub functions: Vec<Function<'i>>,
    pub where_clause: Vec<TraitBound<'i>>,
}

#[derive(Debug)]
pub struct TraitImpl<'i> {
    pub span: Span,
    pub ty: Expr<'i>,
    pub tr: Expr<'i>,
    pub type_params: Vec<TypeParam<'i>>,
    pub functions: Vec<Function<'i>>,
}

#[derive(Debug)]
pub struct Import<'i> {
    pub span: Span,
    pub name: Ident<'i>,
    pub path: String,
    pub visibility: Visibility,
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
    pub span: Span,
    pub items: Vec<BlockItem<'i>>,
    pub has_trailing_expression: bool,
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
    pub span: Span,
    pub value: Option<Expr<'i>>,
}

#[derive(Debug)]
pub struct Return<'i> {
    pub span: Span,
    pub value: Option<Expr<'i>>,
}

#[derive(Debug)]
pub struct Continue<'i> {
    pub span: Span,
    pub value: Option<Expr<'i>>,
}

#[derive(Debug, Clone, Copy)]
pub enum BindingType {
    Let,
    Var,
    Const,
}

#[derive(Debug)]
pub struct Binding<'i> {
    pub span: Span,
    pub binding_type: BindingType,
    pub binding: Pattern<'i>,
    pub value: Expr<'i>,
}

#[derive(Debug)]
pub struct Pattern<'i> {
    pub body: PatternBody<'i>,
    pub guard: Option<Expr<'i>>,
}

#[derive(Debug)]
pub struct NamedPattern<'i> {
    pub name: Ident<'i>,
    pub inner: Box<Pattern<'i>>,
}

#[derive(Debug)]
pub struct VariantPattern<'i> {
    pub span: Span,
    pub name: Ident<'i>,
    pub inner: Option<Box<Pattern<'i>>>,
}

#[derive(Debug)]
pub struct VectorPattern<'i> {
    pub span: Span,
    pub patterns: Vec<Pattern<'i>>,
}

#[derive(Debug)]
pub struct TuplePattern<'i> {
    pub span: Span,
    pub patterns: Vec<Pattern<'i>>,
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
pub struct StructPattern<'i> {
    pub span: Span,
    pub inner: Vec<(Ident<'i>, Pattern<'i>)>,
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
    pub ty: Expr<'i>,
}

spanned_enum! {
    pub enum PatternBody<'i> {
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
    pub pattern: PatternBody<'i>,
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
    pub span: Span,
    pub fields: Vec<Expr<'i>>,
}

#[derive(Debug)]
pub struct Struct<'i> {
    pub span: Span,
    pub props: Vec<StructProp<'i>>,
    pub ty: Option<Box<Expr<'i>>>,
}

#[derive(Debug)]
pub struct PropsTy<'i> {
    pub span: Span,
    pub props: Vec<TyProp<'i>>,
}

#[derive(Debug)]
pub struct TyProp<'i> {
    pub name: Ident<'i>,
    pub ty: Option<Expr<'i>>,
}

#[derive(Debug)]
pub struct StructProp<'i> {
    pub name: Ident<'i>,
    pub value: Option<Expr<'i>>,
}

#[derive(Debug)]
pub struct If<'i> {
    pub span: Span,
    pub condition: Box<Expr<'i>>,
    pub yes: Box<Expr<'i>>,
    pub no: Option<Box<Expr<'i>>>,
}

#[derive(Debug)]
pub struct While<'i> {
    pub span: Span,
    pub condition: Box<Expr<'i>>,
    pub body: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Match<'i> {
    pub span: Span,
    pub scrutinee: Box<Expr<'i>>,
    pub body: Vec<MatchCase<'i>>,
}

#[derive(Debug)]
pub struct MatchCase<'i> {
    pub pattern: Pattern<'i>,
    pub value: MatchArm<'i>,
}

spanned_enum! {
    pub enum MatchArm<'i> {
        Break(Break<'i>),
        Return(Return<'i>),
        Continue(Continue<'i>),
        Expr(Expr<'i>),
    }
}

#[derive(Debug)]
pub struct Call<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub args: Vec<Expr<'i>>,
}

#[derive(Debug)]
pub struct Prop<'i> {
    pub lhs: Box<Expr<'i>>,
    pub prop: Ident<'i>,
    pub tr: Option<Box<Expr<'i>>>,
}

#[derive(Debug)]
pub struct Deref<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Field<'i> {
    pub lhs: Box<Expr<'i>>,
    pub prop: Int,
}

#[derive(Debug)]
pub struct TyArgApply<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub args: Vec<Expr<'i>>,
}

#[derive(Debug)]
pub struct Cast<'i> {
    pub lhs: Box<Expr<'i>>,
    pub ty: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Add<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Sub<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Mul<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Div<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct And<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Or<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Greater<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Less<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct GreaterEq<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct LessEq<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Equals<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Is<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Pattern<'i>>,
}

#[derive(Debug)]
pub struct NotEquals<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Assign<'i> {
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Not<'i> {
    pub span: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct RefTo<'i> {
    pub span: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Neg<'i> {
    pub span: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct AddAssign<'i> {
    pub lhs: Box<Expr<'i>>,
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
pub struct Variant<'i> {
    pub span: Span,
    pub variant: Ident<'i>,
    pub value: Option<Box<Expr<'i>>>,
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
        VariantTy(PropsTy<'i>),
        StructTy(PropsTy<'i>),
        RefTy(RefTo<'i>),
        PtrTy(RefTo<'i>)
    }
}

trait OptSpanned {
    fn opt_right_span(&self) -> Option<RightSpan>;
}

impl<T: Spanned> OptSpanned for Vec<T> {
    fn opt_right_span(&self) -> Option<RightSpan> {
        Some(self.last()?.right_span())
    }
}

impl<T: Spanned> OptSpanned for Option<T> {
    fn opt_right_span(&self) -> Option<RightSpan> {
        self.as_ref().map(Spanned::right_span)
    }
}

impl<T: Spanned> Spanned for Box<T> {
    fn left_span(&self) -> LeftSpan {
        T::left_span(&self)
    }
    fn right_span(&self) -> RightSpan {
        T::right_span(&self)
    }
}

impl Spanned for Span {
    fn left_span(&self) -> LeftSpan {
        self.left()
    }
    fn right_span(&self) -> RightSpan {
        self.right()
    }
}

impl<L: Spanned, R: Spanned> Spanned for (L, R) {
    fn left_span(&self) -> LeftSpan {
        self.0.left_span()
    }
    fn right_span(&self) -> RightSpan {
        self.1.right_span()
    }
}

pub trait Spanned {
    fn span(&self) -> Span {
        let left = self.left_span();
        let right = self.right_span();
        left.to(right)
    }

    fn left_span(&self) -> LeftSpan;
    fn right_span(&self) -> RightSpan;
}

macro_rules! spanned_impls {
    (@single $tyname:ident) => {
        impl<'i> Spanned for $tyname<'i> {
            fn span(&self) -> Span {
                self.span
            }
            fn left_span(&self) -> LeftSpan {
                self.span.left()
            }
            fn right_span(&self) -> RightSpan {
                self.span.right()
            }
        }
    };

    (@single $tyname:ident : $left:ident - $(?$opts:ident)* $last:ident) => {
        impl<'i> Spanned for $tyname<'i> {
            fn left_span(&self) -> LeftSpan {
                self.$left.left_span()
            }
            fn right_span(&self) -> RightSpan {
                None
                    $(.or_else(|| self.$opts.opt_right_span()))*
                    .unwrap_or_else(|| self.$last.right_span())
            }
        }
    };

    ($($tyname:ident $(: $left:ident - $(?$opts:ident)* $last:ident)?),*$(,)?) => {
        $(
            spanned_impls!(@single $tyname $(: $left - $(?$opts)* $last)*);
        )*
    }
}

spanned_impls! {
    Ident,
    TypeParam : name - ?trait_bounds name,
    Parameter : name - ty,
    TypeDecl,
    Signature,
    Function: signature - ?body signature,
    TraitBound: ty - tr,
    Promotion,
    Impl,
    TraitImpl,
    Import,
    Block,
    Break,
    Return,
    Continue,
    Binding,
    Pattern : body - ?guard body,
    StringLit,
    Tuple,
    Struct,
    StructProp : name - ?value name,
    If,
    While,
    Match,
    MatchCase : pattern - value,
    Call,
    Prop : lhs - prop,
    Field : lhs - prop,
    TyArgApply,
    Cast : lhs - ty,
    Add : lhs - rhs,
    Sub : lhs - rhs,
    Mul : lhs - rhs,
    Div : lhs - rhs,
    And : lhs - rhs,
    Or : lhs - rhs,
    Greater : lhs - rhs,
    Less : lhs - rhs,
    GreaterEq : lhs - rhs,
    LessEq : lhs - rhs,
    Equals : lhs - rhs,
    Is : lhs - rhs,
    NotEquals : lhs - rhs,
    Assign : lhs - rhs,
    Not,
    RefTo,
    Deref : lhs - span,
    Neg,
    AddAssign : lhs - rhs,
    Spread,
    Vector,
    Variant,
    NamedPattern : name - inner,
    VariantPattern,
    NamedTyPattern,
    BindPattern : name - name,
    TuplePattern,
    VectorPattern,
    UnionPattern,
    StructPattern,
    VariantTyPattern,
    NarrowTypePattern: inner - ty,
    PropsTy,
    TyProp: name - ?ty name,
}

impl Spanned for Int {
    fn span(&self) -> Span {
        self.span
    }
    fn left_span(&self) -> LeftSpan {
        self.span.left()
    }
    fn right_span(&self) -> RightSpan {
        self.span.right()
    }
}

impl Spanned for Float {
    fn span(&self) -> Span {
        self.span
    }
    fn left_span(&self) -> LeftSpan {
        self.span.left()
    }
    fn right_span(&self) -> RightSpan {
        self.span.right()
    }
}

impl Spanned for Char {
    fn span(&self) -> Span {
        self.span
    }
    fn left_span(&self) -> LeftSpan {
        self.span.left()
    }
    fn right_span(&self) -> RightSpan {
        self.span.right()
    }
}

impl Spanned for Bool {
    fn span(&self) -> Span {
        self.span
    }
    fn left_span(&self) -> LeftSpan {
        self.span.left()
    }
    fn right_span(&self) -> RightSpan {
        self.span.right()
    }
}
