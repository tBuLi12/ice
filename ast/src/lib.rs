use iiv::Span;

#[derive(Debug)]
pub struct Ident<'i> {
    pub span: Span,
    pub value: iiv::str::Str<'i>,
}

#[derive(Debug)]
pub enum Type<'i> {
    Named(NamedType<'i>),
    Tuple(TupleType<'i>),
    Struct(StructType<'i>),
    Vector(VectorType<'i>),
    Union(UnionType<'i>),
    Variant(VariantType<'i>),
}

#[derive(Debug)]
pub struct NamedType<'i> {
    pub span: Span,
    pub name: Path<'i>,
    pub type_argument_names: Vec<Type<'i>>,
}

#[derive(Debug)]
pub struct TupleType<'i> {
    pub span: Span,
    pub fields: Vec<Type<'i>>,
}

#[derive(Debug)]
pub struct StructType<'i> {
    pub span: Span,
    pub properties: Vec<PropertyDeclaration<'i>>,
}

#[derive(Debug)]
pub struct PropertyDeclaration<'i> {
    pub span: Span,
    pub name: String,
    pub typ: Type<'i>,
}

#[derive(Debug)]
pub struct VariantDeclaration<'i> {
    pub span: Span,
    pub name: String,
    pub typ: Option<Type<'i>>,
}

#[derive(Debug)]
pub struct UnionType<'i> {
    pub span: Span,
    pub elements: Vec<Type<'i>>,
}

#[derive(Debug)]
pub struct VariantType<'i> {
    pub span: Span,
    pub variants: Vec<VariantDeclaration<'i>>,
}

#[derive(Debug)]
pub struct VectorType<'i> {
    pub span: Span,
    pub element_ty: Box<Type<'i>>,
}

#[derive(Debug)]
pub struct Path<'i> {
    pub span: Span,
    pub segments: Vec<&'i str>,
}

#[derive(Debug)]
pub struct Trait<'i> {
    pub span: Span,
    pub name: Path<'i>,
    pub type_args: Vec<Type<'i>>,
}

#[derive(Debug)]
pub struct TypeParam<'i> {
    pub span: Span,
    pub name: Ident<'i>,
    pub trait_bounds: Vec<Trait<'i>>,
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
    pub type_params: Vec<TypeParam<'i>>,
    pub visibility: Visibility,
    pub proto_visibility: Visibility,
    pub proto: Type<'i>,
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
    pub span: Span,
    pub name: Ident<'i>,
    pub ty: Type<'i>,
}

#[derive(Debug)]
pub struct Signature<'i> {
    pub span: Span,
    pub name: Ident<'i>,
    pub is_mut: bool,
    pub type_params: Vec<TypeParam<'i>>,
    pub params: Vec<Parameter<'i>>,
    pub return_ty: Option<Type<'i>>,
    pub visibility: Visibility,
}

#[derive(Debug)]
pub struct Function<'i> {
    pub signature: Signature<'i>,
    pub body: Expr<'i>,
}

#[derive(Debug)]
pub struct TraitBound<'i> {
    pub ty: Type<'i>,
    pub tr: Trait<'i>,
}

#[derive(Debug)]
pub struct Promotion<'i> {
    pub span: Span,
    pub prop: Ident<'i>,
}

#[derive(Debug)]
pub struct Impl<'i> {
    pub span: Span,
    pub ty: Type<'i>,
    pub type_params: Vec<TypeParam<'i>>,
    pub promotions: Vec<Promotion<'i>>,
    pub functions: Vec<Function<'i>>,
    pub where_clause: Vec<TraitBound<'i>>,
}

#[derive(Debug)]
pub struct TraitImpl<'i> {
    pub span: Span,
    pub ty: Type<'i>,
    pub tr: Trait<'i>,
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

#[derive(Debug)]
pub enum BlockItem<'i> {
    Expr(Expr<'i>),
    Break(Break<'i>),
    Return(Return<'i>),
    Continue(Continue<'i>),
    Bind(Binding<'i>),
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

#[derive(Debug)]
pub enum BindingType {
    Let,
    Var,
    Const,
}

#[derive(Debug)]
pub struct Binding<'i> {
    pub span: Span,
    pub binding: Pattern<'i>,
    pub value: Expr<'i>,
}

#[derive(Debug)]
pub struct Pattern<'i> {
    pub span: Span,
    pub body: PatternBody<'i>,
    pub guard: Option<Expr<'i>>,
}

#[derive(Debug)]
pub enum PatternBody<'i> {
    Tuple(Vec<Pattern<'i>>),
    Struct(Vec<(Ident<'i>, Pattern<'i>)>),
    Variant(Box<Pattern<'i>>),
    NarrowType(Box<(Pattern<'i>, Expr<'i>)>),
    Named(Ident<'i>, Box<Pattern<'i>>),
    Vector(Vec<Pattern<'i>>),
    NarrowTraitBounds(Box<(Pattern<'i>, Expr<'i>)>),
    UnionTy(Vec<Pattern<'i>>),
    VariantTy(Vec<(Ident<'i>, Pattern<'i>)>),
    NamedTy(Ident<'i>, Vec<Pattern<'i>>),
    Type(Box<Pattern<'i>>),
    Bind(BindingType, Ident<'i>),
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
    pub name: Option<NamedType<'i>>,
}

#[derive(Debug)]
pub struct DestructureTuple<'i> {
    pub span: Span,
    pub fields: Vec<Pattern<'i>>,
    pub name: Option<NamedType<'i>>,
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
    pub ty: Type<'i>,
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

#[derive(Debug)]
pub struct Int {
    pub span: Span,
    pub value: u32,
}

#[derive(Debug)]
pub struct Float {
    pub span: Span,
    pub value: f64,
}

#[derive(Debug)]
pub struct StringLit<'i> {
    pub span: Span,
    pub value: &'i str,
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
    pub ty: Option<NamedType<'i>>,
}

#[derive(Debug)]
pub struct StructProp<'i> {
    pub span: Span,
    pub name: iiv::str::Str<'i>,
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
    pub span: Span,
    pub pattern: Pattern<'i>,
    pub value: MatchArm<'i>,
}

#[derive(Debug)]
pub enum MatchArm<'i> {
    Break(Break<'i>),
    Return(Return<'i>),
    Continue(Continue<'i>),
    Expr(Expr<'i>),
}

#[derive(Debug)]
pub struct Call<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub type_args: Vec<Type<'i>>,
    pub args: Vec<Expr<'i>>,
}

#[derive(Debug)]
pub struct Prop<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub prop: Ident<'i>,
    pub tr: Option<Trait<'i>>,
}

#[derive(Debug)]
pub struct Field<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub prop: Int,
}

#[derive(Debug)]
pub struct Index<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub index: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Cast<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub ty: Type<'i>,
}

#[derive(Debug)]
pub struct Add<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Sub<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Mul<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Div<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct And<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Or<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Greater<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Less<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct GreaterEq<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct LessEq<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Equals<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Is<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Pattern<'i>>,
}

#[derive(Debug)]
pub struct NotEquals<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Assign<'i> {
    pub span: Span,
    pub lhs: Box<Expr<'i>>,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Not<'i> {
    pub span: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Neg<'i> {
    pub span: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct Bs<'i> {
    pub span: Span,
    pub rhs: Box<Expr<'i>>,
}

#[derive(Debug)]
pub struct AddAssign<'i> {
    pub span: Span,
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
pub enum VectorBody<'i> {
    Type(Type<'i>),
    Items(Vec<VectorItem<'i>>),
}

#[derive(Debug)]
pub struct Vector<'i> {
    pub span: Span,
    pub body: VectorBody<'i>,
}

#[derive(Debug)]
pub struct Variant<'i> {
    pub span: Span,
    pub ty: Option<NamedType<'i>>,
    pub variant: Ident<'i>,
    pub value: Option<Box<Expr<'i>>>,
}

#[derive(Debug)]
pub enum Expr<'i> {
    Variable(Ident<'i>),
    Block(Block<'i>),
    Int(Int),
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
    Index(Index<'i>),
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
    Bs(Bs<'i>),
    Vec(Vector<'i>),
    Type(Type<'i>),
    Variant(Variant<'i>),
    AddAssign(AddAssign<'i>),
    Is(Is<'i>),
}

impl<'i> Expr<'i> {
    pub fn span(&self) -> &Span {
        unimplemented!()
    }
}
