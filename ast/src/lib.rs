#[derive(Debug)]
pub struct Span {
    pub first_line: u32,
    pub last_line: u32,
    pub begin_offset: u32,
    pub begin_highlight_offset: u32,
    pub end_highlight_offset: u32,
}

#[derive(Debug)]
pub struct Ident {
    pub span: Span,
    pub value: String,
}

#[derive(Debug)]
pub enum Type {
    Named(NamedType),
    Tuple(TupleType),
    Struct(StructType),
    Vector(VectorType),
    Union(UnionType),
    Variant(VariantType),
}

#[derive(Debug)]
pub struct NamedType {
    pub span: Span,
    pub name: Path,
    pub type_argument_names: Vec<Type>,
}

#[derive(Debug)]
pub struct TupleType {
    pub span: Span,
    pub fields: Vec<Type>,
}

#[derive(Debug)]
pub struct StructType {
    pub span: Span,
    pub properties: Vec<PropertyDeclaration>,
}

#[derive(Debug)]
pub struct PropertyDeclaration {
    pub span: Span,
    pub name: String,
    pub typ: Type,
}

#[derive(Debug)]
pub struct VariantDeclaration {
    pub span: Span,
    pub name: String,
    pub typ: Option<Type>,
}

#[derive(Debug)]
pub struct UnionType {
    pub span: Span,
    pub elements: Vec<Type>,
}

#[derive(Debug)]
pub struct VariantType {
    pub span: Span,
    pub variants: Vec<VariantDeclaration>,
}

#[derive(Debug)]
pub struct VectorType {
    pub span: Span,
    pub element_ty: Box<Type>,
}

#[derive(Debug)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<String>,
}

#[derive(Debug)]
pub struct Trait {
    pub span: Span,
    pub name: Path,
    pub type_args: Vec<Type>,
}

#[derive(Debug)]
pub struct TypeParam {
    pub span: Span,
    pub name: Ident,
    pub trait_bounds: Vec<Trait>,
}

#[derive(Debug)]
pub enum Visibility {
    Private,
    Internal,
    Public,
}

#[derive(Debug)]
pub struct TypeDecl {
    pub span: Span,
    pub name: Ident,
    pub type_params: Vec<TypeParam>,
    pub visibility: Visibility,
    pub proto_visibility: Visibility,
    pub proto: Type,
}

#[derive(Debug)]
pub struct TraitDecl {
    pub span: Span,
    pub name: Ident,
    pub type_params: Vec<TypeParam>,
    pub signatures: Vec<Signature>,
    pub visibility: Visibility,
}

#[derive(Debug)]
pub struct Parameter {
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Signature {
    pub span: Span,
    pub name: Ident,
    pub is_mut: bool,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Parameter>,
    pub return_ty: Option<Type>,
    pub visibility: Visibility,
}

#[derive(Debug)]
pub struct Function {
    pub signature: Signature,
    pub body: Expr,
}

#[derive(Debug)]
pub struct TraitBound {
    pub ty: Type,
    pub tr: Trait,
}

#[derive(Debug)]
pub struct Promotion {
    pub span: Span,
    pub prop: Ident,
}

#[derive(Debug)]
pub struct Impl {
    pub span: Span,
    pub ty: Type,
    pub type_params: Vec<TypeParam>,
    pub promotions: Vec<Promotion>,
    pub functions: Vec<Function>,
    pub where_clause: Vec<TraitBound>,
}

#[derive(Debug)]
pub struct TraitImpl {
    pub span: Span,
    pub ty: Type,
    pub tr: Trait,
    pub type_params: Vec<TypeParam>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Import {
    pub span: Span,
    pub name: Ident,
    pub path: String,
    pub visibility: Visibility,
}

#[derive(Debug)]
pub struct Module {
    pub imports: Vec<Import>,
    pub functions: Vec<Function>,
    pub types: Vec<TypeDecl>,
    pub traits: Vec<TraitDecl>,
    pub impls: Vec<Impl>,
    pub trait_impls: Vec<TraitImpl>,
}

#[derive(Debug)]
pub struct Block {
    pub span: Span,
    pub items: Vec<BlockItem>,
    pub has_trailing_expression: bool,
}

#[derive(Debug)]
pub enum BlockItem {
    Expr(Expr),
    Break(Break),
    Return(Return),
    Continue(Continue),
    Bind(Binding),
}

#[derive(Debug)]
pub struct Break {
    pub span: Span,
    pub value: Option<Expr>,
}

#[derive(Debug)]
pub struct Return {
    pub span: Span,
    pub value: Option<Expr>,
}

#[derive(Debug)]
pub struct Continue {
    pub span: Span,
    pub value: Option<Expr>,
}

#[derive(Debug)]
pub enum BindingType {
    Let,
    Var,
    Const,
}

#[derive(Debug)]
pub struct Binding {
    pub span: Span,
    pub binding_type: BindingType,
    pub binding: Pattern,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Pattern {
    pub span: Span,
    pub body: PatternBody,
    pub guard: Option<Expr>,
}

#[derive(Debug)]
pub enum PatternBody {
    Tuple(Vec<Pattern>),
    Struct(Vec<(Ident, Pattern)>),
    Variant(Box<Pattern>),
    NarrowType(Box<(Pattern, Expr)>),
    Named(Ident, Box<Pattern>),
    Vector(Vec<Pattern>),
    NarrowTraitBounds(Box<(Pattern, Expr)>),
    UnionTy(Vec<Pattern>),
    VariantTy(Vec<(Ident, Pattern)>),
    NamedTy(Ident, Vec<Pattern>),
    Type(Box<Pattern>),
    Bind(BindingType, Ident),
}

#[derive(Debug)]
pub enum DestructureProp {
    Explicit(DestructureExplicitProp),
    Implicit(DestructureImplicitProp),
}

#[derive(Debug)]
pub struct DestructureExplicitProp {
    pub span: Span,
    pub prop: Ident,
    pub pattern: PatternBody,
}

#[derive(Debug)]
pub struct DestructureImplicitProp {
    pub span: Span,
    pub prop: Expr,
}

#[derive(Debug)]
pub struct DestructureStruct {
    pub span: Span,
    pub props: Vec<DestructureProp>,
    pub name: Option<NamedType>,
}

#[derive(Debug)]
pub struct DestructureTuple {
    pub span: Span,
    pub fields: Vec<Pattern>,
    pub name: Option<NamedType>,
}

#[derive(Debug)]
pub struct DestructureVector {
    pub span: Span,
    pub items: Vec<ElementPattern>,
}

#[derive(Debug)]
pub enum ElementPattern {
    Rest(Rest),
    Pattern(Pattern),
}

#[derive(Debug)]
pub struct Rest {
    pub span: Span,
    pub binding: Option<Ident>,
}

#[derive(Debug)]
pub struct DestructureUnion {
    pub span: Span,
    pub ty: Type,
    pub pattern: Box<Pattern>,
}
#[derive(Debug)]
pub struct DestructureVariant {
    pub span: Span,
    pub name: Ident,
    pub pattern: Option<Box<Pattern>>,
}

#[derive(Debug)]
pub enum Destructure {
    Struct(DestructureStruct),
    Tuple(DestructureTuple),
    Vector(DestructureVector),
    Union(DestructureUnion),
    Variant(DestructureVariant),
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
pub struct StringLit {
    pub span: Span,
    pub value: String,
}

#[derive(Debug)]
pub struct Char {
    pub span: Span,
    pub value: i8,
}

#[derive(Debug)]
pub struct Tuple {
    pub span: Span,
    pub fields: Vec<Expr>,
}

#[derive(Debug)]
pub struct Struct {
    pub span: Span,
    pub props: Vec<StructProp>,
    pub ty: Option<NamedType>,
}

#[derive(Debug)]
pub struct StructProp {
    pub span: Span,
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Debug)]
pub struct If {
    pub span: Span,
    pub condition: Box<Condition>,
    pub yes: Box<Expr>,
    pub no: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct While {
    pub span: Span,
    pub condition: Box<Condition>,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub enum Condition {
    Binding(Binding),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Match {
    pub span: Span,
    pub scrutinee: Box<Expr>,
    pub body: Vec<MatchCase>,
}

#[derive(Debug)]
pub struct MatchCase {
    pub span: Span,
    pub pattern: Pattern,
    pub value: MatchArm,
}

#[derive(Debug)]
pub enum MatchArm {
    Break(Break),
    Return(Return),
    Continue(Continue),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Call {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub type_args: Vec<Type>,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct Prop {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub prop: Ident,
    pub tr: Option<Trait>,
}

#[derive(Debug)]
pub struct Field {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub prop: Int,
}

#[derive(Debug)]
pub struct Index {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug)]
pub struct Cast {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Add {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Sub {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Mul {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Div {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct And {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Or {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Greater {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Less {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct GreaterEq {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct LessEq {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Equals {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct NotEquals {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Assign {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Not {
    pub span: Span,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Neg {
    pub span: Span,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Bs {
    pub span: Span,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct AddAssign {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub enum VectorItem {
    Spread(Spread),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Spread {
    pub span: Span,
    pub value: Expr,
}

#[derive(Debug)]
pub enum VectorBody {
    Type(Type),
    Items(Vec<VectorItem>),
}

#[derive(Debug)]
pub struct Vector {
    pub span: Span,
    pub body: VectorBody,
}

#[derive(Debug)]
pub struct Variant {
    pub span: Span,
    pub ty: Option<NamedType>,
    pub variant: Ident,
    pub value: Option<Box<Expr>>,
}

#[derive(Debug)]
pub enum Expr {
    Variable(Ident),
    Block(Block),
    Int(Int),
    Float(Float),
    String(StringLit),
    Char(Char),
    Tuple(Tuple),
    Struct(Struct),
    If(If),
    While(While),
    Match(Match),
    Call(Call),
    Prop(Prop),
    Field(Field),
    Index(Index),
    Cast(Cast),
    Add(Add),
    Mul(Mul),
    Eq(Equals),
    Neq(NotEquals),
    And(And),
    Or(Or),
    Geq(GreaterEq),
    Leq(LessEq),
    Lt(Less),
    Gt(Greater),
    Assign(Assign),
    Div(Div),
    Sub(Sub),
    Neg(Neg),
    Not(Not),
    Bs(Bs),
    Vec(Vector),
    Type(Type),
    Variant(Variant),
    AddAssign(AddAssign),
}
