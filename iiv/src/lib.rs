use pool::FuncRef;
use ty::TypeRef;

pub mod builder;
pub mod diagnostics;
pub mod fun;
pub mod pool;
mod ref_check;
pub mod str;
pub mod ty;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub first_line: u32,
    pub last_line: u32,
    pub begin_offset: u32,
    pub begin_highlight_offset: u32,
    pub end_highlight_offset: u32,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct RawValue(u16);
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Prop(u8);
#[derive(Clone, Copy)]
pub struct InstIndex(u16);
#[derive(Clone, Copy)]
pub struct Label(u16);

#[derive(Clone, Copy)]
pub enum Elem {
    Index(RawValue),
    Prop(Prop),
}

#[derive(Clone)]
pub enum Instruction<'i> {
    Add(RawValue, RawValue),
    Sub(RawValue, RawValue),
    Mul(RawValue, RawValue),
    Div(RawValue, RawValue),
    Not(RawValue),
    Neg(RawValue),
    Eq(RawValue, RawValue),
    Neq(RawValue, RawValue),
    Gt(RawValue, RawValue),
    Lt(RawValue, RawValue),
    GtEq(RawValue, RawValue),
    LtEq(RawValue, RawValue),
    Call(FuncRef<'i>, Vec<RawValue>),
    Assign(RawValue, RawValue),
    RefAssign(RawValue, RawValue),
    Tuple(Vec<RawValue>),
    Name(TypeRef<'i>, RawValue),
    GetElem(RawValue, Vec<Elem>),
    GetElemRef(RawValue, Vec<Elem>),
    Branch(RawValue, Label, Label),
    Jump(Label),
    Phi(Vec<(Label, RawValue)>),
    Return(RawValue),
    Ty(TypeRef<'i>),
}

impl RawValue {
    pub const NULL: Self = Self(0);
}

pub struct Value<'i> {
    pub ty: TypeRef<'i>,
    pub raw: RawValue,
}

struct Ctx {}
