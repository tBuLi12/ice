pub mod builder;
pub mod diagnostics;
pub mod fun;
mod pool;
mod ref_check;
mod str;
pub mod ty;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value(u16);
#[derive(Clone, Copy)]
pub struct TypeId(u32);
#[derive(Clone, Copy)]
pub struct FuncId(u32);
#[derive(Clone, Copy)]
pub struct Prop(u8);
#[derive(Clone, Copy)]
pub struct InstIndex(u16);
#[derive(Clone, Copy)]
pub struct Label(u16);

#[derive(Clone, Copy)]
pub enum Elem {
    Index(Value),
    Prop(Prop),
}

#[derive(Clone)]
pub enum Instruction {
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Not(Value),
    Neg(Value),
    Eq(Value, Value),
    Neq(Value, Value),
    Gt(Value, Value),
    Lt(Value, Value),
    GtEq(Value, Value),
    LtEq(Value, Value),
    Call(FuncId, Vec<Value>),
    Assign(Value, Value),
    RefAssign(Value, Value),
    Tuple(Vec<Value>),
    Name(TypeId, Value),
    GetElem(Value, Vec<Elem>),
    GetElemRef(Value, Vec<Elem>),
    Branch(Value, Label, Label),
    Jump(Label),
    Phi(Vec<(Label, Value)>),
    Return(Value),
}
