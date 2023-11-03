use std::{fmt::Display, fs::File};

use diagnostics::Diagnostics;
use fun::Function;
use pool::FuncRef;
use ty::TypeRef;

pub mod builder;
pub mod diagnostics;
pub mod fun;
pub mod pool;
mod ref_check;
pub mod str;
pub mod ty;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub first_line: u32,
    pub last_line: u32,
    pub begin_offset: u32,
    pub begin_highlight_offset: u32,
    pub end_highlight_offset: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LeftSpan {
    pub first_line: u32,
    pub begin_offset: u32,
    pub begin_highlight_offset: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RightSpan {
    pub last_line: u32,
    pub end_highlight_offset: u32,
}

impl Span {
    pub fn to(self, other: Span) -> Span {
        Span {
            first_line: self.first_line,
            last_line: other.last_line,
            begin_offset: self.begin_offset,
            begin_highlight_offset: self.begin_highlight_offset,
            end_highlight_offset: other.end_highlight_offset,
        }
    }

    pub fn extend_back(mut self, offset: u32) -> Span {
        self.begin_highlight_offset -= offset;
        self
    }

    pub fn first(mut self) -> Span {
        self.end_highlight_offset = self.begin_highlight_offset + 1;
        self
    }

    pub fn left(self) -> LeftSpan {
        LeftSpan {
            first_line: self.first_line,
            begin_offset: self.begin_offset,
            begin_highlight_offset: self.begin_highlight_offset,
        }
    }

    pub fn right(self) -> RightSpan {
        RightSpan {
            last_line: self.last_line,
            end_highlight_offset: self.end_highlight_offset,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct RawValue(pub u16);

impl Display for RawValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Prop(pub u8);
#[derive(Clone, Copy)]
pub struct InstIndex(u16);
#[derive(Clone, Copy, Debug)]
pub struct Label(pub u16);

#[derive(Clone, Copy, Debug)]
pub enum Elem {
    Index(RawValue),
    Prop(Prop),
}

#[derive(Clone, Debug)]
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
    Tuple(Vec<RawValue>, TypeRef<'i>),
    Name(TypeRef<'i>, RawValue),
    GetElem(RawValue, Vec<Elem>),
    GetElemRef(RawValue, Vec<Elem>),
    Branch(RawValue, Label, Vec<RawValue>, Label, Vec<RawValue>),
    Jump(Label, Vec<RawValue>),
    Return(RawValue),
    Ty(TypeRef<'i>),
    Int(u32),
    Bool(bool),
    Variant(TypeRef<'i>, u64, RawValue),
    VariantCast(TypeRef<'i>, RawValue),
    Discriminant(RawValue),
}

impl RawValue {
    pub const NULL: Self = Self(0);
}

#[derive(Clone, Copy, Debug)]
pub struct Value<'i> {
    pub ty: TypeRef<'i>,
    pub raw: RawValue,
}

pub struct Source {
    pub file: File,
    pub name: String,
}

pub struct Ctx<'i> {
    pub type_pool: crate::ty::Pool<'i>,
    pub fun_pool: pool::FunPool<'i>,
    pub diagnostcs: Diagnostics,
    pub source: Source,
}

impl<'i> Ctx<'i> {
    pub fn new(file: File, name: String) -> Self {
        Ctx {
            type_pool: crate::ty::Pool::new(),
            fun_pool: pool::FunPool::new(),
            diagnostcs: Diagnostics::new(),
            source: Source { file, name },
        }
    }

    pub fn flush_diagnostics(&self) -> bool {
        self.diagnostcs.print_all(&self.source)
    }
}

pub struct Package<'i> {
    pub funcs: Vec<FuncRef<'i>>,
}
