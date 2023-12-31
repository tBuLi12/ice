use std::{fmt::Display, fs::File, io};

use diagnostics::Diagnostics;
use pool::{FuncRef, TraitDeclRef};
use ty::TypeRef;

use crate::diagnostics::fmt;

pub mod builder;
pub mod diagnostics;
pub mod fun;
pub mod move_check;
pub mod pool;
pub mod str;
pub mod ty;
pub mod ty_decl;

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

impl LeftSpan {
    pub fn to(self, right: RightSpan) -> Span {
        Span {
            first_line: self.first_line,
            last_line: right.last_line,
            begin_offset: self.begin_offset,
            begin_highlight_offset: self.begin_highlight_offset,
            end_highlight_offset: right.end_highlight_offset,
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
    Call(FuncRef<'i>, Vec<RawValue>, pool::List<'i, TypeRef<'i>>),
    TraitCall(
        TraitDeclRef<'i>,
        u16,
        Vec<RawValue>,
        pool::List<'i, TypeRef<'i>>,
    ),
    Assign(RawValue, Vec<Elem>, RawValue),
    Tuple(Vec<RawValue>, TypeRef<'i>),
    Name(TypeRef<'i>, RawValue),
    MoveElem(RawValue, Vec<u8>),
    CopyElem(RawValue, Vec<Elem>),
    GetElemRef(RawValue, Vec<Elem>),
    Branch(RawValue, Label, Label, Vec<RawValue>),
    Switch(RawValue, Vec<Label>, Vec<RawValue>),
    Jump(Label, Vec<RawValue>),
    Return(RawValue),
    Ty(TypeRef<'i>),
    Int(u32),
    Bool(bool),
    Variant(TypeRef<'i>, u64, RawValue),
    VariantCast(TypeRef<'i>, RawValue),
    Discriminant(RawValue),
    Drop(RawValue),
    CallDrop(RawValue, Vec<Elem>),
    Invalidate(RawValue, Option<Vec<u8>>),
    Null,
}

impl<'i> Instruction<'i> {
    pub fn visit_type(
        &mut self,
        ty_pool: &'i crate::ty::Pool<'i>,
        mut fun: impl FnMut(&mut TypeRef<'i>),
    ) {
        match self {
            Instruction::Int(_)
            | Instruction::Bool(_)
            | Instruction::Add(_, _)
            | Instruction::Sub(_, _)
            | Instruction::Mul(_, _)
            | Instruction::Div(_, _)
            | Instruction::Not(_)
            | Instruction::Neg(_)
            | Instruction::Eq(_, _)
            | Instruction::Neq(_, _)
            | Instruction::Gt(_, _)
            | Instruction::Lt(_, _)
            | Instruction::GtEq(_, _)
            | Instruction::LtEq(_, _)
            | Instruction::Assign(_, _, _)
            | Instruction::CopyElem(_, _)
            | Instruction::MoveElem(_, _)
            | Instruction::GetElemRef(_, _)
            | Instruction::Branch(_, _, _, _)
            | Instruction::Switch(_, _, _)
            | Instruction::Jump(_, _)
            | Instruction::Return(_)
            | Instruction::Discriminant(_)
            | Instruction::Drop(_)
            | Instruction::CallDrop(_, _)
            | Instruction::Invalidate(_, _)
            | Instruction::Null => {}
            Instruction::Ty(_type_id) => unimplemented!(),
            Instruction::Call(_, _, ty_args) => {
                let mut new_args = ty_args.to_vec();
                new_args.iter_mut().for_each(fun);
                *ty_args = ty_pool.get_ty_list(new_args);
            }
            Instruction::TraitCall(_, _, _, ty_args) => {
                eprintln!("visiting trait call {}", fmt::List(ty_args.iter()));
                let mut new_args = ty_args.to_vec();
                new_args.iter_mut().for_each(fun);
                *ty_args = ty_pool.get_ty_list(new_args);
            }
            Instruction::Name(ty, _)
            | Instruction::Variant(ty, _, _)
            | Instruction::VariantCast(ty, _)
            | Instruction::Tuple(_, ty) => fun(ty),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Value<'i> {
    pub ty: TypeRef<'i>,
    pub raw: RawValue,
}

impl<'i> PartialEq for Value<'i> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

pub trait Source {
    type Reader<'this>: io::Read + io::Seek
    where
        Self: 'this;

    fn name(&self) -> &str;
    fn reader(&self) -> Self::Reader<'_>;
}

pub struct FileSource {
    pub file: File,
    pub name: String,
}

impl Source for FileSource {
    type Reader<'a> = &'a File;
    fn name(&self) -> &str {
        &self.name
    }
    fn reader(&self) -> Self::Reader<'_> {
        &self.file
    }
}

pub struct Ctx<'i> {
    pub type_pool: crate::ty::Pool<'i>,
    pub fun_pool: pool::FunPool<'i>,
    pub ty_decl_pool: pool::TyDeclPool<'i>,
    pub trait_decl_pool: pool::TraitDeclPool<'i>,
    pub diagnostcs: Diagnostics,
}

impl<'i> Ctx<'i> {
    pub fn new() -> Self {
        Ctx {
            type_pool: crate::ty::Pool::new(),
            fun_pool: pool::FunPool::new(),
            ty_decl_pool: pool::TyDeclPool::new(),
            trait_decl_pool: pool::TraitDeclPool::new(),
            diagnostcs: Diagnostics::new(),
        }
    }

    pub fn flush_diagnostics(&self, source: &impl Source) -> bool {
        self.diagnostcs.print_all(source)
    }
}

pub struct Package<'i> {
    pub funcs: Vec<FuncRef<'i>>,
    pub main: Option<FuncRef<'i>>,
}
