use std::{cell::Cell, fmt::Display, fs::File, io};

use diagnostics::Diagnostics;
use fun::{Bound, Function, Method, Signature};
use impl_tree::ImplForest;
use pool::{FuncRef, TraitDeclRef, TraitImplRef};
use ty::{TraitRef, TypeRef};
use ty_decl::{TraitDecl, TraitImpl};

use crate::diagnostics::fmt;

pub mod builder;
pub mod diagnostics;
pub mod fun;
pub mod impl_tree;
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
    pub fn null() -> Self {
        Self {
            first_line: 0,
            last_line: 0,
            begin_offset: 0,
            begin_highlight_offset: 0,
            end_highlight_offset: 0,
        }
    }

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
    Discriminant,
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
    RefToPtr(RawValue),
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
            | Instruction::Drop(_)
            | Instruction::RefToPtr(_)
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
    pub trait_impl_pool: pool::TraitImplPool<'i>,
    pub diagnostcs: Diagnostics,
    pub builtins: Builtints<'i>,
}

pub struct Builtints<'i> {
    pub drop: Cell<Option<TraitRef<'i>>>,
    pub copy: Cell<Option<TraitRef<'i>>>,
    pub bitwise_copy_impl: Cell<Option<TraitImplRef<'i>>>,
    pub auto_copy_impl: Cell<Option<TraitImplRef<'i>>>,
    pub ptr_write: Cell<Option<FuncRef<'i>>>,
    pub ptr_add: Cell<Option<FuncRef<'i>>>,
    pub mem_alloc: Cell<Option<FuncRef<'i>>>,
    pub mem_free: Cell<Option<FuncRef<'i>>>,
}

impl<'i> Builtints<'i> {
    pub fn get_drop(&self) -> TraitRef<'i> {
        self.drop.get().unwrap()
    }

    pub fn get_copy(&self) -> TraitRef<'i> {
        self.copy.get().unwrap()
    }

    pub fn get_bitwise_copy(&self) -> TraitImplRef<'i> {
        self.bitwise_copy_impl.get().unwrap()
    }

    pub fn get_auto_copy(&self) -> TraitImplRef<'i> {
        self.auto_copy_impl.get().unwrap()
    }

    pub fn get_ptr_write(&self) -> FuncRef<'i> {
        self.ptr_write.get().unwrap()
    }

    pub fn get_ptr_add(&self) -> FuncRef<'i> {
        self.ptr_add.get().unwrap()
    }

    pub fn get_mem_alloc(&self) -> FuncRef<'i> {
        self.mem_alloc.get().unwrap()
    }

    pub fn get_mem_free(&self) -> FuncRef<'i> {
        self.mem_free.get().unwrap()
    }
}

impl<'i> Ctx<'i> {
    pub fn new() -> Self {
        Ctx {
            type_pool: crate::ty::Pool::new(),
            fun_pool: pool::FunPool::new(),
            ty_decl_pool: pool::TyDeclPool::new(),
            trait_decl_pool: pool::TraitDeclPool::new(),
            trait_impl_pool: pool::TraitImplPool::new(),
            diagnostcs: Diagnostics::new(),
            builtins: Builtints {
                drop: Cell::new(None),
                copy: Cell::new(None),
                bitwise_copy_impl: Cell::new(None),
                auto_copy_impl: Cell::new(None),
                mem_alloc: Cell::new(None),
                mem_free: Cell::new(None),
                ptr_add: Cell::new(None),
                ptr_write: Cell::new(None),
            },
        }
    }

    pub fn init(&'i self) {
        let this_ty = self.type_pool.get_ty_constant(0);
        {
            let mem_alloc = self.fun_pool.insert(Function {
                body: fun::Body::MemAlloc(this_ty),
                ty_cache: vec![],
                sig: Signature {
                    name: self.type_pool.str_pool.get("memAlloc"),
                    name_base: self.type_pool.str_pool.get(""),
                    params: self.type_pool.get_ty_list(vec![self.type_pool.get_int()]),
                    ret_ty: self.type_pool.get_ptr(this_ty),
                    trait_bounds: vec![],
                    ty_params: vec![()],
                },
            });

            let mem_free = self.fun_pool.insert(Function {
                body: fun::Body::MemFree,
                ty_cache: vec![],
                sig: Signature {
                    name: self.type_pool.str_pool.get("memFree"),
                    name_base: self.type_pool.str_pool.get(""),
                    params: self
                        .type_pool
                        .get_ty_list(vec![self.type_pool.get_ptr(this_ty)]),
                    ret_ty: self.type_pool.get_null(),
                    trait_bounds: vec![],
                    ty_params: vec![()],
                },
            });

            let ptr_write = self.fun_pool.insert(Function {
                body: fun::Body::PtrWrite,
                ty_cache: vec![],
                sig: Signature {
                    name: self.type_pool.str_pool.get("ptrWrite"),
                    name_base: self.type_pool.str_pool.get(""),
                    params: self
                        .type_pool
                        .get_ty_list(vec![self.type_pool.get_ptr(this_ty), this_ty]),
                    ret_ty: self.type_pool.get_null(),
                    trait_bounds: vec![],
                    ty_params: vec![()],
                },
            });

            let ptr_add = self.fun_pool.insert(Function {
                body: fun::Body::PtrAdd,
                ty_cache: vec![],
                sig: Signature {
                    name: self.type_pool.str_pool.get("ptrAdd"),
                    name_base: self.type_pool.str_pool.get(""),
                    params: self.type_pool.get_ty_list(vec![
                        self.type_pool.get_ptr(this_ty),
                        self.type_pool.get_int(),
                    ]),
                    ret_ty: self.type_pool.get_ptr(this_ty),
                    trait_bounds: vec![],
                    ty_params: vec![()],
                },
            });

            self.builtins.mem_alloc.set(Some(mem_alloc));
            self.builtins.mem_free.set(Some(mem_free));
            self.builtins.ptr_add.set(Some(ptr_add));
            self.builtins.ptr_write.set(Some(ptr_write));
        }

        let drop_signature = self.fun_pool.insert(Function {
            body: fun::Body::Unsealed(vec![]),
            ty_cache: vec![],
            sig: Signature {
                name: self.type_pool.str_pool.get("drop"),
                name_base: self.type_pool.str_pool.get("Drop."),
                params: self
                    .type_pool
                    .get_ty_list(vec![self.type_pool.get_ref(this_ty)]),
                ret_ty: self.type_pool.get_null(),
                trait_bounds: vec![],
                ty_params: vec![()],
            },
        });

        let drop_decl = self.trait_decl_pool.insert(TraitDecl {
            name: self.type_pool.str_pool.get("Drop"),
            signatures: vec![fun::Method {
                fun: drop_signature,
                receiver: fun::Receiver::Mut,
            }],
            trait_bounds: vec![],
            ty_params: vec![],
        });

        let copy_signature = self.fun_pool.insert(Function {
            body: fun::Body::Unsealed(vec![]),
            ty_cache: vec![],
            sig: Signature {
                name: self.type_pool.str_pool.get("copy"),
                name_base: self.type_pool.str_pool.get("Copy."),
                params: self
                    .type_pool
                    .get_ty_list(vec![self.type_pool.get_ref(this_ty)]),
                ret_ty: this_ty,
                trait_bounds: vec![],
                ty_params: vec![()],
            },
        });

        let copy_decl = self.trait_decl_pool.insert(TraitDecl {
            name: self.type_pool.str_pool.get("Copy"),
            signatures: vec![fun::Method {
                fun: copy_signature,
                receiver: fun::Receiver::Immutable,
            }],
            trait_bounds: vec![],
            ty_params: vec![],
        });

        let copy = TraitRef(copy_decl, self.type_pool.get_ty_list(vec![]));
        let drop = TraitRef(drop_decl, self.type_pool.get_ty_list(vec![]));

        drop_signature.borrow_mut().sig.trait_bounds.push(Bound {
            ty: this_ty,
            tr: drop,
        });

        copy_signature.borrow_mut().sig.trait_bounds.push(Bound {
            ty: this_ty,
            tr: copy,
        });

        let bitwise_copy_impl_signature = self.fun_pool.insert(Function {
            body: fun::Body::BitwiseCopy,
            ty_cache: vec![],
            sig: Signature {
                name: self.type_pool.str_pool.get("copy"),
                name_base: self.type_pool.str_pool.get("Copy."),
                params: self
                    .type_pool
                    .get_ty_list(vec![self.type_pool.get_ref(this_ty)]),
                ret_ty: this_ty,
                trait_bounds: vec![],
                ty_params: vec![()],
            },
        });

        let bitwise_copy_impl = self.trait_impl_pool.insert(TraitImpl {
            functions: vec![Method {
                receiver: crate::fun::Receiver::Immutable,
                fun: bitwise_copy_impl_signature,
            }],
            tr: copy,
            trait_bounds: vec![],
            ty_params: vec![()],
            ty: this_ty,
        });

        let auto_copy_impl_signature = self.fun_pool.insert(Function {
            body: fun::Body::AutoCopy,
            ty_cache: vec![],
            sig: Signature {
                name: self.type_pool.str_pool.get("copy"),
                name_base: self.type_pool.str_pool.get("Copy."),
                params: self
                    .type_pool
                    .get_ty_list(vec![self.type_pool.get_ref(this_ty)]),
                ret_ty: this_ty,
                trait_bounds: vec![],
                ty_params: vec![()],
            },
        });

        let auto_copy_impl = self.trait_impl_pool.insert(TraitImpl {
            functions: vec![Method {
                receiver: crate::fun::Receiver::Immutable,
                fun: auto_copy_impl_signature,
            }],
            tr: copy,
            trait_bounds: vec![],
            ty_params: vec![()],
            ty: this_ty,
        });

        self.builtins.drop.set(Some(drop));
        self.builtins.copy.set(Some(copy));
        self.builtins.bitwise_copy_impl.set(Some(bitwise_copy_impl));
        self.builtins.auto_copy_impl.set(Some(auto_copy_impl));
    }

    pub fn flush_diagnostics(&self, source: &impl Source) -> bool {
        self.diagnostcs.print_all(source)
    }
}

pub struct Package<'i> {
    pub impl_forest: ImplForest<'i>,
    pub funcs: Vec<FuncRef<'i>>,
    pub main: Option<FuncRef<'i>>,
}
