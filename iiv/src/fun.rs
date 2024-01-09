use std::fmt::{Debug, Display};

use crate::{
    builder::{Block, UnsealedBlock},
    pool::{self, FuncRef},
    str::Str,
    ty::{TraitRef, TypeRef},
    Elem, Instruction,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Bound<'i> {
    pub ty: TypeRef<'i>,
    pub tr: TraitRef<'i>,
}

impl<'i> Debug for Bound<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Bound")
            .field("ty", &format!("{}", self.ty))
            .field("tr", &format!("{}", self.tr))
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Signature<'i> {
    pub name: Str<'i>,
    pub name_base: Str<'i>,
    pub params: pool::List<'i, TypeRef<'i>>,
    pub ty_params: Vec<()>,
    pub trait_bounds: Vec<Bound<'i>>,
    pub ret_ty: TypeRef<'i>,
}

#[derive(Debug, Clone)]
pub struct Function<'i> {
    pub sig: Signature<'i>,
    pub body: Body<'i>,
    pub ty_cache: Vec<TypeRef<'i>>,
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Receiver {
    None,
    Immutable,
    Mut,
    Move,
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Method<'i> {
    pub fun: FuncRef<'i>,
    pub receiver: Receiver,
}

#[derive(Debug, Clone)]
pub enum Body<'i> {
    Unsealed(Vec<UnsealedBlock<'i>>),
    Sealed(Vec<Block<'i>>),
    AutoCopy,
    BitwiseCopy,
    MemFree,
    MemAlloc(TypeRef<'i>),
    PtrAdd,
    PtrWrite,
    None,
}

impl<'i> Function<'i> {
    pub fn empty(ty_pool: &'i crate::ty::Pool<'i>, name_base: Str<'i>, name: Str<'i>) -> Self {
        Function {
            sig: Signature {
                name,
                name_base,
                params: ty_pool.get_ty_list(vec![]),
                ty_params: vec![],
                trait_bounds: vec![],
                ret_ty: ty_pool.get_null(),
            },
            body: crate::fun::Body::Unsealed(vec![]),
            ty_cache: vec![],
        }
    }

    pub fn apply_ty_args(
        &mut self,
        ty_pool: &'i crate::ty::Pool<'i>,
        args: pool::List<'i, TypeRef<'i>>,
    ) {
        let mut new_args = self.sig.params.to_vec();
        for ty in self.ty_cache.iter_mut().chain(new_args.iter_mut()) {
            *ty = ty_pool.resolve_ty_args(*ty, &args);
        }
        self.sig.params = ty_pool.get_ty_list(new_args);
        self.sig.ret_ty = ty_pool.resolve_ty_args(self.sig.ret_ty, &args);
        self.sig.ty_params.clear();

        let body = match &mut self.body {
            Body::Unsealed(unsealed) => unsealed,
            Body::Sealed(_) => panic!("apply_ty_args on sealed body"),
            Body::MemAlloc(ty) => {
                *ty = ty_pool.resolve_ty_args(*ty, &args);
                return;
            }
            _ => return,
        };

        for block in body {
            for (param, _) in &mut block.params {
                *param = ty_pool.resolve_ty_args(*param, &args);
            }
            for (inst, _) in &mut block.instructions {
                inst.visit_type(ty_pool, |ty| {
                    *ty = ty_pool.resolve_ty_args(*ty, &args);
                })
            }
        }
    }

    fn add_successors<'a>(
        blocks: &'a [UnsealedBlock<'i>],
        block: &'a UnsealedBlock<'i>,
        add: &mut impl FnMut(usize) -> bool,
    ) {
        eprintln!("add successors! {:p}", block);
        match block.instructions.last() {
            Some((Instruction::Jump(label, _args), _)) => {
                let new_block = &blocks[label.0 as usize];
                if add(label.0 as usize) {
                    Self::add_successors(blocks, new_block, add);
                }
            }
            Some((Instruction::Branch(_, yes_label, no_label, _), _)) => {
                let new_block = &blocks[yes_label.0 as usize];
                if add(yes_label.0 as usize) {
                    Self::add_successors(blocks, new_block, add);
                }
                let new_block = &blocks[no_label.0 as usize];
                if add(no_label.0 as usize) {
                    Self::add_successors(blocks, new_block, add);
                }
            }
            Some((Instruction::Switch(_, labels, _), _)) => {
                for label in labels {
                    let new_block = &blocks[label.0 as usize];
                    if add(label.0 as usize) {
                        Self::add_successors(blocks, new_block, add);
                    }
                }
            }
            Some((Instruction::Return(_), _)) => {}
            _ => panic!("invalid terminator!"),
        }
    }

    pub fn seal(&mut self) {
        let body = match &mut self.body {
            Body::Sealed(_) => panic!("seal called twice"),
            Body::Unsealed(unsealed) => unsealed,
            _ => return,
        };

        let mut inst_indices = vec![0u16; self.ty_cache.len()];

        let mut block_indices = vec![None; body.len()];

        let mut new_block_order = vec![0];

        Self::add_successors(body, &body[0], &mut |idx| -> bool {
            if block_indices[idx].is_none() {
                block_indices[idx] = Some(new_block_order.len());
                new_block_order.push(idx);
                true
            } else {
                false
            }
        });

        let mut current = 0;
        for i in 0..self.sig.params.len() {
            inst_indices[current as usize] = i as u16;
            current += 1;
        }

        let mut new_types = vec![];

        for &block_idx in &new_block_order {
            let block = &body[block_idx];
            for (_, idx) in &block.params {
                inst_indices[*idx as usize] = current;
                current += 1;
                new_types.push(self.ty_cache[*idx as usize]);
            }
            for (inst, idx) in &block.instructions {
                if inst.creates_value() {
                    inst_indices[*idx as usize] = current;
                    current += 1;
                    new_types.push(self.ty_cache[*idx as usize]);
                }
            }
        }

        for &block_idx in &new_block_order {
            let block = &mut body[block_idx];
            for (inst, _) in &mut block.instructions {
                match inst {
                    Instruction::Int(_) | Instruction::Bool(_) => {}
                    Instruction::Add(lhs, rhs)
                    | Instruction::Sub(lhs, rhs)
                    | Instruction::Mul(lhs, rhs)
                    | Instruction::Div(lhs, rhs)
                    | Instruction::Eq(lhs, rhs)
                    | Instruction::Neq(lhs, rhs)
                    | Instruction::Gt(lhs, rhs)
                    | Instruction::Lt(lhs, rhs)
                    | Instruction::GtEq(lhs, rhs)
                    | Instruction::Assign(lhs, _, rhs)
                    | Instruction::LtEq(lhs, rhs) => {
                        rhs.0 = inst_indices[rhs.0 as usize];
                        lhs.0 = inst_indices[lhs.0 as usize];
                    }
                    Instruction::GetElemRef(val, _)
                    | Instruction::CopyElem(val, _)
                    | Instruction::MoveElem(val, _)
                    | Instruction::Name(_, val)
                    | Instruction::Return(val)
                    | Instruction::Neg(val)
                    | Instruction::Variant(_, _, val)
                    | Instruction::VariantCast(_, val)
                    | Instruction::Drop(val)
                    | Instruction::CallDrop(val, _)
                    | Instruction::Invalidate(val, _)
                    | Instruction::Not(val) => {
                        val.0 = inst_indices[val.0 as usize];
                    }

                    Instruction::Call(_, vals, _)
                    | Instruction::TraitCall(_, _, vals, _)
                    | Instruction::Tuple(vals, _) => {
                        for val in vals {
                            val.0 = inst_indices[val.0 as usize];
                        }
                    }
                    Instruction::Jump(label, vals) => {
                        label.0 = block_indices[label.0 as usize].unwrap() as u16;
                        for val in vals {
                            val.0 = inst_indices[val.0 as usize];
                        }
                    }
                    Instruction::Branch(lhs, yes_label, no_label, args) => {
                        yes_label.0 = block_indices[yes_label.0 as usize].unwrap() as u16;
                        no_label.0 = block_indices[no_label.0 as usize].unwrap() as u16;
                        lhs.0 = inst_indices[lhs.0 as usize];
                        for val in args {
                            val.0 = inst_indices[val.0 as usize];
                        }
                    }
                    Instruction::Switch(lhs, labels, args) => {
                        lhs.0 = inst_indices[lhs.0 as usize];
                        for label in labels {
                            label.0 = block_indices[label.0 as usize].unwrap() as u16;
                        }
                        for val in args {
                            val.0 = inst_indices[val.0 as usize];
                        }
                    }
                    Instruction::Ty(_) => {}
                    Instruction::Null => {}
                }
            }
        }

        let final_blocks = new_block_order
            .into_iter()
            .map(|block_idx| {
                let block = std::mem::replace(
                    &mut body[block_idx],
                    UnsealedBlock {
                        instructions: vec![],
                        params: vec![],
                    },
                );
                Block {
                    params: block.params.into_iter().map(|(param, _)| param).collect(),
                    instructions: block
                        .instructions
                        .into_iter()
                        .map(|(inst, _)| inst)
                        .collect(),
                }
            })
            .collect();

        self.body = Body::Sealed(final_blocks);
        self.ty_cache = new_types;
    }
}

impl<'i> Display for Function<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::diagnostics::fmt;

        write!(f, "fun {}(", self.sig.name)?;
        let mut i = 0;
        for param in self.sig.params.iter() {
            write!(f, "%{}: {}, ", i, param)?;
            i += 1;
        }
        writeln!(f, "): {}", self.sig.ret_ty)?;

        match &self.body {
            Body::None => {
                writeln!(f, "    none")?;
            }
            Body::BitwiseCopy => {
                writeln!(f, "    bitwise copy")?;
            }
            Body::AutoCopy => {
                writeln!(f, "    auto copy")?;
            }
            Body::PtrAdd => {
                writeln!(f, "    ptr add")?;
            }
            Body::PtrWrite => {
                writeln!(f, "    ptr write")?;
            }
            Body::MemAlloc(_) => {
                writeln!(f, "    mem alloc")?;
            }
            Body::MemFree => {
                writeln!(f, "    mem free")?;
            }
            Body::Sealed(blocks) => {
                for (block_index, block) in blocks.iter().enumerate() {
                    writeln!(f, "b{} {}:", block_index, fmt::List(block.params.iter()))?;
                    i += block.params.len();
                    for inst in &block.instructions {
                        print_inst(f, &mut i, inst)?;
                    }
                }
            }
            Body::Unsealed(blocks) => {
                for (block_index, block) in blocks.iter().enumerate() {
                    writeln!(
                        f,
                        "b{} {}:",
                        block_index,
                        fmt::List(block.params.iter().map(|(ty, _)| ty))
                    )?;
                    i += block.params.len();
                    for (inst, idx) in &block.instructions {
                        let mut i = *idx as usize;
                        print_inst(f, &mut i, inst)?;
                    }
                }
            }
        }
        Ok(())
    }
}

fn print_inst(
    f: &mut std::fmt::Formatter<'_>,
    idx: &mut usize,
    inst: &Instruction<'_>,
) -> std::fmt::Result {
    use crate::diagnostics::fmt;
    let mut i = *idx;
    match inst {
        Instruction::Int(val) => {
            writeln!(f, "    %{} = {}", i, val)?;
            i += 1;
        }
        Instruction::Bool(val) => {
            writeln!(f, "    %{} = {}", i, val)?;
            i += 1;
        }
        Instruction::Add(lhs, rhs) => {
            writeln!(f, "    %{} = %{} + %{}", i, lhs.0, rhs.0)?;
            i += 1;
        }
        Instruction::Sub(_lhs, _rhs) => unimplemented!(),
        Instruction::Mul(lhs, rhs) => {
            writeln!(f, "    %{} = %{} * %{}", i, lhs.0, rhs.0)?;
            i += 1;
        }
        Instruction::Div(_lhs, _rhs) => unimplemented!(),
        Instruction::Not(_value) => unimplemented!(),
        Instruction::Neg(_value) => unimplemented!(),
        Instruction::Eq(lhs, rhs) => {
            writeln!(f, "    %{} = %{} == %{}", i, lhs.0, rhs.0)?;
            i += 1;
        }
        Instruction::Neq(lhs, rhs) => {
            writeln!(f, "    %{} = %{} != %{}", i, lhs.0, rhs.0)?;
            i += 1;
        }
        Instruction::Gt(lhs, rhs) => {
            writeln!(f, "    %{} = %{} > %{}", i, lhs.0, rhs.0)?;
            i += 1;
        }
        Instruction::Lt(lhs, rhs) => {
            writeln!(f, "    %{} = %{} < %{}", i, lhs.0, rhs.0)?;
            i += 1;
        }
        Instruction::GtEq(lhs, rhs) => {
            writeln!(f, "    %{} = %{} >= %{}", i, lhs.0, rhs.0)?;
            i += 1;
        }
        Instruction::LtEq(lhs, rhs) => {
            writeln!(f, "    %{} = %{} <= %{}", i, lhs.0, rhs.0)?;
            i += 1;
        }
        Instruction::Call(fun, args, ty_args) => {
            write!(
                f,
                "    %{} = call {}{}(",
                i,
                fun.borrow().sig.name,
                fmt::List(ty_args.iter()),
            )?;
            for arg in args {
                write!(f, "%{}, ", arg.0)?;
            }
            writeln!(f, ")")?;
            i += 1;
        }
        Instruction::TraitCall(tr, idx, args, ty_args) => {
            write!(
                f,
                "    %{} = trait call {}.[{}{}]{}{}(",
                i,
                ty_args[0],
                tr.borrow().name,
                fmt::List(ty_args[1..tr.borrow().ty_params.len()].iter()),
                tr.borrow().signatures[*idx as usize].fun.borrow().sig.name,
                fmt::List(ty_args[tr.borrow().ty_params.len()..].iter()),
            )?;
            for arg in args {
                write!(f, "%{}, ", arg.0)?;
            }
            writeln!(f, ")")?;
            i += 1;
        }
        Instruction::Assign(lhs, path, rhs) => {
            write!(f, "    store %{} ", lhs.0)?;
            for elem in path {
                match elem {
                    Elem::Discriminant => {
                        write!(f, "discriminant ")?;
                    }
                    Elem::Index(idx) => {
                        write!(f, "%{} ", idx.0)?;
                    }
                    Elem::Prop(prop) => {
                        write!(f, "{} ", prop.0)?;
                    }
                }
            }
            writeln!(f, "<- %{}", rhs.0)?;
            i += 1;
        }
        Instruction::Tuple(tpl, _) => {
            write!(f, "    %{} = (", i)?;
            for arg in tpl {
                write!(f, "%{}, ", arg.0)?;
            }
            writeln!(f, ")")?;
            i += 1;
        }
        Instruction::Name(ty, value) => {
            writeln!(f, "    %{} = named {} %{} ", i, ty, value.0)?;
            i += 1;
        }
        Instruction::CopyElem(lhs, path) => {
            write!(f, "    %{} = copy elem %{} ", i, lhs.0)?;
            for elem in path {
                match elem {
                    Elem::Discriminant => {
                        write!(f, "discriminant ")?;
                    }
                    Elem::Index(idx) => {
                        write!(f, "%{} ", idx.0)?;
                    }
                    Elem::Prop(prop) => {
                        write!(f, "{} ", prop.0)?;
                    }
                }
            }
            writeln!(f, "")?;
            i += 1;
        }
        Instruction::MoveElem(lhs, path) => {
            write!(f, "    %{} = move elem %{} ", i, lhs.0)?;
            for elem in path {
                write!(f, "{} ", elem)?;
            }
            writeln!(f, "")?;
            i += 1;
        }
        Instruction::GetElemRef(lhs, path) => {
            write!(f, "    %{} = elem ref %{} ", i, lhs.0)?;
            for elem in path {
                match elem {
                    Elem::Discriminant => {
                        write!(f, "discriminant ")?;
                    }
                    Elem::Index(idx) => {
                        write!(f, "%{} ", idx.0)?;
                    }
                    Elem::Prop(prop) => {
                        write!(f, "{} ", prop.0)?;
                    }
                }
            }
            writeln!(f, "")?;
            i += 1;
        }
        Instruction::Branch(lhs, yes, no, args) => {
            writeln!(
                f,
                "    br %{} ? b{} : b{} with {}",
                lhs.0,
                yes.0,
                no.0,
                fmt::List(args.iter()),
            )?;
        }
        Instruction::Switch(lhs, labels, args) => {
            write!(
                f,
                "    switch %{} [{}] with {}",
                lhs.0,
                fmt::List(labels.iter().map(|l| format!("b{}", l.0))),
                fmt::List(args.iter())
            )?;
            writeln!(f, "]")?;
        }
        Instruction::Jump(label, args) => {
            writeln!(f, "    jmp b{} {}", label.0, fmt::List(args.iter()))?;
        }
        Instruction::Return(val) => {
            writeln!(f, "    return %{}", val.0)?;
        }
        Instruction::Ty(_type_id) => unimplemented!(),
        Instruction::Variant(ty, discriminant, inner) => {
            writeln!(
                f,
                "    %{} = variant {} {} %{}",
                i, ty, discriminant, inner.0
            )?;
            i += 1;
        }
        Instruction::VariantCast(ty, inner) => {
            writeln!(f, "    %{} = variant cast {} %{}", i, ty, inner.0)?;
            i += 1;
        }
        Instruction::Drop(value) => {
            writeln!(f, "    drop %{}", value.0)?;
        }
        Instruction::CallDrop(lhs, path) => {
            write!(f, "    call drop %{} ", lhs.0)?;
            for elem in path {
                match elem {
                    Elem::Discriminant => {
                        write!(f, "discriminant ")?;
                    }
                    Elem::Index(idx) => {
                        write!(f, "%{} ", idx.0)?;
                    }
                    Elem::Prop(prop) => {
                        write!(f, "{} ", prop.0)?;
                    }
                }
            }
            writeln!(f, "")?;
        }
        Instruction::Invalidate(lhs, path) => {
            write!(f, "    invalidate %{} ", lhs.0)?;
            if let Some(path) = path {
                write!(f, "@ ")?;
                for elem in path {
                    write!(f, "{} ", elem)?;
                }
            }
            writeln!(f, "")?;
        }
        Instruction::Null => {
            writeln!(f, "    %{} = null", i)?;
            i += 1;
        }
    };
    *idx = i;
    Ok(())
}
