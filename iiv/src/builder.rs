use crate::{
    diagnostics::fmt::List,
    fun::{self, Body, Function, Signature},
    pool::{self, FuncRef},
    ty::TypeRef,
    Elem, Instruction, Label, Prop, RawValue, Value,
};

#[derive(Clone, Debug)]
pub struct UnsealedBlock<'i> {
    pub instructions: Vec<(Instruction<'i>, u16)>,
    pub params: Vec<(TypeRef<'i>, u16)>,
}

#[derive(Clone, Debug)]
pub struct Block<'i> {
    pub instructions: Vec<Instruction<'i>>,
    pub params: Vec<TypeRef<'i>>,
}

#[derive(Clone, Copy)]
pub struct BlockRef {
    pub idx: usize,
}

impl<'f, 'i: 'f> Cursor<'f, 'i> {
    pub fn new(pool: &'i crate::ty::Pool<'i>, func: &'f mut Function<'i>) -> Self {
        let blocks = match &mut func.body {
            Body::Sealed(_) => panic!("cannot create a cursor to a sealed function"),
            Body::Unsealed(blocks) => blocks,
        };

        Cursor {
            sig: &mut func.sig,
            ty_pool: pool,
            body: blocks,
            current_block: 0,
            current_inst: 0,
            types: &mut func.ty_cache,
        }
    }

    pub fn current_signature(&self) -> &'_ &'f mut Signature<'i> {
        &self.sig
    }

    pub fn set_params(&mut self, params: pool::List<'i, TypeRef<'i>>) {
        self.sig.params = params;
        *self.types = params.to_vec();
    }

    pub fn params(&self) -> impl Iterator<Item = Value<'i>> + '_ {
        self.sig.params.iter().zip(0..).map(|(&ty, val)| Value {
            ty,
            raw: RawValue(val),
        })
    }

    pub fn set_ret_ty(&mut self, ty: TypeRef<'i>) {
        self.sig.ret_ty = ty;
    }

    fn next_id(&mut self) -> RawValue {
        let id = self.types.len();
        RawValue(id as u16)
    }

    fn push_value_instruction(&mut self, inst: Instruction<'i>, ty: TypeRef<'i>) -> RawValue {
        let id = self.next_id();
        self.body[self.current_block]
            .instructions
            .insert(self.current_inst, (inst, id.0));
        self.current_inst += 1;
        self.types.push(ty);
        id
    }

    fn push_instruction(&mut self, inst: Instruction<'i>) {
        self.body[self.current_block]
            .instructions
            .insert(self.current_inst, (inst, 0));
        self.current_inst += 1;
    }

    pub fn add(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {
        Value {
            ty: lhs.ty,
            raw: self.push_value_instruction(Instruction::Add(lhs.raw, rhs.raw), lhs.ty),
        }
    }

    pub fn variant(&mut self, ty: TypeRef<'i>, idx: u64, value: Value<'i>) -> Value<'i> {
        Value {
            ty,
            raw: self.push_value_instruction(Instruction::Variant(ty, idx, value.raw), ty),
        }
    }

    pub fn discriminant(&mut self, value: Value<'i>) -> Value<'i> {
        let int = self.ty_pool.get_int();
        Value {
            ty: int,
            raw: self.push_value_instruction(Instruction::Discriminant(value.raw), int),
        }
    }

    pub fn variant_cast(&mut self, ty: TypeRef<'i>, value: Value<'i>) -> Value<'i> {
        Value {
            ty,
            raw: self.push_value_instruction(Instruction::VariantCast(ty, value.raw), ty),
        }
    }
    // pub fn sub(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn mul(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn div(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn not(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn neg(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    pub fn equals(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {
        let bool = self.ty_pool.get_ty_bool();
        Value {
            ty: bool,
            raw: self.push_value_instruction(Instruction::Eq(lhs.raw, rhs.raw), bool),
        }
    }
    // pub fn neq(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn gt(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn lt(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn gt_eq(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn lt_eq(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn push(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}

    pub fn block_param(&mut self, ty: TypeRef<'i>) -> Value<'i> {
        let raw = self.next_id();
        self.body[self.current_block].params.push((ty, raw.0));
        self.types.push(ty);
        Value { ty, raw }
    }

    pub fn call(
        &mut self,
        func: FuncRef<'i>,
        args: &[Value<'i>],
        ty_args: pool::List<'i, TypeRef<'i>>,
    ) -> Value<'i> {
        let ret_ty = self
            .ty_pool
            .resolve_ty_args(func.borrow().sig.ret_ty, &ty_args);

        Value {
            ty: ret_ty,
            raw: self.push_value_instruction(
                Instruction::Call(func, args.iter().map(|arg| arg.raw).collect(), ty_args),
                ret_ty,
            ),
        }
    }

    // pub fn assign(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn ref_assign(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    pub fn make_tuple(&mut self, _values: &[Value<'i>]) -> Value<'i> {
        unimplemented!()
    }

    pub fn make_struct(&mut self, props: &[Value<'i>], ty: TypeRef<'i>) -> Value<'i> {
        Value {
            ty,
            raw: self.push_value_instruction(
                Instruction::Tuple(props.iter().map(|prop| prop.raw).collect(), ty),
                ty,
            ),
        }
    }
    // pub fn name(&mut self, value: Value<'i>, ty: TypeId) -> Value<'i> {}
    // pub fn vector(&mut self, values: &[Value<'i>]) -> Value<'i> {}
    pub fn get_prop(&mut self, value: Value<'i>, prop: u8, ty: TypeRef<'i>) -> Value<'i> {
        Value {
            ty,
            raw: self.push_value_instruction(
                Instruction::CopyElem(value.raw, vec![Elem::Prop(Prop(prop))]),
                ty,
            ),
        }
    }

    pub fn move_prop(&mut self, value: Value<'i>, prop: u8, prop_ty: TypeRef<'i>) -> Value<'i> {
        if &*self.sig.name == "add" {
            panic!("BRUH");
        }
        Value {
            ty: prop_ty,
            raw: self.push_value_instruction(Instruction::MoveElem(value.raw, vec![prop]), prop_ty),
        }
    }

    pub fn move_prop_deep(
        &mut self,
        value: Value<'i>,
        props: Vec<u8>,
        prop_ty: TypeRef<'i>,
    ) -> Value<'i> {
        Value {
            ty: prop_ty,
            raw: self.push_value_instruction(Instruction::MoveElem(value.raw, props), prop_ty),
        }
    }

    pub fn get_deep_prop(
        &mut self,
        value: Value<'i>,
        props: Vec<u8>,
        ty: TypeRef<'i>,
    ) -> Value<'i> {
        Value {
            ty,
            raw: self.push_value_instruction(
                Instruction::CopyElem(
                    value.raw,
                    props
                        .into_iter()
                        .map(|prop| Elem::Prop(Prop(prop)))
                        .collect(),
                ),
                ty,
            ),
        }
    }

    pub fn get_prop_ref(&mut self, value: Value<'i>, props: Vec<u8>, ty: TypeRef<'i>) -> Value<'i> {
        let ref_ty = self.ty_pool.get_ref(ty);
        Value {
            ty: ref_ty,
            raw: self.push_value_instruction(
                Instruction::GetElemRef(
                    value.raw,
                    props
                        .into_iter()
                        .map(|prop| Elem::Prop(Prop(prop)))
                        .collect(),
                ),
                ref_ty,
            ),
        }
    }

    pub fn ref_to(&mut self, value: Value<'i>) -> Value<'i> {
        let ref_ty = self.ty_pool.get_ref(value.ty);
        Value {
            ty: ref_ty,
            raw: self.push_value_instruction(Instruction::GetElemRef(value.raw, vec![]), ref_ty),
        }
    }

    pub fn assign(&mut self, place: Value<'i>, elems: Vec<Elem>, value: Value<'i>) -> Value<'i> {
        let ty_null = self.ty_pool.get_null();
        Value {
            ty: ty_null,
            raw: self
                .push_value_instruction(Instruction::Assign(place.raw, elems, value.raw), ty_null),
        }
    }

    // pub fn ref_prop(&mut self, value: Value<'i>, prop: Prop) -> Value<'i> {}
    pub fn branch(&mut self, condition: Value<'i>, true_label: BlockRef, false_label: BlockRef) {
        self.push_instruction(Instruction::Branch(
            condition.raw,
            Label(true_label.idx as u16),
            Label(false_label.idx as u16),
            vec![],
        ));
    }

    pub fn switch(&mut self, idx: Value<'i>, labels: &[BlockRef]) {
        self.push_instruction(Instruction::Switch(
            idx.raw,
            labels.iter().map(|l| Label(l.idx as u16)).collect(),
            vec![],
        ));
    }

    pub fn jump(&mut self, label: BlockRef, args: &[Value<'i>]) {
        self.push_instruction(Instruction::Jump(
            Label(label.idx as u16),
            args.iter().map(|arg| arg.raw).collect(),
        ));
    }
    // pub fn ret(&mut self, value: Value<'i>) -> Value<'i> { unimplemented!() }

    pub fn ty_expr(&mut self, _ty: TypeRef<'_>) -> Value<'i> {
        unimplemented!()
    }

    pub fn int_lit(&mut self, value: u32) -> Value<'i> {
        let int = self.ty_pool.get_int();
        Value {
            ty: int,
            raw: self.push_value_instruction(Instruction::Int(value), int),
        }
    }

    pub fn bool_lit(&mut self, value: bool) -> Value<'i> {
        let bool = self.ty_pool.get_ty_bool();
        Value {
            ty: bool,
            raw: self.push_value_instruction(Instruction::Bool(value), bool),
        }
    }

    pub fn invalidate(&mut self, value: RawValue, path: Option<Vec<u8>>) {
        println!(
            "adding an invalidate, at {}",
            self.body[self.current_block].instructions.len()
        );

        self.push_instruction(Instruction::Invalidate(value, path));
    }

    pub fn create_block(&mut self) -> BlockRef {
        let idx = self.body.len();
        self.body.push(UnsealedBlock {
            instructions: vec![],
            params: vec![],
        });
        BlockRef { idx }
    }

    pub fn select(&mut self, block: BlockRef) {
        self.current_block = block.idx;
        self.current_inst = self.body[block.idx].instructions.len();
    }

    pub fn ret(&mut self, val: Value<'i>) {
        self.push_instruction(Instruction::Return(val.raw));
    }

    pub fn drop(&mut self, val: Value<'i>) {
        self.push_instruction(Instruction::Drop(val.raw));
    }

    pub fn copy(&mut self, val: Value<'i>) -> Value<'i> {
        Value {
            ty: val.ty,
            raw: self.push_value_instruction(Instruction::CopyElem(val.raw, vec![]), val.ty),
        }
    }

    pub fn null(&mut self) -> Value<'i> {
        let ty_null = self.ty_pool.get_null();
        Value {
            ty: ty_null,
            raw: self.push_value_instruction(Instruction::Null, ty_null),
        }
    }

    pub fn goto_block_start(&mut self) {
        self.current_inst = 0;
    }

    pub fn get_current_block(&self) -> BlockRef {
        BlockRef {
            idx: self.current_block,
        }
    }

    pub fn get_current_inst(&self) -> &Instruction<'i> {
        println!("{} {}", self.current_block, self.current_inst);
        &self.body[self.current_block].instructions[self.current_inst].0
    }

    pub fn advance(&mut self) -> bool {
        let res = if self.current_inst < self.body[self.current_block].instructions.len() {
            self.current_inst += 1;
            self.current_inst != self.body[self.current_block].instructions.len()
        } else {
            false
        };
        println!("advancing {}", res);
        res
    }

    pub fn type_of(&self, val: RawValue) -> TypeRef<'i> {
        self.types[val.0 as usize]
    }

    pub fn split(&mut self) -> BlockRef {
        println!("splitting...");
        let idx = self.body.len();
        let block = &mut self.body[self.current_block];
        let instructions = block.instructions.drain(self.current_inst..).collect();
        self.body.push(UnsealedBlock {
            instructions,
            params: vec![],
        });

        BlockRef { idx }
    }

    fn add_successors<'a>(
        blocks: &'a [UnsealedBlock<'i>],
        block: &'a UnsealedBlock<'i>,
        add: &mut impl FnMut(usize),
    ) {
        match block.instructions.last() {
            Some((Instruction::Jump(label, _args), _)) => {
                let new_block = &blocks[label.0 as usize];
                add(label.0 as usize);
                Self::add_successors(blocks, new_block, add);
            }
            Some((Instruction::Branch(cond, yes_label, no_label, args), _)) => {
                let new_block = &blocks[yes_label.0 as usize];
                add(yes_label.0 as usize);
                Self::add_successors(blocks, new_block, add);
                let new_block = &blocks[no_label.0 as usize];
                add(no_label.0 as usize);
                Self::add_successors(blocks, new_block, add);
            }
            Some((Instruction::Switch(idx, labels, args), _)) => {
                for label in labels {
                    let new_block = &blocks[label.0 as usize];
                    add(label.0 as usize);
                    Self::add_successors(blocks, new_block, add);
                }
            }
            Some((Instruction::Return(value), _)) => {}
            _ => panic!("invalid terminator!"),
        }
    }

    pub fn build(self) -> (Vec<Block<'i>>, Vec<TypeRef<'i>>, usize) {
        let mut inst_indices = vec![0u16; self.types.len()];

        let mut block_indices = vec![None; self.body.len()];

        let mut new_block_order = vec![0];

        Self::add_successors(&self.body, &self.body[0], &mut |idx| {
            if block_indices[idx].is_none() {
                block_indices[idx] = Some(new_block_order.len());
                new_block_order.push(idx);
            }
        });

        let mut current = 0;
        for i in 0..self.sig.params.len() {
            inst_indices[current as usize] = i as u16;
            current += 1;
        }

        let mut new_types = vec![];

        for &block_idx in &new_block_order {
            let block = &self.body[block_idx];
            for (_, idx) in &block.params {
                inst_indices[*idx as usize] = current;
                current += 1;
                new_types.push(self.types[*idx as usize]);
            }
            for (inst, idx) in &block.instructions {
                if inst.creates_value() {
                    inst_indices[*idx as usize] = current;
                    current += 1;
                    new_types.push(self.types[*idx as usize]);
                }
            }
        }

        for &block_idx in &new_block_order {
            let block = &mut self.body[block_idx];
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
                    | Instruction::Discriminant(val)
                    | Instruction::Drop(val)
                    | Instruction::Not(val) => {
                        val.0 = inst_indices[val.0 as usize];
                    }

                    Instruction::Call(_, vals, _) | Instruction::Tuple(vals, _) => {
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
                    Instruction::CallDrop(_, _) | Instruction::Invalidate(_, _) => {
                        panic!("invalid instruction")
                    }
                }
            }
        }

        let final_blocks = new_block_order
            .into_iter()
            .map(|block_idx| {
                let block = std::mem::replace(
                    &mut self.body[block_idx],
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

        (final_blocks, new_types, inst_indices.len())
    }
}

impl<'i> Instruction<'i> {
    pub fn creates_value(&self) -> bool {
        match self {
            Instruction::Int(_)
            | Instruction::Bool(_)
            | Instruction::Add(_, _)
            | Instruction::Sub(_, _)
            | Instruction::Mul(_, _)
            | Instruction::Div(_, _)
            | Instruction::Eq(_, _)
            | Instruction::Neq(_, _)
            | Instruction::Gt(_, _)
            | Instruction::Lt(_, _)
            | Instruction::GtEq(_, _)
            | Instruction::Assign(_, _, _)
            | Instruction::GetElemRef(_, _)
            | Instruction::CopyElem(_, _)
            | Instruction::MoveElem(_, _)
            | Instruction::Name(_, _)
            | Instruction::Neg(_)
            | Instruction::Variant(_, _, _)
            | Instruction::VariantCast(_, _)
            | Instruction::Discriminant(_)
            | Instruction::Ty(_)
            | Instruction::Not(_)
            | Instruction::Tuple(_, _)
            | Instruction::Call(_, _, _)
            | Instruction::Null
            | Instruction::LtEq(_, _) => true,

            Instruction::Jump(_, _)
            | Instruction::Branch(_, _, _, _)
            | Instruction::Switch(_, _, _)
            | Instruction::Return(_)
            | Instruction::Invalidate(_, _)
            | Instruction::CallDrop(_, _)
            | Instruction::Drop(_) => false,
        }
    }
}

impl<'i> UnsealedBlock<'i> {
    pub fn successors(&self) -> impl Iterator<Item = usize> {
        match self.instructions.last() {
            Some((Instruction::Jump(label, _args), _)) => vec![*label].into_iter(),
            Some((Instruction::Branch(_, yes_label, no_label, _), _)) => {
                vec![*no_label, *yes_label].into_iter()
            }
            Some((Instruction::Switch(_, labels, _), _)) => {
                labels.iter().rev().copied().collect::<Vec<_>>().into_iter()
            }
            Some((Instruction::Return(_), _)) => vec![].into_iter(),
            _ => panic!("invalid terminator!"),
        }
        .map(|label| label.0 as usize)
    }
}

pub struct Cursor<'f, 'i: 'f> {
    sig: &'f mut fun::Signature<'i>,
    body: &'f mut Vec<UnsealedBlock<'i>>,
    ty_pool: &'i crate::ty::Pool<'i>,
    types: &'f mut Vec<TypeRef<'i>>,
    current_block: usize,
    current_inst: usize,
}
