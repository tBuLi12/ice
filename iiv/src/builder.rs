use crate::{
    pool::{self, FuncRef},
    str::Str,
    ty::TypeRef,
    Elem, Instruction, Label, Prop, RawValue, Value,
};

pub struct FunctionBuilder<'i> {
    ty_pool: &'i crate::ty::Pool<'i>,
    blocks: Vec<Block<'i>>,
    block_refs: Vec<Option<usize>>,
    current_block: usize,
    current_block_ref: BlockRef,
    next_free_id: u16,
}

#[derive(Clone, Debug)]
pub struct Block<'i> {
    pub instructions: Vec<Instruction<'i>>,
    pub params: Vec<TypeRef<'i>>,
}

#[derive(Clone, Copy)]
pub struct BlockRef {
    idx: usize,
}

#[derive(Clone, Copy)]
pub struct Checkpoint(u16);

impl<'i> FunctionBuilder<'i> {
    pub fn new(pool: &'i crate::ty::Pool<'i>) -> Self {
        FunctionBuilder {
            ty_pool: pool,
            blocks: vec![Block {
                params: vec![],
                instructions: vec![],
            }],
            block_refs: vec![Some(0)],
            current_block: 0,
            current_block_ref: BlockRef { idx: 0 },
            next_free_id: 0,
        }
    }

    pub fn set_up_patch(&self) -> Checkpoint {
        Checkpoint(self.next_free_id)
    }

    pub fn start_patch(&mut self, checkpoint: Checkpoint) -> u16 {
        let old = self.next_free_id;
        self.next_free_id = checkpoint.0;
        old
    }

    pub fn end_patch(&mut self, checkpoint: Checkpoint, end: u16) {
        let diff = self.next_free_id - checkpoint.0;
        if diff == 0 {
            self.next_free_id = end;
            return;
        }
        for block in &mut self.blocks[(self.current_block + 1)..] {
            for inst in &mut block.instructions {
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
                    | Instruction::Assign(lhs, rhs)
                    | Instruction::RefAssign(lhs, rhs)
                    | Instruction::LtEq(lhs, rhs) => {
                        rhs.0 += diff;
                        lhs.0 += diff
                    }
                    Instruction::GetElemRef(val, _)
                    | Instruction::GetElem(val, _)
                    | Instruction::Name(_, val)
                    | Instruction::Return(val)
                    | Instruction::Neg(val)
                    | Instruction::Variant(_, _, val)
                    | Instruction::VariantCast(_, val)
                    | Instruction::Discriminant(val)
                    | Instruction::Not(val) => {
                        val.0 += diff;
                    }

                    Instruction::Call(_, vals)
                    | Instruction::Tuple(vals, _)
                    | Instruction::Jump(_, vals) => {
                        for val in vals {
                            val.0 += diff;
                        }
                    }
                    Instruction::Branch(lhs, _, yes_args, _, no_args) => {
                        lhs.0 += diff;
                        for val in yes_args {
                            val.0 += diff;
                        }
                        for val in no_args {
                            val.0 += diff;
                        }
                    }
                    Instruction::Ty(TypeId) => {}
                }
            }
        }
        self.next_free_id = end + diff;
    }

    fn next_id(&mut self) -> RawValue {
        let id = self.next_free_id;

        if id == 5 {
            // panic!("EEEEEE");
        }

        self.next_free_id += 1;
        RawValue(id)
    }

    pub fn add(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Add(lhs.raw, rhs.raw));

        Value {
            ty: lhs.ty,
            raw: self.next_id(),
        }
    }

    pub fn variant(&mut self, ty: TypeRef<'i>, idx: u64, value: Value<'i>) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Variant(ty, idx, value.raw));

        Value {
            ty,
            raw: self.next_id(),
        }
    }

    pub fn discriminant(&mut self, value: Value<'i>) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Discriminant(value.raw));

        Value {
            ty: self.ty_pool.get_int(),
            raw: self.next_id(),
        }
    }

    pub fn variant_cast(&mut self, ty: TypeRef<'i>, value: Value<'i>) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::VariantCast(ty, value.raw));

        Value {
            ty,
            raw: self.next_id(),
        }
    }
    // pub fn sub(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn mul(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn div(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn not(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn neg(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    pub fn equals(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Eq(lhs.raw, rhs.raw));

        Value {
            ty: self.ty_pool.get_ty_bool(),
            raw: self.next_id(),
        }
    }
    // pub fn neq(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn gt(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn lt(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn gt_eq(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn lt_eq(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn push(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}

    pub fn block_param(&mut self, ty: TypeRef<'i>) -> Value<'i> {
        self.blocks[self.current_block].params.push(ty);
        Value {
            ty,
            raw: self.next_id(),
        }
    }

    pub fn param(&mut self, ty: TypeRef<'i>) -> Value<'i> {
        Value {
            ty,
            raw: self.next_id(),
        }
    }

    pub fn call(&mut self, func: FuncRef<'i>, args: &[Value<'i>]) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Call(
                func,
                args.iter().map(|arg| arg.raw).collect(),
            ));
        Value {
            ty: func.borrow().sig.ret_ty,
            raw: self.next_id(),
        }
    }

    // pub fn assign(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn ref_assign(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    pub fn make_tuple(&mut self, values: &[Value<'i>]) -> Value<'i> {
        unimplemented!()
    }

    pub fn make_struct(&mut self, props: &[Value<'i>], ty: TypeRef<'i>) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Tuple(
                props.iter().map(|prop| prop.raw).collect(),
                ty,
            ));

        Value {
            ty,
            raw: self.next_id(),
        }
    }
    // pub fn name(&mut self, value: Value<'i>, ty: TypeId) -> Value<'i> {}
    // pub fn vector(&mut self, values: &[Value<'i>]) -> Value<'i> {}
    pub fn get_prop(&mut self, value: Value<'i>, prop: u8, ty: TypeRef<'i>) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::GetElem(
                value.raw,
                vec![Elem::Prop(Prop(prop))],
            ));

        Value {
            ty,
            raw: self.next_id(),
        }
    }

    pub fn get_deep_prop(
        &mut self,
        value: Value<'i>,
        props: Vec<u8>,
        ty: TypeRef<'i>,
    ) -> Value<'i> {
        dbg!(value.raw);
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::GetElem(
                value.raw,
                props
                    .into_iter()
                    .map(|prop| Elem::Prop(Prop(prop)))
                    .collect(),
            ));

        Value {
            ty,
            raw: self.next_id(),
        }
    }

    pub fn get_prop_ref(&mut self, value: Value<'i>, props: Vec<u8>, ty: TypeRef<'i>) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::GetElemRef(
                value.raw,
                props
                    .into_iter()
                    .map(|prop| Elem::Prop(Prop(prop)))
                    .collect(),
            ));

        Value {
            ty: self.ty_pool.get_ref(value.ty),
            raw: self.next_id(),
        }
    }

    pub fn ref_to(&mut self, value: Value<'i>) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::GetElemRef(value.raw, vec![]));

        Value {
            ty: self.ty_pool.get_ref(value.ty),
            raw: self.next_id(),
        }
    }

    pub fn assign(&mut self, place: Value<'i>, value: Value<'i>) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Assign(place.raw, value.raw));

        Value {
            ty: self.ty_pool.get_null(),
            raw: RawValue::NULL,
        }
    }
    // pub fn ref_prop(&mut self, value: Value<'i>, prop: Prop) -> Value<'i> {}
    pub fn branch(&mut self, condition: Value<'i>, true_label: BlockRef, false_label: BlockRef) {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Branch(
                condition.raw,
                Label(true_label.idx as u16),
                vec![],
                Label(false_label.idx as u16),
                vec![],
            ));
    }

    pub fn jump(&mut self, label: BlockRef, args: &[Value<'i>]) {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Jump(
                Label(label.idx as u16),
                args.iter().map(|arg| arg.raw).collect(),
            ));
    }
    // pub fn ret(&mut self, value: Value<'i>) -> Value<'i> { unimplemented!() }

    pub fn ty_expr(&mut self, ty: TypeRef<'_>) -> Value<'i> {
        unimplemented!()
    }

    pub fn int_lit(&mut self, value: u32) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Int(value));
        Value {
            ty: self.ty_pool.get_int(),
            raw: self.next_id(),
        }
    }

    pub fn bool_lit(&mut self, value: bool) -> Value<'i> {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Bool(value));
        Value {
            ty: self.ty_pool.get_ty_bool(),
            raw: self.next_id(),
        }
    }

    pub fn append_block(&mut self, block: BlockRef) {
        let idx = self.blocks.len();
        self.blocks.push(Block {
            instructions: vec![],
            params: vec![],
        });
        if self.block_refs[block.idx].is_some() {
            panic!("block inserted twice");
        }
        self.block_refs[block.idx] = Some(idx);
    }

    pub fn create_block(&mut self) -> BlockRef {
        let idx = self.block_refs.len();
        self.block_refs.push(None);
        BlockRef { idx }
    }

    pub fn select(&mut self, block: BlockRef) {
        let Some(idx) = self.block_refs[block.idx] else {
            panic!("cannot select uninserted block");
        };
        self.current_block = idx;
        self.current_block_ref = block;
    }

    pub fn ret(&mut self, val: Value<'i>) {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Return(val.raw));
    }

    pub fn get_current_block(&self) -> BlockRef {
        self.current_block_ref
    }

    pub fn build(mut self) -> Vec<Block<'i>> {
        for block in &mut self.blocks {
            match block.instructions.last_mut() {
                Some(Instruction::Jump(block, _)) => {
                    block.0 = self.block_refs[block.0 as usize].expect("uninserted block") as u16;
                }
                Some(Instruction::Branch(_, yes, _, no, _)) => {
                    yes.0 = self.block_refs[yes.0 as usize].expect("uninserted block") as u16;
                    no.0 = self.block_refs[no.0 as usize].expect("uninserted block") as u16;
                }
                Some(Instruction::Return(_)) => {}
                inst => {
                    panic!("invalid terminator {:#?}", self.blocks)
                }
            }
        }

        self.blocks
    }
}
