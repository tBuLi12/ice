use crate::{
    pool::{self, FuncRef},
    str::Str,
    ty::TypeRef,
    Elem, Instruction, Label, Prop, RawValue, Value,
};

pub struct FunctionBuilder<'i> {
    ty_pool: &'i crate::ty::Pool<'i>,
    blocks: Vec<Block<'i>>,
    current_block: usize,
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
            current_block: 0,
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
                    Instruction::Int(val) => {}
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
        self.next_free_id = dbg!(end + diff);
    }

    fn next_id(&mut self) -> RawValue {
        let id = self.next_free_id;
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

    pub fn create_block(&mut self) -> BlockRef {
        let block_ref = self.blocks.len();
        self.blocks.push(Block {
            instructions: vec![],
            params: vec![],
        });
        BlockRef { idx: block_ref }
    }
    pub fn select(&mut self, block: BlockRef) {
        self.current_block = block.idx;
    }

    pub fn ret(&mut self, val: Value<'i>) {
        self.blocks[self.current_block]
            .instructions
            .push(Instruction::Return(val.raw));
    }

    pub fn build(self) -> Vec<Block<'i>> {
        self.blocks
    }
}
