use crate::{
    fun::{Function, Signature},
    pool,
    str::Str,
    ty::TypeRef,
    Ctx, Instruction, RawValue, Value,
};

pub struct FunctionBuilder<'i> {
    ty_pool: &'i crate::ty::Pool<'i>,
    name: Str<'i>,
    pub params: Vec<TypeRef<'i>>,
    pub ret_ty: Option<TypeRef<'i>>,
    blocks: Vec<Vec<Instruction<'i>>>,
    current_block: usize,
    next_free_id: u16,
}

#[derive(Clone, Copy)]
pub struct Block {
    idx: usize,
}

impl<'i> FunctionBuilder<'i> {
    pub fn new(pool: &'i crate::ty::Pool<'i>, name: Str<'i>) -> Self {
        FunctionBuilder {
            ty_pool: pool,
            name,
            params: vec![],
            ret_ty: None,
            blocks: vec![vec![]],
            current_block: 0,
            next_free_id: 0,
        }
    }

    fn next_id(&mut self) -> RawValue {
        let id = self.next_free_id;
        self.next_free_id += 1;
        RawValue(id)
    }

    pub fn parameter(&mut self, ty: TypeRef<'i>) -> Value<'i> {
        self.params.push(ty);
        Value {
            ty,
            raw: self.next_id(),
        }
    }

    pub fn add(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {
        self.blocks[self.current_block].push(Instruction::Add(lhs.raw, rhs.raw));

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
    // pub fn eq(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn neq(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn gt(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn lt(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn gt_eq(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn lt_eq(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn push(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn call(&mut self, func: FuncId, args: &[Value<'i>]) -> Value<'i> {}
    // pub fn assign(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    // pub fn ref_assign(&mut self, lhs: Value<'i>, rhs: Value<'i>) -> Value<'i> {}
    pub fn make_tuple(&mut self, values: &[Value<'i>]) -> Value<'i> {
        unimplemented!()
    }

    pub fn make_struct(&mut self, props: &[(Str, Value<'i>)]) -> Value<'i> {
        unimplemented!()
    }
    // pub fn name(&mut self, value: Value<'i>, ty: TypeId) -> Value<'i> {}
    // pub fn vector(&mut self, values: &[Value<'i>]) -> Value<'i> {}
    pub fn get_prop(&mut self, value: Value<'i>, prop: Str) -> Value<'i> {
        unimplemented!()
    }
    // pub fn ref_prop(&mut self, value: Value<'i>, prop: Prop) -> Value<'i> {}
    pub fn branch(
        &mut self,
        condition: Value<'i>,
        true_label: Block,
        false_label: Block,
    ) -> Value<'i> {
        unimplemented!()
    }
    pub fn jump(&mut self, label: Block) {
        unimplemented!()
    }
    // pub fn ret(&mut self, value: Value<'i>) -> Value<'i> { unimplemented!() }
    pub fn phi(&mut self, vals: &[(Block, Value<'i>)]) -> Value<'i> {
        unimplemented!()
    }
    pub fn ty_expr(&mut self, ty: TypeRef<'_>) -> Value<'i> {
        unimplemented!()
    }

    pub fn int_lit(&mut self, value: u32) -> Value<'i> {
        self.blocks[self.current_block].push(Instruction::Int(value));
        Value {
            ty: self.ty_pool.get_int(),
            raw: self.next_id(),
        }
    }

    pub fn create_block(&mut self) -> Block {
        unimplemented!()
    }
    pub fn select(&mut self, block: Block) {
        unimplemented!()
    }

    pub fn ret(&mut self, val: Value<'i>) {
        self.blocks[self.current_block].push(Instruction::Return(val.raw));
    }

    pub fn build(self) -> Function<'i> {
        Function {
            sig: Signature {
                ret_ty: self.ret_ty.unwrap(),
                params: self.ty_pool.get_ty_list(self.params),
                name: self.name,
            },
            body: self.blocks,
        }
    }
}
