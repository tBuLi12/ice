use crate::{Instruction, Label, Value};

pub struct Builder {
    blocks: Vec<Vec<Instruction>>,
    current_block: usize,
}

#[derive(Clone, Copy)]
pub struct Block {
    idx: usize,
}

impl Builder {
    fn new() -> Self {
        Builder {
            blocks: vec![vec![]],
            current_block: 0,
        }
    }

    // pub fn add(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn sub(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn mul(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn div(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn not(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn neg(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn eq(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn neq(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn gt(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn lt(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn gt_eq(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn lt_eq(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn push(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn call(&mut self, func: FuncId, args: &[Value]) -> Value {}
    // pub fn assign(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn ref_assign(&mut self, lhs: Value, rhs: Value) -> Value {}
    // pub fn aggregate(&mut self, values: &[Value]) -> Value {}
    // pub fn name(&mut self, value: Value, ty: TypeId) -> Value {}
    // pub fn vector(&mut self, values: &[Value]) -> Value {}
    // pub fn get_prop(&mut self, value: Value, prop: Prop) -> Value {}
    // pub fn ref_prop(&mut self, value: Value, prop: Prop) -> Value {}
    pub fn branch(&mut self, condition: Value, true_label: Label, false_label: Label) -> Value {}
    pub fn jump(&mut self, label: Label) {}
    // pub fn ret(&mut self, value: Value) -> Value {}
    pub fn phi(&mut self, vals: &[(Block, Value)]) -> Value {}
    pub fn create_block(&mut self) -> Block {}
    pub fn select(&mut self, block: Block) {}
}
