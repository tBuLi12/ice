// use cranelift::prelude::InstBuilder;
use cranelift::codegen::ir::InstBuilder;

fn gen(builder: &mut cranelift::frontend::FunctionBuilder, insts: &[iiv::Instruction]) {
    let values = vec![];

    // builder.

    for inst in insts {
        match inst {
            iiv::Instruction::Add(lhs, rhs) => builder.ins().iadd(values[lhs], values[rhs]),
            iiv::Instruction::Sub(lhs, rhs) => {
                builder.ins().ssub_overflow(values[lhs], values[rhs])
            }
            iiv::Instruction::Mul(lhs, rhs) => builder.ins().imul(values[lhs], values[rhs]),
            iiv::Instruction::Div(lhs, rhs) => builder.ins().udiv(values[lhs], values[rhs]),
            iiv::Instruction::Not(Value) => builder.ins().iadd(values[lhs], values[rhs]),
            iiv::Instruction::Neg(Value) => builder.ins().iadd(values[lhs], values[rhs]),
            iiv::Instruction::Eq(lhs, rhs) => builder.ins().iadd(values[lhs], values[rhs]),
            iiv::Instruction::Neq(lhs, rhs) => builder.ins().iadd(values[lhs], values[rhs]),
            iiv::Instruction::Gt(lhs, rhs) => builder.ins().iadd(values[lhs], values[rhs]),
            iiv::Instruction::Lt(lhs, rhs) => builder.ins().iadd(values[lhs], values[rhs]),
            iiv::Instruction::GtEq(lhs, rhs) => builder.ins().iadd(values[lhs], values[rhs]),
            iiv::Instruction::LtEq(lhs, rhs) => builder.ins().iadd(values[lhs], values[rhs]),
            iiv::Instruction::Call(fun, args) => builder.ins().i,
            iiv::Instruction::Assign(lhs, rhs) => unimplemented!(),
            iiv::Instruction::RefAssign(lhs, rhs) => unimplemented!(),
            iiv::Instruction::Tuple(tpl) => unimplemented!(),
            iiv::Instruction::Name(TypeId, Value) => unimplemented!(),
            iiv::Instruction::GetElem(lhs, path) => unimplemented!(),
            iiv::Instruction::GetElemRef(lhs, path) => unimplemented!(),
            iiv::Instruction::Branch(lhs, Label, Label) => unimplemented!(),
            iiv::Instruction::Jump(Label) => unimplemented!(),
            iiv::Instruction::Phi(labels) => unimplemented!(),
            iiv::Instruction::Return(Value) => unimplemented!(),
            iiv::Instruction::Ty(TypeId) => unimplemented!(),
        }
    }
    // builder.ins().
}
