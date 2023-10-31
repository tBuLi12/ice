use std::{cell::UnsafeCell, fmt::Display};

use crate::{pool, str::Str, ty::TypeRef, Elem, Instruction};

#[derive(Debug)]
pub struct Signature<'i> {
    pub name: Str<'i>,
    pub params: pool::List<'i, TypeRef<'i>>,
    pub ret_ty: TypeRef<'i>,
}

#[derive(Debug)]
pub struct Function<'i> {
    pub sig: Signature<'i>,
    pub body: Vec<Vec<Instruction<'i>>>,
}

impl<'i> Display for Function<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}(", self.sig.name)?;
        let mut i = 0;
        for param in self.sig.params.iter() {
            write!(f, "%{}: {}, ", i, param)?;
            i += 1;
        }
        writeln!(f, "): {}", self.sig.ret_ty)?;
        for (block_index, block) in self.body.iter().enumerate() {
            writeln!(f, "b{}:", block_index)?;
            for inst in block {
                match inst {
                    Instruction::Int(val) => {
                        writeln!(f, "    %{} = {}", i, val)?;
                        i += 1;
                    }
                    Instruction::Add(lhs, rhs) => {
                        writeln!(f, "    %{} = %{} + %{}", i, lhs.0, rhs.0)?;
                        i += 1;
                    }
                    Instruction::Sub(lhs, rhs) => unimplemented!(),
                    Instruction::Mul(lhs, rhs) => unimplemented!(),
                    Instruction::Div(lhs, rhs) => unimplemented!(),
                    Instruction::Not(Value) => unimplemented!(),
                    Instruction::Neg(Value) => unimplemented!(),
                    Instruction::Eq(lhs, rhs) => unimplemented!(),
                    Instruction::Neq(lhs, rhs) => unimplemented!(),
                    Instruction::Gt(lhs, rhs) => unimplemented!(),
                    Instruction::Lt(lhs, rhs) => unimplemented!(),
                    Instruction::GtEq(lhs, rhs) => unimplemented!(),
                    Instruction::LtEq(lhs, rhs) => unimplemented!(),
                    Instruction::Call(fun, args) => {
                        write!(f, "    %{} = call {}(", i, fun.borrow().sig.name)?;
                        for arg in args {
                            write!(f, "%{}, ", arg.0)?;
                        }
                        writeln!(f, ")")?;
                        i += 1;
                    }
                    Instruction::Assign(lhs, rhs) => unimplemented!(),
                    Instruction::RefAssign(lhs, rhs) => unimplemented!(),
                    Instruction::Tuple(tpl, _) => {
                        write!(f, "    %{} = (", i)?;
                        for arg in tpl {
                            write!(f, "%{}, ", arg.0)?;
                        }
                        writeln!(f, ")")?;
                        i += 1;
                    }
                    Instruction::Name(TypeId, Value) => unimplemented!(),
                    Instruction::GetElem(lhs, path) => {
                        write!(f, "    %{} = elem %{} ", i, lhs.0)?;
                        for elem in path {
                            match elem {
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
                    Instruction::GetElemRef(lhs, path) => unimplemented!(),
                    Instruction::Branch(lhs, yes, no) => unimplemented!(),
                    Instruction::Jump(Label) => unimplemented!(),
                    Instruction::Phi(labels) => unimplemented!(),
                    Instruction::Return(val) => {
                        writeln!(f, "    return %{}", val.0)?;
                    }
                    Instruction::Ty(TypeId) => unimplemented!(),
                };
            }
        }
        Ok(())
    }
}
