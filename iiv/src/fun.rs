use std::{cell::UnsafeCell, fmt::Display};

use crate::{builder::Block, pool, str::Str, ty::TypeRef, Elem, Instruction};

#[derive(Debug)]
pub struct Signature<'i> {
    pub name: Str<'i>,
    pub params: pool::List<'i, TypeRef<'i>>,
    pub ret_ty: TypeRef<'i>,
}

#[derive(Debug)]
pub struct Function<'i> {
    pub sig: Signature<'i>,
    pub body: Vec<Block<'i>>,
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
        for (block_index, block) in self.body.iter().enumerate() {
            writeln!(f, "b{} {}:", block_index, fmt::List(&block.params))?;
            i += block.params.len();
            for inst in &block.instructions {
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
                    Instruction::Sub(lhs, rhs) => unimplemented!(),
                    Instruction::Mul(lhs, rhs) => unimplemented!(),
                    Instruction::Div(lhs, rhs) => unimplemented!(),
                    Instruction::Not(Value) => unimplemented!(),
                    Instruction::Neg(Value) => unimplemented!(),
                    Instruction::Eq(lhs, rhs) => {
                        writeln!(f, "    %{} = %{} == %{}", i, lhs.0, rhs.0)?;
                        i += 1;
                    }
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
                    Instruction::Assign(lhs, rhs) => {
                        writeln!(f, "    store {} <- {}", lhs.0, rhs.0)?;
                    }
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
                    Instruction::GetElemRef(lhs, path) => {
                        write!(f, "    %{} = elem ref %{} ", i, lhs.0)?;
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
                    Instruction::Branch(lhs, yes, yes_args, no, no_args) => {
                        writeln!(
                            f,
                            "    br %{} ? b{} {} : b{} {}",
                            lhs.0,
                            yes.0,
                            fmt::List(&yes_args),
                            no.0,
                            fmt::List(&no_args),
                        )?;
                    }
                    Instruction::Jump(label, args) => {
                        writeln!(f, "    jmp b{} {}", label.0, fmt::List(&args))?;
                    }
                    Instruction::Return(val) => {
                        writeln!(f, "    return %{}", val.0)?;
                    }
                    Instruction::Ty(TypeId) => unimplemented!(),
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
                    Instruction::Discriminant(value) => {
                        writeln!(f, "    %{} = discriminant %{}", i, value.0)?;
                        i += 1;
                    }
                };
            }
        }
        Ok(())
    }
}
