use std::{fs::File, path::Path};

// use cranelift::prelude::InstBuilder;
use cranelift::{
    codegen::{
        ir::{self, InstBuilder, Signature},
        isa, settings,
    },
    prelude::{types, AbiParam, FunctionBuilder, FunctionBuilderContext},
};
use cranelift_module::{default_libcall_names, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use iiv::ty::TypeRef;

pub struct Backend {
    isa: isa::OwnedTargetIsa,
    fun_builder_ctx: FunctionBuilderContext,
}

struct PackageTransformer<'b> {
    module: ObjectModule,
    fun_builder_ctx: &'b mut FunctionBuilderContext,
}

impl Backend {
    pub fn new() -> Self {
        let isa = isa::lookup(target_lexicon::HOST)
            .unwrap()
            .finish(settings::Flags::new(settings::builder()))
            .unwrap();

        Self {
            isa,
            fun_builder_ctx: FunctionBuilderContext::new(),
        }
    }
    pub fn transform(&mut self, package: &iiv::Package, out: impl AsRef<Path>) {
        PackageTransformer {
            module: ObjectModule::new(
                ObjectBuilder::new(self.isa.clone(), "mod_i_guess", default_libcall_names())
                    .unwrap(),
            ),
            fun_builder_ctx: &mut self.fun_builder_ctx,
        }
        .transform(package, out);
    }
}

impl<'b> PackageTransformer<'b> {
    fn transform(mut self, package: &iiv::Package, out: impl AsRef<Path>) {
        package.funcs.iter().for_each(|fun| {
            let id = {
                let mut signature = self.module.make_signature();
                for &param in fun.sig.params.iter() {
                    signature.params.push(AbiParam::new(as_ty(param)));
                }
                signature.returns.push(AbiParam::new(as_ty(fun.sig.ret_ty)));
                self.module
                    .declare_function(&fun.sig.name, Linkage::Export, &signature)
            }
            .unwrap();

            let mut ctx = self.module.make_context();

            for &param in fun.sig.params.iter() {
                ctx.func.signature.params.push(AbiParam::new(as_ty(param)));
            }
            ctx.func
                .signature
                .returns
                .push(AbiParam::new(as_ty(fun.sig.ret_ty)));

            let mut builder = FunctionBuilder::new(&mut ctx.func, self.fun_builder_ctx);
            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);
            builder.seal_block(entry);
            gen(&mut builder, &fun.body[0]);
            builder.finalize();
            self.module.define_function(id, &mut ctx).unwrap();
            self.module.clear_context(&mut ctx);
        });

        let file = File::create(out).unwrap();
        self.module.finish().object.write_stream(file).unwrap();
    }
}

fn gen(builder: &mut cranelift::frontend::FunctionBuilder, insts: &[iiv::Instruction]) {
    let mut values = vec![];

    for param in builder.block_params(builder.current_block().unwrap()) {
        values.push(*param);
    }

    // builder.

    for inst in insts {
        match inst {
            iiv::Instruction::Int(val) => {
                values.push(builder.ins().iconst(types::I64, *val as i64))
            }
            iiv::Instruction::Add(lhs, rhs) => {
                let lhs = values[lhs.0 as usize];
                let rhs = values[rhs.0 as usize];
                values.push(builder.ins().iadd(lhs, rhs))
            }
            iiv::Instruction::Sub(lhs, rhs) => unimplemented!(),
            iiv::Instruction::Mul(lhs, rhs) => unimplemented!(),
            iiv::Instruction::Div(lhs, rhs) => unimplemented!(),
            iiv::Instruction::Not(Value) => unimplemented!(),
            iiv::Instruction::Neg(Value) => unimplemented!(),
            iiv::Instruction::Eq(lhs, rhs) => unimplemented!(),
            iiv::Instruction::Neq(lhs, rhs) => unimplemented!(),
            iiv::Instruction::Gt(lhs, rhs) => unimplemented!(),
            iiv::Instruction::Lt(lhs, rhs) => unimplemented!(),
            iiv::Instruction::GtEq(lhs, rhs) => unimplemented!(),
            iiv::Instruction::LtEq(lhs, rhs) => unimplemented!(),
            iiv::Instruction::Call(fun, args) => unimplemented!(),
            iiv::Instruction::Assign(lhs, rhs) => unimplemented!(),
            iiv::Instruction::RefAssign(lhs, rhs) => unimplemented!(),
            iiv::Instruction::Tuple(tpl) => unimplemented!(),
            iiv::Instruction::Name(TypeId, Value) => unimplemented!(),
            iiv::Instruction::GetElem(lhs, path) => unimplemented!(),
            iiv::Instruction::GetElemRef(lhs, path) => unimplemented!(),
            iiv::Instruction::Branch(lhs, yes, no) => unimplemented!(),
            iiv::Instruction::Jump(Label) => unimplemented!(),
            iiv::Instruction::Phi(labels) => unimplemented!(),
            iiv::Instruction::Return(val) => {
                let val = values[val.0 as usize];
                builder.ins().return_(&[val]);
            }
            iiv::Instruction::Ty(TypeId) => unimplemented!(),
        };
    }
    // builder.ins().
}

fn as_ty<'i>(ty: TypeRef<'i>) -> ir::Type {
    types::I64
}
