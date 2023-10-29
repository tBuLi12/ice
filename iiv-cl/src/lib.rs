use std::{collections::HashMap, fs::File, path::Path};

// use cranelift::prelude::InstBuilder;
use cranelift::{
    codegen::{
        ir::{self, InstBuilder, Signature},
        isa, settings,
    },
    prelude::{
        types, AbiParam, FunctionBuilder, FunctionBuilderContext, StackSlotData, StackSlotKind,
    },
};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use iiv::{pool::FuncRef, ty::TypeRef};

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
    fn transform<'i>(mut self, package: &iiv::Package<'i>, out: impl AsRef<Path>) {
        let mut funs = HashMap::<FuncRef<'i>, FuncId>::new();
        package.funcs.iter().for_each(|fun_ref| {
            let fun = fun_ref.borrow();

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
            funs.insert(*fun_ref, id);
        });

        package.funcs.iter().for_each(|fun_ref| {
            let fun = fun_ref.borrow();
            let id = funs[fun_ref];

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
            gen(&mut self.module, &mut builder, &fun.body[0], &funs);
            builder.finalize();
            self.module.define_function(id, &mut ctx).unwrap();
            self.module.clear_context(&mut ctx);
        });

        let file = File::create(out).unwrap();
        self.module.finish().object.write_stream(file).unwrap();
    }
}

fn gen<'i>(
    module: &mut ObjectModule,
    builder: &mut cranelift::frontend::FunctionBuilder,
    insts: &[iiv::Instruction<'i>],
    funs: &HashMap<FuncRef<'i>, FuncId>,
) {
    let mut values = vec![];

    for param in builder.block_params(builder.current_block().unwrap()) {
        values.push(*param);
    }

    builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 12));

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
            iiv::Instruction::Call(fun, args) => {
                let local_func = module.declare_func_in_func(funs[fun], &mut builder.func);
                let args: Vec<_> = args.iter().map(|arg| values[arg.0 as usize]).collect();
                let call = builder.ins().call(local_func, &args);
                values.push(builder.inst_results(call)[0])
            }
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
