use core::panic;
use std::{cell::UnsafeCell, collections::HashMap};

use iiv::{
    fun::Body,
    impl_tree::ImplForest,
    pool::{FuncRef, List, TraitImplRef},
    ty::{self, TraitRef, TypeRef},
    RawValue,
};

pub struct Backend<'i> {
    ctx: &'i iiv::Ctx<'i>,
}

impl<'i> Backend<'i> {
    pub fn new(ctx: &'i iiv::Ctx<'i>) -> Self {
        Self { ctx }
    }

    pub fn transform(&mut self, package: &iiv::Package<'i>, out: &str) {
        let main = package.main.expect("main is not defined");

        let ctx = llvm::LLVMCtx::new();
        let module = ctx.create_module();

        let builtins = Builtints::get(&ctx, &module);

        let mut ir_gen = IRGen {
            module,
            ir: ctx.create_builder(),
            fun_opt_manager: ctx.create_function_opt_manager(),
            phis: vec![],
            ctx: self.ctx,
            funcs: HashMap::new(),
            values: vec![],
            work_stack: vec![],
            llvm_ctx: &ctx,
            blocks: vec![],
            llvm_ty_cache: UnsafeCell::new(vec![]),
            builtins,
        };

        iiv::move_check::resolve_drops(
            &self.ctx,
            &mut *main.borrow_mut(),
            self.ctx.builtins.get_drop(),
        );
        main.borrow_mut().seal();
        ir_gen.emit_main(&mut *main.borrow_mut());

        eprintln!("dump mod");
        ctx.emit(ir_gen.module, out);
        eprintln!("main done");
    }
}

enum FuturePhi<'ll> {
    Future(Vec<(llvm::Block<'ll>, Vec<llvm::Value<'ll>>)>),
    Existing(Vec<llvm::inst::Phi<'ll>>),
}

#[derive(Clone, Copy)]
struct LLVMValue<'ll, 'i> {
    ty: TypeRef<'i>,
    value: llvm::Value<'ll>,
    gen_ptr: llvm::Value<'ll>,
}

#[derive(Clone, Copy)]
struct LLVMStackValue<'ll, 'i> {
    ty: TypeRef<'i>,
    alloca: llvm::Value<'ll>,
    gen_ptr: llvm::Value<'ll>,
    value: llvm::Value<'ll>,
}

impl<'ll, 'i> LLVMStackValue<'ll, 'i> {
    fn value(self) -> LLVMValue<'ll, 'i> {
        LLVMValue {
            ty: self.ty,
            value: self.value,
            gen_ptr: self.gen_ptr,
        }
    }
}

struct Builtints<'ll> {
    rt_invalidate: llvm::Function<'ll>,
    rt_validate: llvm::Function<'ll>,
    rt_init: llvm::Function<'ll>,
    rt_gen_alloc: llvm::Function<'ll>,
    rt_malloc: llvm::Function<'ll>,
    rt_free: llvm::Function<'ll>,
}

impl<'ll> Builtints<'ll> {
    fn get(ctx: &'ll llvm::LLVMCtx, module: &llvm::LLVMModule<'ll>) -> Builtints<'ll> {
        let rt_invalidate = module.create_func("rt_invalidate", &[ctx.ty_ptr()], ctx.ty_void());
        let rt_validate =
            module.create_func("rt_validate", &[ctx.ty_ptr(), ctx.ty_int()], ctx.ty_void());
        let rt_init = module.create_func("rt_init", &[], ctx.ty_void());
        let rt_gen_alloc = module.create_func("rt_gen_alloc", &[ctx.ty_ptr()], ctx.ty_void());
        let rt_malloc = module.create_func("rt_malloc", &[ctx.ty_int()], ctx.ty_ptr());
        let rt_free = module.create_func("rt_free", &[ctx.ty_ptr()], ctx.ty_void());

        Builtints {
            rt_invalidate,
            rt_validate,
            rt_init,
            rt_gen_alloc,
            rt_free,
            rt_malloc,
        }
    }
}

pub struct IRGen<'ll, 'i> {
    ir: llvm::IRBuilder<'ll>,
    module: llvm::LLVMModule<'ll>,
    fun_opt_manager: llvm::FunctionOptManager<'ll>,
    phis: Vec<FuturePhi<'ll>>,
    values: Vec<LLVMStackValue<'ll, 'i>>,
    blocks: Vec<llvm::Block<'ll>>,
    funcs: HashMap<(FuncRef<'i>, List<'i, TypeRef<'i>>), llvm::Function<'ll>>,
    work_stack: Vec<(FuncRef<'i>, List<'i, TypeRef<'i>>)>,
    ctx: &'i iiv::Ctx<'i>,
    llvm_ctx: &'ll llvm::LLVMCtx,
    llvm_ty_cache: UnsafeCell<Vec<UnsafeCell<CachedType<'ll>>>>,
    builtins: Builtints<'ll>,
}

impl<'ll, 'i> IRGen<'ll, 'i> {
    pub fn emit_main(&mut self, func: &mut iiv::fun::Function<'i>) {
        let main = self.create_signature(func, self.ctx.type_pool.get_ty_list(vec![]));
        self.write_body(func, main);

        while let Some((func, ty_args)) = self.work_stack.pop() {
            let llvm_func = self.funcs.get(&(func, ty_args)).unwrap();

            let mut fun = func.borrow().clone();
            fun.apply_ty_args(&self.ctx.type_pool, ty_args);
            iiv::move_check::resolve_drops(&self.ctx, &mut fun, self.ctx.builtins.get_drop());
            fun.seal();

            self.write_body(&mut fun, *llvm_func);
        }
    }

    pub fn write_body(
        &mut self,
        func: &mut iiv::fun::Function<'i>,
        llvm_func: llvm::Function<'ll>,
    ) {
        let body = match &func.body {
            Body::Sealed(body) => body,
            Body::Unsealed(_) => panic!("codegen on unsealed body"),
            Body::None => return,
            Body::AutoCopy => {
                func.body = Body::Unsealed(vec![]);
                iiv::move_check::inject_auto_copy_impl(self.ctx, func);
                self.write_body(func, llvm_func);
                if llvm_func.verify() {
                    panic!("invalid function");
                }
                return;
            }
            Body::MemAlloc(ty) => {
                let ty = *ty;
                func.body = Body::Unsealed(vec![]);
                let block = self.llvm_ctx.create_block();
                llvm_func.append(block);
                let count = llvm_func.args().next().unwrap();
                self.ir.set_insert_point(block);
                let size = self.module.alloc_size_of(self.llvm_ty(ty));
                let size = self.ir.mul(self.llvm_ctx.int(size as u32).val(), count);
                let ptr = self.ir.call(self.builtins.rt_malloc, &[size]);
                self.ir.ret(ptr);
                if llvm_func.verify() {
                    panic!("invalid function");
                }
                return;
            }
            Body::MemFree => {
                func.body = Body::Unsealed(vec![]);
                let block = self.llvm_ctx.create_block();
                llvm_func.append(block);
                let ptr = llvm_func.args().next().unwrap();
                self.ir.set_insert_point(block);
                self.ir.call(self.builtins.rt_free, &[ptr]);
                self.ir.ret(self.llvm_null());
                if llvm_func.verify() {
                    panic!("invalid function");
                }
                return;
            }
            Body::PtrAdd => {
                func.body = Body::Unsealed(vec![]);
                let block = self.llvm_ctx.create_block();
                llvm_func.append(block);
                self.ir.set_insert_point(block);
                let mut args = llvm_func.args();
                let ptr = args.next().unwrap();
                let offset = args.next().unwrap();
                let iiv::ty::Type::Ptr(inner) = &*func.sig.params[0] else {
                    panic!("invalid ptr_add arg");
                };
                eprintln!("gep 1");
                let offset_ptr = self.ir.gep(self.llvm_ty(*inner), ptr, &[offset]);
                self.ir.ret(offset_ptr);
                if llvm_func.verify() {
                    panic!("invalid function");
                }
                return;
            }
            Body::PtrWrite => {
                func.body = Body::Unsealed(vec![]);
                let block = self.llvm_ctx.create_block();
                llvm_func.append(block);
                self.ir.set_insert_point(block);
                let mut args = llvm_func.args();
                let ptr = args.next().unwrap();
                let value = args.next().unwrap();
                self.ir.store(ptr, value);
                self.ir.ret(self.llvm_null());
                if llvm_func.verify() {
                    panic!("invalid function");
                }
                return;
            }
            Body::BitwiseCopy => {
                let block = self.llvm_ctx.create_block();
                llvm_func.append(block);
                self.ir.set_insert_point(block);
                let arg = self.on_the_stack(llvm_func.args().next().unwrap(), func.sig.params[0]);
                let copied = self.elem_ptr(arg.value(), 0);
                let ret = self.load(copied);
                self.ir.ret(ret);
                if llvm_func.verify() {
                    panic!("invalid function");
                }
                return;
            }
        };

        self.phis.clear();
        self.values.clear();
        self.blocks.clear();

        for _ in body {
            let llvm_block = self.llvm_ctx.create_block();
            llvm_func.append(llvm_block);
            self.blocks.push(llvm_block);
            self.phis.push(FuturePhi::Future(vec![]));
        }

        self.ir.set_insert_point(self.blocks[0]);
        self.ir.call(self.builtins.rt_init, &[]);

        for (arg, &ty) in llvm_func.args().zip(func.sig.params.iter()) {
            self.on_the_stack(arg, ty);
        }

        for (i, block) in body.iter().enumerate() {
            let llvm_block = self.blocks[i];
            self.ir.set_insert_point(llvm_block);
            self.emit_block(block, i);
        }

        if llvm_func.verify() {
            panic!("invalid function");
        }

        // self.fun_opt_manager.optimize(llvm_func);
    }

    pub fn emit_block(&mut self, block: &iiv::builder::Block<'i>, i: usize) {
        // init phis
        {
            let p = &mut self.phis[i];

            let phis = match p {
                FuturePhi::Future(vals) => {
                    let phis: Vec<_> = if let Some(first) = vals.first() {
                        first.1.iter().map(|val| self.ir.phi(val.ty())).collect()
                    } else {
                        vec![]
                    };
                    for (src_block, valset) in vals {
                        for (&val, &phi) in valset.iter().zip(&phis) {
                            phi.add_incomming(*src_block, val);
                        }
                    }
                    phis
                }
                FuturePhi::Existing(_) => {
                    panic!("phi should not exist");
                    // for (&phi, param) in phis.iter().zip(block.params) {
                    //     // phi.add_incomming()
                    // }
                }
            };
            for (&phi, &ty) in phis.iter().zip(block.params.iter()) {
                self.on_the_stack(phi.val(), ty);
            }
            self.phis[i] = FuturePhi::Existing(phis);
        }

        // rest
        for instruction in &block.instructions {
            match instruction {
                iiv::Instruction::Int(val) => {
                    self.on_the_stack(self.llvm_ctx.int(*val).val(), self.ctx.type_pool.get_int());
                }
                iiv::Instruction::Bool(val) => {
                    self.on_the_stack(
                        self.llvm_ctx.boolean(*val).val(),
                        self.ctx.type_pool.get_ty_bool(),
                    );
                }
                iiv::Instruction::Add(lhs, rhs) => {
                    let ty = self.val(*lhs).ty;
                    let lhs = self.get(*lhs);
                    let rhs = self.get(*rhs);
                    self.on_the_stack(self.ir.add(lhs, rhs), ty);
                }
                iiv::Instruction::Sub(_lhs, _rhs) => unimplemented!(),
                iiv::Instruction::Mul(lhs, rhs) => {
                    let ty = self.val(*lhs).ty;
                    let lhs = self.get(*lhs);
                    let rhs = self.get(*rhs);
                    self.on_the_stack(self.ir.mul(lhs, rhs), ty);
                }
                iiv::Instruction::Div(_lhs, _rhs) => unimplemented!(),
                iiv::Instruction::Not(_value) => unimplemented!(),
                iiv::Instruction::Neg(_value) => unimplemented!(),
                iiv::Instruction::Eq(lhs, rhs) => {
                    let lhs = self.get(*lhs);
                    let rhs = self.get(*rhs);
                    self.on_the_stack(self.ir.eq(lhs, rhs), self.ctx.type_pool.get_ty_bool());
                }
                iiv::Instruction::Neq(lhs, rhs) => {
                    let lhs = self.get(*lhs);
                    let rhs = self.get(*rhs);
                    self.on_the_stack(self.ir.add(lhs, rhs), self.ctx.type_pool.get_ty_bool());
                }
                iiv::Instruction::Gt(lhs, rhs) => {
                    let lhs = self.get(*lhs);
                    let rhs = self.get(*rhs);
                    self.on_the_stack(self.ir.add(lhs, rhs), self.ctx.type_pool.get_ty_bool());
                }
                iiv::Instruction::Lt(lhs, rhs) => {
                    let lhs = self.get(*lhs);
                    let rhs = self.get(*rhs);
                    self.on_the_stack(self.ir.add(lhs, rhs), self.ctx.type_pool.get_ty_bool());
                }
                iiv::Instruction::GtEq(lhs, rhs) => {
                    let lhs = self.get(*lhs);
                    let rhs = self.get(*rhs);
                    self.on_the_stack(self.ir.add(lhs, rhs), self.ctx.type_pool.get_ty_bool());
                }
                iiv::Instruction::LtEq(lhs, rhs) => {
                    let lhs = self.get(*lhs);
                    let rhs = self.get(*rhs);
                    self.on_the_stack(self.ir.add(lhs, rhs), self.ctx.type_pool.get_ty_bool());
                }
                iiv::Instruction::TraitCall(tr_decl, idx, args, ty_args) => {
                    let (this_ty, rest) = ty_args.split_first().unwrap();
                    let (tr_ty_args, method_ty_args) =
                        rest.split_at(tr_decl.borrow().ty_params.len());

                    let (impl_ref, ty_args) = self
                        .ctx
                        .impl_forest
                        .find(
                            *this_ty,
                            TraitRef {
                                decl: *tr_decl,
                                ty_args: self.ctx.type_pool.get_ty_list(tr_ty_args.to_vec()),
                            },
                        )
                        .unwrap();
                    let raw_args: Vec<_> = args.iter().map(|arg| self.get(*arg)).collect();
                    self.trait_call(impl_ref, *idx, ty_args, method_ty_args, &raw_args);
                }
                iiv::Instruction::Call(fun, args, ty_args) => {
                    let ret_ty = self
                        .ctx
                        .type_pool
                        .resolve_ty_args(fun.borrow().sig.ret_ty, &ty_args);
                    let mut raw_args = Vec::new();

                    let func = self.get_or_create_fn(*fun, *ty_args);

                    raw_args.extend(args.iter().map(|arg| self.get(*arg)));

                    let call = self.ir.call(func, &raw_args);

                    self.on_the_stack(call, ret_ty);
                }
                iiv::Instruction::Assign(lhs, path, rhs) => {
                    let mut lhs = self.val(*lhs);
                    for elem in path {
                        let iiv::Elem::Prop(iiv::Prop(idx)) = elem else {
                            panic!("invalid get_elem");
                        };
                        lhs = self.elem_ptr(lhs, *idx as usize);
                    }
                    let rhs = self.val(*rhs);
                    self.write(lhs, rhs);
                    self.null_val();
                }
                iiv::Instruction::Tuple(tpl, ty) => {
                    let tuple_value = self.alloc(*ty).value();

                    for (i, val) in tpl.iter().enumerate() {
                        let field = self.elem_ptr(tuple_value, i);
                        let val = self.val(*val);
                        self.write(field, val);
                    }
                }
                iiv::Instruction::Name(ty, value) => {
                    let value = self.val(*value);
                    let named_value = self.alloc(*ty).value();
                    let inner = self.elem_ptr(named_value, 0);
                    self.write(inner, value);
                }
                iiv::Instruction::CopyElem(lhs, path) => {
                    let mut value = self.val(*lhs);

                    for elem in path {
                        match elem {
                            iiv::Elem::Prop(iiv::Prop(idx)) => {
                                value = self.elem_ptr(value, *idx as usize);
                            }
                            iiv::Elem::Discriminant => {
                                value = self.discriminant_ptr(value);
                            }
                            _ => panic!("invalid get_elem"),
                        }
                    }

                    let copy_tr = self.ctx.builtins.get_copy();
                    let copy_impl = self.ctx.impl_forest.find(value.ty, copy_tr).unwrap();
                    if copy_impl.0 == self.ctx.builtins.get_bitwise_copy() {
                        self.read(value);
                    } else {
                        let arg = self.make_ref(value);
                        self.values.pop();
                        let raw_arg = self.load(arg.value());
                        self.trait_call(copy_impl.0, 0, copy_impl.1, &[], &[raw_arg]);
                    }
                }
                iiv::Instruction::MoveElem(lhs, path) => {
                    let mut value = self.val(*lhs);

                    for elem in path {
                        value = self.elem_ptr(value, *elem as usize);
                    }

                    self.read(value);
                }
                iiv::Instruction::GetElemRef(lhs, path) => {
                    let mut value = self.val(*lhs);

                    for elem in path {
                        let iiv::Elem::Prop(iiv::Prop(idx)) = elem else {
                            panic!("invalid get_elem");
                        };
                        value = self.elem_ptr(value, *idx as usize);
                    }

                    self.make_ref(value);
                }
                iiv::Instruction::Branch(lhs, yes_label, no_label, args) => {
                    let args: Vec<_> = args.iter().map(|arg| self.get(*arg)).collect();
                    let cond = self.get(*lhs);

                    self.apply_block_args(yes_label, &args);
                    self.apply_block_args(no_label, &args);

                    self.ir.condbr(
                        cond,
                        self.blocks[yes_label.0 as usize],
                        self.blocks[no_label.0 as usize],
                    );
                }
                iiv::Instruction::Switch(lhs, labels, args) => {
                    let idx = self.get(*lhs);

                    let args: Vec<_> = args.iter().map(|arg| self.get(*arg)).collect();
                    for label in labels {
                        self.apply_block_args(label, &args);
                    }
                    let blocks: Vec<_> = labels
                        .iter()
                        .map(|label| self.blocks[label.0 as usize])
                        .collect();
                    self.ir.switch(idx, &blocks);
                }
                iiv::Instruction::Jump(label, args) => {
                    let args: Vec<_> = args.iter().map(|arg| self.get(*arg)).collect();
                    self.apply_block_args(label, &args);
                    self.ir.br(self.blocks[label.0 as usize]);
                }
                iiv::Instruction::Return(val) => {
                    let val = self.get(*val);
                    self.ir.ret(val);
                }
                iiv::Instruction::Ty(_type_id) => unimplemented!(),
                iiv::Instruction::Variant(ty, idx, val) => {
                    let variant = self.alloc(*ty).value();
                    let idx_val = self.llvm_ctx.int(*idx as u32).val();
                    let discriminant = self.discriminant_ptr(variant);
                    self.ir.store(discriminant.value, idx_val);
                    let zero = self.llvm_ctx.int(0).val();
                    let gen_ptr_ptr =
                        self.ir
                            .gep(self.llvm_ty(variant.ty), variant.value, &[zero, zero]);
                    self.ir
                        .store(gen_ptr_ptr, self.llvm_ctx.ty_ptr().null().val());
                    let body = self.elem_ptr(variant, *idx as usize);
                    let val = self.val(*val);
                    self.write(body, val);
                }
                iiv::Instruction::VariantCast(ty, val) => {
                    let target = self.alloc(*ty).value();
                    let discriminant = self.discriminant_ptr(target);
                    let src_variant = self.val(*val);
                    let iiv::ty::Type::Variant(src_elems) = &*src_variant.ty else {
                        panic!("variant cast has non-variant operands")
                    };
                    let src_discriminant = self.discriminant_ptr(src_variant);
                    self.write(target, src_variant);
                    let llvm_int = self.llvm_ty(self.ctx.type_pool.get_int());
                    let cast_table_ty = self.llvm_ctx.array_ty(llvm_int, src_elems.len() as u64);
                    let cast_table =
                        self.module
                            .get_or_create_global("cast_table", cast_table_ty, || {
                                let indices: Vec<_> = match &**ty {
                                    iiv::ty::Type::Variant(dst_elems) => src_elems
                                        .iter()
                                        .map(|elem| {
                                            dst_elems.iter().position(|e| *e == *elem).unwrap()
                                                as u32
                                        })
                                        .map(|idx| self.llvm_ctx.int(idx))
                                        .collect(),
                                    _ => panic!("variant cast has non-variant operands"),
                                };
                                self.llvm_ctx.const_array(llvm_int, &indices)
                            });

                    let discriminant_value = self.ir.load(src_discriminant.value, llvm_int);
                    let new_discriminant = self.ir.load(
                        self.ir.gep(llvm_int, cast_table, &[discriminant_value]),
                        llvm_int,
                    );

                    self.ir.store(discriminant.value, new_discriminant);
                    let zero = self.llvm_ctx.int(0).val();
                    let gen_ptr_ptr =
                        self.ir
                            .gep(self.llvm_ty(target.ty), target.value, &[zero, zero]);
                    self.ir
                        .store(gen_ptr_ptr, self.llvm_ctx.ty_ptr().null().val());
                }
                iiv::Instruction::RefToPtr(val) => {
                    let val = self.val(*val);
                    let ty::Type::Ref(inner) = &*val.ty else {
                        panic!("invalid ref to ptr");
                    };
                    let ptr = self.ir.load(
                        self.ir.gep(
                            self.llvm_ty(val.ty),
                            val.value,
                            &[self.llvm_ctx.int(0).val(), self.llvm_ctx.int(2).val()],
                        ),
                        self.llvm_ctx.ty_ptr(),
                    );
                    self.on_the_stack(ptr, self.ctx.type_pool.get_ptr(*inner));
                }
                iiv::Instruction::Null => {
                    self.null_val();
                }
                iiv::Instruction::Drop(_) => {
                    panic!("invalid instruction")
                }
                iiv::Instruction::Invalidate(val, path) => {
                    let stack_value = self.values[val.0 as usize];
                    if let Some(path) = path {
                        let mut value = stack_value.value();
                        for elem in path {
                            value = self.elem_ptr(value, *elem as usize);
                        }
                        let zero = self.llvm_ctx.int(0).val();
                        let gen_ptr =
                            self.ir
                                .gep(self.llvm_ty(value.ty), value.value, &[zero, zero]);
                        self.ir.call(self.builtins.rt_invalidate, &[gen_ptr]);
                    } else {
                        self.ir
                            .call(self.builtins.rt_invalidate, &[stack_value.gen_ptr]);
                    }
                }
                iiv::Instruction::CallDrop(_, _) => {
                    unimplemented!();
                }
            }
        }
    }

    fn trait_call(
        &mut self,
        impl_ref: TraitImplRef<'i>,
        idx: u16,
        impl_ty_args: List<'i, TypeRef<'i>>,
        method_ty_args: &[TypeRef<'i>],
        args: &[llvm::Value<'ll>],
    ) {
        let call_ty_args = self.ctx.type_pool.get_ty_list(
            impl_ty_args
                .iter()
                .copied()
                .chain(method_ty_args.iter().copied())
                .collect(),
        );

        let fun = impl_ref.borrow().functions[idx as usize].fun;

        let ret_ty = self
            .ctx
            .type_pool
            .resolve_ty_args(fun.borrow().sig.ret_ty, &call_ty_args);

        let func = self.get_or_create_fn(fun, call_ty_args);

        let call = self.ir.call(func, args);

        self.on_the_stack(call, ret_ty);
    }

    fn null_val(&mut self) {
        let null = self.llvm_null();
        self.on_the_stack(null, self.ctx.type_pool.get_null());
    }

    fn llvm_null(&self) -> llvm::Value<'ll> {
        self.llvm_ty(self.ctx.type_pool.get_null()).null().val()
    }

    fn apply_block_args(&mut self, block: &iiv::Label, args: &[llvm::Value<'ll>]) {
        let future_phi = &mut self.phis[block.0 as usize];
        let current_block = self.ir.current_block();

        match future_phi {
            FuturePhi::Future(vals) => {
                vals.push((self.ir.current_block(), args.to_owned()));
            }
            FuturePhi::Existing(phis) => {
                for (&phi, &param) in phis.iter().zip(args) {
                    phi.add_incomming(current_block, param);
                }
            }
        };
    }

    fn on_the_stack(&mut self, val: llvm::Value<'ll>, ty: TypeRef<'i>) -> LLVMStackValue<'ll, 'i> {
        let gen_ptr_ty = self.llvm_ctx.ty_ptr();
        let storage_ty = self.llvm_ctx.struct_ty(&[gen_ptr_ty, val.ty()]);
        let alloca = self.ir.create_alloca(storage_ty);
        let zero = self.llvm_ctx.int(0).val();
        let one = self.llvm_ctx.int(1).val();
        let gen_ptr_ptr = self.ir.gep(storage_ty, alloca, &[zero, zero]);
        let val_ptr = self.ir.gep(storage_ty, alloca, &[zero, one]);
        self.ir.store(gen_ptr_ptr, gen_ptr_ty.null().val());
        self.ir.store(val_ptr, val);
        let value = LLVMStackValue {
            ty,
            value: val_ptr,
            gen_ptr: gen_ptr_ptr,
            alloca,
        };
        self.values.push(value);
        value
    }

    fn elem_ptr(&mut self, val: LLVMValue<'ll, 'i>, elem: usize) -> LLVMValue<'ll, 'i> {
        use iiv::ty::Type;

        let ty = self.llvm_ty(val.ty);

        match *val.ty {
            Type::Struct(props) => LLVMValue {
                ty: props[elem].1,
                value: {
                    self.ir.gep(
                        ty,
                        val.value,
                        &[
                            self.llvm_ctx.int(0).val(),
                            self.llvm_ctx.int(elem as u32).val(),
                        ],
                    )
                },
                gen_ptr: val.gen_ptr,
            },
            Type::Variant(elems) => LLVMValue {
                ty: elems[elem].1,
                value: self.ir.gep(
                    ty,
                    val.value,
                    &[self.llvm_ctx.int(0).val(), self.llvm_ctx.int(2).val()],
                ),
                gen_ptr: self.ir.gep(
                    ty,
                    val.value,
                    &[self.llvm_ctx.int(0).val(), self.llvm_ctx.int(0).val()],
                ),
            },
            Type::Ref(inner) => {
                assert!(elem == 0);

                self.ir.call(
                    self.builtins.rt_validate,
                    &[
                        self.ir.load(
                            self.ir.gep(
                                ty,
                                val.value,
                                &[self.llvm_ctx.int(0).val(), self.llvm_ctx.int(1).val()],
                            ),
                            self.llvm_ctx.ty_ptr(),
                        ),
                        self.ir.load(
                            self.ir.gep(
                                ty,
                                val.value,
                                &[self.llvm_ctx.int(0).val(), self.llvm_ctx.int(0).val()],
                            ),
                            self.llvm_ctx.ty_int(),
                        ),
                    ],
                );

                LLVMValue {
                    ty: inner,
                    value: self.ir.load(
                        self.ir.gep(
                            ty,
                            val.value,
                            &[self.llvm_ctx.int(0).val(), self.llvm_ctx.int(2).val()],
                        ),
                        self.llvm_ctx.ty_ptr(),
                    ),
                    gen_ptr: self.ir.gep(
                        ty,
                        val.value,
                        &[self.llvm_ctx.int(0).val(), self.llvm_ctx.int(1).val()],
                    ),
                }
            }
            Type::Ptr(inner) => {
                assert!(elem == 0);
                eprintln!("gep 2");
                LLVMValue {
                    ty: inner,
                    value: self.ir.load(val.value, self.llvm_ctx.ty_ptr()),
                    gen_ptr: self.llvm_ctx.ty_ptr().null().val(),
                }
            }
            Type::Named(_, _, proto) => LLVMValue {
                ty: proto,
                value: {
                    self.ir.gep(
                        ty,
                        val.value,
                        &[self.llvm_ctx.int(0).val(), self.llvm_ctx.int(0).val()],
                    )
                },
                gen_ptr: val.gen_ptr,
            },
            _ => panic!("invalid elem_ptr"),
        }
    }

    fn discriminant_ptr(&mut self, val: LLVMValue<'ll, 'i>) -> LLVMValue<'ll, 'i> {
        use iiv::ty::Type;
        match *val.ty {
            Type::Variant(_) => LLVMValue {
                ty: self.ctx.type_pool.get_int(),
                value: {
                    self.ir.gep(
                        self.llvm_ty(val.ty),
                        val.value,
                        &[self.llvm_ctx.int(0).val(), self.llvm_ctx.int(1).val()],
                    )
                },
                gen_ptr: val.gen_ptr,
            },
            _ => panic!("invalid discriminant_ptr"),
        }
    }

    fn make_ref(&mut self, val: LLVMValue<'ll, 'i>) -> LLVMStackValue<'ll, 'i> {
        let raw_ptr = val.value;
        let raw_gen_ptr_ptr = val.gen_ptr;
        self.ir.call(self.builtins.rt_gen_alloc, &[raw_gen_ptr_ptr]);
        let raw_gen_ptr = self.ir.load(raw_gen_ptr_ptr, self.llvm_ctx.ty_ptr());
        let raw_gen_value = self.ir.load(raw_gen_ptr, self.llvm_ctx.ty_int());
        let ref_ty = self.ctx.type_pool.get_ref(val.ty);
        let llvm_ref_ty = self.llvm_ty(ref_ty);
        let stack_val = self.alloc(ref_ty);
        let val = stack_val.value();

        let zero = self.llvm_ctx.int(0).val();
        let one = self.llvm_ctx.int(1).val();
        let two = self.llvm_ctx.int(2).val();

        let gen = self.ir.gep(llvm_ref_ty, val.value, &[zero, zero]);
        let gen_ptr = self.ir.gep(llvm_ref_ty, val.value, &[zero, one]);
        let val_ptr = self.ir.gep(llvm_ref_ty, val.value, &[zero, two]);

        self.ir.store(gen, raw_gen_value);
        self.ir.store(gen_ptr, raw_gen_ptr);
        self.ir.store(val_ptr, raw_ptr);

        stack_val
    }

    fn alloc(&mut self, ty: TypeRef<'i>) -> LLVMStackValue<'ll, 'i> {
        let val_ty = self.llvm_ty(ty);
        let gen_ptr_ty = self.llvm_ctx.ty_ptr();
        let storage_ty = self.llvm_ctx.struct_ty(&[gen_ptr_ty, val_ty]);
        let storage = self.ir.create_alloca(storage_ty);
        let zero = self.llvm_ctx.int(0).val();
        let one = self.llvm_ctx.int(1).val();
        let gen_ptr_ptr = self.ir.gep(storage_ty, storage, &[zero, zero]);
        let val_ptr = self.ir.gep(storage_ty, storage, &[zero, one]);
        self.ir
            .store(gen_ptr_ptr, self.llvm_ctx.ty_ptr().null().val());
        let value = LLVMStackValue {
            ty,
            value: val_ptr,
            gen_ptr: gen_ptr_ptr,
            alloca: storage,
        };
        self.values.push(value);
        value
    }

    fn create_signature(
        &mut self,
        func: &iiv::fun::Function<'i>,
        ty_args: List<'i, TypeRef<'i>>,
    ) -> llvm::Function<'ll> {
        let ret_ty = self.llvm_ty(
            self.ctx
                .type_pool
                .resolve_ty_args(func.sig.ret_ty, &ty_args),
        );
        let params: Vec<_> = func
            .sig
            .params
            .iter()
            .map(|&param| self.llvm_ty(self.ctx.type_pool.resolve_ty_args(param, &ty_args)))
            .collect();

        let name = format!(
            "{}{}",
            iiv::diagnostics::fmt::List(func.sig.name.iter(), "."),
            iiv::diagnostics::fmt::List(ty_args.iter(), ",")
        );

        let llvm_func = self.module.create_func(&name, &params, ret_ty);
        llvm_func
    }

    fn get_or_create_fn(
        &mut self,
        func: FuncRef<'i>,
        ty_args: List<'i, TypeRef<'i>>,
    ) -> llvm::Function<'ll> {
        if let Some(llvm_func) = self.funcs.get(&(func, ty_args)) {
            return *llvm_func;
        }
        let llvm_func = self.create_signature(&*func.borrow(), ty_args);
        self.funcs.insert((func, ty_args), llvm_func);

        self.work_stack.push((func, ty_args));
        llvm_func
    }

    fn val(&mut self, val: iiv::RawValue) -> LLVMValue<'ll, 'i> {
        self.values[val.0 as usize].value()
    }

    fn get(&mut self, val: iiv::RawValue) -> llvm::Value<'ll> {
        let var = self.val(val);
        self.load(var)
    }

    fn load(&mut self, val: LLVMValue<'ll, 'i>) -> llvm::Value<'ll> {
        self.ir.load(val.value, self.llvm_ty(val.ty))
    }

    fn write(&mut self, ptr: LLVMValue<'ll, 'i>, val: LLVMValue<'ll, 'i>) {
        let val = self.load(val);
        self.ir.store(ptr.value, val);
    }

    fn read(&mut self, val: LLVMValue<'ll, 'i>) {
        let raw_val = self.load(val);
        self.on_the_stack(raw_val, val.ty);
    }

    fn llvm_ty(&self, ty: TypeRef<'i>) -> llvm::Type<'ll> {
        use iiv::ty::BuiltinType;
        use iiv::ty::Type;

        let idx = self.ctx.type_pool.index_of(ty);

        {
            let cache = unsafe { &mut *self.llvm_ty_cache.get() };
            if idx >= cache.len() {
                cache.resize_with(idx + 1, || UnsafeCell::new(CachedType::NotComputed))
            }
            let cached_ty = unsafe { &mut *cache[idx].get() };
            match cached_ty {
                CachedType::Cached(ty) => return *ty,
                CachedType::NotComputed => {}
                CachedType::Computing => panic!("recursive type"),
            };
            *cached_ty = CachedType::Computing;
        }

        let llvm_ty = match *ty {
            Type::Builtin(BuiltinType::Int) => self.llvm_ctx.ty_int(),
            Type::Builtin(BuiltinType::Null) => self.llvm_ctx.create_named_ty("null", &[]),
            Type::Builtin(BuiltinType::Bool) => self.llvm_ctx.ty_bool(),
            Type::Ref(_) => {
                let int_ty = self.llvm_ctx.ty_int();
                let ptr_ty = self.llvm_ctx.ty_ptr();
                self.llvm_ctx.struct_ty(&[int_ty, ptr_ty, ptr_ty])
            }
            Type::Ptr(_) => self.llvm_ctx.ty_ptr(),
            Type::Struct(props) => {
                let prop_types: Vec<_> = props.iter().map(|prop| self.llvm_ty(prop.1)).collect();
                self.llvm_ctx.struct_ty(&prop_types)
            }
            Type::Tuple(fields) => {
                let field_types: Vec<_> = fields.iter().map(|field| self.llvm_ty(*field)).collect();
                self.llvm_ctx.struct_ty(&field_types)
            }
            Type::Named(decl, args, proto) => {
                let name = format!(
                    "{}{}",
                    decl.name,
                    iiv::diagnostics::fmt::List(args.iter(), ", ")
                );
                self.llvm_ctx.create_named_ty(&name, &[self.llvm_ty(proto)])
            }
            Type::Vector(_) => {
                unimplemented!()
            }
            Type::Union(_elems) => {
                unimplemented!()
            }
            Type::Variant(variants) => {
                let largest = variants
                    .iter()
                    .map(|elem| self.llvm_ty(elem.1))
                    .max_by_key(|&ty| self.module.alloc_size_of(ty));

                if let Some(largest) = largest {
                    let llvm_int = self.llvm_ctx.ty_int();
                    self.llvm_ctx.struct_ty(&[llvm_int, llvm_int, largest])
                } else {
                    self.llvm_ty(self.ctx.type_pool.get_null())
                }
            }
            Type::Invalid => panic!("size_of invalid type"),
            Type::Type(_) => panic!("invalid type - type"),
            Type::Constant(_) => panic!("invalid type - constant"),
            Type::InferenceVar(_) => panic!("invalid type - inf var"),
        };

        let cached_type = unsafe { &mut *(&*self.llvm_ty_cache.get())[idx].get() };
        *cached_type = CachedType::Cached(llvm_ty);
        llvm_ty
    }
}

#[derive(Clone, Copy)]
enum CachedType<'ll> {
    NotComputed,
    Computing,
    Cached(llvm::Type<'ll>),
}
