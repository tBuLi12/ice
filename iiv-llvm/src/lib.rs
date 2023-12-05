use std::{cell::UnsafeCell, collections::HashMap};

use iiv::{pool::FuncRef, ty::TypeRef};

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

        let mut ir_gen = IRGen {
            module,
            ir: ctx.create_builder(),
            phis: vec![],
            ctx: self.ctx,
            funcs: HashMap::new(),
            values: vec![],
            work_stack: vec![],
            llvm_ctx: &ctx,
            blocks: vec![],
            llvm_ty_cache: vec![],
        };

        ir_gen
            .llvm_ty_cache
            .resize_with(self.ctx.type_pool.len(), || {
                UnsafeCell::new(CachedType::NotComputed)
            });

        ir_gen.emit_main(&*main.borrow());

        println!("dump mod");
        ctx.emit(ir_gen.module, out);
        println!("main done");
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
}

pub struct IRGen<'ll, 'i> {
    ir: llvm::IRBuilder<'ll>,
    module: llvm::LLVMModule<'ll>,
    phis: Vec<FuturePhi<'ll>>,
    values: Vec<LLVMValue<'ll, 'i>>,
    blocks: Vec<llvm::Block<'ll>>,
    funcs: HashMap<FuncRef<'i>, llvm::Function<'ll>>,
    work_stack: Vec<FuncRef<'i>>,
    ctx: &'i iiv::Ctx<'i>,
    llvm_ctx: &'ll llvm::LLVMCtx,
    llvm_ty_cache: Vec<UnsafeCell<CachedType<'ll>>>,
}

impl<'ll, 'i> IRGen<'ll, 'i> {
    pub fn emit_main(&mut self, func: &iiv::fun::Function<'i>) {
        let main = self.create_signature(func);
        self.write_body(func, main);

        while let Some(func) = self.work_stack.pop() {
            let llvm_func = self.funcs.get(&func).unwrap();
            dbg!(func.borrow().sig.name);
            self.write_body(&*func.borrow(), *llvm_func);
        }
    }

    pub fn write_body(&mut self, func: &iiv::fun::Function<'i>, llvm_func: llvm::Function<'ll>) {
        self.phis.clear();
        self.values.clear();
        self.blocks.clear();

        for _ in &func.body {
            let llvm_block = self.llvm_ctx.create_block();
            llvm_func.append(llvm_block);
            self.blocks.push(llvm_block);
            self.phis.push(FuturePhi::Future(vec![]));
        }

        self.ir.set_insert_point(self.blocks[0]);

        for (arg, &ty) in llvm_func.args().zip(func.sig.params.iter()) {
            let value = self.on_the_stack(arg);
            self.values.push(LLVMValue { ty, value });
        }

        for (i, block) in func.body.iter().enumerate() {
            let llvm_block = self.blocks[i];
            self.ir.set_insert_point(llvm_block);
            self.emit_block(block, i);
        }

        if llvm_func.verify() {
            panic!("invalid function");
        }
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
                let value = self.on_the_stack(phi.val());
                self.values.push(LLVMValue { ty, value })
            }
            self.phis[i] = FuturePhi::Existing(phis);
        }

        // rest
        for instruction in &block.instructions {
            match &instruction {
                iiv::Instruction::Int(val) => {
                    let int = self.get_int(*val as u64);
                    self.values.push(int)
                }
                iiv::Instruction::Bool(val) => {
                    let boolean = self.get_bool(*val);
                    self.values.push(boolean)
                }
                iiv::Instruction::Add(lhs, rhs) => {
                    let ty = self.val(*lhs).ty;
                    let lhs = self.get(*lhs);
                    let rhs = self.get(*rhs);
                    let value = self.on_the_stack(self.ir.add(lhs, rhs));
                    self.values.push(LLVMValue { value, ty })
                }
                iiv::Instruction::Sub(lhs, rhs) => unimplemented!(),
                iiv::Instruction::Mul(lhs, rhs) => unimplemented!(),
                iiv::Instruction::Div(lhs, rhs) => unimplemented!(),
                iiv::Instruction::Not(Value) => unimplemented!(),
                iiv::Instruction::Neg(Value) => unimplemented!(),
                iiv::Instruction::Eq(lhs, rhs) => {
                    let lhs = self.get(*lhs);
                    let rhs = self.get(*rhs);
                    let stack_value = self.on_the_stack(self.ir.eq(lhs, rhs));
                    self.values.push(LLVMValue {
                        value: stack_value,
                        ty: self.ctx.type_pool.get_ty_bool(),
                    })
                }
                iiv::Instruction::Neq(lhs, rhs) => unimplemented!(),
                iiv::Instruction::Gt(lhs, rhs) => unimplemented!(),
                iiv::Instruction::Lt(lhs, rhs) => unimplemented!(),
                iiv::Instruction::GtEq(lhs, rhs) => unimplemented!(),
                iiv::Instruction::LtEq(lhs, rhs) => unimplemented!(),
                iiv::Instruction::Call(fun, args) => {
                    let ret_ty = fun.borrow().sig.ret_ty;
                    let mut raw_args = Vec::new();

                    let func = self.get_or_create_fn(*fun);

                    raw_args.extend(args.iter().map(|arg| self.get(*arg)));

                    let call = self.ir.call(func, &raw_args);

                    let value = self.on_the_stack(call);
                    self.values.push(LLVMValue { value, ty: ret_ty });
                }
                iiv::Instruction::Assign(lhs, rhs) => {
                    let lhs = self.val(*lhs);
                    let rhs = self.val(*rhs);
                    self.write(lhs, rhs);
                }
                iiv::Instruction::RefAssign(lhs, rhs) => unimplemented!(),
                iiv::Instruction::Tuple(tpl, ty) => {
                    let tuple_value = self.alloc(*ty);
                    self.values.push(tuple_value);

                    for (i, val) in tpl.iter().enumerate() {
                        let field = self.elem_ptr(tuple_value, i);
                        let val = self.val(*val);
                        self.write(field, val);
                    }
                }
                iiv::Instruction::Name(TypeId, Value) => unimplemented!(),
                iiv::Instruction::GetElem(lhs, path) => {
                    let mut value = self.val(*lhs);

                    for elem in path {
                        let iiv::Elem::Prop(iiv::Prop(idx)) = elem else {
                            panic!("invalid get_elem");
                        };
                        value = self.elem_ptr(value, *idx as usize);
                    }

                    let elem_value = self.read(value);
                    self.values.push(elem_value);
                }
                iiv::Instruction::GetElemRef(lhs, path) => {
                    let mut value = self.val(*lhs);

                    for elem in path {
                        let iiv::Elem::Prop(iiv::Prop(idx)) = elem else {
                            panic!("invalid get_elem");
                        };
                        value = self.elem_ptr(value, *idx as usize);
                    }

                    let elem_ptr = self.make_ref(value);
                    self.values.push(elem_ptr);
                }
                iiv::Instruction::Branch(lhs, yes, yes_args, no, no_args) => {
                    let yes_args: Vec<_> = yes_args.iter().map(|arg| self.get(*arg)).collect();
                    let no_args: Vec<_> = no_args.iter().map(|arg| self.get(*arg)).collect();
                    let cond = self.get(*lhs);

                    self.apply_block_args(yes, &yes_args);
                    self.apply_block_args(yes, &no_args);

                    self.ir.condbr(
                        cond,
                        self.blocks[yes.0 as usize],
                        self.blocks[no.0 as usize],
                    );
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
                iiv::Instruction::Ty(TypeId) => unimplemented!(),
                iiv::Instruction::Variant(ty, idx, val) => {
                    let variant = self.alloc(*ty);
                    let idx_val = self.get_int(*idx);
                    let discriminant = self.discriminant_ptr(variant);
                    self.write(discriminant, idx_val);
                    let body = self.elem_ptr(variant, *idx as usize);
                    let val = self.val(*val);
                    self.write(body, val);
                    self.values.push(variant);
                }
                iiv::Instruction::VariantCast(ty, val) => {
                    let target = self.alloc(*ty);
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
                                                as u64
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
                    self.values.push(target);
                }
                iiv::Instruction::Discriminant(value) => {
                    let value = self.val(*value);
                    let discriminant_loc = self.discriminant_ptr(value);
                    let discriminant = self.read(discriminant_loc);
                    self.values.push(discriminant);
                }
            }
        }
    }

    fn apply_block_args(&mut self, block: &iiv::Label, args: &[llvm::Value<'ll>]) {
        let future_phi = &mut self.phis[block.0 as usize];
        let current_block = self.ir.current_block();

        let phis = match future_phi {
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

    fn on_the_stack(&mut self, val: llvm::Value<'ll>) -> llvm::Value<'ll> {
        let alloca = self.ir.create_alloca(val.ty());
        self.ir.store(alloca, val);
        alloca
    }

    fn elem_ptr(&mut self, val: LLVMValue<'ll, 'i>, elem: usize) -> LLVMValue<'ll, 'i> {
        use iiv::ty::Type;

        let ty = self.llvm_ty(val.ty);

        match *val.ty {
            Type::Struct(props) => LLVMValue {
                ty: props[elem].1,
                value: self.ir.gep(
                    ty,
                    val.value,
                    &[
                        self.llvm_ctx.int(0).val(),
                        self.llvm_ctx.int(elem as u64).val(),
                    ],
                ),
            },
            Type::Variant(elems) => LLVMValue {
                ty: elems[elem].1,
                value: self.ir.gep(
                    ty,
                    val.value,
                    &[
                        self.llvm_ctx.int(0).val(),
                        self.llvm_ctx.int(1).val(),
                        self.llvm_ctx.int(elem as u64).val(),
                    ],
                ),
            },
            _ => panic!("invalid elem_ptr"),
        }
    }

    fn discriminant_ptr(&mut self, val: LLVMValue<'ll, 'i>) -> LLVMValue<'ll, 'i> {
        use iiv::ty::Type;
        match *val.ty {
            Type::Variant(elems) => LLVMValue {
                ty: self.ctx.type_pool.get_int(),
                value: self.ir.gep(
                    self.llvm_ty(self.ctx.type_pool.get_int()),
                    val.value,
                    &[self.llvm_ctx.int(0).val(), self.llvm_ctx.int(0).val()],
                ),
            },
            _ => panic!("invalid elem_ptr"),
        }
    }

    fn make_ref(&mut self, val: LLVMValue<'ll, 'i>) -> LLVMValue<'ll, 'i> {
        let ptr = self.on_the_stack(val.value);
        LLVMValue {
            ty: self.ctx.type_pool.get_ref(val.ty),
            value: ptr,
        }
    }

    fn alloc(&mut self, ty: TypeRef<'i>) -> LLVMValue<'ll, 'i> {
        let storage = self.ir.create_alloca(self.llvm_ty(ty));
        LLVMValue { ty, value: storage }
    }

    fn create_signature(&mut self, func: &iiv::fun::Function<'i>) -> llvm::Function<'ll> {
        let ret_ty = self.llvm_ty(func.sig.ret_ty);
        let params: Vec<_> = func
            .sig
            .params
            .iter()
            .map(|&param| self.llvm_ty(param))
            .collect();

        let llvm_func = self.module.create_func(&func.sig.name, &params, ret_ty);
        llvm_func
    }

    fn get_or_create_fn(&mut self, func: FuncRef<'i>) -> llvm::Function<'ll> {
        if let Some(llvm_func) = self.funcs.get(&func) {
            return *llvm_func;
        }
        let llvm_func = self.create_signature(&*func.borrow());
        self.funcs.insert(func, llvm_func);
        self.work_stack.push(func);
        llvm_func
    }

    fn val(&mut self, val: iiv::RawValue) -> LLVMValue<'ll, 'i> {
        self.values[val.0 as usize]
    }

    fn get(&mut self, val: iiv::RawValue) -> llvm::Value<'ll> {
        let var = self.val(val);
        self.load(var)
    }

    fn get_int(&mut self, val: u64) -> LLVMValue<'ll, 'i> {
        LLVMValue {
            value: self.on_the_stack(self.llvm_ctx.int(val).val()),
            ty: self.ctx.type_pool.get_int(),
        }
    }

    fn get_bool(&mut self, val: bool) -> LLVMValue<'ll, 'i> {
        LLVMValue {
            value: self.on_the_stack(self.llvm_ctx.boolean(val).val()),
            ty: self.ctx.type_pool.get_int(),
        }
    }

    fn load(&mut self, val: LLVMValue<'ll, 'i>) -> llvm::Value<'ll> {
        self.ir.load(val.value, self.llvm_ty(val.ty))
    }

    fn write(&mut self, ptr: LLVMValue<'ll, 'i>, val: LLVMValue<'ll, 'i>) {
        let val = self.load(val);
        self.ir.store(ptr.value, val);
    }

    fn read(&mut self, val: LLVMValue<'ll, 'i>) -> LLVMValue<'ll, 'i> {
        let raw_val = self.load(val);
        LLVMValue {
            ty: val.ty,
            value: self.on_the_stack(raw_val),
        }
    }

    fn llvm_ty(&self, ty: TypeRef<'i>) -> llvm::Type<'ll> {
        use iiv::ty::BuiltinType;
        use iiv::ty::Type;

        let idx = self.ctx.type_pool.index_of(ty);

        {
            let cached_ty = unsafe { &*self.llvm_ty_cache[idx].get() };
            match cached_ty {
                CachedType::Cached(ty) => return *ty,
                CachedType::NotComputed => {}
                CachedType::Computing => panic!("recursive type"),
            };
            let cached_ty = unsafe { &mut *self.llvm_ty_cache[idx].get() };
            *cached_ty = CachedType::Computing;
        }

        let llvm_ty = match *ty {
            Type::Builtin(BuiltinType::Int) => self.llvm_ctx.ty_int(),
            Type::Builtin(BuiltinType::Null) => self.llvm_ctx.create_named_ty("null", &[]),
            Type::Builtin(BuiltinType::Bool) => self.llvm_ctx.ty_bool(),
            Type::Ref(_) => self.llvm_ctx.ty_ptr(),
            Type::Struct(props) => {
                let prop_types: Vec<_> = props.iter().map(|prop| self.llvm_ty(prop.1)).collect();
                self.llvm_ctx.struct_ty(&prop_types)
            }
            Type::Tuple(fields) => {
                let field_types: Vec<_> = fields.iter().map(|field| self.llvm_ty(*field)).collect();
                self.llvm_ctx.struct_ty(&field_types)
            }
            Type::Vector(_) => {
                unimplemented!()
            }
            Type::Union(elems) => {
                unimplemented!()
            }
            Type::Variant(variants) => {
                let largest = variants
                    .iter()
                    .map(|elem| self.llvm_ty(elem.1))
                    .max_by_key(|&ty| self.module.alloc_size_of(ty));

                if let Some(largest) = largest {
                    let llvm_int = self.llvm_ctx.ty_int();
                    self.llvm_ctx.struct_ty(&[llvm_int, largest])
                } else {
                    self.llvm_ty(self.ctx.type_pool.get_null())
                }
            }
            Type::Invalid => panic!("size_of invalid type"),
            Type::Type(_) => panic!("invalid type - type"),
            Type::Constant(_) => panic!("invalid type - constant"),
        };

        let cached_type = unsafe { &mut *self.llvm_ty_cache[idx].get() };
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
