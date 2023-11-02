use std::{cell::UnsafeCell, collections::HashMap, fs::File, path::Path};

// use cranelift::prelude::InstBuilder;
use cranelift::{
    codegen::{
        ir::{self, InstBuilder},
        isa, settings,
    },
    prelude::{
        types, AbiParam, FunctionBuilder, FunctionBuilderContext, IntCC, MemFlags, StackSlotData,
        StackSlotKind,
    },
};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use iiv::{
    pool::FuncRef,
    ty::{BuiltinType, TypeRef},
    Ctx,
};

pub struct Backend<'i> {
    ty_pool: &'i iiv::ty::Pool<'i>,
    isa: isa::OwnedTargetIsa,
    fun_builder_ctx: FunctionBuilderContext,
    layouts: LayoutCache<'i>,
}

struct PackageTransformer<'i, 'b> {
    module: ObjectModule,
    ty_pool: &'i iiv::ty::Pool<'i>,
    fun_builder_ctx: &'b mut FunctionBuilderContext,
    layouts: &'b LayoutCache<'i>,
    funs: HashMap<FuncRef<'i>, FuncId>,
}

struct FunctionTransformer<'i, 'b, 'p, 'fb> {
    module: &'p mut ObjectModule,
    ty_pool: &'i iiv::ty::Pool<'i>,
    layouts: &'b LayoutCache<'i>,
    funs: &'p HashMap<FuncRef<'i>, FuncId>,
    fb: &'fb mut FunctionBuilder<'p>,
    values: Vec<ClftValue<'i>>,
    blocks: Vec<ir::Block>,
}

struct LayoutCache<'i> {
    storage: Vec<UnsafeCell<LayoutState>>,
    ty_pool: &'i iiv::ty::Pool<'i>,
}

#[derive(Clone, Copy)]
enum ClftValueRaw {
    Ssa(ir::Value),
    StackMemory(ir::StackSlot),
}

#[derive(Clone, Copy)]
struct ClftValue<'i> {
    value: ClftValueRaw,
    ty: TypeRef<'i>,
}

#[derive(Clone, Copy)]
struct Location<'i> {
    offset: u32,
    ptr: ClftValue<'i>,
}

impl<'i> From<ClftValue<'i>> for Location<'i> {
    fn from(value: ClftValue<'i>) -> Self {
        Location {
            offset: 0,
            ptr: value,
        }
    }
}

// impl<'i> ClftValue<'i> {
//     fn new(value: ir::Value, ty: TypeRef<'i>) -> Self {
//         ClftValue {
//             value: if ty.has_primitive_repr() {
//                 ClftValueRaw::Ssa(value)
//             } else {
//                 ClftValueRaw::Ptr(value)
//             },
//             ty,
//         }
//     }
// }

impl<'i> Backend<'i> {
    pub fn new(ctx: &'i Ctx<'i>) -> Self {
        let isa = isa::lookup(target_lexicon::HOST)
            .unwrap()
            .finish(settings::Flags::new(settings::builder()))
            .unwrap();

        let layouts = Vec::<UnsafeCell<LayoutState>>::new();

        Self {
            ty_pool: &ctx.type_pool,
            isa,
            fun_builder_ctx: FunctionBuilderContext::new(),
            layouts: LayoutCache {
                storage: layouts,
                ty_pool: &ctx.type_pool,
            },
        }
    }
    pub fn transform(&mut self, package: &iiv::Package<'i>, out: impl AsRef<Path>) {
        self.layouts.storage.resize_with(self.ty_pool.len(), || {
            UnsafeCell::new(LayoutState::NotComputed)
        });

        PackageTransformer {
            module: ObjectModule::new(
                ObjectBuilder::new(self.isa.clone(), "mod_i_guess", default_libcall_names())
                    .unwrap(),
            ),
            fun_builder_ctx: &mut self.fun_builder_ctx,
            ty_pool: self.ty_pool,
            funs: HashMap::new(),
            layouts: &mut self.layouts,
        }
        .transform(package, out);
    }
}

impl<'i, 'b> PackageTransformer<'i, 'b> {
    fn transform(mut self, package: &iiv::Package<'i>, out: impl AsRef<Path>) {
        // signatures
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
            self.funs.insert(*fun_ref, id);
        });

        // bodies
        package.funcs.iter().for_each(|fun_ref| {
            let fun = fun_ref.borrow();
            let id = self.funs[fun_ref];

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
            FunctionTransformer {
                funs: &self.funs,
                layouts: self.layouts,
                module: &mut self.module,
                ty_pool: self.ty_pool,
                fb: &mut builder,
                values: Vec::new(),
                blocks: vec![entry],
            }
            .gen(&fun.sig, &fun.body);
            builder.seal_all_blocks();
            builder.finalize();
            self.module.define_function(id, &mut ctx).unwrap();
            println!("{}", ctx.func);
            self.module.clear_context(&mut ctx);
        });

        let file = File::create(out).unwrap();
        self.module.finish().object.write_stream(file).unwrap();
    }
}

impl<'i, 'b, 't, 'fb> FunctionTransformer<'i, 'b, 't, 'fb> {
    fn get(&mut self, value: iiv::RawValue) -> ir::Value {
        match self.values[value.0 as usize].value {
            ClftValueRaw::Ssa(value) => value,
            ClftValueRaw::StackMemory(slot) => self.fb.ins().stack_addr(types::R64, slot, 0),
        }
    }

    fn read(&mut self, location: Location<'i>) -> ClftValue<'i> {
        let ty = location.ptr.ty;
        if ty.has_primitive_repr() {
            let value = match location.ptr.value {
                ClftValueRaw::Ssa(val) => val,
                ClftValueRaw::StackMemory(slot) => {
                    self.fb
                        .ins()
                        .stack_load(as_ty(ty), slot, location.offset as i32)
                }
            };
            ClftValue {
                ty,
                value: ClftValueRaw::Ssa(value),
            }
        } else {
            let copied = self.alloc(ty);
            self.write(copied.into(), location);
            copied
        }
    }

    fn write(&mut self, location: Location<'i>, data: Location<'i>) {
        match (location.ptr, data.ptr) {
            (
                ClftValue {
                    value: ClftValueRaw::Ssa(_),
                    ..
                },
                _,
            ) => {
                panic!("at the disco cannot write to ssa value");
            }
            (
                ClftValue {
                    value: ClftValueRaw::StackMemory(dst_slot),
                    ty,
                },
                ClftValue {
                    value: ClftValueRaw::StackMemory(src_slot),
                    ..
                },
            ) => {
                let l = self.layouts.layout_of(ty);
                let dst = self
                    .fb
                    .ins()
                    .stack_addr(types::R64, dst_slot, location.offset as i32);
                let src = self
                    .fb
                    .ins()
                    .stack_addr(types::R64, src_slot, data.offset as i32);
                self.fb.emit_small_memory_copy(
                    self.module.target_config(),
                    dst,
                    src,
                    l.size as u64,
                    1,
                    1,
                    true,
                    MemFlags::new(),
                );
            }
            (
                ClftValue {
                    value: ClftValueRaw::StackMemory(dst_slot),
                    ..
                },
                ClftValue {
                    value: ClftValueRaw::Ssa(value),
                    ..
                },
            ) => {
                self.fb
                    .ins()
                    .stack_store(value, dst_slot, location.offset as i32);
            }
        }
    }

    fn alloc(&mut self, ty: TypeRef<'i>) -> ClftValue<'i> {
        let layout = self.layouts.layout_of(ty);
        let slot = self
            .fb
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, layout.size));

        ClftValue {
            value: ClftValueRaw::StackMemory(slot),
            ty,
        }
    }

    fn elem(&mut self, location: Location<'i>, elem: usize) -> Location<'i> {
        let raw_value = location.ptr.value;
        let ty = location.ptr.ty;
        let prop_ty = ty.elem(elem as u8).expect("invalid element reference");
        let layout = self.layouts.layout_of(ty);

        Location {
            offset: location.offset + layout.fields[elem],
            ptr: ClftValue {
                value: raw_value,
                ty: prop_ty,
            },
        }
    }

    fn gen(&mut self, sig: &iiv::fun::Signature<'i>, blocks: &[iiv::builder::Block<'i>]) {
        for (param, &ty) in self
            .fb
            .block_params(self.fb.current_block().unwrap())
            .iter()
            .zip(sig.params.iter())
        {
            self.values.push(ClftValue {
                value: ClftValueRaw::Ssa(*param),
                ty,
            })
        }

        for block in &blocks[1..] {
            let new_block = self.fb.create_block();
            self.blocks.push(new_block);
            for param in &block.params {
                self.fb.append_block_param(new_block, as_ty(*param));
            }
        }

        for (i, block) in blocks.iter().enumerate() {
            self.fb.switch_to_block(self.blocks[i]);
            for param in self
                .fb
                .block_params(self.blocks[i])
                .iter()
                .zip(&block.params)
            {
                self.values.push(ClftValue {
                    value: ClftValueRaw::Ssa(*param.0),
                    ty: *param.1,
                });
            }
            for inst in &block.instructions {
                match inst {
                    iiv::Instruction::Int(val) => self.values.push(ClftValue {
                        value: ClftValueRaw::Ssa(self.fb.ins().iconst(types::I64, *val as i64)),
                        ty: self.ty_pool.get_int(),
                    }),
                    iiv::Instruction::Add(lhs, rhs) => {
                        let ty = self.values[lhs.0 as usize].ty;
                        let lhs = self.get(*lhs);
                        let rhs = self.get(*rhs);
                        self.values.push(ClftValue {
                            value: ClftValueRaw::Ssa(self.fb.ins().iadd(lhs, rhs)),
                            ty,
                        })
                    }
                    iiv::Instruction::Sub(lhs, rhs) => unimplemented!(),
                    iiv::Instruction::Mul(lhs, rhs) => unimplemented!(),
                    iiv::Instruction::Div(lhs, rhs) => unimplemented!(),
                    iiv::Instruction::Not(Value) => unimplemented!(),
                    iiv::Instruction::Neg(Value) => unimplemented!(),
                    iiv::Instruction::Eq(lhs, rhs) => {
                        let lhs = self.get(*lhs);
                        let rhs = self.get(*rhs);
                        self.values.push(ClftValue {
                            value: ClftValueRaw::Ssa(self.fb.ins().icmp(IntCC::Equal, lhs, rhs)),
                            ty: self.ty_pool.get_ty_bool(),
                        })
                    }
                    iiv::Instruction::Neq(lhs, rhs) => unimplemented!(),
                    iiv::Instruction::Gt(lhs, rhs) => unimplemented!(),
                    iiv::Instruction::Lt(lhs, rhs) => unimplemented!(),
                    iiv::Instruction::GtEq(lhs, rhs) => unimplemented!(),
                    iiv::Instruction::LtEq(lhs, rhs) => unimplemented!(),
                    iiv::Instruction::Call(fun, args) => {
                        let local_func = self
                            .module
                            .declare_func_in_func(self.funs[fun], &mut self.fb.func);
                        let args: Vec<_> = args.iter().map(|arg| self.get(*arg)).collect();
                        let call = self.fb.ins().call(local_func, &args);
                        self.values.push(ClftValue {
                            value: ClftValueRaw::Ssa(self.fb.inst_results(call)[0]),
                            ty: fun.borrow().sig.ret_ty,
                        })
                    }
                    iiv::Instruction::Assign(lhs, rhs) => unimplemented!(),
                    iiv::Instruction::RefAssign(lhs, rhs) => unimplemented!(),
                    iiv::Instruction::Tuple(tpl, ty) => {
                        let tuple_value = self.alloc(*ty);
                        self.values.push(tuple_value);

                        let tuple_location: Location = tuple_value.into();

                        for (i, val) in tpl.iter().enumerate() {
                            let field_location = self.elem(tuple_location, i);
                            self.write(field_location, self.values[val.0 as usize].into());
                        }
                    }
                    iiv::Instruction::Name(TypeId, Value) => unimplemented!(),
                    iiv::Instruction::GetElem(lhs, path) => {
                        let value = self.values[lhs.0 as usize];
                        let iiv::Elem::Prop(iiv::Prop(idx)) = path[0] else {
                            panic!("invalid get_elem");
                        };

                        let elem_loc = self.elem(value.into(), idx as usize);
                        let elem_value = self.read(elem_loc);
                        self.values.push(elem_value);
                    }
                    iiv::Instruction::GetElemRef(lhs, path) => unimplemented!(),
                    iiv::Instruction::Branch(lhs, yes, yes_args, no, no_args) => {
                        let yes_args: Vec<_> = yes_args.iter().map(|arg| self.get(*arg)).collect();
                        let no_args: Vec<_> = no_args.iter().map(|arg| self.get(*arg)).collect();
                        let cond = self.get(*lhs);
                        self.fb.ins().brif(
                            cond,
                            self.blocks[yes.0 as usize],
                            &yes_args,
                            self.blocks[no.0 as usize],
                            &no_args,
                        );
                    }
                    iiv::Instruction::Jump(label, args) => {
                        let args: Vec<_> = args.iter().map(|arg| self.get(*arg)).collect();
                        self.fb.ins().jump(self.blocks[label.0 as usize], &args);
                    }
                    iiv::Instruction::Return(val) => {
                        let val = self.get(*val);
                        self.fb.ins().return_(&[val]);
                    }
                    iiv::Instruction::Ty(TypeId) => unimplemented!(),
                };
            }
        }
    }
}

impl<'i> LayoutCache<'i> {
    fn layout_of(&self, ty: TypeRef<'i>) -> &Layout {
        const PTR_LAYOUT: Layout = Layout {
            align: 8,
            size: 8,
            fields: vec![],
        };
        use iiv::ty::Type;

        let idx = self.ty_pool.index_of(ty);

        {
            let layout_state = unsafe { &*self.storage[idx].get() };
            match layout_state {
                LayoutState::Cached(layout) => return layout,
                LayoutState::NotComputed => {}
                LayoutState::Computing => panic!("recursive type"),
            };
            let layout_state = unsafe { &mut *self.storage[idx].get() };
            *layout_state = LayoutState::Computing;
        }

        let layout = match *ty {
            Type::Builtin(BuiltinType::Int) => Layout {
                size: 8,
                align: 8,
                fields: vec![],
            },
            Type::Builtin(BuiltinType::Null) => Layout {
                size: 8,
                align: 8,
                fields: vec![],
            },
            Type::Builtin(BuiltinType::Bool) => Layout {
                size: 1,
                align: 1,
                fields: vec![],
            },
            Type::Constant(_) => PTR_LAYOUT,
            Type::Ref(_) => PTR_LAYOUT,
            Type::Struct(props) => props.iter().fold(Layout::empty(), |layout, prop| {
                layout.with_field(self.layout_of(prop.1))
            }),
            Type::Tuple(fields) => fields.iter().fold(Layout::empty(), |layout, field| {
                layout.with_field(self.layout_of(*field))
            }),
            Type::Vector(_) => Layout {
                size: 16,
                align: 8,
                fields: vec![0, 8, 12],
            },
            Type::Union(elems) => {
                // Layout { size: 8, align: 8 }
                //     + elems
                //         .iter()
                //         .map(|field| self.layout_of(*field))
                //         .reduce(ops::BitAnd::bitand)
                //         .unwrap()
                unimplemented!()
            }
            Type::Variant(variants) => {
                // Layout { size: 8, align: 8 }
                //     + variants
                //         .iter()
                //         .map(|field| self.layout_of(field.1))
                //         .reduce(ops::BitAnd::bitand)
                //         .unwrap()
                unimplemented!()
            }
            Type::Invalid => panic!("size_of invalid type"),
            Type::Type(_) => panic!("invalid type - type"),
        };

        let layout_state = unsafe { &mut *self.storage[idx].get() };
        *layout_state = LayoutState::Cached(layout);
        match layout_state {
            LayoutState::Cached(layout) => layout,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone)]
struct Layout {
    size: u32,
    align: u32,
    fields: Vec<u32>,
}

impl Layout {
    fn add_field(&mut self, field: &Layout) {
        self.align = self.align.max(field.align);
        let excess = self.size % field.align;
        let offset = if excess > 0 {
            self.size - excess + field.align
        } else {
            self.size
        };
        self.fields.push(offset);
        self.size = offset + field.size;
    }

    fn with_field(mut self, field: &Layout) -> Self {
        self.add_field(field);
        self
    }

    fn ovelay_field(&mut self, field: &Layout) {
        unimplemented!()
    }

    fn empty() -> Layout {
        Layout {
            size: 0,
            align: 1,
            fields: vec![],
        }
    }
}

#[derive(Clone)]
enum LayoutState {
    NotComputed,
    Computing,
    Cached(Layout),
}

fn as_ty<'i>(ty: TypeRef<'i>) -> ir::Type {
    use iiv::ty::Type;

    match *ty {
        Type::Builtin(BuiltinType::Int) => types::I64,
        _ => unimplemented!(),
    }
}
