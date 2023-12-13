use std::{cell::UnsafeCell, collections::HashMap, fs::File};

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
use cranelift_module::{default_libcall_names, DataDescription, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use iiv::{
    pool::FuncRef,
    ty::{BuiltinType, TypeRef},
    Ctx, RawValue,
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

#[derive(Clone, Copy, Debug)]
enum ClftValueRaw {
    Ssa(ir::Value),
    StackMemory(ir::StackSlot),
}

#[derive(Clone, Copy, Debug)]
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
    pub fn transform(&mut self, package: &iiv::Package<'i>, out: &File) {
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
    fn transform(mut self, package: &iiv::Package<'i>, out: &File) {
        // signatures
        package.funcs.iter().for_each(|fun_ref| {
            let fun = fun_ref.borrow();

            let id = {
                let mut signature = self.module.make_signature();
                let return_via_pointer = !fun.sig.ret_ty.has_primitive_repr();

                if return_via_pointer {
                    signature
                        .params
                        .push(AbiParam::new(self.module.target_config().pointer_type()));
                } else {
                    signature.returns.push(AbiParam::new(as_ty(fun.sig.ret_ty)));
                }
                for &param in fun.sig.params.iter() {
                    signature.params.push(AbiParam::new(as_ty(param)));
                }
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

            let return_via_pointer = !fun.sig.ret_ty.has_primitive_repr();

            if return_via_pointer {
                ctx.func
                    .signature
                    .params
                    .push(AbiParam::new(self.module.target_config().pointer_type()));
            } else {
                ctx.func
                    .signature
                    .returns
                    .push(AbiParam::new(as_ty(fun.sig.ret_ty)));
            }
            for &param in fun.sig.params.iter() {
                ctx.func.signature.params.push(AbiParam::new(as_ty(param)));
            }

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

        self.module.finish().object.write_stream(out).unwrap();
    }
}

impl<'i, 'b, 't, 'fb> FunctionTransformer<'i, 'b, 't, 'fb> {
    fn get(&mut self, value: iiv::RawValue) -> ir::Value {
        let val = self.val(value);
        match val.value {
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
            let copied = self.alloc(ty).0;
            self.write(copied.into(), location);
            copied
        }
    }

    fn address_of(&mut self, location: Location<'i>) -> ClftValue<'i> {
        let ty = location.ptr.ty;
        let value = match location.ptr.value {
            ClftValueRaw::Ssa(_) => panic!("address of an SSA value"),
            ClftValueRaw::StackMemory(slot) => {
                self.fb
                    .ins()
                    .stack_addr(as_ty(ty), slot, location.offset as i32)
            }
        };
        ClftValue {
            ty: self.ty_pool.get_ref(location.ptr.ty),
            value: ClftValueRaw::Ssa(value),
        }
    }

    fn ptr_write(&mut self, dst: ir::Value, data: Location<'i>) {
        match data.ptr {
            ClftValue {
                value: ClftValueRaw::Ssa(val),
                ..
            } => {
                self.fb.ins().store(MemFlags::new(), val, dst, 0);
            }
            ClftValue {
                value: ClftValueRaw::StackMemory(slot),
                ty,
            } => {
                let l = self.layouts.layout_of(ty);
                let src = self
                    .fb
                    .ins()
                    .stack_addr(types::R64, slot, data.offset as i32);
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
                    ..
                },
                ClftValue {
                    value: ClftValueRaw::StackMemory(src_slot),
                    ty,
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

    fn alloc(&mut self, ty: TypeRef<'i>) -> (ClftValue<'i>, ir::StackSlot) {
        let layout = self.layouts.layout_of(ty);
        let slot = self
            .fb
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, layout.size));

        (
            ClftValue {
                value: ClftValueRaw::StackMemory(slot),
                ty,
            },
            slot,
        )
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

    fn raw_union(&mut self, mut location: Location<'i>) -> Location<'i> {
        location.offset += 8;
        let iiv::ty::Type::Variant(props) = *location.ptr.ty else {
            panic!("invalid raw_union")
        };

        let (biggest_elem, _) = props
            .iter()
            .map(|prop| (prop.1, self.layouts.layout_of(prop.1).size))
            .max_by_key(|(_, size)| *size)
            .unwrap();

        location.ptr.ty = biggest_elem;
        location
    }

    fn discriminant(&mut self, mut location: Location<'i>) -> Location<'i> {
        location.ptr.ty = self.ty_pool.get_int();
        location
    }

    fn get_int(&mut self, value: u64) -> ClftValue<'i> {
        ClftValue {
            value: ClftValueRaw::Ssa(self.fb.ins().iconst(types::I64, value as i64)),
            ty: self.ty_pool.get_int(),
        }
    }

    fn get_bool(&mut self, value: bool) -> ClftValue<'i> {
        ClftValue {
            value: ClftValueRaw::Ssa(self.fb.ins().iconst(types::I8, if value { 1 } else { 0 })),
            ty: self.ty_pool.get_ty_bool(),
        }
    }

    fn val(&mut self, val: RawValue) -> ClftValue<'i> {
        if val.0 == RawValue::NULL.0 {
            return ClftValue {
                ty: self.ty_pool.get_null(),
                value: ClftValueRaw::Ssa(self.fb.ins().iconst(types::I8, 0 as i64)),
            };
        }

        self.values[val.0 as usize]
    }

    fn gen(&mut self, sig: &iiv::fun::Signature<'i>, blocks: &[iiv::builder::Block<'i>]) {
        let mut params = self
            .fb
            .block_params(self.fb.current_block().unwrap())
            .iter();

        let return_via_pointer = !sig.ret_ty.has_primitive_repr();
        let return_pointer = if return_via_pointer {
            Some(*params.next().unwrap())
        } else {
            None
        };

        for (param, &ty) in params.zip(sig.params.iter()) {
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

        let mut loc = 1;

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
                loc += 1;
                self.fb.set_srcloc(ir::SourceLoc::new(loc));
                match inst {
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
                        self.values.push(ClftValue {
                            value: ClftValueRaw::Ssa(self.fb.ins().iadd(lhs, rhs)),
                            ty,
                        })
                    }
                    iiv::Instruction::Sub(_lhs, _rhs) => unimplemented!(),
                    iiv::Instruction::Mul(_lhs, _rhs) => unimplemented!(),
                    iiv::Instruction::Div(_lhs, _rhs) => unimplemented!(),
                    iiv::Instruction::Not(_value) => unimplemented!(),
                    iiv::Instruction::Neg(_value) => unimplemented!(),
                    iiv::Instruction::Eq(lhs, rhs) => {
                        let lhs = self.get(*lhs);
                        let rhs = self.get(*rhs);
                        self.values.push(ClftValue {
                            value: ClftValueRaw::Ssa(self.fb.ins().icmp(IntCC::Equal, lhs, rhs)),
                            ty: self.ty_pool.get_ty_bool(),
                        })
                    }
                    iiv::Instruction::Neq(_lhs, _rhs) => unimplemented!(),
                    iiv::Instruction::Gt(_lhs, _rhs) => unimplemented!(),
                    iiv::Instruction::Lt(_lhs, _rhs) => unimplemented!(),
                    iiv::Instruction::GtEq(_lhs, _rhs) => unimplemented!(),
                    iiv::Instruction::LtEq(_lhs, _rhs) => unimplemented!(),
                    iiv::Instruction::Call(fun, args) => {
                        let ret_ty = fun.borrow().sig.ret_ty;
                        let return_via_pointer = !ret_ty.has_primitive_repr();
                        let mut raw_args = Vec::new();
                        let ret_ptr = if return_via_pointer {
                            let (ret, ret_slot) = self.alloc(ret_ty);
                            let ret_ptr = self.fb.ins().stack_addr(
                                self.module.target_config().pointer_type(),
                                ret_slot,
                                0,
                            );
                            raw_args.push(ret_ptr);
                            Some(ret)
                        } else {
                            None
                        };

                        let local_func = self
                            .module
                            .declare_func_in_func(self.funs[fun], &mut self.fb.func);
                        raw_args.extend(args.iter().map(|arg| self.get(*arg)));
                        let call = self.fb.ins().call(local_func, &raw_args);
                        self.values.push(if let Some(val) = ret_ptr {
                            val
                        } else {
                            ClftValue {
                                value: ClftValueRaw::Ssa(self.fb.inst_results(call)[0]),
                                ty: ret_ty,
                            }
                        })
                    }
                    iiv::Instruction::Assign(lhs, rhs) => {
                        let ClftValue {
                            value: ClftValueRaw::Ssa(ptr),
                            ..
                        } = self.val(*lhs)
                        else {
                            panic!("non pointer assignment")
                        };
                        let rhs = self.val(*rhs);
                        self.ptr_write(ptr, rhs.into());
                    }
                    iiv::Instruction::RefAssign(_lhs, _rhs) => unimplemented!(),
                    iiv::Instruction::Tuple(tpl, ty) => {
                        let tuple_value = self.alloc(*ty).0;
                        self.values.push(tuple_value);

                        let tuple_location: Location = tuple_value.into();

                        for (i, val) in tpl.iter().enumerate() {
                            let field_location = self.elem(tuple_location, i);
                            let elem_loc = self.val(*val).into();
                            self.write(field_location, elem_loc);
                        }
                    }
                    iiv::Instruction::Name(_type_id, _value) => unimplemented!(),
                    iiv::Instruction::GetElem(lhs, path) => {
                        let value = self.val(*lhs);
                        let mut elem_loc = value.into();

                        for elem in path {
                            let iiv::Elem::Prop(iiv::Prop(idx)) = elem else {
                                panic!("invalid get_elem");
                            };
                            elem_loc = self.elem(elem_loc, *idx as usize);
                        }

                        let elem_value = self.read(elem_loc);
                        self.values.push(elem_value);
                    }
                    iiv::Instruction::GetElemRef(lhs, path) => {
                        let value = self.val(*lhs);
                        let mut elem_loc = value.into();

                        for elem in path {
                            let iiv::Elem::Prop(iiv::Prop(idx)) = elem else {
                                panic!("invalid get_elem");
                            };
                            dbg!("REFF", lhs.0);
                            elem_loc = self.elem(elem_loc, *idx as usize);
                        }

                        let elem_ptr = self.address_of(elem_loc);
                        self.values.push(elem_ptr);
                    }
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
                        if let Some(ret_ptr) = return_pointer {
                            let ClftValue {
                                value: ClftValueRaw::StackMemory(slot),
                                ..
                            } = self.val(*val)
                            else {
                                panic!("invalid return type")
                            };
                            let addr = self.fb.ins().stack_addr(
                                self.module.target_config().pointer_type(),
                                slot,
                                0,
                            );
                            let layout = self.layouts.layout_of(sig.ret_ty);
                            self.fb.emit_small_memory_copy(
                                self.module.target_config(),
                                ret_ptr,
                                addr,
                                layout.size as u64,
                                1,
                                1,
                                true,
                                MemFlags::new(),
                            );
                            self.fb.ins().return_(&[]);
                        } else {
                            let val = self.get(*val);
                            self.fb.ins().return_(&[val]);
                        }
                    }
                    iiv::Instruction::Ty(_type_id) => unimplemented!(),
                    iiv::Instruction::Variant(ty, idx, val) => {
                        let variant = self.alloc(*ty).0;
                        let idx_val = self.get_int(*idx);
                        let discriminant = self.discriminant(variant.into());
                        self.write(discriminant, idx_val.into());
                        let value_loc = self.elem(variant.into(), *idx as usize);
                        let src_loc = self.val(*val).into();
                        self.write(value_loc, src_loc);
                        self.values.push(variant);
                    }
                    iiv::Instruction::VariantCast(ty, val) => {
                        let target = self.alloc(*ty).0;
                        let discriminant = self.discriminant(target.into());
                        let src_variant = self.val(*val);
                        let src_discriminant = self.discriminant(src_variant.into());
                        let src_value = self.raw_union(src_variant.into());
                        let value = self.raw_union(target.into());
                        self.fb.set_srcloc(ir::SourceLoc::new(loc + 1));
                        dbg!("VAR CAST");
                        self.write(value, src_value.into());
                        self.fb.set_srcloc(ir::SourceLoc::new(loc));
                        let cast_table = self
                            .module
                            .declare_data("cast_table", Linkage::Local, false, false)
                            .unwrap();
                        let indices: Vec<_> = match (&**ty, &*src_variant.ty) {
                            (
                                iiv::ty::Type::Variant(dst_elems),
                                iiv::ty::Type::Variant(src_elems),
                            ) => src_elems
                                .iter()
                                .map(|elem| {
                                    dst_elems.iter().position(|e| *e == *elem).unwrap() as u64
                                })
                                .collect(),
                            _ => panic!("variant cast has non-variant operands"),
                        };
                        let mut data_decs = DataDescription::new();
                        data_decs.define(bytes_of(&indices).into());
                        self.module.define_data(cast_table, &data_decs).unwrap();
                        let global = self
                            .module
                            .declare_data_in_func(cast_table, &mut self.fb.func);
                        let item_val = self
                            .fb
                            .ins()
                            .symbol_value(self.module.target_config().pointer_type(), global);

                        let ClftValue {
                            value: ClftValueRaw::Ssa(src_discriminant_val),
                            ..
                        } = self.read(src_discriminant)
                        else {
                            panic!("invalid discriminant type")
                        };
                        let addr = self.fb.ins().iadd(item_val, src_discriminant_val);
                        let new_discriminant =
                            self.fb.ins().load(types::I64, MemFlags::new(), addr, 0);
                        self.write(
                            discriminant,
                            ClftValue {
                                ty: self.ty_pool.get_int(),
                                value: ClftValueRaw::Ssa(new_discriminant),
                            }
                            .into(),
                        );

                        self.values.push(target);
                    }
                    iiv::Instruction::Discriminant(value) => {
                        let value = self.val(*value);
                        let discriminant_loc = self.discriminant(value.into());
                        let discriminant = self.read(discriminant_loc);
                        self.values.push(discriminant);
                    }
                    iiv::Instruction::Drop(_) => {
                        unimplemented!()
                    }
                };
            }
        }
    }
}

fn bytes_of<T>(slice: &[T]) -> &[u8] {
    unsafe {
        std::slice::from_raw_parts(
            slice.as_ptr() as *const u8,
            slice.len() * std::mem::size_of_val(slice),
        )
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
            Type::Union(_) => {
                // Layout { size: 8, align: 8 }
                //     + elems
                //         .iter()
                //         .map(|field| self.layout_of(*field))
                //         .reduce(ops::BitAnd::bitand)
                //         .unwrap()
                unimplemented!()
            }
            Type::Variant(variants) => variants.iter().fold(
                Layout {
                    size: 8,
                    align: 8,
                    fields: vec![],
                },
                |layout, elem| layout.with_variant(self.layout_of(elem.1)),
            ),
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

#[derive(Clone, Debug)]
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

    fn ovelay_variant(&mut self, variant: &Layout) {
        self.fields.push(8);
        self.size = self.size.max(variant.size + 8);
    }

    fn with_variant(mut self, variant: &Layout) -> Self {
        self.ovelay_variant(variant);
        self
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
    dbg!(ty);
    match *ty {
        Type::Builtin(BuiltinType::Int) => types::I64,
        Type::Builtin(BuiltinType::Null) => types::I8,
        Type::Struct(_) => types::I64,
        Type::Variant(_) => types::I64,
        Type::Ref(_) => types::I64,
        _ => unimplemented!(),
    }
}
