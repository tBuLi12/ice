pub mod inst;

use std::{marker::PhantomData, ptr};

#[repr(C)]
pub struct Opaque {
    _private: [u8; 0],
}

#[repr(C)]
struct LLVMCtxData(Opaque);

#[repr(C)]
struct TargetMachineData(Opaque);

#[repr(C)]
struct LLVMModuleData(Opaque);

#[repr(C)]
struct IRBuilderData(Opaque);

#[repr(C)]
struct FunctionOptManagerData(Opaque);

#[repr(C)]
struct ValueData(Opaque);

#[repr(C)]
struct ConstantData(Opaque);

#[repr(C)]
struct GlobalVariableData(Opaque);

#[repr(C)]
struct TypeData(Opaque);

#[repr(C)]
struct BlockData(Opaque);
#[repr(C)]
struct FunctionData(Opaque);

extern "C" {
    fn getCtx() -> *mut LLVMCtxData;
    fn destroyCtx(ctx: *mut LLVMCtxData);
    fn getMod(ctx: *mut LLVMCtxData, machine: *mut TargetMachineData) -> *mut LLVMModuleData;
    fn destroyMod(module: *mut LLVMModuleData);
    fn getBuilder(ctx: *mut LLVMCtxData) -> *mut IRBuilderData;
    fn destroyBuilder(module: *mut IRBuilderData);
    fn getTargetMachine(ctx: *mut LLVMCtxData, out_machine: *mut *mut TargetMachineData) -> i32;
    fn emitModule(
        module: *mut LLVMModuleData,
        machine: *mut TargetMachineData,
        name: *const u8,
        name_len: u32,
    ) -> i32;

    fn ctxGetInt(ctx: *mut LLVMCtxData, val: u32) -> *mut ConstantData;
    fn ctxGetBool(ctx: *mut LLVMCtxData, val: bool) -> *mut ConstantData;
    fn ctxGetTyInt(ctx: *mut LLVMCtxData) -> *mut TypeData;
    fn ctxGetTyBool(ctx: *mut LLVMCtxData) -> *mut TypeData;
    fn ctxGetTyPtr(ctx: *mut LLVMCtxData) -> *mut TypeData;
    fn ctxGetTyVoid(ctx: *mut LLVMCtxData) -> *mut TypeData;
    fn ctxCreateStructTy(
        ctx: *mut LLVMCtxData,
        name: *const u8,
        name_len: u32,
        types: *const *mut TypeData,
        types_len: u32,
    ) -> *mut TypeData;
    fn ctxGetStructTy(
        ctx: *mut LLVMCtxData,
        types: *const *mut TypeData,
        types_len: u32,
    ) -> *mut TypeData;
    fn ctxGetArrayTy(ctx: *mut LLVMCtxData, elem_ty: *mut TypeData, size: u64) -> *mut TypeData;
    fn ctxArrayConstant(
        ctx: *mut LLVMCtxData,
        elem_ty: *mut TypeData,
        vals: *const *mut ConstantData,
        vals_len: u32,
    ) -> *mut ConstantData;
    fn ctxCreateBlock(ctx: *mut LLVMCtxData) -> *mut BlockData;

    fn valueType(val: *mut ValueData) -> *mut TypeData;

    fn modCreateFunc(
        module: *mut LLVMModuleData,
        name: *const u8,
        name_len: u32,
        types: *const *mut TypeData,
        types_len: u32,
        ret_ty: *mut TypeData,
    ) -> *mut FunctionData;
    fn modAllocSizeOf(module: *mut LLVMModuleData, ty: *mut TypeData) -> u64;
    fn modGetOrCreateGlobal(
        module: *mut LLVMModuleData,
        name: *const u8,
        name_len: u32,
        ty: *mut TypeData,
    ) -> *mut GlobalVariableData;
    fn globalHasInitializer(global: *mut GlobalVariableData) -> bool;
    fn globalSetInitializer(global: *mut GlobalVariableData, init: *mut ConstantData);

    fn builderPhi(builder: *mut IRBuilderData, ty: *mut TypeData) -> *mut inst::PhiData;
    fn builderAdd(
        builder: *mut IRBuilderData,
        lhs: *mut ValueData,
        rhs: *mut ValueData,
    ) -> *mut ValueData;
    fn builderEq(
        builder: *mut IRBuilderData,
        lhs: *mut ValueData,
        rhs: *mut ValueData,
    ) -> *mut ValueData;
    fn builderCall(
        builder: *mut IRBuilderData,
        func: *mut FunctionData,
        args: *const *mut ValueData,
        args_len: u32,
    ) -> *mut ValueData;
    fn builderCondBr(
        builder: *mut IRBuilderData,
        cond: *mut ValueData,
        yes: *mut BlockData,
        no: *mut BlockData,
    );
    fn builderLoad(
        builder: *mut IRBuilderData,
        ptr: *mut ValueData,
        ty: *mut TypeData,
    ) -> *mut ValueData;
    fn builderStore(builder: *mut IRBuilderData, ptr: *mut ValueData, ty: *mut ValueData);
    fn builderBr(builder: *mut IRBuilderData, target: *mut BlockData);
    fn builderRet(builder: *mut IRBuilderData, val: *mut ValueData);
    fn builderGep(
        builder: *mut IRBuilderData,
        ty: *mut TypeData,
        base: *mut ValueData,
        offsets: *const *mut ValueData,
        offsets_len: u32,
    ) -> *mut ValueData;
    fn builderCurrentBlock(builder: *mut IRBuilderData) -> *mut BlockData;
    fn builderCreateAlloca(builder: *mut IRBuilderData, ty: *mut TypeData) -> *mut ValueData;
    fn builderSetInsertPoint(builder: *mut IRBuilderData, block: *mut BlockData);

    fn functionAppendBlock(func: *mut FunctionData, block: *mut BlockData);
    fn functionVerify(func: *mut FunctionData) -> bool;
    fn functionArgSize(func: *mut FunctionData) -> u64;
    fn functionArgAt(func: *mut FunctionData, idx: u32) -> *mut ValueData;

    fn getFunctionOptManager(ctx: *mut TargetMachineData) -> *mut FunctionOptManagerData;
    fn destroyFunctionOptManager(funOptManager: *mut FunctionOptManagerData);
    fn functionOptManagerOptimize(
        funOptManager: *mut FunctionOptManagerData,
        func: *mut FunctionData,
    );

    fn tyNullVal(ty: *mut TypeData) -> *mut ConstantData;
}

pub struct LLVMCtx {
    ptr: *mut LLVMCtxData,
    machine: *mut TargetMachineData,
}

pub struct LLVMModule<'ll> {
    _marker: PhantomData<&'ll mut &'ll ()>,
    ptr: *mut LLVMModuleData,
}

pub struct IRBuilder<'ll> {
    _marker: PhantomData<&'ll mut &'ll ()>,
    ptr: *mut IRBuilderData,
}

pub struct FunctionOptManager<'ll> {
    _marker: PhantomData<&'ll mut &'ll ()>,
    ptr: *mut FunctionOptManagerData,
}

impl LLVMCtx {
    pub fn new() -> Self {
        let raw_ctx = unsafe { getCtx() };
        let mut machine = ptr::null_mut();
        let err = unsafe { getTargetMachine(raw_ctx, &mut machine) };
        if err != 0 {
            unsafe { destroyCtx(raw_ctx) };
            panic!("machine error");
        }
        Self {
            ptr: raw_ctx,
            machine,
        }
    }

    pub fn create_module<'ll>(&'ll self) -> LLVMModule<'ll> {
        let raw_mod = unsafe { getMod(self.ptr, self.machine) };
        LLVMModule {
            _marker: PhantomData,
            ptr: raw_mod,
        }
    }

    pub fn create_builder<'ll>(&'ll self) -> IRBuilder<'ll> {
        let builder = unsafe { getBuilder(self.ptr) };
        IRBuilder {
            _marker: PhantomData,
            ptr: builder,
        }
    }

    pub fn create_function_opt_manager<'ll>(&'ll self) -> FunctionOptManager<'ll> {
        let manager = unsafe { getFunctionOptManager(self.machine) };
        FunctionOptManager {
            _marker: PhantomData,
            ptr: manager,
        }
    }

    pub fn emit<'ll>(&'ll self, module: LLVMModule<'ll>, name: &str) {
        let err = unsafe { emitModule(module.ptr, self.machine, name.as_ptr(), name.len() as u32) };
        if err != 0 {
            panic!("can't emit");
        }
    }

    pub fn int<'ll>(&'ll self, value: u32) -> Constant<'ll> {
        Constant {
            ptr: unsafe { ctxGetInt(self.ptr, value) },
            _marker: PhantomData,
        }
    }

    pub fn boolean<'ll>(&'ll self, value: bool) -> Constant<'ll> {
        Constant {
            ptr: unsafe { ctxGetBool(self.ptr, value) },
            _marker: PhantomData,
        }
    }

    pub fn ty_int<'ll>(&'ll self) -> Type<'ll> {
        Type {
            ptr: unsafe { ctxGetTyInt(self.ptr) },
            _marker: PhantomData,
        }
    }

    pub fn ty_bool<'ll>(&'ll self) -> Type<'ll> {
        Type {
            ptr: unsafe { ctxGetTyBool(self.ptr) },
            _marker: PhantomData,
        }
    }

    pub fn ty_ptr<'ll>(&'ll self) -> Type<'ll> {
        Type {
            ptr: unsafe { ctxGetTyPtr(self.ptr) },
            _marker: PhantomData,
        }
    }

    pub fn ty_void<'ll>(&'ll self) -> Type<'ll> {
        Type {
            ptr: unsafe { ctxGetTyVoid(self.ptr) },
            _marker: PhantomData,
        }
    }

    pub fn create_named_ty<'ll>(&'ll self, name: &str, fields: &[Type<'ll>]) -> Type<'ll> {
        Type {
            ptr: unsafe {
                ctxCreateStructTy(
                    self.ptr,
                    name.as_ptr(),
                    name.len() as u32,
                    fields.as_ptr() as *const *mut TypeData,
                    fields.len() as u32,
                )
            },
            _marker: PhantomData,
        }
    }

    pub fn struct_ty<'ll>(&'ll self, fields: &[Type<'ll>]) -> Type<'ll> {
        Type {
            ptr: unsafe {
                ctxGetStructTy(
                    self.ptr,
                    fields.as_ptr() as *const *mut TypeData,
                    fields.len() as u32,
                )
            },
            _marker: PhantomData,
        }
    }

    pub fn array_ty<'ll>(&'ll self, elem_ty: Type<'ll>, size: u64) -> Type<'ll> {
        Type {
            ptr: unsafe { ctxGetArrayTy(self.ptr, elem_ty.ptr, size) },
            _marker: PhantomData,
        }
    }

    pub fn const_array<'ll>(
        &'ll self,
        elem_ty: Type<'ll>,
        elems: &[Constant<'ll>],
    ) -> Constant<'ll> {
        Constant {
            ptr: unsafe {
                ctxArrayConstant(
                    self.ptr,
                    elem_ty.ptr,
                    elems.as_ptr() as *const *mut ConstantData,
                    elems.len() as u32,
                )
            },
            _marker: PhantomData,
        }
    }

    pub fn create_block<'ll>(&'ll self) -> Block<'ll> {
        Block {
            ptr: unsafe { ctxCreateBlock(self.ptr) },
            _marker: PhantomData,
        }
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Type<'ll> {
    _marker: PhantomData<&'ll mut &'ll ()>,
    ptr: *mut TypeData,
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Value<'ll> {
    _marker: PhantomData<&'ll mut &'ll ()>,
    ptr: *mut ValueData,
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Constant<'ll> {
    _marker: PhantomData<&'ll mut &'ll ()>,
    ptr: *mut ConstantData,
}

impl<'ll> Value<'ll> {
    pub fn ty(self) -> Type<'ll> {
        Type {
            _marker: PhantomData,
            ptr: unsafe { valueType(self.ptr) },
        }
    }
}

impl<'ll> Constant<'ll> {
    pub fn val(self) -> Value<'ll> {
        Value {
            _marker: PhantomData,
            ptr: self.ptr as *mut ValueData,
        }
    }
}

impl<'ll> Type<'ll> {
    pub fn null(self) -> Constant<'ll> {
        Constant {
            _marker: PhantomData,
            ptr: unsafe { tyNullVal(self.ptr) },
        }
    }
}

impl<'ll> Function<'ll> {
    pub fn append(self, block: Block<'ll>) {
        unsafe { functionAppendBlock(self.ptr, block.ptr) }
    }

    pub fn verify(self) -> bool {
        unsafe { functionVerify(self.ptr) }
    }

    pub fn args(self) -> impl Iterator<Item = Value<'ll>> + 'll {
        let argc = unsafe { functionArgSize(self.ptr) };
        (0..argc).map(move |idx| Value {
            _marker: PhantomData,
            ptr: unsafe { functionArgAt(self.ptr, idx as u32) },
        })
    }
}

impl<'ll> FunctionOptManager<'ll> {
    pub fn optimize(&self, func: Function<'ll>) {
        unsafe { functionOptManagerOptimize(self.ptr, func.ptr) }
    }
}

#[derive(Clone, Copy)]
pub struct Function<'ll> {
    _marker: PhantomData<&'ll mut &'ll ()>,
    ptr: *mut FunctionData,
}

#[derive(Clone, Copy)]
pub struct Block<'ll> {
    _marker: PhantomData<&'ll mut &'ll ()>,
    ptr: *mut BlockData,
}

impl<'ll> LLVMModule<'ll> {
    pub fn create_func(
        &self,
        name: &str,
        params: &[Type<'ll>],
        ret_ty: Type<'ll>,
    ) -> Function<'ll> {
        Function {
            _marker: PhantomData,
            ptr: unsafe {
                modCreateFunc(
                    self.ptr,
                    name.as_ptr(),
                    name.len() as u32,
                    params.as_ptr() as *const *mut TypeData,
                    params.len() as u32,
                    ret_ty.ptr,
                )
            },
        }
    }

    pub fn alloc_size_of(&self, ty: Type<'ll>) -> u64 {
        unsafe { modAllocSizeOf(self.ptr, ty.ptr) }
    }

    pub fn get_or_create_global(
        &self,
        name: &str,
        ty: Type<'ll>,
        init: impl FnOnce() -> Constant<'ll>,
    ) -> Value<'ll> {
        let global =
            unsafe { modGetOrCreateGlobal(self.ptr, name.as_ptr(), name.len() as u32, ty.ptr) };

        unsafe {
            if !globalHasInitializer(global) {
                globalSetInitializer(global, init().ptr);
            }
        }

        Value {
            _marker: PhantomData,
            ptr: global as *mut ValueData,
        }
    }
}

impl<'ll> IRBuilder<'ll> {
    pub fn phi(&self, ty: Type<'ll>) -> inst::Phi<'ll> {
        inst::Phi {
            _marker: PhantomData,
            ptr: unsafe { builderPhi(self.ptr, ty.ptr) },
        }
    }

    pub fn add(&self, lhs: Value<'ll>, rhs: Value<'ll>) -> Value<'ll> {
        Value {
            _marker: PhantomData,
            ptr: unsafe { builderAdd(self.ptr, lhs.ptr, rhs.ptr) },
        }
    }

    pub fn eq(&self, lhs: Value<'ll>, rhs: Value<'ll>) -> Value<'ll> {
        Value {
            _marker: PhantomData,
            ptr: unsafe { builderEq(self.ptr, lhs.ptr, rhs.ptr) },
        }
    }

    pub fn call(&self, func: Function<'ll>, args: &[Value<'ll>]) -> Value<'ll> {
        Value {
            _marker: PhantomData,
            ptr: unsafe {
                builderCall(
                    self.ptr,
                    func.ptr,
                    args.as_ptr() as *const *mut ValueData,
                    args.len() as u32,
                )
            },
        }
    }

    pub fn condbr(&self, cond: Value<'ll>, yes: Block<'ll>, no: Block<'ll>) {
        unsafe { builderCondBr(self.ptr, cond.ptr, yes.ptr, no.ptr) }
    }

    pub fn load(&self, ptr: Value<'ll>, src: Type<'ll>) -> Value<'ll> {
        Value {
            _marker: PhantomData,
            ptr: unsafe { builderLoad(self.ptr, ptr.ptr, src.ptr) },
        }
    }

    pub fn store(&self, ptr: Value<'ll>, src: Value<'ll>) {
        unsafe { builderStore(self.ptr, ptr.ptr, src.ptr) }
    }

    pub fn br(&self, target: Block<'ll>) {
        unsafe { builderBr(self.ptr, target.ptr) }
    }

    pub fn ret(&self, value: Value<'ll>) {
        unsafe { builderRet(self.ptr, value.ptr) }
    }

    pub fn gep(&self, ty: Type<'ll>, base: Value<'ll>, offsets: &[Value<'ll>]) -> Value<'ll> {
        Value {
            _marker: PhantomData,
            ptr: unsafe {
                builderGep(
                    self.ptr,
                    ty.ptr,
                    base.ptr,
                    offsets.as_ptr() as *const *mut ValueData,
                    offsets.len() as u32,
                )
            },
        }
    }

    pub fn current_block(&self) -> Block<'ll> {
        Block {
            _marker: PhantomData,
            ptr: unsafe { builderCurrentBlock(self.ptr) },
        }
    }

    pub fn create_alloca(&self, ty: Type<'ll>) -> Value<'ll> {
        Value {
            _marker: PhantomData,
            ptr: unsafe { builderCreateAlloca(self.ptr, ty.ptr) },
        }
    }

    pub fn set_insert_point(&self, block: Block<'ll>) {
        unsafe { builderSetInsertPoint(self.ptr, block.ptr) }
    }
}

impl Drop for LLVMCtx {
    fn drop(&mut self) {
        unsafe { destroyCtx(self.ptr) }
    }
}

impl<'ll> Drop for LLVMModule<'ll> {
    fn drop(&mut self) {
        unsafe { destroyMod(self.ptr) }
    }
}

impl<'ll> Drop for IRBuilder<'ll> {
    fn drop(&mut self) {
        unsafe { destroyBuilder(self.ptr) }
    }
}

impl<'ll> Drop for FunctionOptManager<'ll> {
    fn drop(&mut self) {
        unsafe { destroyFunctionOptManager(self.ptr) }
    }
}
