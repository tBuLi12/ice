use std::marker::PhantomData;

use crate::{Block, BlockData, Opaque, Value, ValueData};

#[repr(C)]
pub(super) struct PhiData(Opaque);

extern "C" {
    fn phiAddIncomming(phi: *mut PhiData, block: *mut BlockData, val: *mut ValueData);
}

#[derive(Clone, Copy)]
pub struct Phi<'ll> {
    pub(super) _marker: PhantomData<&'ll mut &'ll ()>,
    pub(super) ptr: *mut PhiData,
}

#[repr(C)]
struct AllocaData(Opaque);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Alloca<'ll> {
    _marker: PhantomData<&'ll mut &'ll ()>,
    ptr: *mut AllocaData,
}

impl<'ll> Alloca<'ll> {
    pub fn ptr(self) -> Value<'ll> {
        Value {
            _marker: self._marker,
            ptr: self.ptr as *mut ValueData,
        }
    }
}

impl<'ll> Phi<'ll> {
    pub fn add_incomming(self, block: Block<'ll>, value: Value<'ll>) {
        unsafe { phiAddIncomming(self.ptr, block.ptr, value.ptr) }
    }

    pub fn val(self) -> Value<'ll> {
        Value {
            _marker: PhantomData,
            ptr: self.ptr as *mut ValueData,
        }
    }
}
