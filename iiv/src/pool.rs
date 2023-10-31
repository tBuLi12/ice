use std::{
    alloc::{self, Layout},
    cell::{RefCell, UnsafeCell},
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    mem::MaybeUninit,
    ops::Deref,
    ptr, slice,
};

use crate::{fun, str::Str};

struct PinnedVec<T> {
    // This is probably UB when the enclosing pool cell is mutably borrowed, because the slice is owned - use ptrs
    bufs: Vec<Box<[MaybeUninit<T>]>>,
    next_idx: usize,
}

impl<T> PinnedVec<T> {
    const INITIAL_SIZE: usize = 64;

    fn get_new_box(size: usize) -> Box<[MaybeUninit<T>]> {
        let layout = Layout::array::<T>(size).unwrap();
        assert!(layout.size() != 0);
        let raw_buf = unsafe { alloc::alloc(layout) }.cast::<MaybeUninit<T>>();
        let slice_ptr = ptr::slice_from_raw_parts_mut(raw_buf, size);
        unsafe { Box::from_raw(slice_ptr) }
    }

    fn new() -> Self {
        PinnedVec {
            bufs: vec![Self::get_new_box(Self::INITIAL_SIZE)],
            next_idx: 0,
        }
    }

    fn len(&self) -> usize {
        self.bufs.last().unwrap().len() - Self::INITIAL_SIZE + self.next_idx
    }

    fn next(&mut self) -> &mut MaybeUninit<T> {
        let len = self.bufs.last().unwrap().len();
        if self.next_idx == len {
            self.next_idx = 0;
            self.bufs.push(Self::get_new_box(len * 2));
        }
        let last = self.bufs.last_mut().unwrap();
        let idx = self.next_idx;
        self.next_idx += 1;
        &mut last[idx]
    }
}

struct RawPool<'i, T> {
    index: HashMap<&'i T, Ref<'i, T>>,
    storage: PinnedVec<T>,
}

pub struct Ref<'i, T>(&'i T);

impl<'i, T: Debug> Debug for Ref<'i, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("Ref<{:p}>", self.0))
            .field(self.0)
            .finish()
    }
}

impl<'i, T> Deref for Ref<'i, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'i, T> Clone for Ref<'i, T> {
    fn clone(&self) -> Self {
        Ref(self.0)
    }
}
impl<'i, T> Copy for Ref<'i, T> {}
impl<'i, T> PartialEq for Ref<'i, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}
impl<'i, T> Eq for Ref<'i, T> {}
impl<'i, T> Hash for Ref<'i, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(self.0, state)
    }
}

impl<'i, T: PartialOrd> PartialOrd for Ref<'i, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        T::partial_cmp(&self.0, other.0)
    }
}
impl<'i, T: Ord> Ord for Ref<'i, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        T::cmp(&self.0, other.0)
    }
}

impl<'i, T: Hash + Eq + Debug> RawPool<'i, T> {
    fn get(&'i mut self, value: T) -> Ref<'_, T> {
        if let Some(&item) = self.index.get(&value) {
            return item;
        }
        let val = Ref(self.storage.next().write(value));
        self.index.insert(val.0, val);
        val
    }

    fn index_of(&'i self, value: Ref<'i, T>) -> usize {
        let (size, offset) = self
            .storage
            .bufs
            .iter()
            .rev()
            .find_map(|buf| {
                let range = buf.as_ptr_range();
                let raw_ref = value.0 as *const _ as *const _;
                if range.contains(&raw_ref) {
                    Some((buf.len(), unsafe { raw_ref.offset_from(range.start) }))
                } else {
                    None
                }
            })
            .unwrap();

        size - PinnedVec::<T>::INITIAL_SIZE + offset as usize
    }

    fn len(&'i self) -> usize {
        self.storage.len()
    }

    fn new() -> Self {
        Self {
            index: HashMap::new(),
            storage: PinnedVec::new(),
        }
    }
}

struct ListPoolStorage<T> {
    buf: Vec<Box<[MaybeUninit<T>]>>,
    next_idx: usize,
}

impl<T> ListPoolStorage<T> {
    fn put(&mut self, value: Vec<T>) -> List<'_, T> {
        if value.is_empty() {
            return List(unsafe { slice::from_raw_parts(self.buf[0].as_ptr() as *const _, 0) });
        }

        let len = self.buf.last().unwrap().len();
        if (self.next_idx + value.len() - 1) == len {
            self.next_idx = 0;
            let mut new_len = len * 2;
            while new_len < value.len() {
                new_len *= 2;
            }
            self.buf.push(Self::get_new_box(new_len));
        }
        let last = self.buf.last_mut().unwrap();
        let start_index = self.next_idx;
        let len = value.len();
        for item in value.into_iter() {
            last[self.next_idx].write(item);
            self.next_idx += 1;
        }
        List(unsafe { slice::from_raw_parts(last[start_index].as_ptr(), len) })
    }

    fn get_new_box(size: usize) -> Box<[MaybeUninit<T>]> {
        let layout = Layout::array::<MaybeUninit<T>>(size).unwrap();
        assert!(layout.size() != 0);
        let raw_buf = unsafe { alloc::alloc(layout) }.cast::<MaybeUninit<T>>();
        let slice_ptr = ptr::slice_from_raw_parts_mut(raw_buf, size);
        unsafe { Box::from_raw(slice_ptr) }
    }

    fn new() -> Self {
        Self {
            buf: vec![Self::get_new_box(64)],
            next_idx: 1,
        }
    }
}

struct RawListPool<'i, T> {
    index: HashMap<&'i [T], List<'i, T>>,
    storage: ListPoolStorage<T>,
}

#[derive(Debug)]
pub struct List<'i, T>(&'i [T]);

impl<'i, T> Deref for List<'i, T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'i, T> Clone for List<'i, T> {
    fn clone(&self) -> Self {
        List(self.0)
    }
}
impl<'i, T> Copy for List<'i, T> {}
impl<'i, T> PartialEq for List<'i, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0.as_ptr(), other.0.as_ptr())
    }
}
impl<'i, T> Eq for List<'i, T> {}
impl<'i, T> Hash for List<'i, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(self.0.as_ptr(), state)
    }
}

impl<'i, T: PartialOrd> PartialOrd for List<'i, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        <[T] as PartialOrd>::partial_cmp(self.0, other.0)
    }
}
impl<'i, T: Ord> Ord for List<'i, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        <[T] as Ord>::cmp(self.0, other.0)
    }
}

impl<'i, T: Hash + Eq> RawListPool<'i, T> {
    fn get(&'i mut self, value: Vec<T>) -> List<'_, T> {
        if let Some(val) = self.index.get(&value[..]) {
            return *val;
        }

        let index = &mut self.index;
        let storage = &mut self.storage;

        let val = storage.put(value);
        index.insert(val.0, val);
        val
    }

    fn new() -> Self {
        Self {
            storage: ListPoolStorage::new(),
            index: HashMap::new(),
        }
    }
}

pub struct Pool<'i, T>(UnsafeCell<RawPool<'i, T>>);

pub struct ListPool<'i, T>(UnsafeCell<RawListPool<'i, T>>);

pub struct SetPool<'i, T>(ListPool<'i, T>);

impl<'i, T: Hash + Eq + Debug> Pool<'i, T> {
    pub fn get(&self, value: T) -> Ref<'_, T> {
        let inner = unsafe { &mut *self.0.get() };
        inner.get(value)
    }

    pub fn index_of(&'i self, value: Ref<'i, T>) -> usize {
        let inner = unsafe { &*self.0.get() };
        inner.index_of(value)
    }

    pub fn len(&'i self) -> usize {
        let inner = unsafe { &*self.0.get() };
        inner.len()
    }

    pub fn new() -> Self {
        Self(UnsafeCell::new(RawPool::new()))
    }
}

impl<'i, T: Hash + Eq> ListPool<'i, T> {
    pub fn get(&self, value: Vec<T>) -> List<'_, T> {
        let inner = unsafe { &mut *self.0.get() };
        inner.get(value)
    }

    pub fn new() -> Self {
        Self(UnsafeCell::new(RawListPool::new()))
    }
}

impl<'i, T: Hash + Eq + Ord> ListPool<'i, T> {
    pub fn get_set(&self, mut value: Vec<T>) -> List<'_, T> {
        value.sort();
        value.dedup();
        self.get(value)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FuncRef<'i>(&'i RefCell<fun::Function<'i>>);

impl<'i> Hash for FuncRef<'i> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(self.0, state)
    }
}

struct RawFunPool<'i> {
    storage: PinnedVec<RefCell<fun::Function<'i>>>,
    index: HashMap<Str<'i>, FuncRef<'i>>,
}

impl<'i> PartialEq for FuncRef<'i> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}
impl<'i> Eq for FuncRef<'i> {}

impl<'i> Deref for FuncRef<'i> {
    type Target = RefCell<fun::Function<'i>>;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'i> RawFunPool<'i> {
    pub fn new() -> Self {
        RawFunPool {
            storage: PinnedVec::new(),
            index: HashMap::new(),
        }
    }

    pub fn insert(&'i mut self, name: Str<'i>, fun: fun::Function<'i>) -> FuncRef<'i> {
        let fun = FuncRef(self.storage.next().write(RefCell::new(fun)));
        self.index.insert(name, fun);
        fun
    }

    pub fn get(&mut self, name: Str<'i>) -> Option<FuncRef<'i>> {
        self.index.get(&name).copied()
    }
}

pub struct FunPool<'i>(UnsafeCell<RawFunPool<'i>>);

impl<'i> FunPool<'i> {
    pub fn new() -> Self {
        Self(UnsafeCell::new(RawFunPool::new()))
    }

    pub fn insert(&'i self, fun: fun::Function<'i>) -> FuncRef<'i> {
        let inner = unsafe { &mut *self.0.get() };
        inner.insert(fun.sig.name, fun)
    }

    pub fn get(&self, name: Str<'i>) -> Option<FuncRef<'i>> {
        let inner = unsafe { &mut *self.0.get() };
        inner.get(name)
    }
}
