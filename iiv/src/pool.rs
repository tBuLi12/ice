use std::{
    alloc::{self, Layout},
    cell::UnsafeCell,
    collections::HashMap,
    hash::Hash,
    mem::MaybeUninit,
    ops::Deref,
    ptr, slice,
};

struct RawPool<'i, T> {
    types_bufs: Vec<Box<[MaybeUninit<T>]>>,
    index: HashMap<&'i T, Ref<'i, T>>,
    next_idx: usize,
}

#[derive(Debug)]
pub struct Ref<'i, T>(&'i T);

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

impl<'i, T: Hash + Eq> RawPool<'i, T> {
    fn get(&mut self, value: T) -> Ref<'_, T> {
        if let Some(&item) = self.index.get(&value) {
            return item;
        }
        self.put(value)
    }

    fn put(&mut self, value: T) -> Ref<'_, T> {
        let len = self.types_bufs.last().unwrap().len();
        if self.next_idx == len {
            self.next_idx = 0;
            self.types_bufs.push(Self::get_new_box(len * 2));
        }
        let last = self.types_bufs.last_mut().unwrap();
        Ref(last[self.next_idx].write(value))
    }

    fn new() -> Self {
        Self {
            types_bufs: vec![Self::get_new_box(64)],
            index: HashMap::new(),
            next_idx: 0,
        }
    }

    fn get_new_box(size: usize) -> Box<[MaybeUninit<T>]> {
        let layout = Layout::array::<MaybeUninit<T>>(size).unwrap();
        assert!(layout.size() != 0);
        let raw_buf = unsafe { alloc::alloc(layout) }.cast::<MaybeUninit<T>>();
        let slice_ptr = ptr::slice_from_raw_parts_mut(raw_buf, size);
        unsafe { Box::from_raw(slice_ptr) }
    }
}

struct RawListPool<'i, T> {
    types_bufs: Vec<Box<[MaybeUninit<T>]>>,
    index: HashMap<&'i [T], List<'i, T>>,
    next_idx: usize,
}

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
    fn get(&mut self, value: Vec<T>) -> List<'_, T> {
        if let Some(&item) = self.index.get(&value[..]) {
            return item;
        }
        self.put(value)
    }

    fn put(&mut self, value: Vec<T>) -> List<'_, T> {
        let len = self.types_bufs.last().unwrap().len();
        if (self.next_idx + value.len() - 1) == len {
            self.next_idx = 0;
            let mut new_len = len * 2;
            while new_len < value.len() {
                new_len *= 2;
            }
            self.types_bufs.push(Self::get_new_box(new_len));
        }
        let last = self.types_bufs.last_mut().unwrap();
        let start_index = self.next_idx;
        let len = value.len();
        for item in value.into_iter() {
            last[self.next_idx].write(item);
            self.next_idx += 1;
        }
        List(unsafe { slice::from_raw_parts(last[start_index].as_ptr(), len) })
    }

    fn new() -> Self {
        Self {
            types_bufs: vec![Self::get_new_box(64)],
            index: HashMap::new(),
            next_idx: 0,
        }
    }

    fn get_new_box(size: usize) -> Box<[MaybeUninit<T>]> {
        let layout = Layout::array::<MaybeUninit<T>>(size).unwrap();
        assert!(layout.size() != 0);
        let raw_buf = unsafe { alloc::alloc(layout) }.cast::<MaybeUninit<T>>();
        let slice_ptr = ptr::slice_from_raw_parts_mut(raw_buf, size);
        unsafe { Box::from_raw(slice_ptr) }
    }
}

pub struct Pool<'i, T>(UnsafeCell<RawPool<'i, T>>);

pub struct ListPool<'i, T>(UnsafeCell<RawListPool<'i, T>>);

pub struct SetPool<'i, T>(ListPool<'i, T>);

impl<'i, T: Hash + Eq> Pool<'i, T> {
    pub fn get(&self, value: T) -> Ref<'_, T> {
        let inner = unsafe { &mut *self.0.get() };
        inner.get(value)
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
