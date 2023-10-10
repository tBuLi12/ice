use std::ops::Deref;

use crate::pool;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Str<'i>(pool::List<'i, u8>);

pub struct StrPool<'i> {
    pool: pool::ListPool<'i, u8>,
}

impl<'i> StrPool<'i> {
    pub fn get(&self, string: &str) -> Str<'_> {
        Str(self.pool.get(string.bytes().collect()))
    }
}

impl<'i> Deref for Str<'i> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        unsafe { std::str::from_utf8_unchecked(&*self.0) }
    }
}
