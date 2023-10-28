use std::{fmt, ops::Deref};

use crate::pool;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Str<'i>(pool::List<'i, u8>);

pub struct StrPool<'i> {
    pool: pool::ListPool<'i, u8>,
}

impl<'i> StrPool<'i> {
    pub fn new() -> Self {
        StrPool {
            pool: pool::ListPool::new(),
        }
    }

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

impl<'i> fmt::Display for Str<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: &str = &*self;
        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod test {
    use super::StrPool;

    #[test]
    fn pool_test() {
        let pool = StrPool::new();
        let a = pool.get("a");
        let a2 = pool.get("a");
        assert!(a == a2);
    }
}
