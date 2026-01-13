//! Type-safe indexed vector collection.

use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

/// A trait for types that can be used as indices.
pub trait Idx: Copy + Eq {
    fn new(raw: u32) -> Self;
    fn index(self) -> usize;
}

/// A Vec indexed by a newtype index for type safety.
#[derive(Debug, Clone)]
pub struct IndexVec<I: Idx, T> {
    raw: Vec<T>,
    _marker: PhantomData<fn(I) -> I>,
}

impl<I: Idx, T> Default for IndexVec<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Idx, T> IndexVec<I, T> {
    pub fn new() -> Self {
        Self {
            raw: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            raw: Vec::with_capacity(cap),
            _marker: PhantomData,
        }
    }

    /// Push a value and return its index.
    pub fn push(&mut self, value: T) -> I {
        let idx = I::new(self.raw.len() as u32);
        self.raw.push(value);
        idx
    }

    pub fn len(&self) -> usize {
        self.raw.len()
    }

    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn get(&self, idx: I) -> Option<&T> {
        self.raw.get(idx.index())
    }

    pub fn get_mut(&mut self, idx: I) -> Option<&mut T> {
        self.raw.get_mut(idx.index())
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.raw.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.raw.iter_mut()
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item = (I, &T)> {
        self.raw
            .iter()
            .enumerate()
            .map(|(i, v)| (I::new(i as u32), v))
    }

    pub fn next_idx(&self) -> I {
        I::new(self.raw.len() as u32)
    }
}

impl<I: Idx, T> Index<I> for IndexVec<I, T> {
    type Output = T;

    fn index(&self, idx: I) -> &T {
        &self.raw[idx.index()]
    }
}

impl<I: Idx, T> IndexMut<I> for IndexVec<I, T> {
    fn index_mut(&mut self, idx: I) -> &mut T {
        &mut self.raw[idx.index()]
    }
}

// Implement Idx for all our ID types
macro_rules! impl_idx {
    ($ty:ty) => {
        impl Idx for $ty {
            fn new(raw: u32) -> Self {
                Self(raw)
            }
            fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}

impl_idx!(crate::ids::DefId);
impl_idx!(crate::ids::FieldIdx);
impl_idx!(crate::ids::VariantIdx);
impl_idx!(crate::ids::LocalId);
impl_idx!(crate::ids::ExprId);
impl_idx!(crate::ids::NodeId);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ids::DefId;

    #[test]
    fn test_index_vec_push() {
        let mut vec: IndexVec<DefId, String> = IndexVec::new();

        let id0 = vec.push("first".to_string());
        let id1 = vec.push("second".to_string());

        assert_eq!(id0, DefId::new(0));
        assert_eq!(id1, DefId::new(1));
        assert_eq!(vec.len(), 2);
    }

    #[test]
    fn test_index_vec_access() {
        let mut vec: IndexVec<DefId, i32> = IndexVec::new();
        let id = vec.push(42);

        assert_eq!(vec[id], 42);
        assert_eq!(vec.get(id), Some(&42));

        vec[id] = 100;
        assert_eq!(vec[id], 100);
    }

    #[test]
    fn test_index_vec_iter_enumerated() {
        let mut vec: IndexVec<DefId, &str> = IndexVec::new();
        vec.push("a");
        vec.push("b");
        vec.push("c");

        let items: Vec<_> = vec.iter_enumerated().collect();
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], (DefId::new(0), &"a"));
        assert_eq!(items[1], (DefId::new(1), &"b"));
        assert_eq!(items[2], (DefId::new(2), &"c"));
    }
}
