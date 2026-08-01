//! Side tables — every analysis result about a HIR node, stored beside it.
//!
//! [B3](../../../plans/rewrite/anti-spec.md): no analysis result lives on the
//! node it describes. A `Ty`, a capture set, a dependency set, a doc comment, a
//! desugaring's provenance — each is a [`NodeMap`], and adding one is a new table
//! rather than a new field on a node every other pass also reads.

use rustc_hash::FxHashMap;

use crate::ids::HirId;

/// One value per [`HirId`], write-once.
///
/// # Why `insert` asserts instead of returning the old value
///
/// A side table is an *analysis result*. Overwriting one silently discards a
/// conclusion some pass reached, and the loss is invisible: the table still has
/// an entry, still answers `get`, and the wrong answer is a plausible one. ark's
/// `NodeMap::insert` asserts for the same reason and it is worth keeping —
/// [A8](../../../plans/rewrite/anti-spec.md) is about invariants that are stated
/// and never observed, and *"each node is analysed once"* is exactly that shape.
///
/// A pass that legitimately needs to revise a value wants
/// [`get_mut`](Self::get_mut), which says so at the call site.
#[derive(Debug, Clone)]
pub struct NodeMap<V> {
    map: FxHashMap<HirId, V>,
}

impl<V> Default for NodeMap<V> {
    fn default() -> Self {
        Self {
            map: FxHashMap::default(),
        }
    }
}

impl<V> NodeMap<V> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record `value` for `id`.
    ///
    /// # Panics
    ///
    /// If `id` already has a value.
    pub fn insert(&mut self, id: HirId, value: V) {
        let previous = self.map.insert(id, value);
        assert!(
            previous.is_none(),
            "{id:?} already has a value in this side table; \
             overwriting it would discard an analysis result",
        );
    }

    pub fn get(&self, id: HirId) -> Option<&V> {
        self.map.get(&id)
    }

    pub fn get_mut(&mut self, id: HirId) -> Option<&mut V> {
        self.map.get_mut(&id)
    }

    pub fn contains(&self, id: HirId) -> bool {
        self.map.contains_key(&id)
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Every entry, **sorted by [`HirId`]**.
    ///
    /// Iteration order comes from a hash map, so anything derived from this table
    /// that reaches output — a dump, an artifact's type table — must be ordered
    /// first ([A6](../../../plans/rewrite/anti-spec.md)). Sorting here rather
    /// than at each consumer means a consumer cannot forget.
    pub fn iter_sorted(&self) -> impl Iterator<Item = (HirId, &V)> {
        let mut entries: Vec<(HirId, &V)> =
            self.map.iter().map(|(&id, value)| (id, value)).collect();
        entries.sort_by_key(|(id, _)| *id);
        entries.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_value_is_readable_after_insertion() {
        let mut map: NodeMap<u32> = NodeMap::new();
        map.insert(HirId::new(1), 42);
        assert_eq!(map.get(HirId::new(1)), Some(&42));
        assert_eq!(map.get(HirId::new(2)), None);
        assert!(map.contains(HirId::new(1)));
        assert_eq!(map.len(), 1);
    }

    /// The whole point of the type. An overwritten side table is an analysis
    /// result lost, and losing it silently is the failure mode.
    #[test]
    #[should_panic(expected = "already has a value")]
    fn a_second_insert_for_one_node_panics() {
        let mut map: NodeMap<u32> = NodeMap::new();
        map.insert(HirId::new(1), 1);
        map.insert(HirId::new(1), 2);
    }

    /// A6: the order this table is read in must not come from the hash map.
    /// Inserted out of order, and out of the order `FxHashMap` would produce.
    #[test]
    fn iteration_is_sorted_by_hir_id_not_by_insertion() {
        let mut map: NodeMap<&str> = NodeMap::new();
        for (index, name) in [(9u32, "nine"), (2, "two"), (40, "forty"), (0, "zero")] {
            map.insert(HirId::new(index), name);
        }
        let order: Vec<(u32, &str)> = map.iter_sorted().map(|(id, &name)| (id.0, name)).collect();
        assert_eq!(
            order,
            vec![(0, "zero"), (2, "two"), (9, "nine"), (40, "forty")],
        );
    }

    /// Revision is possible and has to be spelled differently from insertion.
    ///
    /// Two entries, not one: with a single entry the assertion also passes for a
    /// `get_mut` that ignores its argument and returns *some* entry, which is a
    /// distinct bug and a vacuous test — found by mutating it.
    #[test]
    fn get_mut_revises_the_named_node_and_only_it() {
        let mut map: NodeMap<u32> = NodeMap::new();
        map.insert(HirId::new(0), 1);
        map.insert(HirId::new(1), 10);

        *map.get_mut(HirId::new(1)).unwrap() = 20;

        assert_eq!(map.get(HirId::new(1)), Some(&20));
        assert_eq!(map.get(HirId::new(0)), Some(&1), "the other entry moved");
        assert_eq!(map.len(), 2);
        assert!(map.get_mut(HirId::new(2)).is_none());
    }
}
