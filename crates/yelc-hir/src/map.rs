//! The `HirId ↔ SourceNodeId` map — how a HIR node points back at source.
//!
//! What lets a diagnostic name a construct the user wrote, and what the LSP needs
//! to answer *"what is under the cursor"*. Invariant
//! [H2](../../../plans/rewrite/stage-3-hir-build.md).

use rustc_hash::FxHashMap;

use crate::ids::{HirId, SourceNodeId};

/// A bidirectional, injective map between HIR nodes and the AST nodes they came
/// from.
///
/// # Allocating and recording are one call
///
/// [`next_hir_id`](Self::next_hir_id) is the only way to obtain a [`HirId`], and
/// it records the mapping in the same step. There is no *"allocate now, map
/// later"* path, so a `HirId` with no source cannot be produced by forgetting
/// the second half — the shape ark's `hir_map.rs` established, and the reason to
/// keep the counter private.
///
/// # Injective, not merely total
///
/// [H2](../../../plans/rewrite/stage-3-hir-build.md) originally read *"total and
/// bidirectional"*, asserted by `hir_of(node_of(h)) == h`. That round-trip
/// **passes under a key collision**: if two distinct AST nodes hash to one key,
/// the reverse map keeps the last writer, the forward map still answers, and the
/// property holds for the survivor. So the insert asserts injectivity where the
/// invariant is established rather than leaving it to a test that cannot observe
/// it ([A8](../../../plans/rewrite/anti-spec.md)).
///
/// This is not hypothetical. Keyed by a bare `yelc_syntax::NodeId` — which is
/// what the brief specifies and what ark does — the collision is guaranteed the
/// moment a second file is lowered, because `NodeId`s restart at zero per file.
/// See [`SourceNodeId`].
#[derive(Debug, Default)]
pub struct HirMap {
    map: FxHashMap<HirId, SourceNodeId>,
    rev_map: FxHashMap<SourceNodeId, HirId>,
    next: u32,
}

impl HirMap {
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocate a [`HirId`] for `node` and record both directions.
    ///
    /// # Panics
    ///
    /// If `node` already has a `HirId`. Two HIR nodes claiming one AST node is a
    /// lowering bug — the second write would erase the first and leave a `HirId`
    /// that maps forward but not back.
    pub fn next_hir_id(&mut self, node: SourceNodeId) -> HirId {
        let hir = HirId::new(self.next);
        self.next += 1;

        assert!(
            !self.rev_map.contains_key(&node),
            "AST node {node:?} already mapped to {:?}; a second HirId for one \
             node breaks H2's injectivity",
            self.rev_map[&node],
        );
        self.map.insert(hir, node);
        self.rev_map.insert(node, hir);
        hir
    }

    /// Allocate a [`HirId`] for a node the lowering **synthesized** — a
    /// desugaring product with no AST node of its own.
    ///
    /// `origin` is the construct it was made from: `x += 1` synthesizes a
    /// `Binary` whose origin is the assignment statement. Recorded **forward
    /// only** — the reverse map keeps pointing at the *primary* lowering of
    /// that AST node, so H2's injectivity is a statement about primaries and a
    /// synthesized node can never shadow one. `node_of` still answers for every
    /// id, which is what diagnostics need; `hir_of` deliberately does not,
    /// because "the HIR node for this syntax" has exactly one right answer.
    pub fn synthesize(&mut self, origin: SourceNodeId) -> HirId {
        let hir = HirId::new(self.next);
        self.next += 1;
        self.map.insert(hir, origin);
        hir
    }

    /// The AST node a HIR node came from.
    pub fn node_of(&self, hir: HirId) -> Option<SourceNodeId> {
        self.map.get(&hir).copied()
    }

    /// The HIR node built from an AST node, if one was.
    ///
    /// `None` is an ordinary answer: not every AST node becomes a HIR node — a
    /// desugaring can drop the syntax it consumed.
    pub fn hir_of(&self, node: SourceNodeId) -> Option<HirId> {
        self.rev_map.get(&node).copied()
    }

    /// How many HIR nodes have been allocated.
    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yelc_base::SourceId;
    use yelc_syntax::NodeId;

    fn node(source: u32, index: u32) -> SourceNodeId {
        SourceNodeId::new(SourceId::new(source), NodeId::new(index))
    }

    #[test]
    fn allocation_records_both_directions() {
        let mut map = HirMap::new();
        let source_node = node(0, 3);
        let hir = map.next_hir_id(source_node);

        assert_eq!(map.node_of(hir), Some(source_node));
        assert_eq!(map.hir_of(source_node), Some(hir));
        assert_eq!(map.hir_of(map.node_of(hir).unwrap()), Some(hir));
    }

    #[test]
    fn ids_are_allocated_densely_from_zero() {
        let mut map = HirMap::new();
        let ids: Vec<HirId> = (0..3)
            .map(|index| map.next_hir_id(node(0, index)))
            .collect();
        assert_eq!(ids, vec![HirId::new(0), HirId::new(1), HirId::new(2)]);
        assert_eq!(map.len(), 3);
    }

    #[test]
    fn an_unmapped_node_reports_none_rather_than_a_default() {
        let map = HirMap::new();
        assert_eq!(map.hir_of(node(0, 0)), None);
        assert_eq!(map.node_of(HirId::new(0)), None);
    }

    /// **The test the bare-`NodeId` key fails.** Two files, the same `NodeId`,
    /// two HIR nodes: each must keep its own mapping in *both* directions.
    ///
    /// Keyed by `NodeId` the reverse map holds one entry, the second insert wins,
    /// and `hir_of` answers the first file's node with the second file's `HirId`.
    /// Note the round-trip assertion H2 was originally written as still passes in
    /// that world, which is why this test asserts the cross terms instead.
    #[test]
    fn two_files_with_the_same_node_id_do_not_collide() {
        let mut map = HirMap::new();
        let in_first = node(0, 7);
        let in_second = node(1, 7);

        let first = map.next_hir_id(in_first);
        let second = map.next_hir_id(in_second);

        assert_ne!(first, second);
        assert_eq!(map.hir_of(in_first), Some(first));
        assert_eq!(map.hir_of(in_second), Some(second));
        assert_eq!(map.node_of(first), Some(in_first));
        assert_eq!(map.node_of(second), Some(in_second));
        assert_eq!(map.len(), 2, "one entry means the two files were merged");
    }

    /// Injectivity, asserted where it is established.
    #[test]
    #[should_panic(expected = "already mapped")]
    fn one_ast_node_cannot_get_two_hir_ids() {
        let mut map = HirMap::new();
        let source_node = node(0, 1);
        map.next_hir_id(source_node);
        map.next_hir_id(source_node);
    }
}
