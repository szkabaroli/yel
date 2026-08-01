//! The index spaces this IR introduces, and the one it borrows.
//!
//! Four newtypes and no raw `usize` anywhere
//! ([keep-list §4](../../../plans/rewrite/keep-list.md)). Three of them index
//! this crate's own arenas; [`SourceNodeId`] is different in kind — it names an
//! *input* node, and it exists because a `yelc_syntax::NodeId` alone cannot.

use yelc_base::{Idx, SourceId};
use yelc_syntax::NodeId;

/// Identifies one HIR node.
///
/// A distinct index space from [`NodeId`] (AST, per file) and from
/// [`DefId`](yelc_sema::DefId) (definitions, per package). Allocated by
/// [`HirMap::next_hir_id`](crate::HirMap::next_hir_id), which is the only way to
/// obtain one — allocation and recording the node it came from are the same
/// call, so an unmapped `HirId` cannot be created by forgetting a step.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirId(pub u32);

impl HirId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl Idx for HirId {
    fn new(raw: u32) -> Self {
        Self(raw)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

/// Identifies one body — a function's, a handler's, a closure's.
///
/// Bodies are separated from the items that own them and reached by id, so a
/// consumer that only needs signatures never walks one.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BodyId(pub u32);

impl BodyId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl Idx for BodyId {
    fn new(raw: u32) -> Self {
        Self(raw)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

/// Identifies one top-level item in a [`HirModule`](crate::HirModule).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirItemId(pub u32);

impl HirItemId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl Idx for HirItemId {
    fn new(raw: u32) -> Self {
        Self(raw)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

/// An AST node, qualified by the file it was parsed from.
///
/// # Why a bare `NodeId` is not enough, and why the reference misleads here
///
/// `yelc-syntax` allocates `NodeId`s **per file, starting at zero**, and says so
/// (`yelc-syntax/src/lib.rs`): a process-global counter would make a node's id
/// depend on how many files were parsed before it, which is the determinism
/// hazard [A6](../../../plans/rewrite/anti-spec.md) forbids and would make any
/// golden containing node ids unstable.
///
/// [`lower_files`](crate::lower_files) is handed the **whole file set**
/// ([D8](../../../plans/rewrite/stage-3-hir-build.md)). So across that set a
/// `NodeId` is ambiguous — file 1 and file 2 both have a node `7` — and a map
/// keyed by one silently merges them.
///
/// The brief specifies `HirMap { map: HirId → NodeId, rev_map: NodeId → HirId }`,
/// copied from ark's `hir_map.rs`. That is correct **in ark**, whose `NodeId`
/// comes from one process-global `AtomicUsize` (`arkc-parser/src/parser.rs`), and
/// it does not transfer. Both directions are qualified here, not just the reverse
/// one: a `node_of` that returned a bare `NodeId` would hand back a number the
/// caller cannot interpret without already knowing the file.
/// Note the absent `Ord`: `yelc_base::SourceId` does not implement it, and this
/// crate does not get to add it — `yelc-base` is a landed seam. Nothing here
/// needs it (the map is a hash map, and [`NodeMap`](crate::NodeMap) sorts by
/// [`HirId`]), so the ordering is left un-invented rather than worked around.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SourceNodeId {
    pub source: SourceId,
    pub node: NodeId,
}

impl SourceNodeId {
    pub fn new(source: SourceId, node: NodeId) -> Self {
        Self { source, node }
    }
}

/// A written type, by reference to the AST `TypeRef` it was written as.
///
/// # HIR does not re-represent types
///
/// There is no `ParsedType` mirror of `ast::TypeKind`. A mirror is a second tree
/// to keep in step, it narrows what stage 1 deliberately kept wide (`args` are
/// stored *as written*, because `result<a,b,c>` is real input), and the
/// `OnceCell<DefId>` such a mirror wanted is a resolution result stored on the
/// node it describes — [B3](../../../plans/rewrite/anti-spec.md) exactly.
///
/// Instead a HIR entity points at the syntax, and one resolve-and-intern step
/// turns it into a [`Ty`](yelc_sema::Ty) when the definition tables are
/// populated.
///
/// That step is [`crate::lower`]'s `type_of`, on the lowering context — the
/// owner named 2026-07-30 after the seam landed without it (the brief wrote
/// `fn type_of(&mut self, …)` with no receiver, and a memo keyed by the wrong
/// id space; `plans/rewrite/stage-3-hir-build.md` records both defects).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TypeId(pub SourceNodeId);

impl TypeId {
    pub fn new(node: SourceNodeId) -> Self {
        Self(node)
    }

    /// The AST node this refers to.
    pub fn node(self) -> SourceNodeId {
        self.0
    }
}

/// Identifies one local within one [`HirBody`](crate::HirBody) — a parameter,
/// a `let` binding, a loop or arm binder.
///
/// **Body-scoped**: local 0 of one body and local 0 of another are unrelated.
/// Allocation order is source order within the body, and that order is
/// load-bearing — `LocalId` ordinals reach the checker, and D1's uniform prop
/// list is only free if a closure-valued prop's locals come out in the same
/// order the frozen split produced (stage 3 D1, the caveat).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct LocalId(pub u32);

impl LocalId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl Idx for LocalId {
    fn new(raw: u32) -> Self {
        Self(raw)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The property the whole type exists for: the same `NodeId` in two files is
    /// two different nodes. A bare `NodeId` key cannot say this.
    #[test]
    fn the_same_node_id_in_two_files_is_two_nodes() {
        let first = SourceNodeId::new(SourceId::new(0), NodeId::new(7));
        let second = SourceNodeId::new(SourceId::new(1), NodeId::new(7));
        assert_ne!(first, second);
        assert_eq!(first.node, second.node, "the bare NodeIds do collide");
    }

    #[test]
    fn a_type_id_is_the_node_it_was_built_from() {
        let node = SourceNodeId::new(SourceId::new(2), NodeId::new(9));
        assert_eq!(TypeId::new(node).node(), node);
    }
}
