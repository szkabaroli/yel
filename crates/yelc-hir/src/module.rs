//! The unit stage 3 produces and stage 4 annotates.

use yelc_base::{IndexVec, SourceId};
use yelc_sema::{PackageId, Ty};

use crate::ids::{BodyId, HirItemId};
use crate::map::HirMap;
use crate::node_map::NodeMap;

/// One top-level item: a component, a global, an element, an extern component,
/// a record, an enum, a variant.
///
/// # Deliberately uninhabited, and that is the seam
///
/// Phase 2 lands the *types*; phase 3 declares this vocabulary and writes the
/// lowering ([`plans/rewrite/stage-3-hir-build.md` § Phase 2](../../../plans/rewrite/stage-3-hir-build.md)).
/// An empty enum is the honest form of *"not yet decided"*: it cannot be
/// constructed, so nothing can be lowered into it by accident and no placeholder
/// variant exists to become permanent
/// ([no silent fallbacks](../../../plans/rewrite/keep-list.md)).
///
/// What is already decided about it, so phase 3 does not re-derive it:
/// **one uniform item spine** — a real `{Component, Global, …}` enum, not two
/// parallel pipelines ([D1](../../../plans/rewrite/anti-spec.md)) — and
/// **globals carry no body**, only their functions
/// ([D4](../../../plans/rewrite/stage-3-hir-build.md)).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirItem {}

/// One body: a function's, a handler's, a closure's, a generated region's.
///
/// Uninhabited for the same reason as [`HirItem`]. Bodies are held here and
/// reached by [`BodyId`] rather than nested inside items, so a pass that only
/// needs signatures never walks one.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirBody {}

/// The HIR for one package.
///
/// # The noun is one level off, and it is flagged rather than fixed
///
/// `fbaa95e` renamed `ModuleId` → `PackageId` because *"the noun was one level
/// off"*: the thing compiled, versioned and serialized is the **package**, and
/// `module` is becoming a surface keyword meaning *WIT interface*
/// ([`plans/modules.md` §6](../../../plans/modules.md)). `ModuleId` now means
/// something else entirely — one symbol-table node per `include`.
///
/// The same argument applies to this type's name: it holds a [`PackageId`], it
/// spans a set of sources, and [D8](../../../plans/rewrite/stage-3-hir-build.md)
/// says it *is* the package. `HirPackage` is what that reasoning produces. It
/// landed as `HirModule` because that is the name the seam contract gives, and
/// inventing contract while implementing it is what the types-before-body
/// sequencing exists to prevent. **Decide before phase 3 writes the lowering.**
#[derive(Debug)]
pub struct HirModule {
    /// What this package *is*.
    ///
    /// Not a `SourceId`. A field that identifies the thing it is on is different
    /// from a field that happens to be available on it — the distinction
    /// [D8](../../../plans/rewrite/stage-3-hir-build.md) turns on, and the reason
    /// a serialized cross-package reference cannot say *"the package whose first
    /// file was `foo.yel`"*.
    pub id: PackageId,
    /// The files it was built from, in the order they were handed to
    /// [`lower_files`](crate::lower_files). Provenance, for diagnostics — the
    /// second kind of field.
    pub sources: Vec<SourceId>,
    pub items: IndexVec<HirItemId, HirItem>,
    pub bodies: IndexVec<BodyId, HirBody>,
    /// Where every HIR node came from. See [`HirMap`].
    pub map: HirMap,
    /// The type of every expression.
    ///
    /// # Empty after stage 3, total after stage 4
    ///
    /// Stage 3 **does not populate this**, and that is a contract rather than an
    /// omission: HIR is name-resolved before it is typed, so an entry written
    /// here during the build would be a guess. Declared types — fields, property
    /// types, parameters, returns, variant payloads — go into
    /// [`Definitions`](yelc_sema::Definitions) instead, which is rustc's
    /// `type_of(def_id)`-before-body-check split.
    ///
    /// Stage 4's postcondition is that this is total **and contains no
    /// unresolved inference variable** — `TyKind::Infer` must not outlive
    /// checking, and `TyKind::Param` must not outlive substitution
    /// ([A3/A4](../../../plans/rewrite/open-decisions.md)).
    pub types: NodeMap<Ty>,
}

impl HirModule {
    /// An empty package, before anything is lowered into it.
    pub fn new(id: PackageId, sources: Vec<SourceId>) -> Self {
        Self {
            id,
            sources,
            items: IndexVec::new(),
            bodies: IndexVec::new(),
            map: HirMap::new(),
            types: NodeMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_package_is_identified_by_itself_and_carries_its_files_as_provenance() {
        let sources = vec![SourceId::new(0), SourceId::new(1)];
        let module = HirModule::new(PackageId::LOCAL, sources.clone());

        assert_eq!(module.id, PackageId::LOCAL);
        assert_eq!(module.sources, sources);
    }

    /// The documented postcondition of stage 3, asserted rather than commented.
    #[test]
    fn the_types_table_is_empty_before_stage_4_runs() {
        let module = HirModule::new(PackageId::LOCAL, vec![]);
        assert!(module.types.is_empty());
        assert!(module.items.is_empty());
        assert!(module.bodies.is_empty());
        assert!(module.map.is_empty());
    }
}
