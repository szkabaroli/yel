//! The unit stage 3 produces and stage 4 annotates.

use rustc_hash::FxHashMap;
use yelc_base::{IndexVec, Name, SourceId};
use yelc_sema::{DefId, PackageId, Ty};

use crate::expr::{HirBlock, HirLocal};
use crate::ids::{BodyId, HirId, HirItemId, LocalId};
use crate::map::HirMap;
use crate::node_map::NodeMap;

/// One top-level item: a component or a global.
///
/// # Where everything else went
///
/// A record, enum, variant, element or extern component declares *shape* and no
/// behaviour — it registers a definition with member rows
/// ([`Definitions`](yelc_sema::Definitions)) and there is nothing left to
/// lower, so it has no HIR item. The frozen tree reached the same two-variant
/// spine and it is kept by writing, not by copying
/// (`plans/rewrite/README.md` § read, do not port).
///
/// What was decided before this vocabulary was written, honoured here:
/// **one uniform item spine** (D1 — no parallel pipelines), and **globals carry
/// no body, only their functions** (D4).
#[derive(Debug)]
pub enum HirItem {
    Component(HirComponent),
    Global(HirGlobal),
    /// A root function — the .yelir subset's `name: func(…) { … }` at item
    /// or module level. No owner row: `member` on the inner function is 0
    /// and meaningless, which is why the `DefId` rides alongside.
    Function {
        def: DefId,
        function: HirFunction,
    },
}

impl HirItem {
    pub fn def(&self) -> DefId {
        match self {
            HirItem::Component(component) => component.def,
            HirItem::Global(global) => global.def,
            HirItem::Function { def, .. } => *def,
        }
    }

    pub fn name(&self) -> Name {
        match self {
            HirItem::Component(component) => component.name,
            HirItem::Global(global) => global.name,
            HirItem::Function { function, .. } => function.name,
        }
    }
}

/// A component: its member functions, its property defaults, and its UI tree
/// desugared to one **build body** whose value is a builder expression.
#[derive(Debug)]
pub struct HirComponent {
    pub hir_id: HirId,
    pub def: DefId,
    pub name: Name,
    pub is_export: bool,
    /// Reactive-property defaults, in member order. Kept on the item and not in
    /// the member table because a default is an *expression*, and `yelc-sema`
    /// sits below the expression vocabulary — the same wall that placed
    /// `type_of`. D4's letter ("defaults stay in the definition") bends here so
    /// its spirit (no body on the definition side) can hold.
    pub defaults: Vec<HirDefault>,
    /// User-written functions, source order.
    pub functions: Vec<HirFunction>,
    /// The desugared UI tree. Its tail is a [`Fragment`] of the root builder
    /// expressions; a component with no UI nodes has a build body with an
    /// empty one. Handler closures inside it are ordinary [`Closure`] bodies.
    ///
    /// [`Fragment`]: crate::HirExprKind::Fragment
    /// [`Closure`]: crate::HirExprKind::Closure
    pub build: BodyId,
}

/// A global: a host-boundary singleton. No body, no build — only its functions
/// (D4) and its property defaults.
#[derive(Debug)]
pub struct HirGlobal {
    pub hir_id: HirId,
    pub def: DefId,
    pub name: Name,
    pub is_export: bool,
    pub defaults: Vec<HirDefault>,
    /// Callbacks. `body: None` means the host implements it.
    pub functions: Vec<HirFunction>,
}

/// One member property's default expression, as a body of its own — a default
/// is evaluated outside any function, so it cannot borrow another body's local
/// arena, and a closure written as a default needs one.
#[derive(Debug)]
pub struct HirDefault {
    /// Index into the owner's rows in
    /// [`Definitions::members`](yelc_sema::Definitions::members).
    pub member: u32,
    /// A parameterless body whose tail is the default's value.
    pub body: BodyId,
}

/// One member function: a component function, a global callback.
#[derive(Debug)]
pub struct HirFunction {
    pub hir_id: HirId,
    pub name: Name,
    /// Index into the owner's member rows — the declared `Func` type lives
    /// there after phase 2.
    pub member: u32,
    pub is_export: bool,
    /// `None` when someone else implements it: a host callback, an extern
    /// method. Not a hole — the ordinary case for a global.
    pub body: Option<BodyId>,
}

/// One body: a function's, a closure's, a setter's, a component's build.
///
/// Parameters are the leading locals; `locals` is every local in **allocation
/// order**, which is source order — the order is load-bearing (`LocalId`
/// ordinals reach the checker; stage 3 D1's caveat).
#[derive(Debug)]
pub struct HirBody {
    pub hir_id: HirId,
    /// The leading `params.len()` entries of `locals`.
    pub params: u32,
    pub locals: IndexVec<LocalId, HirLocal>,
    pub block: HirBlock,
}

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
    /// D6 — doc comments, attached: the nearest preceding **`///`** run with
    /// no blank line between it and the item, joined in source order, `///`
    /// and one leading space stripped. Plain `//` is commentary and never
    /// attaches (Rust's line; WIT's parser blurs the two and any comment
    /// becomes docs — read and deliberately not copied). A side table, not a
    /// node field (B3).
    ///
    /// Keyed by `DefId` rather than the decision's literal `NodeMap<HirId>`:
    /// the member-row design left type declarations (records, enums,
    /// variants, elements, externs) with no HIR node to key on, and `DefId`
    /// is the id space that covers every registered item. Top-level
    /// definitions only for now; member docs follow with the LSP work.
    /// Write-once, asserted in [`HirModule::attach_doc`]. Iterate sorted when
    /// output needs it (A6) — the map itself is hash-ordered.
    ///
    /// Known edge, recorded not chased: an item whose span starts *after* its
    /// `@attribute` list would have the run broken by the attribute line.
    /// No top-level item carries attributes in practice yet (intrinsics are
    /// members, and members are not covered by v1); when one does, the fix is
    /// to anchor the scan at the attribute list's start.
    pub docs: FxHashMap<DefId, Name>,
    /// Signal dependencies per body — what each reads and writes of reactive
    /// state, keyed by the body's `hir_id`. Computed at the end of stage 3 on
    /// the resolved, desugared bodies; sorted and deduplicated (A6). The
    /// frozen equivalent lived on `CompilerContext` (`signal_deps`), and D0a
    /// moved it here: analysis about HIR belongs beside HIR (B3's shape).
    pub dependencies: NodeMap<crate::signalck::BodyDependencies>,
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
    /// Attach an item's doc text. Write-once: two docs for one definition is
    /// a lowering bug, not a merge.
    pub fn attach_doc(&mut self, def: DefId, text: Name) {
        let previous = self.docs.insert(def, text);
        assert!(
            previous.is_none(),
            "definition {def:?} was given a second doc comment"
        );
    }

    /// An empty package, before anything is lowered into it.
    pub fn new(id: PackageId, sources: Vec<SourceId>) -> Self {
        Self {
            id,
            sources,
            items: IndexVec::new(),
            bodies: IndexVec::new(),
            map: HirMap::new(),
            docs: FxHashMap::default(),
            dependencies: NodeMap::new(),
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
