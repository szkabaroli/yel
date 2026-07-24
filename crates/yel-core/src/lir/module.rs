//! Module-scoped LIR: the compilation unit.
//!
//! A `LirModule` aggregates every artifact that a Yel module produces during
//! compilation: all components (exported or not), module-scoped global
//! singleton property defaults, and the package identity. Codegen consumes a
//! single `LirModule` per compile, not a loose `Vec<LirResource>` + side
//! tables.

use rustc_hash::FxHashMap as HashMap;

use crate::definitions::GlobalPropDirection;
use crate::ids::{DefId, InterfaceId};
use crate::index_vec::IndexVec;
use crate::interner::Name;
use crate::syntax::ast::PackageId;
use crate::types::Ty;

use super::arena::{LirExprArena, LirResourceArena, LirSlotArena, LirStringArena};
use super::block::{LirBlock, LirSlotInfo, StringId};
use super::expr::LirExpr;
use super::node::LirResource;
use super::struct_types::{LirArrayTypeDecl, LirStructTypeDecl};

/// Whether a world item is imported from or exported to the host.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterfaceDirection {
    Import,
    Export,
}

/// A WIT interface declared in the module's world: a named group of
/// functions / types / a resource, imported or exported. Members
/// (resources, imported functions) reference one by [`InterfaceId`].
///
/// This mirrors the wit-parser world model directly: an interface-grouped
/// item surfaces as `WorldKey::Interface(id) → WorldItem::Interface`,
/// whereas an import with no interface is a freeform world function
/// (`WorldKey::Name → WorldItem::Function`).
#[derive(Debug, Clone)]
pub struct LirInterface {
    /// Local kebab name within its package (e.g. `theme`, `app-component`,
    /// `dom`). The fully-qualified WIT name composes as
    /// `{package}:{…}/{name}@{version}`.
    pub name: Name,
    /// Imported from the host, or exported to it.
    pub direction: InterfaceDirection,
    /// Owning package. `None` = this module's own package; `Some` = a
    /// foreign package (e.g. `yel:ui` for the DOM interface).
    pub package: Option<PackageId>,
    /// ADTs (record/variant/enum) this interface defines **inline**. A
    /// foreign interface owns its own types (a host package can't `use`
    /// the module's shared types); a local interface leaves this empty and
    /// its function signatures reference the module's shared-types
    /// interface.
    pub owned_types: Vec<Ty>,
    /// Resources nested in this interface, referenced by their component
    /// `DefId` (an index into [`LirModule::resources`]). The resource
    /// owns its own constructor/method surface.
    pub resources: Vec<DefId>,
    /// Freestanding functions in this interface (host imports for DOM /
    /// globals; the module-scoped `dispatch` export). Resource
    /// constructors/methods are NOT here — they live on the resource.
    pub functions: Vec<LirIfaceFn>,
}

/// What receiver a host-boundary function takes as its implicit first
/// parameter — the frontend-agnostic encoding of "resource method vs.
/// freestanding".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LirReceiver {
    /// No receiver — a freestanding function (global / DOM callbacks,
    /// module dispatch). At the WIT boundary it is `FunctionKind::Freestanding`;
    /// at the core ABI it takes no leading handle.
    None,
    /// Takes `borrow<resource>` as its first parameter, where the resource
    /// is the component identified by this `DefId` (component callbacks).
    /// At the core ABI this lowers to a leading `i32` handle.
    Borrow(DefId),
}

/// One function in an interface contract — a plain WIT signature plus its
/// receiver. Frontend-agnostic: a DOM import, a global's `set-<prop>` /
/// `on-<prop>-changed` accessor, a component callback, and (later) a flow
/// node port all lower into this same shape. The renderer never learns what
/// a function "meant" — it emits `name(params) -> result` and, when part of
/// the import registry, a matching import keyed by `def`.
#[derive(Debug, Clone)]
pub struct LirIfaceFn {
    /// Kebab WIT name, already lowered (`set-attribute`, `set-count`,
    /// `on-count-changed`).
    pub name: Name,
    /// Parameter list: `(name, type)` pairs in order. Does NOT include the
    /// receiver — [`Self::receiver`] carries that.
    pub params: Vec<(Name, Ty)>,
    /// Result type; `None` is unit.
    pub result: Option<Ty>,
    /// Receiver taken as the implicit first parameter.
    pub receiver: LirReceiver,
    /// The callable identity the core module's call sites key on, so
    /// codegen can correlate this signature to its import/export slot.
    pub def: DefId,
}

/// A single host-imported function — the unit of the module's import
/// registry ([`LirModule::imports`]). Both the core module's import section
/// (index space + emission) and the WIT import interfaces derive from this
/// one ordered list, so the two can no longer drift (the old code re-derived
/// each independently from `ctx.defs`).
#[derive(Debug, Clone)]
pub struct LirImport {
    /// Callee identity — call sites resolve their import index by this
    /// `DefId`, and it correlates the import to its [`LirIfaceFn`] in the
    /// owning interface.
    pub def_id: DefId,
    /// Source function name (kebab-cased at emission).
    pub name: Name,
    /// The import interface this function belongs to (index into
    /// [`LirModule::interfaces`]).
    pub interface: InterfaceId,
    /// Parameter list `(name, type)`, excluding the receiver.
    pub params: Vec<(Name, Ty)>,
    /// Result type; `None` is unit.
    pub result: Option<Ty>,
    /// Receiver — `Borrow` adds a leading core-ABI handle (component
    /// callbacks); `None` is freestanding (global / DOM callbacks).
    pub receiver: LirReceiver,
}

/// One property backing a [`LirGlobal`]'s singleton state: a `DefId`, its
/// host-boundary direction, and (for properties with a declared default) the
/// LIR-lowered default expression. The default is a top-level node whose
/// `LirExprId` children index into the module-shared [`LirModule::global_exprs`]
/// arena.
#[derive(Debug, Clone)]
pub struct LirGlobalProperty {
    pub def_id: DefId,
    pub direction: GlobalPropDirection,
    pub default: Option<LirExpr>,
}

/// A top-level `global` block, lowered to LIR as a first-class item — the
/// direct peer of [`LirResource`] for components. A global is a host-boundary
/// declaration (its `func`-typed members are host imports) with optional
/// in-tree singleton state (its properties); unlike a component it is never
/// instantiated, so it owns no blocks/signals/effects, only default-value
/// expressions.
#[derive(Debug, Clone)]
pub struct LirGlobal {
    pub def_id: DefId,
    pub name: Name,
    pub is_export: bool,
    /// Owning package for this global's interface. `None` = the module's own
    /// package; `Some` = a foreign package (the built-in `Dom` global lives
    /// in `yel:ui`).
    pub package: Option<PackageId>,
    /// This global's singleton properties, in declaration order.
    pub properties: Vec<LirGlobalProperty>,
    /// Host-imported callback `DefId`s (in declaration order).
    pub callbacks: Vec<DefId>,
}

/// The emission scope for **module-scope expressions** — global-singleton
/// property defaults and module-scope filter predicates — as a minimal
/// [`LirResourceArena`]. It replaces the fabricated `LirResource` carrier the
/// back-end used to synthesize (`DefId::INVALID`, a placeholder block): the
/// shared wasm expression emitter reads its owning scope through the arena
/// traits, so module scope plugs in as a purpose-built adapter that owns only
/// the expression arena its expressions' `LirExprId` children index into.
///
/// It has no signals, slots, blocks, or GC types, because module-scope
/// expressions reference only globals — which resolve through core wasm
/// globals, not a resource. A component-local lookup that leaks in resolves
/// against the empty tables and fails loudly (No-Silent-Fallbacks).
pub struct ModuleScope {
    name: Name,
    exprs: Vec<LirExpr>,
}

impl ModuleScope {
    /// Build a module-scope emission arena over `exprs` (typically a
    /// [`LirModule::global_exprs`] arena).
    pub fn new(name: Name, exprs: Vec<LirExpr>) -> Self {
        Self { name, exprs }
    }
}

impl LirExprArena for ModuleScope {
    fn exprs(&self) -> &[LirExpr] {
        &self.exprs
    }
}

impl LirStringArena for ModuleScope {
    fn string(&self, id: StringId) -> &str {
        unreachable!(
            "ModuleScope owns no interned strings (id {id:?}) — module-scope \
             expressions inline their string literals"
        )
    }
}

impl LirSlotArena for ModuleScope {
    fn slots(&self) -> &[LirSlotInfo] {
        &[]
    }
}

impl LirResourceArena for ModuleScope {
    fn def_id(&self) -> DefId {
        DefId::INVALID
    }
    fn name(&self) -> Name {
        self.name
    }
    fn is_export(&self) -> bool {
        false
    }
    fn blocks(&self) -> &[LirBlock] {
        &[]
    }
    fn struct_types(&self) -> &[LirStructTypeDecl] {
        &[]
    }
    fn array_types(&self) -> &[LirArrayTypeDecl] {
        &[]
    }
}

/// A Yel module — one or more `.yel` files compiled together.
///
/// Holds every LIR artifact produced for the module: components, globals,
/// and the package header. Successive compiler passes attach further
/// module-scope state (callbacks, type registry, etc.) here.
#[derive(Debug, Clone, Default)]
pub struct LirModule {
    /// All exported instantiable units declared in the module (exported
    /// and private alike) — UI components today, flow nodes later. Each
    /// surfaces in the component model as a resource.
    pub resources: Vec<LirResource>,
    /// Every top-level `global` block, as a first-class per-item unit (the
    /// peer of `resources` for the non-instantiable half of the module).
    /// Their default expressions' `LirExprId` children index into the
    /// shared [`Self::global_exprs`] arena.
    pub globals: Vec<LirGlobal>,
    /// The module's host-import registry — every function the core module
    /// imports (component callbacks, global callbacks, DOM), in the order
    /// they are assigned import indices. The single source of truth: the
    /// core module's import section and its WIT import interfaces both
    /// derive from this and [`Self::interfaces`], so they cannot drift.
    pub imports: Vec<LirImport>,
    /// Shared expression arena for every global's default expressions.
    /// Module-scoped rather than per-global because all global defaults are
    /// seeded together at module start and lower through one module scope —
    /// the analogue of a single `LirResource`'s `exprs`, but shared across
    /// the module's globals.
    pub global_exprs: Vec<LirExpr>,
    /// The synthesized module-start **globals-init block** — the LIR plan for
    /// seeding every defaulted global property at instantiation, as an
    /// ordinary `LirBlock` (scratch slots + `EvalExprToSlots`/`GlobalFieldSet`
    /// ops). The backend transcribes it verbatim as the `(start)` function;
    /// the init is thus part of the LIR, not imperative codegen. `None` when
    /// no global property has a default. Its op exprs index into
    /// [`Self::global_exprs`].
    pub global_init_block: Option<LirBlock>,
    /// Every interface the module's world imports or exports. World items
    /// reference these by [`InterfaceId`]; resolve an id here to get the
    /// interface's name, direction, and owning package. Populated by the
    /// frontend during lowering so codegen/WIT consume interface structure
    /// as data rather than re-deriving it from UI conventions.
    pub interfaces: IndexVec<InterfaceId, LirInterface>,
    /// Module package header (from a single `package` decl across the
    /// module's files). `None` for anonymous modules (tests / shims).
    pub package: Option<PackageId>,
}

impl LirModule {
    pub fn new() -> Self {
        Self::default()
    }

    /// Exported-resources view used by WIT and codegen export loops.
    pub fn exported_resources(&self) -> impl Iterator<Item = &LirResource> {
        self.resources.iter().filter(|c| c.is_export)
    }

    /// A `property DefId → default expression` map over every global's
    /// properties, for codegen sites that seed a single property by id.
    /// A plain regrouping of [`Self::globals`] — the defaults already index
    /// into the shared [`Self::global_exprs`] arena, so no id rewriting is
    /// involved.
    pub fn global_defaults_map(&self) -> HashMap<DefId, LirExpr> {
        self.globals
            .iter()
            .flat_map(|g| &g.properties)
            .filter_map(|p| p.default.as_ref().map(|d| (p.def_id, d.clone())))
            .collect()
    }
}
