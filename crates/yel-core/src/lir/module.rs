//! Module-scoped LIR: the compilation unit.
//!
//! A `LirModule` aggregates every artifact that a Yel module produces during
//! compilation: all components (exported or not), module-scoped global
//! singleton property defaults, and the package identity. Codegen consumes a
//! single `LirModule` per compile, not a loose `Vec<LirResource>` + side
//! tables.

use std::collections::HashMap;

use crate::ids::{DefId, InterfaceId};
use crate::index_vec::IndexVec;
use crate::interner::Name;
use crate::syntax::ast::PackageId;
use crate::types::Ty;

use super::expr::LirExpr;
use super::node::LirResource;

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

/// One freestanding function in an interface contract — a plain WIT
/// signature. Frontend-agnostic: a DOM import, a global's `set-<prop>` /
/// `on-<prop>-changed` accessor, and (later) a flow node port all lower
/// into this same shape. The renderer never learns what a function
/// "meant" — it emits `name(params) -> result` and a matching import,
/// keyed by `def`.
#[derive(Debug, Clone)]
pub struct LirIfaceFn {
    /// Kebab WIT name, already lowered (`set-attribute`, `set-count`,
    /// `on-count-changed`).
    pub name: Name,
    /// Parameter list: `(name, type)` pairs in order.
    pub params: Vec<(Name, Ty)>,
    /// Result type; `None` is unit.
    pub result: Option<Ty>,
    /// The callable identity the core module's call sites key on, so
    /// codegen can correlate this signature to its import/export slot.
    pub def: DefId,
}

/// A Yel module — one or more `.yel` files compiled together.
///
/// Holds every LIR artifact produced for the module: components, global
/// singleton defaults, and the package header. Successive compiler passes
/// attach further module-scope state (callbacks, type registry, etc.) here.
#[derive(Debug, Clone, Default)]
pub struct LirModule {
    /// All exported instantiable units declared in the module (exported
    /// and private alike) — UI components today, flow nodes later. Each
    /// surfaces in the component model as a resource.
    pub resources: Vec<LirResource>,
    /// LIR-lowered default expressions for global singleton properties,
    /// keyed by property `DefId`. Each value is a top-level expression whose
    /// `LirExprId` children index into [`Self::global_default_exprs`]. The
    /// module start function seeds the backing slot for each entry at
    /// instantiation time.
    pub global_defaults: HashMap<DefId, LirExpr>,
    /// Shared expression arena for the global-default expressions. The
    /// top-level nodes live in [`Self::global_defaults`]; every child handle
    /// they hold indexes into this arena.
    pub global_default_exprs: Vec<LirExpr>,
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

    pub fn has_exports(&self) -> bool {
        self.resources.iter().any(|c| c.is_export)
    }
}
