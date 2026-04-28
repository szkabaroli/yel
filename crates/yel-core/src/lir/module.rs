//! Module-scoped LIR: the compilation unit.
//!
//! A `LirModule` aggregates every artifact that a Yel module produces during
//! compilation: all components (exported or not), module-scoped global
//! singleton property defaults, and the package identity. Codegen consumes a
//! single `LirModule` per compile, not a loose `Vec<LirComponent>` + side
//! tables.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::ids::DefId;
use crate::syntax::ast::PackageId;

use super::expr::LirExpr;
use super::node::LirComponent;

/// A Yel module — one or more `.yel` files compiled together.
///
/// Holds every LIR artifact produced for the module: components, global
/// singleton defaults, and the package header. Successive compiler passes
/// attach further module-scope state (callbacks, type registry, etc.) here.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LirModule {
    /// All components declared in the module (exported and private alike).
    pub components: Vec<LirComponent>,
    /// LIR-lowered default expressions for global singleton properties,
    /// keyed by property `DefId`. The module start function seeds the
    /// backing memory slot for each entry at instantiation time.
    pub global_defaults: HashMap<DefId, LirExpr>,
    /// Module package header (from a single `package` decl across the
    /// module's files). `None` for anonymous modules (tests / shims).
    pub package: Option<PackageId>,
}

impl LirModule {
    pub fn new() -> Self {
        Self::default()
    }

    /// Exported-components view used by WIT and codegen export loops.
    pub fn exported_components(&self) -> impl Iterator<Item = &LirComponent> {
        self.components.iter().filter(|c| c.is_export)
    }

    pub fn has_exports(&self) -> bool {
        self.components.iter().any(|c| c.is_export)
    }
}
