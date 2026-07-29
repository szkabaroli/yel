//! The definition table: every name the program declares, and what it names.
//!
//! Replaces the frozen `definitions.rs` (742 lines). Registration happens before
//! any body is lowered, which is what makes forward references work in both
//! directions — see stage 3's register-then-lower invariant.

use rustc_hash::FxHashMap;
use yelc_base::{Name, Span};

use crate::ids::{DefId, PackageId};
use crate::types::Ty;

/// Which index space a name lives in.
///
/// Separate namespaces are why a record and a component may share a name
/// without either shadowing the other.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Namespace {
    /// Records, enums, variants.
    Type,
    /// Functions, constants, properties.
    Value,
    /// Components and extern components.
    Component,
    /// Global singletons.
    Global,
}

/// One registered definition.
///
/// Deliberately thin: what a definition *is*, not what analysis later concluded
/// about it ([B3](../../../plans/rewrite/anti-spec.md)). `ty` is the **declared**
/// type, filled at registration from the syntax — not an inference result.
#[derive(Clone, Debug)]
pub struct Definition {
    pub id: DefId,
    pub name: Name,
    pub namespace: Namespace,
    /// Where the name was written. Every diagnostic about this definition
    /// points here.
    pub span: Span,
    /// The declared type, where the syntax gives one. `None` until stage 3
    /// resolves it, never a placeholder.
    pub ty: Option<Ty>,
    /// Whether the definition is published in the package interface.
    pub is_export: bool,
}

/// Every definition in the package being compiled.
pub struct Definitions {
    package: PackageId,
    defs: Vec<Definition>,
    by_name: FxHashMap<(Name, Namespace), DefId>,
}

/// Returned when a name is registered twice in one namespace. The caller pushes
/// the diagnostic — this table reports the collision and keeps the original,
/// rather than deciding how to complain about it.
#[derive(Clone, Copy, Debug)]
pub struct Duplicate {
    /// The definition already holding the name.
    pub existing: DefId,
}

impl Definitions {
    pub fn new(package: PackageId) -> Self {
        Self {
            package,
            defs: Vec::new(),
            by_name: FxHashMap::default(),
        }
    }

    pub fn package(&self) -> PackageId {
        self.package
    }

    /// Register a name. Returns `Err(Duplicate)` if it is taken, leaving the
    /// original in place.
    ///
    /// Registration **continues** after a collision — the caller pushes a
    /// diagnostic and carries on, so one duplicate name does not hide every
    /// later error in the file (`yelc-base`'s accumulate-and-continue policy).
    pub fn register(
        &mut self,
        name: Name,
        namespace: Namespace,
        span: Span,
        is_export: bool,
    ) -> Result<DefId, Duplicate> {
        if let Some(&existing) = self.by_name.get(&(name, namespace)) {
            return Err(Duplicate { existing });
        }
        let id = DefId::new(self.package, self.defs.len() as u32);
        self.defs.push(Definition {
            id,
            name,
            namespace,
            span,
            ty: None,
            is_export,
        });
        self.by_name.insert((name, namespace), id);
        Ok(id)
    }

    pub fn lookup(&self, name: Name, namespace: Namespace) -> Option<DefId> {
        self.by_name.get(&(name, namespace)).copied()
    }

    pub fn get(&self, id: DefId) -> &Definition {
        debug_assert_eq!(
            id.package, self.package,
            "DefId from another package read out of this table",
        );
        &self.defs[id.index as usize]
    }

    /// Record the declared type discovered during resolution.
    pub fn set_ty(&mut self, id: DefId, ty: Ty) {
        debug_assert_eq!(id.package, self.package);
        self.defs[id.index as usize].ty = Some(ty);
    }

    pub fn len(&self) -> usize {
        self.defs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.defs.is_empty()
    }

    /// Every definition, in **registration order**.
    ///
    /// Registration order rather than `by_name` order, because anything derived
    /// from a hash map must be deterministic before it reaches output
    /// ([A6](../../../plans/rewrite/anti-spec.md)) — and this iterator feeds
    /// WIT emission.
    pub fn iter(&self) -> impl Iterator<Item = &Definition> {
        self.defs.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yelc_base::{Interner, SourceId};

    fn span() -> Span {
        Span::new(SourceId::new(0), 0, 1)
    }

    #[test]
    fn namespaces_do_not_collide() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let name = interner.intern("Panel");
        assert!(defs.register(name, Namespace::Type, span(), false).is_ok());
        assert!(
            defs.register(name, Namespace::Component, span(), false)
                .is_ok(),
            "a record and a component may share a name",
        );
    }

    #[test]
    fn a_duplicate_reports_and_keeps_the_original() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let name = interner.intern("R");
        let first = defs.register(name, Namespace::Type, span(), false).unwrap();
        let err = defs
            .register(name, Namespace::Type, span(), false)
            .unwrap_err();
        assert_eq!(err.existing, first);
        assert_eq!(defs.lookup(name, Namespace::Type), Some(first));
        assert_eq!(defs.len(), 1, "the duplicate must not be registered");
    }

    /// DefIds carry their package, so a table can catch a foreign one rather
    /// than silently indexing with it (decision B2).
    #[test]
    fn defids_are_package_qualified() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let id = defs
            .register(interner.intern("R"), Namespace::Type, span(), false)
            .unwrap();
        assert_eq!(id.package, PackageId::LOCAL);
        assert!(id.is_local());
        assert_ne!(id, DefId::new(PackageId(1), id.index));
    }

    /// A6: iteration order must not come from the hash map.
    #[test]
    fn iteration_is_registration_order() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        for name in ["zeta", "alpha", "mu"] {
            defs.register(interner.intern(name), Namespace::Type, span(), false)
                .unwrap();
        }
        let order: Vec<_> = defs
            .iter()
            .map(|d| interner.str(d.name).to_string())
            .collect();
        assert_eq!(order, vec!["zeta", "alpha", "mu"]);
    }

    #[test]
    fn declared_types_start_absent_not_placeholder() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let id = defs
            .register(interner.intern("x"), Namespace::Value, span(), false)
            .unwrap();
        assert_eq!(defs.get(id).ty, None, "no Ty::ERROR placeholder");
        defs.set_ty(id, Ty::S32);
        assert_eq!(defs.get(id).ty, Some(Ty::S32));
    }
}
