//! Definitions the **compiler itself** refers to by name.
//!
//! Replaces the frozen `known.rs` (413 lines). rustc calls this `lang_items`,
//! and it is a real pattern rather than inherited clutter: these are registered
//! in [`Definitions`] like anything else, so ordinary name lookup finds them —
//! but lowering also needs to say *"the `Color` record"* directly, and a name
//! lookup at every such site is both slower and a chance to typo.
//!
//! # Why these are not in the builtin table (decision C1b)
//!
//! A builtin row is `{ arity, type scheme, lowering target }`. An element or a
//! known type has none of the three. Three dead columns per row is two tables
//! sharing a name.
//!
//! # Resolved once, at construction (decision C2)
//!
//! The frozen `KnownDefinitions` is six sub-structs holding **47**
//! `Option<DefId>` fields. Every read is an unwrap-or-diagnostic for a case that
//! cannot occur once registration has run — 47 re-checks of an invariant at
//! points that cannot observe it, which is
//! [A8](../../../plans/rewrite/anti-spec.md) exactly.
//!
//! Here the invariant is established **where it is established**: resolution
//! either produces a complete table or reports everything missing at once, and
//! afterwards every accessor returns a plain [`DefId`].

use rustc_hash::FxHashMap;
use yelc_base::{Interner, Name};

use crate::definitions::{Definitions, Namespace};
use crate::ids::DefId;

/// A definition the compiler refers to by name.
///
/// The inventory grows as lowerings need entries. Because [`Known::ALL`] drives
/// resolution and the match in [`Known::spec`] is exhaustive, adding a variant
/// is a compile error in one place rather than a `None` discovered at runtime.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Known {
    /// The `Color` record. `#ff0000` desugars to `Color.rgba((r, g, b, a))`,
    /// which is the one desugaring that names a definition today.
    Color,
}

impl Known {
    pub const ALL: &'static [Known] = &[Known::Color];

    /// The name and namespace to resolve this against.
    const fn spec(self) -> (&'static str, Namespace) {
        match self {
            Known::Color => ("Color", Namespace::Type),
        }
    }

    pub const fn source_name(self) -> &'static str {
        self.spec().0
    }

    pub const fn namespace(self) -> Namespace {
        self.spec().1
    }
}

/// Everything the compiler names, resolved.
#[derive(Debug)]
pub struct KnownItems {
    resolved: FxHashMap<Known, DefId>,
}

/// Resolution failed. Carries **every** missing entry, not the first — a
/// half-registered builtin set is one bug, and reporting it one name per run is
/// the slowest possible way to find that out.
#[derive(Clone, Debug)]
pub struct MissingKnownItems {
    pub missing: Vec<Known>,
}

impl std::fmt::Display for MissingKnownItems {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "builtin registration is incomplete; missing: ")?;
        for (i, item) in self.missing.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", item.source_name())?;
        }
        Ok(())
    }
}

impl KnownItems {
    /// Resolve every [`Known`] against the definition table.
    ///
    /// Call **after** builtin registration and before any lowering. This is the
    /// single point at which "the builtins are all present" is checked; every
    /// later read is infallible because of it.
    pub fn resolve(defs: &Definitions, interner: &Interner) -> Result<Self, MissingKnownItems> {
        let mut resolved = FxHashMap::default();
        let mut missing = Vec::new();

        for &item in Known::ALL {
            let name: Name = interner.intern(item.source_name());
            match defs.lookup(name, item.namespace()) {
                Some(id) => {
                    resolved.insert(item, id);
                }
                None => missing.push(item),
            }
        }

        if missing.is_empty() {
            Ok(Self { resolved })
        } else {
            Err(MissingKnownItems { missing })
        }
    }

    /// The definition. Infallible — [`KnownItems::resolve`] already proved it
    /// exists, which is the whole point of the type.
    pub fn get(&self, item: Known) -> DefId {
        self.resolved[&item]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ids::PackageId;
    use yelc_base::{SourceId, Span};

    fn span() -> Span {
        Span::new(SourceId::new(0), 0, 1)
    }

    fn registered_all(interner: &Interner) -> Definitions {
        let mut defs = Definitions::new(PackageId::LOCAL);
        for &item in Known::ALL {
            defs.register(
                interner.intern(item.source_name()),
                item.namespace(),
                span(),
                false,
            )
            .unwrap();
        }
        defs
    }

    #[test]
    fn a_complete_registration_resolves_and_reads_infallibly() {
        let interner = Interner::new();
        let defs = registered_all(&interner);
        let known = KnownItems::resolve(&defs, &interner).unwrap();
        for &item in Known::ALL {
            // No unwrap, no Option — that is decision C2.
            let _: DefId = known.get(item);
        }
    }

    /// C2's point: the invariant fails at the place it is established, not at
    /// each of the places that assume it.
    #[test]
    fn an_incomplete_registration_fails_at_resolve() {
        let interner = Interner::new();
        let defs = Definitions::new(PackageId::LOCAL); // nothing registered
        let err = KnownItems::resolve(&defs, &interner).unwrap_err();
        assert_eq!(err.missing, Known::ALL.to_vec());
    }

    /// Reporting one name per run makes finding a half-registered builtin set
    /// take as many runs as there are missing names.
    #[test]
    fn every_missing_entry_is_reported_not_just_the_first() {
        let interner = Interner::new();
        let defs = Definitions::new(PackageId::LOCAL);
        let err = KnownItems::resolve(&defs, &interner).unwrap_err();
        assert_eq!(err.missing.len(), Known::ALL.len());
        for &item in Known::ALL {
            assert!(err.to_string().contains(item.source_name()));
        }
    }

    /// A `Known` resolved in the wrong namespace is missing, not silently
    /// matched against a same-named definition of another kind.
    #[test]
    fn namespace_is_part_of_the_lookup() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        defs.register(interner.intern("Color"), Namespace::Value, span(), false)
            .unwrap();
        assert!(KnownItems::resolve(&defs, &interner).is_err());
    }
}
