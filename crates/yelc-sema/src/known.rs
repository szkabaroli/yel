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

use crate::definitions::{DefKind, Definitions};
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

    /// The name and kind to resolve this against.
    const fn spec(self) -> (&'static str, DefKind) {
        match self {
            Known::Color => ("Color", DefKind::Type),
        }
    }

    pub const fn source_name(self) -> &'static str {
        self.spec().0
    }

    /// What the definition must *be*.
    ///
    /// Not a namespace to look in — lookup is single-namespace. It is a check on
    /// the one definition the name resolves to: a `Color` declared as a global
    /// means the program has no `Color` record, and reporting it missing is then
    /// the correct answer rather than a lookup miss.
    pub const fn kind(self) -> DefKind {
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
            match defs.lookup_def(name, item.kind()) {
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
                item.kind(),
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
    ///
    /// Also the standing assertion that resolution **accumulates**: the
    /// expectation is `Known::ALL`, not a literal, so an early return from the
    /// loop fails it as soon as the inventory has a second entry. See
    /// [`the_message_names_every_entry_it_carries`] for why that is the best
    /// available statement of the property today.
    #[test]
    fn an_incomplete_registration_fails_at_resolve() {
        let interner = Interner::new();
        let defs = Definitions::new(PackageId::LOCAL); // nothing registered
        let err = KnownItems::resolve(&defs, &interner).unwrap_err();
        assert_eq!(err.missing, Known::ALL.to_vec());
    }

    /// The **message** names every entry it carries, not just the first.
    ///
    /// # What this replaces, and why the replacement is smaller
    ///
    /// Until 2026-07-30 this test was `every_missing_entry_is_reported_not_just_the_first`
    /// and drove [`KnownItems::resolve`] with an empty table. It was **vacuous
    /// and could not be made otherwise**: `Known::ALL` has one element, so a
    /// `break` after the first miss passed it. There is no witness for
    /// "accumulates rather than stops" in a one-element loop, and manufacturing
    /// one — a second `Known` variant with no consumer, or a `resolve` that
    /// takes the item list only so a test can pass a different one — is the
    /// shape-only port [A9](../../../plans/rewrite/anti-spec.md) forbids.
    ///
    /// So the claim is split. The half that *is* falsifiable today is the
    /// rendering, tested here against a value carrying two entries. The
    /// accumulation half is asserted structurally by
    /// [`an_incomplete_registration_fails_at_resolve`] — `err.missing ==
    /// Known::ALL`, written against the inventory rather than a literal, so it
    /// becomes a real multi-entry assertion the moment a second lang-item
    /// lands, with no test to remember to update.
    #[test]
    fn the_message_names_every_entry_it_carries() {
        let one = MissingKnownItems {
            missing: vec![Known::Color],
        };
        assert_eq!(
            one.to_string(),
            "builtin registration is incomplete; missing: Color",
        );

        let two = MissingKnownItems {
            missing: vec![Known::Color, Known::Color],
        };
        assert_eq!(
            two.to_string(),
            "builtin registration is incomplete; missing: Color, Color",
            "the message stopped after the first entry",
        );
    }

    /// The registration and the resolution loop the **same** inventory, so an
    /// entry cannot be one without the other.
    ///
    /// This is the half of A1 that lives here: `stdlib::register_builtins` is
    /// where the registration happens and
    /// `stdlib::tests::register_builtins_registers_the_lang_items_into_definitions`
    /// is where it is checked, but the property is about this file's
    /// `Known::ALL` and belongs beside it.
    #[test]
    fn resolution_covers_the_whole_inventory() {
        let interner = Interner::new();
        let defs = registered_all(&interner);
        let known = KnownItems::resolve(&defs, &interner).unwrap();
        for &item in Known::ALL {
            assert_eq!(
                defs.get(known.get(item)).name,
                interner.intern(item.source_name()),
                "`{}` resolved to a definition with another name",
                item.source_name(),
            );
        }
    }

    /// A `Known` whose name is taken by a definition of another kind is
    /// **missing**, not silently matched against it.
    ///
    /// The lookup is single-namespace, so this is no longer "it looked in a
    /// different index space" — the name resolves to exactly one definition and
    /// that definition is the wrong kind. The answer is the same and the reason
    /// is not.
    #[test]
    fn a_known_of_the_wrong_kind_is_missing() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        defs.register(interner.intern("Color"), DefKind::Value, span(), false)
            .unwrap();
        assert!(KnownItems::resolve(&defs, &interner).is_err());
    }
}
