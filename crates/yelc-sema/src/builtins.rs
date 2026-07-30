//! The builtin table.
//!
//! One row per builtin: name, arity, type scheme, lowering target. Replaces the
//! frozen `stdlib_lookup.rs` (1,029 lines) and the `KnownFunctions` half of
//! `known.rs`, which between them implement **one builtin as four things that
//! must agree, checked by nothing** ([F12](../../../plans/rewrite/findings.md)).
//!
//! Populated from Rust today. `directions.md` §2 later changes *where the rows
//! come from* — not what a row is — so nothing here is blocked on it.

use rustc_hash::FxHashMap;
use yelc_base::Name;

use crate::types::Ty;

/// How many arguments a builtin takes.
///
/// # Why variadic is a case rather than N fixed arities (decision C1c)
///
/// `concat` is genuinely variadic — the frozen registration says so in a comment
/// it cannot enforce (`stdlib_lookup.rs:293`, `// concat: func(string...) ->
/// string`) while declaring an empty parameter list.
///
/// The alternative, "register `concat` at N fixed arities", has no principled
/// `N`: **string interpolation desugars to `concat` with one argument per
/// part**, and a 10-part interpolation compiles today. So the N+1 case would
/// fail on a call *the user never wrote* — the desugaring is compiler-generated
/// — and the diagnostic would name `concat` at a source position containing a
/// string literal.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Arity {
    Fixed(usize),
    /// At least `min` arguments, every one of type `element`.
    Variadic {
        min: usize,
        element: Ty,
    },
}

impl Arity {
    pub fn accepts(&self, count: usize) -> bool {
        match self {
            Arity::Fixed(n) => count == *n,
            Arity::Variadic { min, .. } => count >= *min,
        }
    }

    /// Human-readable expectation, for diagnostics.
    pub fn describe(&self) -> String {
        match self {
            Arity::Fixed(0) => "no arguments".to_string(),
            Arity::Fixed(1) => "1 argument".to_string(),
            Arity::Fixed(n) => format!("{n} arguments"),
            Arity::Variadic { min: 0, .. } => "any number of arguments".to_string(),
            Arity::Variadic { min, .. } => format!("at least {min} arguments"),
        }
    }
}

/// What a builtin lowers to. Read by lowering; **never** by the checker.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LoweringTarget {
    /// A generic op, named. Resolved by the lowering stage, which owns the
    /// vocabulary — this crate must not know what the op *does*.
    Op(&'static str),
    /// An import from the host interface.
    HostImport {
        interface: &'static str,
        func: &'static str,
    },
}

/// Whether a user may write this name.
///
/// `concat` and the `*-to-string` family are real rows the checker must type
/// and lowering must find, but `LANGUAGE.md` documents none of them because a
/// user never writes one — they are targets of desugarings. Recording that as a
/// field beats omitting them, which is how `concat` ended up with a comment
/// saying it was variadic and a declaration saying it took nothing.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Visibility {
    /// Documented in `LANGUAGE.md`; resolvable from source.
    UserFacing,
    /// A desugaring target. Not name-resolvable from source.
    Internal,
}

/// One builtin, in one place.
#[derive(Clone, Debug)]
pub struct Builtin {
    pub name: Name,
    pub arity: Arity,
    /// Parameter types. A [`Arity::Variadic`] builtin lists its fixed prefix
    /// here; the repeating tail is `Arity::Variadic::element`.
    pub params: Vec<Ty>,
    pub ret: Option<Ty>,
    pub lowering: LoweringTarget,
    pub visibility: Visibility,
}

/// Index into [`BuiltinTable`]. Dense, stable within one table.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BuiltinId(pub u32);

/// All builtins, and the two views onto them.
///
/// # One table, two accessors (decision C1a)
///
/// Not two tables plus a key-alignment test. [F12](../../../plans/rewrite/findings.md)
/// is exactly the failure of *"four things that must agree, checked by
/// nothing"*; two unchecked things is the same bug at smaller scale, and the
/// alignment test is the part that rots first because it passes for years. One
/// row makes misalignment **unrepresentable** rather than tested.
///
/// The constraint that `yelc-lir` must see neither accessor is enforced by the
/// crate graph — lir has no dependency path here — so it is not paid for again
/// in table shape.
///
/// # Builtin *elements* are not in here (decision C1b)
///
/// An element has no arity, no type scheme in this sense, and no lowering
/// target: three dead columns on every element row. A table whose columns are
/// meaningless for half its rows is two tables sharing a name. They live in
/// [`crate::known`].
#[derive(Default)]
pub struct BuiltinTable {
    rows: Vec<Builtin>,
    by_name: FxHashMap<Name, Vec<BuiltinId>>,
}

impl BuiltinTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, builtin: Builtin) -> BuiltinId {
        let id = BuiltinId(self.rows.len() as u32);
        self.by_name.entry(builtin.name).or_default().push(id);
        self.rows.push(builtin);
        id
    }

    pub fn get(&self, id: BuiltinId) -> &Builtin {
        &self.rows[id.0 as usize]
    }

    /// **Accessor 1 — the checker's view.** Every overload of `name`.
    ///
    /// Returns a slice rather than one row because `len` is both
    /// `list<T> -> s32` and `string -> s32`; picking between them needs the
    /// argument types, which is the caller's job, not the table's.
    pub fn overloads(&self, name: Name) -> &[BuiltinId] {
        self.by_name.get(&name).map_or(&[], Vec::as_slice)
    }

    /// **Accessor 2 — the lowering stage's view.**
    pub fn lowering(&self, id: BuiltinId) -> &LoweringTarget {
        &self.rows[id.0 as usize].lowering
    }

    pub fn len(&self) -> usize {
        self.rows.len()
    }

    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    /// Every row, in registration order.
    ///
    /// Registration order, not `by_name` order — anything derived from a map
    /// must be deterministic before it reaches output
    /// ([A6](../../../plans/rewrite/anti-spec.md)).
    pub fn iter(&self) -> impl Iterator<Item = (BuiltinId, &Builtin)> {
        self.rows
            .iter()
            .enumerate()
            .map(|(i, b)| (BuiltinId(i as u32), b))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yelc_base::Interner;

    fn table() -> (Interner, BuiltinTable) {
        let interner = Interner::new();
        let mut table = BuiltinTable::new();
        table.register(Builtin {
            name: interner.intern("concat"),
            arity: Arity::Variadic {
                min: 0,
                element: Ty::STRING,
            },
            params: Vec::new(),
            ret: Some(Ty::STRING),
            lowering: LoweringTarget::Op("concat"),
            visibility: Visibility::Internal,
        });
        table.register(Builtin {
            name: interner.intern("len"),
            arity: Arity::Fixed(1),
            params: vec![Ty::STRING],
            ret: Some(Ty::S32),
            lowering: LoweringTarget::Op("string_len"),
            visibility: Visibility::UserFacing,
        });
        (interner, table)
    }

    /// The point of C1c: a compiler-generated `concat` call can have any number
    /// of arguments, so no fixed arity is correct.
    #[test]
    fn a_variadic_builtin_accepts_an_interpolations_worth_of_arguments() {
        let (interner, table) = table();
        let concat = table.overloads(interner.intern("concat"))[0];
        for parts in [0, 1, 2, 10, 64] {
            assert!(
                table.get(concat).arity.accepts(parts),
                "concat rejected {parts} arguments; interpolation generates one per part",
            );
        }
    }

    #[test]
    fn a_fixed_builtin_rejects_the_wrong_count() {
        let (interner, table) = table();
        let len = table.overloads(interner.intern("len"))[0];
        assert!(table.get(len).arity.accepts(1));
        assert!(!table.get(len).arity.accepts(0));
        assert!(!table.get(len).arity.accepts(2));
    }

    /// C1a: both views read the same row, so they cannot disagree.
    #[test]
    fn both_accessors_address_the_same_row() {
        let (interner, table) = table();
        let id = table.overloads(interner.intern("len"))[0];
        assert_eq!(table.get(id).name, interner.intern("len"));
        assert_eq!(table.lowering(id), &LoweringTarget::Op("string_len"));
    }

    #[test]
    fn a_name_can_carry_several_overloads() {
        let (interner, mut table) = table();
        table.register(Builtin {
            name: interner.intern("len"),
            arity: Arity::Fixed(1),
            params: vec![Ty::ERROR], // list<T>, once generics exist
            ret: Some(Ty::S32),
            lowering: LoweringTarget::Op("list_len"),
            visibility: Visibility::UserFacing,
        });
        assert_eq!(table.overloads(interner.intern("len")).len(), 2);
    }

    /// Iteration must not inherit the hash map's order — A6.
    ///
    /// **Asserted on the rows, not on the ids.** The version of this test that
    /// stood until 2026-07-30 collected only `(id, _)`, and `iter()` synthesizes
    /// those ids from `enumerate()` — so reversing the underlying rows still
    /// yielded `[BuiltinId(0), BuiltinId(1)]` and the test passed while every
    /// row was paired with the wrong id. Reversing now fails on both the names
    /// and the id→row agreement.
    #[test]
    fn iteration_is_registration_order() {
        let (interner, table) = table();
        let seen: Vec<(BuiltinId, String)> = table
            .iter()
            .map(|(id, builtin)| (id, interner.str(builtin.name).to_string()))
            .collect();
        assert_eq!(
            seen,
            vec![
                (BuiltinId(0), "concat".to_string()),
                (BuiltinId(1), "len".to_string()),
            ],
            "iteration must yield the rows in the order they were registered",
        );

        // And the id a row is yielded with must address that same row — the
        // property C1a exists to make unrepresentable.
        for (id, builtin) in table.iter() {
            assert_eq!(table.get(id).name, builtin.name);
            assert_eq!(table.get(id).lowering, builtin.lowering);
        }
    }
}
