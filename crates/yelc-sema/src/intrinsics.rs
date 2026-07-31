//! The intrinsic table.
//!
//! An **intrinsic** maps a compiler-internal symbol to source: the stdlib
//! declares it as an `extern func` under an `@intrinsic(op = "…")` attribute,
//! and a call to it is **inlined** as that op at codegen — for an
//! [`LoweringTarget::Op`] row, no wasm call is ever emitted. `extern` is the
//! honest surface spelling: *someone else implements this*, and here the
//! someone is the compiler.
//!
//! One row per intrinsic: name, arity, type scheme, lowering target. Replaces
//! the frozen `stdlib_lookup.rs` (1,029 lines) and the `KnownFunctions` half
//! of `known.rs`, which between them implement **one builtin as four things
//! that must agree, checked by nothing**
//! ([F12](../../../plans/rewrite/findings.md)).
//!
//! Named `builtins` until 2026-07-31; renamed with the decision that stdlib
//! rows come from yel source. Populated from Rust today — `directions.md` §2
//! changes *where the rows come from*, not what a row is, and the
//! `@intrinsic` declaration form is the second half of that move (which also
//! retires the `primitive` item-form idea `scope.md` was sitting on).

use rustc_hash::FxHashMap;
use yelc_base::Name;

use crate::types::Ty;

// ~~`Arity`~~ — deleted 2026-07-31, and the reasoning is worth the tombstone.
//
// The enum had `Fixed(n)` and `Variadic { min, element }`, and exactly one
// row was variadic: `concat`, because interpolation desugars to one argument
// per part (decision C1c argued there is no principled maximum N). What
// killed it is the std-as-source direction: **every intrinsic must be
// declarable as an `extern func` in yel, and yel has no varargs syntax** — a
// variadic row is a row the stdlib cannot write. So:
//
// - `concat` is now `func(parts: list<string>) -> string` — declarable
//   surface yel. The interpolation desugar emits a **list literal**, whose
//   static length carries the same arity information one position over, so
//   codegen keeps the `concat_N` monomorphization by reading the literal
//   instead of the call.
// - With `Variadic` gone, `Fixed(n)` duplicated `params.len()` — one fact in
//   two places, F12's shape inside the table built to retire F12. Arity is
//   now *derived*: [`Intrinsic::accepts`].
//
// C1c's argument was against N fixed-arity *rows*, and stands; the list-typed
// signature was the option it did not consider.

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

/// One intrinsic, in one place. The arity is `params.len()` — derived, not
/// stored (see the `Arity` tombstone above).
#[derive(Clone, Debug)]
pub struct Intrinsic {
    pub name: Name,
    pub params: Vec<Ty>,
    pub ret: Option<Ty>,
    pub lowering: LoweringTarget,
    pub visibility: Visibility,
}

impl Intrinsic {
    pub fn accepts(&self, count: usize) -> bool {
        count == self.params.len()
    }

    /// Human-readable expectation, for diagnostics.
    pub fn describe_arity(&self) -> String {
        match self.params.len() {
            0 => "no arguments".to_string(),
            1 => "1 argument".to_string(),
            n => format!("{n} arguments"),
        }
    }
}

/// Index into [`IntrinsicTable`]. Dense, stable within one table.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct IntrinsicId(pub u32);

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
/// # Intrinsic *elements* are not in here (decision C1b)
///
/// An element has no arity, no type scheme in this sense, and no lowering
/// target: three dead columns on every element row. A table whose columns are
/// meaningless for half its rows is two tables sharing a name. They live in
/// [`crate::known`].
#[derive(Default)]
pub struct IntrinsicTable {
    rows: Vec<Intrinsic>,
    by_name: FxHashMap<Name, Vec<IntrinsicId>>,
}

impl IntrinsicTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, intrinsic: Intrinsic) -> IntrinsicId {
        let id = IntrinsicId(self.rows.len() as u32);
        self.by_name.entry(intrinsic.name).or_default().push(id);
        self.rows.push(intrinsic);
        id
    }

    pub fn get(&self, id: IntrinsicId) -> &Intrinsic {
        &self.rows[id.0 as usize]
    }

    /// **Accessor 1 — the checker's view.** Every overload of `name`.
    ///
    /// Returns a slice rather than one row because `len` is both
    /// `list<T> -> s32` and `string -> s32`; picking between them needs the
    /// argument types, which is the caller's job, not the table's.
    pub fn overloads(&self, name: Name) -> &[IntrinsicId] {
        self.by_name.get(&name).map_or(&[], Vec::as_slice)
    }

    /// **Accessor 2 — the lowering stage's view.**
    pub fn lowering(&self, id: IntrinsicId) -> &LoweringTarget {
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
    pub fn iter(&self) -> impl Iterator<Item = (IntrinsicId, &Intrinsic)> {
        self.rows
            .iter()
            .enumerate()
            .map(|(i, b)| (IntrinsicId(i as u32), b))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yelc_base::Interner;

    fn table() -> (Interner, IntrinsicTable) {
        let interner = Interner::new();
        let mut table = IntrinsicTable::new();
        table.register(Intrinsic {
            name: interner.intern("concat"),
            // `list<string>` in the real table; ERROR here only because this
            // fixture has no type interner to build one with.
            params: vec![Ty::ERROR],
            ret: Some(Ty::STRING),
            lowering: LoweringTarget::Op("concat"),
            visibility: Visibility::Internal,
        });
        table.register(Intrinsic {
            name: interner.intern("len"),
            params: vec![Ty::STRING],
            ret: Some(Ty::S32),
            lowering: LoweringTarget::Op("string_len"),
            visibility: Visibility::UserFacing,
        });
        (interner, table)
    }

    /// C1c's successor: `concat` takes ONE argument — the parts list — and an
    /// interpolation of any length fits inside it as a list literal. The
    /// unbounded-arity problem moved out of the table and into a value.
    #[test]
    fn concat_takes_one_list_argument_whatever_the_interpolation_length() {
        let (interner, table) = table();
        let concat = table.overloads(interner.intern("concat"))[0];
        assert!(table.get(concat).accepts(1));
        assert!(!table.get(concat).accepts(0));
        assert!(!table.get(concat).accepts(2));
    }

    #[test]
    fn arity_is_the_parameter_count() {
        let (interner, table) = table();
        let len = table.overloads(interner.intern("len"))[0];
        assert!(table.get(len).accepts(1));
        assert!(!table.get(len).accepts(0));
        assert!(!table.get(len).accepts(2));
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
        table.register(Intrinsic {
            name: interner.intern("len"),
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
    /// yielded `[IntrinsicId(0), IntrinsicId(1)]` and the test passed while every
    /// row was paired with the wrong id. Reversing now fails on both the names
    /// and the id→row agreement.
    #[test]
    fn iteration_is_registration_order() {
        let (interner, table) = table();
        let seen: Vec<(IntrinsicId, String)> = table
            .iter()
            .map(|(id, intrinsic)| (id, interner.str(intrinsic.name).to_string()))
            .collect();
        assert_eq!(
            seen,
            vec![
                (IntrinsicId(0), "concat".to_string()),
                (IntrinsicId(1), "len".to_string()),
            ],
            "iteration must yield the rows in the order they were registered",
        );

        // And the id a row is yielded with must address that same row — the
        // property C1a exists to make unrepresentable.
        for (id, intrinsic) in table.iter() {
            assert_eq!(table.get(id).name, intrinsic.name);
            assert_eq!(table.get(id).lowering, intrinsic.lowering);
        }
    }
}
