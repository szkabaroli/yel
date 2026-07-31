//! The phase-1 standalone checkpoint: the builtin tables, frozen vs new,
//! **before a byte of source is parsed**.
//!
//! Owed since `9a54ad1` by the stage-3 DoD line *"`yelc-sema` exists, and its
//! builtin `Definitions` table is compared against the frozen one"* — and never
//! taken, which that line's own paragraph predicted would happen once sema lost
//! its ratchet row. Taken 2026-07-31.
//!
//! # What is comparable, and what is deliberately not
//!
//! The two compilers factor builtins differently. The frozen tree registers
//! everything — functions, elements, enums, variants — as `DefId`s in one
//! `(Name, Namespace)`-keyed table. The new tree splits by shape: callables in
//! [`IntrinsicTable`] (arity, type scheme, lowering target), named definitions in
//! [`Definitions`], and only the lang-item inventory in the latter.
//!
//! So the honest comparison is **per inventory**, with every divergence
//! asserted on *both* sides — the frozen compiler really has the thing, the new
//! one really lacks it, and the reason is written at the assertion
//! ([A10](../../../plans/rewrite/anti-spec.md)). A divergence checked on one
//! side only rots into folklore when either table changes.
//!
//! Like the other two oracle harnesses (`yelc-syntax/tests/parity.rs`,
//! `tests/single_namespace.rs`), `yel-core` is a dev-dependency that vanishes
//! at cutover phase 4.

use std::collections::BTreeSet;

use yelc_sema::ids::PackageId;
use yelc_sema::intrinsics::Visibility;
use yelc_sema::{CompilerContext, Known};

// ---------------------------------------------------------------------------
// The two tables
// ---------------------------------------------------------------------------

/// The frozen compiler's builtin `Definitions`, exactly as every pipeline sees
/// them: `Compiler::new()` runs `lookup_known_definitions` before any parse.
fn frozen() -> yel_core::Compiler {
    yel_core::Compiler::new()
}

/// Frozen builtin names of one `DefKind` shape, in **registration order** —
/// `DefId` order is `iter()` order, and ordinals reach output.
fn frozen_names_where(
    compiler: &yel_core::Compiler,
    pred: impl Fn(&yel_core::DefKind) -> bool,
) -> Vec<String> {
    let ctx = compiler.context();
    ctx.defs
        .iter()
        .filter(|(_, item)| pred(&item.kind))
        .map(|(_, item)| ctx.interner.str(item.name).to_string())
        .collect()
}

fn new_table() -> CompilerContext {
    CompilerContext::with_intrinsics(PackageId::LOCAL)
}

/// The `yel:ui/dom` host imports, read out of the frozen table the way the
/// frozen tree itself does: *"the global's `callbacks` ARE the DOM functions —
/// the single source of truth"* (`yel-core/src/context.rs`). They are
/// registered as `DefKind::Function`, but they are the **host interface**, not
/// the stdlib — the new tree models them as an imported module
/// (`plans/desugar/counter.yelir`'s `module Dom`), so they are a separate
/// inventory here, excluded from the stdlib comparison *by membership in this
/// set* rather than by a hand-written name list.
fn frozen_dom_import_names(compiler: &yel_core::Compiler) -> BTreeSet<String> {
    let ctx = compiler.context();
    // Found by iterating, not by `lookup`: the Dom global is deliberately
    // never name-registered — "reachable only via `ctx.dom_imports()`, never
    // by user name resolution" (`dom_imports.rs`) — which
    // `namespace_registration_measured` pins.
    let global = ctx
        .defs
        .iter()
        .find_map(|(_, item)| match &item.kind {
            yel_core::DefKind::Global(g) if &*ctx.interner.str(item.name) == "Dom" => Some(g),
            _ => None,
        })
        .expect("the frozen table allocates the Dom global");
    global
        .callbacks
        .iter()
        .map(|&id| {
            let (_, item) = ctx
                .defs
                .iter()
                .find(|(def_id, _)| *def_id == id)
                .expect("callback DefId resolves");
            ctx.interner.str(item.name).to_string()
        })
        .collect()
}

/// Every name in the new callable table, deduplicated — overload rows share a
/// name by design (`len`).
fn new_callable_names(ctx: &CompilerContext) -> BTreeSet<String> {
    ctx.intrinsics
        .iter()
        .map(|(_, b)| ctx.names.str(b.name).to_string())
        .collect()
}

// ---------------------------------------------------------------------------
// The callable inventory
// ---------------------------------------------------------------------------

/// The mapping, stated: every frozen `DefKind::Function` must be accounted for
/// by the new [`IntrinsicTable`] — as a matching row, or as an enumerated
/// divergence with the reason at the assertion.
#[test]
fn every_frozen_function_is_accounted_for() {
    let frozen = frozen();
    let frozen_functions: BTreeSet<String> =
        frozen_names_where(&frozen, |k| matches!(k, yel_core::DefKind::Function(_)))
            .into_iter()
            .collect();
    let new_names = new_callable_names(&new_table());
    let dom = frozen_dom_import_names(&frozen);

    let mut unaccounted = Vec::new();
    for name in frozen_functions.difference(&dom) {
        let accounted = match name.as_str() {
            // Respelled, not dropped: LANGUAGE.md § Hyphens documents kebab-case
            // and the frozen tree registers the underscore form. The new table
            // carries the documented spelling. Asserted in both directions in
            // `starts_with_is_respelled_not_dropped`.
            "starts_with" => new_names.contains("starts-with"),
            // The untyped escape hatch: the frozen `_ =>` to-string fallback,
            // typed `ERROR -> string`. The typed-GC direction removes the "any"
            // representation it converts from, so the new table refuses the row
            // rather than porting it. It cannot be reached from checked code
            // without `Ty::ERROR` already in play.
            "object-to-string" => !new_names.contains(name),
            // Tier C, blocked on generics (§3 monomorphization): both are
            // `list<T>`-generic and the frozen rows type them `ERROR -> ERROR`.
            // `open-decisions.md` cluster A hangs "whether tier C is reachable"
            // on the generics decision; the rows land with it, typed.
            "list-get" | "append" => !new_names.contains(name),
            // Everything else must simply be there.
            _ => new_names.contains(name),
        };
        if !accounted {
            unaccounted.push(name.clone());
        }
    }

    assert_eq!(
        unaccounted,
        Vec::<String>::new(),
        "frozen builtin functions neither matched nor enumerated as a divergence"
    );
}

/// The reverse direction: a name in the new callable table that the frozen tree
/// never registered is a *surface addition* and must be one of the enumerated
/// ones — `some`/`none`, which the frozen tree models as variant **cases** of
/// `option` rather than as functions.
#[test]
fn every_new_callable_is_accounted_for() {
    let frozen = frozen();
    let frozen_functions: BTreeSet<String> =
        frozen_names_where(&frozen, |k| matches!(k, yel_core::DefKind::Function(_)))
            .into_iter()
            .collect();
    let new_names = new_callable_names(&new_table());

    let mut unaccounted = Vec::new();
    for name in &new_names {
        let accounted = match name.as_str() {
            "starts-with" => frozen_functions.contains("starts_with"),
            // Same value, different mechanism: the frozen tree registers
            // `some`/`none` as `DefKind::VariantCase` under `option`, checked in
            // `some_and_none_are_variant_cases_in_the_frozen_tree`.
            "some" | "none" => !frozen_functions.contains(name),
            _ => frozen_functions.contains(name),
        };
        if !accounted {
            unaccounted.push(name.clone());
        }
    }

    assert_eq!(
        unaccounted,
        Vec::<String>::new(),
        "new callable rows the frozen tree has no counterpart for, and no enumeration covers"
    );
}

/// Both halves of the respelling, so neither table can drift back silently.
#[test]
fn starts_with_is_respelled_not_dropped() {
    let frozen = frozen();
    let frozen_functions: BTreeSet<String> =
        frozen_names_where(&frozen, |k| matches!(k, yel_core::DefKind::Function(_)))
            .into_iter()
            .collect();
    assert!(frozen_functions.contains("starts_with"));
    assert!(!frozen_functions.contains("starts-with"));

    let new_names = new_callable_names(&new_table());
    assert!(new_names.contains("starts-with"));
    assert!(!new_names.contains("starts_with"));
}

/// The `some`/`none` divergence, asserted on the frozen side: they exist there
/// as cases of the `option` variant, not as functions.
#[test]
fn some_and_none_are_variant_cases_in_the_frozen_tree() {
    let frozen = frozen();
    let cases: BTreeSet<String> =
        frozen_names_where(&frozen, |k| matches!(k, yel_core::DefKind::VariantCase(_)))
            .into_iter()
            .collect();
    assert!(cases.contains("some"));
    assert!(cases.contains("none"));
}

/// Arity agreement over the matched overlap. The frozen table's generic rows
/// use `Ty::ERROR` placeholders, so *types* are not comparable across the two
/// interners — but parameter **count** is.
///
/// Read from the interned `Func` type, not from `FunctionDef.params`: the
/// frozen `register_function` writes `params: vec![]` on every builtin (*"don't
/// have param DefIds"*) and carries the real signature only in the type it
/// `set_type`s. The first run of this test compared against the def and found
/// eleven "arity 0" rows — the measurement was of the wrong field, not of a
/// divergence.
///
/// Exceptions, where the divergence is the point:
///
/// - `concat`: frozen writes zero params with a comment claiming variadic; the
///   new row *declares* variadic. Same behaviour, one honest declaration.
/// - `len`: one untyped frozen row (`ERROR -> s32`), two typed new rows. The
///   overload split is decision B3's reason for existing.
#[test]
fn arity_agrees_over_the_matched_overlap() {
    let frozen = frozen();
    let ctx = frozen.context();
    let new = new_table();

    let dom = frozen_dom_import_names(&frozen);
    let mut mismatches = Vec::new();
    for (_, item) in ctx.defs.iter() {
        let yel_core::DefKind::Function(f) = &item.kind else {
            continue;
        };
        let name = ctx.interner.str(item.name).to_string();
        if dom.contains(&name) {
            continue;
        }
        // Enumerated divergences are covered by the tests above, not re-judged.
        if matches!(
            name.as_str(),
            "starts_with" | "object-to-string" | "list-get" | "append" | "concat" | "len"
        ) {
            continue;
        }
        let frozen_arity = match ctx.defs.type_of(f.def_id).map(|ty| ctx.types.kind(ty)) {
            Some(yel_core::InternedTyKind::Func { params, .. }) => params.len(),
            other => {
                mismatches.push(format!("{name}: frozen def has no Func type ({other:?})"));
                continue;
            }
        };
        let name_new = new.names.intern(&name);
        let rows = new.intrinsics.overloads(name_new);
        let agreed = rows
            .iter()
            .any(|&id| new.intrinsics.get(id).accepts(frozen_arity));
        if !agreed {
            mismatches.push(format!("{name}: frozen arity {frozen_arity}"));
        }
    }
    assert_eq!(mismatches, Vec::<String>::new());
}

/// `concat`'s declaration divergence, both sides. The frozen row is zero
/// params plus a comment *claiming* variadic; the new row is `func(parts:
/// list<string>)` — one argument, unbounded arity carried by the list value.
/// (The intermediate design, an `Arity::Variadic` case, was deleted
/// 2026-07-31 when std-as-source made it undeclarable — see the tombstone in
/// `intrinsics.rs`.)
#[test]
fn concat_takes_the_parts_as_one_list() {
    let frozen = frozen();
    let ctx = frozen.context();
    let frozen_concat = ctx
        .defs
        .iter()
        .find_map(|(_, item)| match &item.kind {
            yel_core::DefKind::Function(f) if &*ctx.interner.str(item.name) == "concat" => {
                Some(f.params.len())
            }
            _ => None,
        })
        .expect("frozen table registers concat");
    assert_eq!(frozen_concat, 0, "frozen: zero params plus a comment");

    let new = new_table();
    let name = new.names.intern("concat");
    let rows = new.intrinsics.overloads(name);
    assert_eq!(rows.len(), 1);
    let row = new.intrinsics.get(rows[0]);
    assert_eq!(row.params.len(), 1);
    assert_eq!(
        new.types.kind(row.params[0]),
        yelc_sema::TyKind::List(yelc_sema::Ty::STRING),
        "the one parameter is the parts list"
    );
}

// ---------------------------------------------------------------------------
// The named-definition inventory
// ---------------------------------------------------------------------------

/// The doc-comment on `register_known_definitions` claims the frozen tree
/// registers *"9 names into `Namespace::Type` and 51 into
/// `Namespace::Component`"* as its reason for not porting the inventory.
/// **Measured here: both counts are right, and the membership is not the
/// obvious guess** — `option` and `result` are allocated but never
/// name-registered, while `Brush` and `event-value` (in no documentation) are.
/// The `names` map is private, so registration is measured the way a program
/// observes it: `lookup(name, namespace)` per candidate, over every name the
/// builtin table allocates.
#[test]
fn the_frozen_inventory_is_the_size_the_narrowing_reasoning_assumes() {
    let frozen = frozen();
    let ctx = frozen.context();

    let mut all_names = BTreeSet::new();
    for (_, item) in ctx.defs.iter() {
        all_names.insert(ctx.interner.str(item.name).to_string());
    }

    let registered_in = |ns: yel_core::Namespace| -> Vec<String> {
        all_names
            .iter()
            .filter(|n| ctx.defs.lookup(ctx.interner.intern(n), ns).is_some())
            .cloned()
            .collect()
    };

    let type_ns = registered_in(yel_core::Namespace::Type);
    let component_ns = registered_in(yel_core::Namespace::Component);
    let global_ns = registered_in(yel_core::Namespace::Global);

    // The measured truth, and it is not the obvious guess: `option` and
    // `result` are ALLOCATED as variants but never name-registered — they
    // resolve only through the type-syntax path (`AstTyKind::Option`/`Result`),
    // so `option` as a bare name does not exist in the frozen compiler either.
    // `Brush` and `event-value` are registered; neither is in LANGUAGE.md.
    assert_eq!(
        type_ns,
        [
            "Align",
            "AttributeValue",
            "Brush",
            "ButtonVariant",
            "Color",
            "Direction",
            "Justify",
            "Weight",
            "event-value",
        ],
        "names resolvable in Namespace::Type before any parse"
    );
    assert_eq!(
        component_ns.len(),
        51,
        "names resolvable in Namespace::Component: {component_ns:?}"
    );
    // The Dom global is allocated but deliberately unregistered — host
    // machinery, not a name. Nothing else claims the Global namespace either.
    assert_eq!(global_ns, Vec::<String>::new());
}

/// The new `Definitions` deliberately carries **only** the lang-item inventory
/// (`Known::ALL`) — the 60-name frozen inventory arrives from Yel source later
/// (`directions.md` §2) rather than from Rust. Asserting the exact contents
/// keeps "only the lang-items" true by measurement: a helpful future
/// registration widens this list or fails here, either way visibly.
#[test]
fn the_new_definitions_table_is_exactly_the_lang_item_inventory() {
    let new = new_table();
    let registered: Vec<String> = new
        .defs
        .iter()
        .map(|def| new.names.str(def.name).to_string())
        .collect();
    let expected: Vec<String> = Known::ALL
        .iter()
        .map(|item| item.source_name().to_string())
        .collect();
    assert_eq!(registered, expected);
}

/// Every lang-item name must also exist in the frozen table — a `Known` entry
/// naming something the frozen compiler never had would be an invention, not a
/// port.
#[test]
fn every_lang_item_exists_in_the_frozen_table() {
    let frozen = frozen();
    let ctx = frozen.context();
    let all: BTreeSet<String> = ctx
        .defs
        .iter()
        .map(|(_, item)| ctx.interner.str(item.name).to_string())
        .collect();
    for item in Known::ALL {
        assert!(
            all.contains(item.source_name()),
            "lang-item `{}` is absent from the frozen builtin table",
            item.source_name()
        );
    }
}

// ---------------------------------------------------------------------------
// Visibility
// ---------------------------------------------------------------------------

/// The user-facing/internal split matches what LANGUAGE.md documents: every
/// user-facing row is a documented name, every desugaring target is internal.
/// The split is load-bearing for completions (the LSP lists user-facing only),
/// so it is pinned as a set, not spot-checked.
#[test]
fn the_visibility_split_is_the_documented_one() {
    let new = new_table();
    let mut user_facing = BTreeSet::new();
    let mut internal = BTreeSet::new();
    for (_, b) in new.intrinsics.iter() {
        let name = new.names.str(b.name).to_string();
        match b.visibility {
            Visibility::UserFacing => user_facing.insert(name),
            Visibility::Internal => internal.insert(name),
        };
    }
    assert_eq!(
        user_facing.into_iter().collect::<Vec<_>>(),
        ["filter", "len", "max", "min", "none", "some", "starts-with"]
    );
    assert_eq!(
        internal.into_iter().collect::<Vec<_>>(),
        [
            "bool-to-string",
            "char-to-string",
            "concat",
            "f32-to-string",
            "f64-to-string",
            "s32-to-string",
            "s64-to-string",
            "u32-to-string",
            "u64-to-string",
        ]
    );
}
