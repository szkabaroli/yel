//! Populating the builtin table.
//!
//! The **specification** is [`LANGUAGE.md` § Built-in Functions](../../../LANGUAGE.md),
//! not the frozen `stdlib_lookup.rs` — the frozen file is read to discover what
//! exists, and the table below is written from the documented signatures.
//!
//! `directions.md` §2 later moves the rows into embedded Yel source. That
//! changes *where a row comes from*, not what a row is, so nothing here blocks
//! on it.

use yelc_base::Span;

use crate::builtins::{Arity, Builtin, BuiltinTable, LoweringTarget, Visibility};
use crate::context::CompilerContext;
use crate::known::Known;
use crate::types::{Ty, TyKind};

/// Register every builtin into `ctx` — **both tables**.
///
/// Takes no source input at all, which is what makes the result **comparable
/// against the frozen table before a single file is parsed** — the standalone
/// checkpoint this phase owes.
///
/// # Two tables, because a builtin is one of two shapes (decisions C1b / C2)
///
/// | shape | table | why |
/// |---|---|---|
/// | callable — `len`, `concat`, `min` | [`BuiltinTable`] | it has an arity, a type scheme and a lowering target |
/// | named definition — the [`Known`] inventory | [`Definitions`](crate::definitions::Definitions) | it has none of the three, and ordinary name lookup must find it |
///
/// The second half was **missing until 2026-07-30**: `Known::resolve` looks
/// names up in `ctx.defs`, nothing ever wrote to `ctx.defs`, and so
/// [`CompilerContext::resolve_known`] could not succeed outside a test fixture
/// — a complete mechanism with no registration site
/// ([anti-spec A9](../../../plans/rewrite/anti-spec.md)).
pub fn register_builtins(ctx: &mut CompilerContext) {
    let t = &ctx.types;

    // The one type parameter the stdlib needs. Every generic builtin below is
    // `T`-in-position-0; nothing here is higher-arity, so a single param
    // suffices (decision A3).
    let param0 = t.intern(TyKind::Param(0));
    let list_t = t.intern(TyKind::List(param0));
    let option_t = t.intern(TyKind::Option(param0));
    let predicate = t.intern(TyKind::Func {
        params: vec![param0],
        ret: Some(Ty::BOOL),
    });

    let mut add = |name: &str, arity, params, ret, lowering, visibility| {
        let name = ctx.names.intern(name);
        ctx.builtins.register(Builtin {
            name,
            arity,
            params,
            ret,
            lowering,
            visibility,
        });
    };

    // ── User-facing: LANGUAGE.md § Built-in Functions ────────────────────────
    //
    // `len` is the reason `overloads()` returns a slice rather than a row: a
    // name does not identify a definition here, which is also why B3's
    // OverloadKey exists.
    add(
        "len",
        Arity::Fixed(1),
        vec![list_t],
        Some(Ty::S32),
        LoweringTarget::Op("list_len"),
        Visibility::UserFacing,
    );
    add(
        "len",
        Arity::Fixed(1),
        vec![Ty::STRING],
        Some(Ty::S32),
        LoweringTarget::Op("string_len"),
        Visibility::UserFacing,
    );
    add(
        "filter",
        Arity::Fixed(2),
        vec![list_t, predicate],
        Some(list_t),
        LoweringTarget::Op("list_filter"),
        Visibility::UserFacing,
    );
    // Kebab at the surface, per LANGUAGE.md § Hyphens. The frozen tree
    // registers `starts_with`; the documented spelling is the one that must
    // resolve, because it is what a user writes.
    add(
        "starts-with",
        Arity::Fixed(2),
        vec![Ty::STRING, Ty::STRING],
        Some(Ty::BOOL),
        LoweringTarget::Op("string_starts_with"),
        Visibility::UserFacing,
    );
    add(
        "min",
        Arity::Fixed(2),
        vec![Ty::S32, Ty::S32],
        Some(Ty::S32),
        LoweringTarget::Op("s32_min"),
        Visibility::UserFacing,
    );
    add(
        "max",
        Arity::Fixed(2),
        vec![Ty::S32, Ty::S32],
        Some(Ty::S32),
        LoweringTarget::Op("s32_max"),
        Visibility::UserFacing,
    );
    add(
        "some",
        Arity::Fixed(1),
        vec![param0],
        Some(option_t),
        LoweringTarget::Op("option_some"),
        Visibility::UserFacing,
    );
    add(
        "none",
        Arity::Fixed(0),
        vec![],
        Some(option_t),
        LoweringTarget::Op("option_none"),
        Visibility::UserFacing,
    );

    // ── Compiler-internal: desugaring targets ────────────────────────────────
    //
    // Absent from LANGUAGE.md because a user never writes them. Recorded as
    // Internal rather than omitted: they are real rows the checker must type
    // and lowering must find, and leaving them undeclared is how `concat` ended
    // up with a comment saying it was variadic and a declaration saying it took
    // nothing (F12's shape).
    add(
        "concat",
        Arity::Variadic {
            min: 0,
            element: Ty::STRING,
        },
        vec![],
        Some(Ty::STRING),
        LoweringTarget::Op("concat"),
        Visibility::Internal,
    );
    for (name, from, op) in [
        ("bool-to-string", Ty::BOOL, "bool_to_string"),
        ("char-to-string", Ty::CHAR, "char_to_string"),
        ("s32-to-string", Ty::S32, "s32_to_string"),
        ("s64-to-string", Ty::S64, "s64_to_string"),
        ("u32-to-string", Ty::U32, "u32_to_string"),
        ("u64-to-string", Ty::U64, "u64_to_string"),
        ("f32-to-string", Ty::F32, "f32_to_string"),
        ("f64-to-string", Ty::F64, "f64_to_string"),
    ] {
        add(
            name,
            Arity::Fixed(1),
            vec![from],
            Some(Ty::STRING),
            LoweringTarget::Op(op),
            Visibility::Internal,
        );
    }

    register_known_definitions(ctx);
}

/// Register the [`Known`] inventory into [`Definitions`](crate::definitions::Definitions).
///
/// [`KnownItems::resolve`](crate::known::KnownItems::resolve) reads these back
/// out by name, so this function and [`Known::ALL`] are the two halves of one
/// mechanism: both loop the same inventory, so an entry cannot be resolvable
/// without being registered, or registered without being resolvable.
///
/// # Why only the lang-items, and not the frozen tree's 60 builtin names
///
/// The frozen `stdlib_lookup.rs` registers 9 names into `Namespace::Type` and
/// 51 into `Namespace::Component` — builtin elements (`Text`, `Button`,
/// `List`, …), enums and variants. Under a **single namespace** every one of
/// them would also claim the Component and Global spellings of its name, which
/// is ~240 program shapes the frozen compiler accepts and this one would not,
/// and `stdlib/list.yel`'s `export global List` would stop compiling.
///
/// That inventory is not registered here because nothing in this tree consumes
/// it yet, and because it is scheduled to arrive from **Yel source** rather
/// than from Rust (`plans/directions.md` §2, `plans/modules.md` §2–3) — at
/// which point its own narrowing gets measured against the frozen compiler, on
/// the evidence of the day. Registering it now to serve a mechanism whose only
/// entry is `Color` would buy the break early and twice.
///
/// The narrowing this *does* buy is 4 program shapes and is enumerated against
/// the frozen compiler in `tests/single_namespace.rs`.
///
/// # Panics
///
/// If the inventory names one definition twice. Registration runs before any
/// source is read, so a collision here is a duplicate `Known` variant — a
/// compiler bug with no user input to blame.
fn register_known_definitions(ctx: &mut CompilerContext) {
    for &item in Known::ALL {
        let name = ctx.names.intern(item.source_name());
        ctx.defs
            .register(name, item.kind(), Span::default(), false)
            .unwrap_or_else(|_| {
                panic!(
                    "the lang-item inventory claims `{}` twice",
                    item.source_name(),
                )
            });
    }
}

/// Names a user may write. Everything else is a desugaring target.
pub fn user_facing(table: &BuiltinTable) -> impl Iterator<Item = &Builtin> {
    table
        .iter()
        .map(|(_, b)| b)
        .filter(|b| b.visibility == Visibility::UserFacing)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins::BuiltinId;
    use crate::definitions::DefKind;

    fn ctx() -> CompilerContext {
        let mut ctx = CompilerContext::default();
        register_builtins(&mut ctx);
        ctx
    }

    /// **The registration site A9 said had to exist.** Until 2026-07-30 this
    /// function touched `ctx.builtins` and never `ctx.defs`, so `Known::resolve`
    /// — which looks names up in `ctx.defs` — could not succeed anywhere but a
    /// test fixture.
    ///
    /// Asserted against `Known::ALL` rather than against the string `"Color"`,
    /// so it covers the inventory rather than today's one entry: deleting the
    /// `register_known_definitions` call fails this, and so does adding a
    /// `Known` variant without registering it.
    #[test]
    fn register_builtins_registers_the_lang_items_into_definitions() {
        let ctx = ctx();
        assert!(
            !Known::ALL.is_empty(),
            "an empty inventory would make every assertion below vacuous",
        );
        for &item in Known::ALL {
            let name = ctx.names.intern(item.source_name());
            assert!(
                ctx.defs.lookup_def(name, item.kind()).is_some(),
                "`{}` is a lang-item and register_builtins did not put it in \
                 Definitions; resolve_known cannot succeed",
                item.source_name(),
            );
        }
        assert_eq!(
            ctx.defs.len(),
            Known::ALL.len(),
            "Definitions holds the lang-item inventory and nothing else — see \
             `register_known_definitions` on why the frozen tree's other 60 \
             builtin names are not here",
        );
    }

    /// The other half of the same mechanism: a name in the builtin *table* is
    /// not thereby a definition. `len` must not become a top-level name a user
    /// program collides with.
    #[test]
    fn a_callable_builtin_is_not_a_definition() {
        let ctx = ctx();
        for name in ["len", "filter", "concat", "s32-to-string"] {
            let interned = ctx.names.intern(name);
            assert!(
                !ctx.builtins.overloads(interned).is_empty(),
                "`{name}` is a builtin",
            );
            assert!(
                ctx.defs.lookup(interned).is_empty(),
                "`{name}` is callable, not a definition; putting it in \
                 Definitions would make `record {name}` a duplicate",
            );
        }
    }

    /// The registration must not silently claim a *kind* the inventory does not
    /// ask for — a `Color` registered as a global is not the `Color` record
    /// `#ff0000` desugars against, and `Known::resolve` would report it missing.
    #[test]
    fn a_lang_item_is_registered_as_the_kind_it_declares() {
        let ctx = ctx();
        for &item in Known::ALL {
            let name = ctx.names.intern(item.source_name());
            for &kind in DefKind::ALL {
                assert_eq!(
                    ctx.defs.lookup_def(name, kind).is_some(),
                    kind == item.kind(),
                    "`{}` resolves as {kind:?} but declares {:?}",
                    item.source_name(),
                    item.kind(),
                );
            }
        }
    }

    /// LANGUAGE.md's table is the spec, so every row in it must resolve — by
    /// the spelling a user writes, which is kebab.
    #[test]
    fn every_documented_builtin_resolves_by_its_documented_name() {
        let ctx = ctx();
        for name in ["len", "filter", "starts-with", "min", "max", "some", "none"] {
            let interned = ctx.names.intern(name);
            assert!(
                !ctx.builtins.overloads(interned).is_empty(),
                "LANGUAGE.md documents `{name}` and it does not resolve",
            );
        }
    }

    /// `len` is the case that forces `overloads()` to return a slice.
    #[test]
    fn len_has_two_overloads_that_lower_differently() {
        let ctx = ctx();
        let ids: Vec<BuiltinId> = ctx.builtins.overloads(ctx.names.intern("len")).to_vec();
        assert_eq!(ids.len(), 2);
        let targets: Vec<_> = ids.iter().map(|&id| ctx.builtins.lowering(id)).collect();
        assert_ne!(
            targets[0], targets[1],
            "two overloads that lower to the same op are one builtin",
        );
    }

    /// A3 earning its keep: `filter`'s signature is only expressible with a
    /// type parameter, and `list<T>` must be the *same* handle in argument and
    /// return position or monomorphization has nothing to key on.
    #[test]
    fn generic_builtins_use_one_shared_param_type() {
        let ctx = ctx();
        let id = ctx.builtins.overloads(ctx.names.intern("filter"))[0];
        let filter = ctx.builtins.get(id);
        assert_eq!(filter.params[0], filter.ret.unwrap(), "list<T> -> list<T>");
        assert!(matches!(
            ctx.types.kind(filter.params[0]),
            TyKind::List(inner) if matches!(ctx.types.kind(inner), TyKind::Param(0)),
        ));
    }

    /// C1c, against the real registration rather than a fixture.
    #[test]
    fn concat_accepts_any_interpolation_length() {
        let ctx = ctx();
        let id = ctx.builtins.overloads(ctx.names.intern("concat"))[0];
        for parts in [0, 1, 7, 32] {
            assert!(ctx.builtins.get(id).arity.accepts(parts));
        }
    }

    /// The internal rows are real rows, not omissions — but they are not part
    /// of the documented surface, and conflating the two is how `concat` would
    /// end up in a user-facing completion list.
    #[test]
    fn internal_builtins_are_separable_from_the_documented_surface() {
        let ctx = ctx();
        let documented = user_facing(&ctx.builtins).count();
        assert_eq!(documented, 8, "7 documented names, `len` twice");
        assert!(ctx.builtins.len() > documented, "internal rows exist too");
    }

    /// Registration takes no input, so it must be reproducible — this is what
    /// makes the table comparable against the frozen one before parsing.
    #[test]
    fn registration_is_deterministic() {
        let a = ctx();
        let b = ctx();
        assert_eq!(a.builtins.len(), b.builtins.len());
        for ((_, x), (_, y)) in a.builtins.iter().zip(b.builtins.iter()) {
            assert_eq!(a.names.str(x.name), b.names.str(y.name));
            assert_eq!(x.arity, y.arity);
            assert_eq!(x.lowering, y.lowering);
        }

        // The definition half. Registration order decides `DefId`s, and a
        // `DefId` reaches output through the artifact writer.
        assert_eq!(a.defs.len(), b.defs.len());
        for (x, y) in a.defs.iter().zip(b.defs.iter()) {
            assert_eq!(x.id, y.id);
            assert_eq!(a.names.str(x.name), b.names.str(y.name));
            assert_eq!(x.kind, y.kind);
        }
    }
}
