//! Populating the builtin table.
//!
//! The **specification** is [`LANGUAGE.md` § Built-in Functions](../../../LANGUAGE.md),
//! not the frozen `stdlib_lookup.rs` — the frozen file is read to discover what
//! exists, and the table below is written from the documented signatures.
//!
//! `directions.md` §2 later moves the rows into embedded Yel source. That
//! changes *where a row comes from*, not what a row is, so nothing here blocks
//! on it.

use crate::builtins::{Arity, Builtin, BuiltinTable, LoweringTarget, Visibility};
use crate::context::CompilerContext;
use crate::types::{Ty, TyKind};

/// Register every builtin into `ctx`.
///
/// Takes no source input at all, which is what makes the result **comparable
/// against the frozen table before a single file is parsed** — the standalone
/// checkpoint this phase owes.
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

    fn ctx() -> CompilerContext {
        let mut ctx = CompilerContext::default();
        register_builtins(&mut ctx);
        ctx
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
    }
}
