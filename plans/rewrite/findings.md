# Findings — measured facts about the frozen compiler

> **What this is.** One canonical home for every fact about the frozen tree that
> a decision rests on. Directions and stage briefs **cite** these; they do not
> restate them.
>
> **Rules.** Append-only. Every entry carries evidence — a `file:line` or a
> command that reproduces it. A claim with neither does not go here. If a
> finding is later shown wrong, add a correction line to it; do not delete it.
>
> **Reading order for an agent:** you do not need to read this file top to
> bottom. Follow the citation you were given.

| id | claim | area |
|---|---|---|
| [F1](#f1) | `InternedTyKind` has no type-variable variant | types |
| [F2](#f2) | The grammar has no generic type application | types |
| [F3](#f3) | `intern_ast_ty` runs during registration, so named types become `Unknown` | resolution |
| [F4](#f4) | Cross-file references are order-dependent | resolution |
| [F5](#f5) | The definition tables are typed, and stage 2 types them | resolution |
| [F6](#f6) | A closure capturing an enclosing local panics in codegen | closures |
| [F7](#f7) | Codegen matches the `filter` builtin by string | closures |
| [F8](#f8) | A block typechecks and compiles against a `func()` prop | closures |
| [F9](#f9) | Three deferred-body mechanisms; one carries six env-snapshot fields | closures |
| [F10](#f10) | `else if` and nested `if` produce different output | control flow |
| [F11](#f11) | `else_if_branches` propagates through three IRs | control flow |
| [F12](#f12) | Builtins are a field per builtin across 1,442 lines | builtins |
| [F13](#f13) | `bind` desugars to getter + empty setter at HIR | lowering |
| [F14](#f14) | There is no HIR dump, so stage 2 has no artifact | verification |

---

## F1

**`InternedTyKind` has no `TyVar` / `Param` variant.** `List(Ty)`, `Option(Ty)`
are always concrete. `Mode::Infer` in `typeck.rs` is a bidirectional checking
mode ("synthesize"), not a type variable. The type system is monomorphic
throughout.

Corollary: `option` is registered with `payload: Some(Ty::ERROR)` under the
comment *"Generic placeholder — actual types are `option<T>`"*.

`types/interner.rs:50` · `stdlib_lookup.rs:63` · cited by [§3](directions.md)

## F2

**The grammar has no generic type application.** `ast::TypeKind` has `List`,
`Option`, `Result { args }`, `Tuple` as four **hardcoded productions**. There is
no `Named<Args>` form, so a user cannot write `Box<T>`.

`Result` stores `args: Vec<TypeRef>` **as written**, not `{ok, err}`, because
`result<a,b,c>` is real input and truncating drops a subtree (stage-1 S5).

`yelc-syntax/src/ast.rs:604` · cited by [§3](directions.md), [stage 2](stage-2-hir.md)

## F3

**`intern_ast_ty` is called during registration, before all names exist**, so a
named type resolves to nothing and returns `Unknown`:

```rust
AstTyKind::Named(_) => {
    // Named types need resolution - return Unknown for now
    self.intern(InternedTyKind::Unknown)
}
```

A record field typed as a user record is `Unknown` in the definition table.

`types/interner.rs:331`, called from `hir/lower.rs:206` and 11 other sites ·
anti-spec B2 · cited by [stage 2 H1](stage-2-hir.md)

## F4

**Cross-file references resolve in one direction only.** `lower_all` runs full
HIR lowering per file inside the driver loop, then concatenates
(`yelc/src/pipeline.rs:78-90`), so file A's bodies lower before file B's items
are registered.

```bash
# b.yel: record Person { name: string }
# a.yel: component App { p: Person = { name: "x" }; … }
yelc check b.yel a.yel   # OK: 1 component(s) checked
yelc check a.yel b.yel   # error[E0002]: cannot infer type of anonymous record literal
```

Same defect class as [F3](#f3), one level up. Measured 2026-07-28 ·
cited by [stage 2](stage-2-hir.md)

## F5

**The definition tables carry real `Ty`, and HIR lowering fills them.**
`FieldDef.ty`, `PropertyDef.ty`, `FunctionDef.ret_ty`, `VariantCaseDef.payload`
are all `Ty`; `hir/lower.rs` fills them with **12+ `intern_ast_ty` calls**.

So "HIR is untyped" is false as stated: *item signatures* are typed at stage 2,
*expressions* are not. This is rustc's `type_of(def_id)`-before-body-check split,
not a deviation from it. The frozen bug is only *when* it runs — see [F3](#f3).

`definitions.rs:130,143,156,171,184` · cited by [stage 2 H1](stage-2-hir.md)

## F6

**A closure capturing an enclosing local panics in codegen.**

```
not yet implemented: Local not found in captured locals or local_to_slot: Local(LocalId(2))
  wasm/expr.rs:192, in generate_filter_function (codegen/record_list.rs:463)
```

Repro: a filter predicate inside a `for` body referencing the loop variable.

Capturing a **component signal** works, by a different route — `SignalRead`
resolves through `$self`'s `$Comp` struct "or a filter-captured WASM param"
(`lir/expr.rs:94`), which is why `for_filter_over_signal.yel` passes.

The corpus is 2000/2000 compiling, so **no corpus program does this**: there is
no output to match and nothing to diverge from. Measured 2026-07-28 ·
cited by [§4](directions.md)

## F7

**The back end recognises a frontend builtin by string.**

```rust
if func_name != "filter" || args.len() != 2 { return; }   // wasm/mod.rs:1661
```

`LirExprKind::Closure` is otherwise unemittable (`wasm/expr.rs:1578` returns an
error). `FilterCallEntry` is `(comp_idx, elem_ty, elem_size, (LocalId, Ty),
LirExpr)` — one parameter, one predicate expression, **no environment**.

anti-spec C1 · cited by [§4](directions.md)

## F8

**A handler-shaped block is already a value of function type.**

```yel
component Child { bumped: func(); … }
component App  { Child { bumped: { count += 1; } } }
```

`yelc check` → OK; `compile -o wasm` → exit 0, `wasm-tools validate` → valid.
The type system does not believe in the handler/closure split.

Also: `on-change: func(value: s32) -> string` is a "handler" with a parameter
*and* a return type. Measured 2026-07-28 · cited by [§5](directions.md)

## F9

**Three mechanisms implement "a deferred body evaluated later in a captured
environment":**

| | node | deferred as | captures via |
|---|---|---|---|
| event handler | `ThirHandler` | `DeferredHandlerBody` | **six env-snapshot fields** |
| filter predicate | `ThirExprKind::Closure` | inlined by `generate_filter_function` | one hardcoded param + `$Comp` |
| derived signal | — | `DeferredDerivedBody` | an interned `LirExprId` |

`DeferredHandlerBody` snapshots `local_bindings`, `outer_item_field_slots`,
`for_stack`, `for_iter_body_stack`, `for_item_iter_body` — **that is capture
analysis**, performed at the LIR layer because THIR's `captures` is always
`vec![]`.

`lower_to_lir/blocks.rs:658-704` · anti-spec A3, B4 · cited by [§5](directions.md)

## F10

**`else if` and an explicitly nested `if` compile to different output.**

```yel
if n==1 {…} else if n==2 {…} else {…}
if n==1 {…} else { if n==2 {…} else {…} }
```

| | `else if` | nested `if` |
|---|---|---|
| DOM anchors | 1 | **2** |
| if-update blocks | 1 | **2** |
| effect subscriptions on the same signal | 1 | **2** |

The frozen lowering treats `else if` as a flat N-way selector at one anchor and
nested `if` as two independent 1-way selectors. Measured via `compile -o dot`,
2026-07-28 · cited by [stage 2 D7](stage-2-hir.md)

## F11

**`else_if_branches` is a third field on `If` in three IRs** — HIR
(`hir/node.rs`), THIR (`thir/node.rs:144`), LIR (`lir/node.rs:363`) — so every
`If` consumer in three stages handles three shapes where one would do.

anti-spec B4 · cited by [stage 2 D7](stage-2-hir.md)

## F12

**Builtins are addressed as name-as-Rust-field.** `stdlib_lookup.rs` is 1,029
lines of imperative `register_*` calls; `known.rs` is 413 lines of
`Option<DefId>` fields across `KnownElements`/`Enums`/`Variants`/`Functions`/
`BuiltinTypes`.

Call sites: 51 in `stdlib_lookup.rs`, 24 in `thir/typeck.rs`, 5 in
`lower_to_lir/`. Adding one builtin touches four places that must agree and are
checked by nothing.

anti-spec A8 · cited by [§1](directions.md)

## F13

**`bind value: x` desugars to `value: x` + `set value: { }` at HIR** — the
getter publishes to the DOM, the empty-but-present setter enables DOM→signal
auto-sync. It is also a **merge**: props sharing a name collapse into one
`HirBinding { value, setter }`.

Implemented with `HashMap<String, _>` plus a parallel `binding_order:
Vec<String>` to recover the determinism the map destroyed.

`hir/lower.rs:967-1018` · undocumented in `docs/PIPELINE.md` ·
cited by [stage 2](stage-2-hir.md)

## F14

**There is no HIR dump.** The CLI exposes `ast`, `ir` (LIR), `check`, `compile`
(`yelc/src/main.rs:35-140`). The frozen tree may not be edited to add one, and
the two HIRs are designed to differ in shape, so a serialized byte-diff would be
meaningless.

Consequence: stage 2 has **no artifact** and cannot be differentially verified
on its own. What is comparable instead: the `Definitions` table (contents *and*
order — `DefId`s are ordinals that reach output), HIR-stage diagnostics via
`yelc check`, and total-lowering-without-panic.

cited by [stage 2](stage-2-hir.md), [§6](directions.md)
