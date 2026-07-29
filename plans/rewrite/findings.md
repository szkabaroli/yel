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
| [F5](#f5) | The definition tables are typed, and 2a types them | resolution |
| [F6](#f6) | A closure capturing an enclosing local panics in codegen | closures |
| [F7](#f7) | Codegen matches the `filter` builtin by string | closures |
| [F8](#f8) | A block typechecks and compiles against a `func()` prop | closures |
| [F9](#f9) | Three deferred-body mechanisms; one carries six env-snapshot fields | closures |
| [F10](#f10) | `else if` and nested `if` produce different output | control flow |
| [F11](#f11) | `else_if_branches` propagates through three IRs | control flow |
| [F12](#f12) | Builtins are a field per builtin across 1,442 lines | builtins |
| [F13](#f13) | `bind` desugars to getter + empty setter at HIR | lowering |
| [F14](#f14) | There is no HIR dump, so 2a has no artifact of its own | verification |
| [F15](#f15) | `filter` is already monomorphized **per call site**, not per type | code size |
| [F16](#f16) | Signal dispatch is already **fully static** — no runtime effect registry | reactivity |
| [F17](#f17) | Coercions are decided and **discarded** — no `Coerce` node exists; `list<T>` coercion is a front/back mismatch | types |
| [F18](#f18) | `Range`, `Ternary` and **three separate conditional forms** are carried by all four IRs | desugaring |

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

`yelc-syntax/src/ast.rs:604` · cited by [§3](directions.md), [2a](stage-2a-hir-build.md)

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
anti-spec B2 · cited by [2a H1](stage-2a-hir-build.md#h1)

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
cited by [2a § Multiple files](stage-2a-hir-build.md#multiple-files)

## F5

**The definition tables carry real `Ty`, and HIR lowering fills them.**
`FieldDef.ty`, `PropertyDef.ty`, `FunctionDef.ret_ty`, `VariantCaseDef.payload`
are all `Ty`; `hir/lower.rs` fills them with **12+ `intern_ast_ty` calls**.

So "HIR is untyped" is false as stated: *item signatures* are typed at stage 2,
*expressions* are not. This is rustc's `type_of(def_id)`-before-body-check split,
not a deviation from it. The frozen bug is only *when* it runs — see [F3](#f3).

`definitions.rs:130,143,156,171,184` · cited by [2a H1](stage-2a-hir-build.md#h1)

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
2026-07-28 · cited by [2a D7](stage-2a-hir-build.md#d7--flatten-else-if-chains)

## F11

**`else_if_branches` is a third field on `If` in three IRs** — HIR
(`hir/node.rs`), THIR (`thir/node.rs:144`), LIR (`lir/node.rs:363`) — so every
`If` consumer in three stages handles three shapes where one would do.

anti-spec B4 · cited by [2a D7](stage-2a-hir-build.md#d7--flatten-else-if-chains)

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
cited by [2a](stage-2a-hir-build.md#what-lowerings-belong-here)

## F14

**There is no HIR dump.** The CLI exposes `ast`, `ir` (LIR), `check`, `compile`
(`yelc/src/main.rs:35-140`). The frozen tree may not be edited to add one, and
the two HIRs are designed to differ in shape, so a serialized byte-diff would be
meaningless.

Consequence: 2a has **no artifact** and cannot be differentially verified on
its own; the artifact arrives after 2b. What is comparable instead: the `Definitions` table (contents *and*
order — `DefId`s are ordinals that reach output), HIR-stage diagnostics via
`yelc check`, and total-lowering-without-panic.

cited by [2a](stage-2a-hir-build.md#verification), [2b](stage-2b-hir-check.md#verification), [§6](directions.md)

## F15

**The frozen compiler already monomorphizes, and with the worst possible key.**
`collect_filter_calls` records one entry per
`(component, list-expr-id, predicate-expr-id)` (`wasm/mod.rs:1684`), so two
`filter` calls over the *same* element type with the *same* predicate shape emit
two generated functions.

```yel
a: list<s32>; b: list<s32>; t: s32 = 2;
for x in a.filter({ v -> v > t }) { … }
for y in b.filter({ v -> v > t }) { … }
```
→ `$filter_0 $filter_1 $filter_2 $filter_3` — **4 symbols for 2 call sites**.

Consequence for [§3](directions.md#3--generics-are-monomorphization-by-name):
per-*type* monomorphization is a **reduction** against this baseline, not an
increase. The comparison "monomorphization costs code size" is measured against
zero duplication; the actual baseline is worse than what §3 proposes.

Also measured: `-Oz` took the module 17,927 → 8,900 bytes but left the function
count at 10 — it did not merge them. *Caveat:* the two functions genuinely differ
(they read different globals), so this does **not** show whether wasm-opt merges
*identical* instantiations. That test needs §3 built.

`wasm/mod.rs:1661-1690` · measured 2026-07-28 ·
cited by [§3](directions.md#3--generics-are-monomorphization-by-name),
[A1](open-decisions.md#a1--how-are-parameterized-types-represented)

## F16

**Signal dispatch is fully static. There is no runtime effect registry, and no
dirty mask.**

`emit_trigger_for_signal` (`lower_to_lir/blocks.rs:5554`):

- **Component-local signal** — looks up `signal_to_update_blocks`, a
  *compile-time* map, and emits **direct `CallBlock`s** to each dependent.
- **Global signal** — emits a `TriggerEffects { signal }` placeholder, because
  the observer set is unknowable mid-lowering (other components are not lowered
  yet). The module-level `resolve_global_triggers` pass expands each one into
  direct `CallBlock`s to the observing components' fanout blocks.
- `TriggerEffects` **must not reach codegen** — that arm is a hard
  `CodegenError::InvalidIR` (`op_emit.rs:909`).

`effects_by_signal` (`lir/node.rs:173`) is compile-time metadata driving that
resolution, **not** a runtime table.

**Correction to how this was first read.** The DOT output shows
`effect 0 → if-update-b0 → {mount, unmount}` and was initially taken for a
Solid-style runtime registry. It is a **compile-time call graph**. The frozen
compiler already uses the dispatch strategy that
[§8](directions.md#8--the-reactive-plan-is-an-artifact-and-its-shape-is-open)
was about to propose as an improvement.

`lower_to_lir/blocks.rs:5554-5580`, `lir/block.rs:818-830`, `op_emit.rs:909` ·
2026-07-28 · cited by [§8](directions.md#8--the-reactive-plan-is-an-artifact-and-its-shape-is-open)

## F17

**Coercions are decided and thrown away.** `types_compatible`
(`thir/typeck.rs:2651`) returns **`bool`**. It permits integer widening, float
widening, int→float, `Color → Brush`, and recurses through `List`/`Option` — then
records **nothing**. There is no `Cast`, `Coerce`, `Convert` or `Adjust` node in
either `thir/expr.rs` or `lir/expr.rs`.

So every consumer must re-derive *which* conversion applies from the types at the
use site — and the cases are not uniform: `s32→s64` sign-extends, `u32→u64`
zero-extends, `s32→f64` converts, `f32→f64` promotes, and **`Color → Brush` is a
representation change** ([C4](anti-spec.md#c4--no-type-whose-storage-shape-depends-on-where-it-appears)
is about exactly those two types).

**The recursive `List` arm is a front/back mismatch.** Measured:

```yel
a: list<s32> = [1, 2, 3];
b: list<s64> = a;
```
```
yelc check    → OK: 1 component(s) checked
yelc compile  → encoding error: type mismatch: expected (ref null $type), found (ref null $type)
```

Typeck accepts, because element types are "compatible"; nothing converts the
elements, so a `list<s32>` GC array reaches a `list<s64>` slot. It fails **loudly
at the encoder**, not silently — but it is a program the front end accepts and
the back end cannot emit. `known_bugs` material.

**Why a materialized coercion node prevents this by construction**, rather than
catching it later: typeck would have to *build* the conversion, and there is no
conversion from `list<s32>` to `list<s64>` short of an element-wise map. Being
unable to construct the node **is** the rejection, at the right place, with a
span. This is what rustc's THIR does with adjustments, and the reason it does it.

`thir/typeck.rs:2651-2680` · measured 2026-07-28 ·
cited by [3b](stage-2b-hir-check.md)

## F18

**Sugar that never desugars, carried by every layer.**

| construct | AST | HIR | THIR | LIR |
|---|---|---|---|---|
| `Range` (`0..10`, `0..=10`) | ✓ | ✓ | ✓ | ✓ |
| `Ternary` (`c ? a : b`) | ✓ | ✓ | ✓ | ✓ |
| `If` **statement** | ✓ | ✓ | ✓ | ✓ |
| `If` **UI node** | ✓ | ✓ | ✓ | ✓ |

`ExprKind` has `Ternary` and **no `If`**, so yel carries **three unrelated
constructs for one concept** — a conditional *expression*, a conditional
*statement*, and a conditional *UI node* — each with its own variant in each of
four IRs. That is [B4](anti-spec.md#b4--no-special-cased-control-flow-where-a-general-form-exists)
at four times the scale of [F11](#f11)'s `else_if_branches`.

For contrast, the two constructs that *do* desugar: `MethodCall` exists in HIR
and is gone by THIR (typeck resolves it); `Interpolation` survives to THIR and is
gone by LIR (lowering turns it into `concat`).

**Neither `Range` nor `Ternary` can be desugared today**, and the reason is the
same in both cases — *the target form does not exist*:

- `Ternary` would desugar to a conditional expression or a `match`. Yel has
  neither; `match` is listed as a gap in
  [3b](stage-2b-hir-check.md#gaps-inherited-as-decisions-not-copies).
- `Range` would desugar to a `Range { start, end }` struct literal, the way Rust
  does. Yel has no `Range` type to desugar *into* — but the stdlib is planned, so
  this is **a requirement on
  [§2's contents](directions.md#what-the-stdlib-must-provide-not-just-what-can-move-into-it)**,
  not a blocker. It is not generic, so it does not wait on §3 either.

So `Ternary` waits on an open decision; `Range` waits only on sequencing.

`yelc-syntax/src/ast.rs` (`ExprKind`), `{hir,thir,lir}/{expr,node}.rs` ·
measured 2026-07-28 · cited by [3a](stage-2a-hir-build.md#what-lowerings-belong-here)

## F19

**`|` in expression position reports `expected \`||\`` — a suggestion that cannot
help.** There is no lone `|` token in yel. `lexer.rs:410` sees `|`, peeks for a
second one, and on failing to find it emits `E0060: expected \`||\``. So a user
writing a Rust-style closure gets told to write a logical-or:

```
$ yelc2 known_bugs/silent_discard/global_member.yel
error[E0060]: expected `||`
  --> …:26:44
  26 |     evens: list<s32> = [1, 2, 3, 4].filter(|x| x > 2);
```

The verdict is right and the span is right; only the advice is wrong. The fix the
user needs is `{ x -> x > 2 }` ([`LANGUAGE.md:618`](../../LANGUAGE.md) — closures
are `{ params -> body }`). The message also fires **twice**, once per `|`, which
is a second reading of the same mistake.

**Not a parity or ratchet concern.** Both compilers reject — the frozen one
differently and worse, see [F20](#f20) — so no accept/reject bit and no golden
moves. This is diagnostic quality only, and it is filed so the next person to see
`expected \`||\`` does not have to re-derive why a lexer peek produced it.

**Why it is worth fixing rather than tolerating.** `|` in expression position
where a closure is grammatical is an unusually strong signal: there is no other
construct it could be starting. That makes "did you mean `{ x -> … }`?" a
one-arm suggestion with no ambiguity to resolve — the cheap end of diagnostic
work, and the exact case a Rust-literate newcomer hits first.

`yelc-syntax/src/lexer.rs:410` · measured 2026-07-29 with `yelc2` ·
owed to [2b](stage-2b-hir-check.md), which owns diagnostics

## F20

**The frozen compiler *accepts* the same input and drops the member.** Same
source as [F19](#f19): `yelc check` prints `OK: 1 component(s) checked` and
`evens` is absent from `file.globals[0].properties`.

`BLOCK_LEVEL_CATCH_ALL` (`grammar.pest:18`) swallows the whole line so
`global_decl` still matches, and `parse_global` iterates members with a trailing
`_ => {}`. Two of the four catch-all sites are silent this way — `parse_record`
spells it differently (`if field_pair.as_rule() == Rule::record_field`,
`parser.rs:321`) while `parse_component` (:823) and `parse_element_node` (:1186)
**do** report. It is not a uniform policy; it is two omissions.

The new parser reports *and* keeps the property — `Ident evens` with
`Expr Error` for its value — which is invariant S5: a diagnostic **and** an Error
node, never a dropped subtree. The frozen tree loses the name and the span
together, so nothing downstream can even know something was there.

Pinned two ways, deliberately: `known_bugs/silent_discard/global_member.yel`
records it in the frozen tree where a reader of that tree will find it, and
`support::catch_all::DIVERGENCES` (18 entries, each proved causally by
`explains_our_report`) records it against the new parser.

`hir`-visible via `yelc ast` · `syntax/parser.rs` `parse_global`/`parse_record` ·
measured 2026-07-29 · cited by
[2a phase 0](stage-2a-hir-build.md#phase-0--oracle-hygiene---done-2026-07-29-1d12250)
