# Yel — Tech Debt, Hacks & Shortcuts

> Honest inventory of known hacks, shortcuts, transitional bridges, and ugly
> corners — so a refactor can target them deliberately instead of rediscovering
> them. Line numbers are approximate; grep the cited symbol if it has drifted.
> Reviewed against `main` on 2026-06-29.
>
> Counts of debt markers (`TODO`/`for now`/`legacy`/`transitional`/`todo!`)
> at last review: **yel-core ~136**, **yel-wasm-codegen ~86**. (No literal
> `HACK`/`FIXME`/`XXX` markers — debt hides behind `legacy`, `for now`, and
> `todo!`.)

Two crate-level invariants keep debt *loud* rather than silent — keep them:
- **No silent fallbacks** — unimplemented paths use `todo!(...)` / `Err(CodegenError::…)`, never placeholder IR/instructions. (`crates/yel-core/CLAUDE.md`, `crates/yel-wasm-codegen/CLAUDE.md`.)
- **Never soften a test assertion** to match known-wrong output (`yel-wasm-codegen/tests/execution.rs`).

---

## 1. Big transitional bridges (highest leverage)

> These all exist because the back-end is mid-migration from UI-specific to
> generic (shared with the flow language) — see
> [`ARCHITECTURE.md §0`](ARCHITECTURE.md). Each bridge is debt *only* until that
> migration lands; the fix is "finish the generalization," not "patch the bridge."

### 1.1 `LirSlotId::legacy_u32()` — the slot-ladder bridge
Slots are mid-migration from a raw `u32` index to a typed `LirSlotId` enum. The
shim `legacy_u32()` is called all over codegen and LIR helpers to fall back to
the old flat index, e.g. `lir/arena.rs:62`, `lir/dedupe.rs:765`,
`lir/boundary_rewrite.rs:530`, `lir/function.rs:251`. **Every `legacy_u32()` call
is a migration site**; the typed path should eventually make it unreachable.
(Git log: "Wire up per-block slot ladder; defer allocator flip".)

### 1.2 `tree_shape` side-channel + `BoundaryField` chain walk
`LirResource.tree_shape` is a parallel representation that codegen reads to emit
GC types and resolve `LirSlotKind::BoundaryField { boundary_id, field_idx }`
symbolically — a runtime **chain walk** over `current_boundary_locals`
(`lir/boundary_rewrite.rs:102,618`, `lir/function.rs`). The
[`lir-resource-flatten`](../plans/lir-resource-flatten.md) plan replaces this
with explicit `StructGet`/`StructSet` ops + typed struct-ref params, deleting the
walk. Until then, boundary-field reads/writes are indirect and hard to follow.

### 1.3 `LirComponent` → `LirResource` rename (partial)
The type is `LirResource` now, but framing/comments still assume "a component"
(`lir/arena.rs` calls the trait `LirComponentArena`; docs say "acts like a
component"). UI-only fields on the resource are "still read by codegen during the
transitional phases" and "disappear as THIR→LIR lowers them inline"
(`lir/arena.rs:165`). Mixed mental model: a `LirResource` is sometimes a UI
component, sometimes a generic function body.

### 1.4 Legacy reactivity emission path
`context.rs:65` gates a "legacy `LirOp::TriggerEffects` emission" — the signal
writer "falls through to the legacy path" when the new per-boundary update path
isn't taken. `blocks.rs:6168,6259` keep the "legacy `SignalWriteExpr`/`InitSignal`
path … for now … to preserve semantics." Two reactivity codepaths coexist.

---

### 1.5 WASM-GC representation migration (canonical-flat → typed GC)
Values are mid-migration from a **canonical-flat** representation (a type
decomposed into its flat ABI valtypes, spread across multiple slots / linear
memory) to **typed WASM-GC** structs and arrays (a single typed ref). The
intended single source of truth for "how is this Yel value represented on the
stack / in a block" is `wasm/repr.rs::InternalRepr` (`Zero`, `Scalar`,
`GcRef(struct_idx)`, `GcArrayRef(arr_idx)`, `GcVariant(idx)`). The
half-migrated state is the debt:

- **Internal representation — RESOLVED.** Every value shape is a single typed ref (or scalar) internally: records/tuples/lists are GC refs, option/result/variant are `GcVariant` subtype hierarchies (nullable-ref collapse for eligible `option<T>`). The WIT boundary is served by two recursive pairs in `wasm/codegen/accessors.rs` — `emit_member_lift_to_memory` (GC → canonical, over `GcRefSource`) and `emit_member_pack` (canonical → GC, over `CanonicalSource`) — instead of per-shape generators.
- **The flat bridge that must shrink to the edges:** `lir/layout.rs:414` `canonical_flat_valtypes` / `:521` `canonical_flat_valtype_counts` still drive flat slot allocation internally. The target (memory `project_typed_gc_migration_stage0`) is to **kill the canonical-flat bridges and keep flattening only at WIT boundaries** (the lift/lower materializers). The invariant "only the boundary-shim generator calls `flatten_core_valtypes` outside `repr.rs`" is **enforced by convention, not the type system** (`repr.rs` docstring) — fragile; a stray internal caller silently reintroduces the split.
- **Incomplete materialize paths:** `lower_to_lir/signals_inline.rs:~299,306,362` bail (`todo!()` / "Bail until the const-materialize ops are available") — signal writes for some types can't lower until the const-materialize ops exist.
- **Dual signal storage — RESOLVED.** Pointer-repr signals (records/tuples) used to be stored in **both** the `$Comp` GC struct **and** a per-instance linear-memory cell. That backing is fully **removed**: records/tuples live solely on the GC struct, and boundary getters/setters lift/lower through a `cabi_realloc` scratch. The scaffolding is gone too — `SignalStorage.mem` / `MemSlot` / `memory_size` / `signal_memory_offset` deleted from `signal_layout.rs`, `MemoryLayout::signal_addr` + `signal_offsets` deleted from codegen, and every dead per-signal memory branch (`op_emit.rs` InitSignal/SignalWrite/InitSignalDefault, `expr.rs` SignalRead/Def, the `accessors.rs` scalar getter/variant setter fallthroughs, `blocks.rs` inline routing) removed or turned into `unreachable!`. Verified byte-for-byte behavior-neutral: full suite green + a 100-seed fuzz run with an identical failing-seed set (69/100). Only the WIT-boundary lift/lower shims and the memory-resident **globals** path survive — see below.
- **Globals-in-memory (the last memory-resident values).** Non-struct global properties (record/tuple globals) are still stored in linear memory via `global_property_addrs` (`build.rs:1650`) and written through `signals_inline::lower_signal_write_to_global_memory`. This is the sibling of the just-removed per-signal path and the next target of the same treatment: migrate global record/tuple properties onto their per-block `$globals_<i>` GC struct, then the `global_property_addrs` memory path and the `MemConstGlobalProp` op can go too.
- **Phased, with phase labels in comments:** e.g. `lir/block.rs:520` "Phase 2.2b switches the…". Expect `Phase N` markers; grep them to see what's done vs pending.

This is part of the same generic-back-end push as [§1.1–1.4](#1-big-transitional-bridges-highest-leverage) and `ARCHITECTURE.md §0`: a uniform typed-GC representation is what lets a non-UI frontend share codegen without inheriting the linear-memory flat ABI.

## 2. `lower_to_lir/blocks.rs` — the 8.5k-line monster

`BlockLowering` is a single struct with **50+ fields** (output vecs, monotonic
counters `next_slot`/`next_block`/`next_memory_offset`, `current_ops` +
`ops_stack`, for-loop stacks, deferred-body queues, signal layout) and the file
is ~8,500 lines — by far the largest in the repo. It works but is the hardest
thing to modify safely. Notable shortcuts inside:
- **Expr table is not interned/deduped** — `intern_expr` "always add[s] - could deduplicate later" (`blocks.rs` ~6749). Identical exprs get distinct `ExprId`s. (Strings *are* deduped.)
- **Deferred emission** with `pending_block_id_override` — block ids are pre-allocated and stashed so deferred handler/derived bodies can reference a block before it's emitted (`blocks.rs:~544`). Correct but subtle ordering dependency.
- Hard-coded sizing: `blocks.rs:705` "TODO: Compute this dynamically in codegen based on actual string/signal sizes."
- `todo!()` cliffs: unsupported for-loop iterable has "no LIR classifier" (`blocks.rs:3758`); other arms at `blocks.rs:2659`.

A safe-decomposition target: split by concern (slots, signals, control flow,
deferred bodies) the way `wasm/codegen/` is split.

---

## 3. Type system / front-end shortcuts

- **Named types unresolved in the type interner**: `AstTyKind::Named(_)` is interned as `Unknown` "for now" until name resolution (`types/interner.rs:331`); similarly HIR "keep[s] as identifier for now, will be resolved in THIR" (`hir/lower.rs:1068`).
- **Lambdas/closures incompletely typed**: `thir/typeck.rs:978,1652` "TODO: capture analysis"; `:1655` "TODO: infer function type from params and body". Closure capture and full function-type inference are stubbed.
- **`match` not real yet**: `lower_to_lir/component.rs:626` "TODO: Desugar to match expression" — conditional lowering is special-cased rather than general match.
- **Error expr reaching LIR is a crash, by design**: `component.rs:790` `todo!("Error expression reached LIR lowering")` — relies on typeck having stopped the pipeline first (see No-Silent-Fallbacks).

---

## 4. Codegen (`yel-wasm-codegen`) shortcuts

- **Callbacks/host-imported handlers not wired**: `wasm/expr.rs:406` "TODO: Wire up actual host-imported callback calls." Event handlers that should call back into the host are incomplete.
- **No `[resource-drop]`** for freed handles: `wasm/codegen/signal_emit.rs:451` — freed component handles don't return to the registry → **handle leak** over a long-running session.
- **Stringify fast-paths only**: `wasm/expr.rs:463,475,487` `*-to-string` arms `todo!()` on unexpected arity; numeric repr "hit[s] the scalar fast-path and fall[s] back to S32 for…" (`wasm/functions.rs:477`).
- **WIT version hard-defaulted**: package version "default[s] … to `0.1.0` for now" when a source omits it (`wasm/functions.rs:328`, mirrored in `yelc/pipeline.rs::wit_options`).
- **`color`/`brush` unsupported as property/signal types**: they work only as element *attribute values* (`Text { color: #ff0000 }`). As a stored property (`c: color = #ff0000`) the surface primitive `InternedTyKind::Color` (4-byte, `lir/layout.rs:276`) and the ADT a hex literal / named case desugars to (`Adt(known.variants.color)`, `hir/lower.rs:1123`) have different storage shapes, so typeck rejects the assignment (`expected color, found Color`) and codegen can't flatten it (`variant ctor payload flattens to 4 slots but joined shape only has 0`). Fixing needs the two representations unified across typeck + layout + codegen. `yel-smith` deliberately does not emit them as property types (`crates/yel-smith/src/lib.rs`).

---

## 5. Layout / definitions stubs

- **Variant layout incomplete**: `lir/layout.rs:160` "TODO: Compute for user-defined variants", `:166` "TODO: Look up from definitions" — user-defined variant sizes are not fully computed.
- **Block-role classification placeholders**: `lir/block.rs:157,159` "TODO #105: classify"; `lir/function.rs:179` `FunctionRole` has a "Catch-all for less-classified blocks (legacy / migration)" variant.
- **Stdlib element callbacks missing**: `stdlib_lookup.rs:720,731,772,783` — `clicked`/`changed`/`submitted`/`toggled` callbacks are TODO on several builtin elements.

---

## 6. Missing abstractions — walker traits & builders (research)

The IRs have **no shared traversal or construction abstraction**, so the same
"match every expr/node/stmt variant and recurse into children" boilerplate is
hand-written in many places, and IR nodes are constructed imperatively with
ad-hoc counters. This is the single most pervasive structural debt.

### 6.1 Duplicated recursive walkers

**THIR side — done.** The typed-tree read / write / dependency walkers now
share one traversal: [`thir::visit::ThirVisitor`] + the free `walk_expr` /
`walk_stmt` functions hold the recursion in one place, and each analysis
overrides only the arms whose *action* it needs (plus a `visit_closure` hook
for whether to descend into closure bodies). `signalck`'s `collect_expr_reads`
/ `collect_stmt_writes` (the `typeck` copies were already folded into these
shared fns) and `lower_to_lir/component.rs`'s `collect_dependencies` are now
thin `ThirVisitor` impls. `walk_expr`/`walk_stmt` are exhaustive with **no**
`_` arm, so a new `ThirExprKind`/`ThirStatement` variant is a compile error in
`thir/visit.rs` — the single place to teach the new shape.

**Still hand-rolled:**

- `lower_to_lir/blocks.rs` `collect_deps_recursive` / `collect_deps_from_stmt`
  (LIR). Now that exprs are a flat `LirExprId` arena (§3, done), these could
  fold into either a `LirVisitor` over the arena or a plain linear arena scan —
  the latter is exactly what the codegen aggregators in `wasm/mod.rs`
  (`collect_strings_from_expr`, `collect_runtime_needs`, …) now do, since every
  subexpression is its own arena entry.
- The lowering walkers (`hir/lower.rs`, `lower_to_lir/component.rs`/`blocks.rs`
  `lower_node`/`lower_expr`/`lower_statement`) repeat the same dispatch
  skeleton, and the rewrite passes (`lir/boundary_rewrite.rs`, `lir/dedupe.rs`)
  hand-walk op streams to remap slot ids. A `Fold`-style trait (returns
  rewritten nodes) fits the rewrite passes; the lowering walkers produce a
  *different* IR so they don't map onto a same-IR visitor as cleanly.

### 6.2 What to research / adopt

**Visitor / walk traits (read-only + mut).** The proven pattern (rustc
[`rustc_hir::intravisit::Visitor`](https://rustc-dev-guide.rust-lang.org/hir.html#hir-visitors)
and `rustc_ast::visit`) splits *traversal* from *action*: a `Visitor` trait whose
`visit_expr`/`visit_node` default to calling free `walk_expr(self, e)` functions
that recurse into children. An analysis overrides only the arms it cares about;
the recursion lives in exactly one place. `syn`'s
[`Visit`/`VisitMut`/`Fold`](https://docs.rs/syn/latest/syn/visit/index.html) are
the same idea, code-generated.

`thir/visit.rs` (`ThirVisitor` + `walk_expr`/`walk_stmt`) adopts the read-only
half of this for THIR. It follows intravisit's `visit_*` → `walk_*` split but
**not** intravisit's `NestedFilter`/`nested_visit_map` machinery: that exists to
let a HIR visitor decide, per nested *item/body*, whether to descend, and HIR
has many such nesting points. THIR has exactly one — the closure body — so the
"descend into this nested body?" knob is a single `visit_closure` hook
(`DepCollector` overrides it to a no-op; the read collectors take the default
and descend). A `LirVisitor` (§6.1) is still open; for the flat LIR arena a
linear scan is often simpler than a visitor.

- Hand-roll one `walk_*` module per IR (HIR/THIR/LIR), or
- generate it with a derive (`derive-visitor`, or a small local `macro_rules!`/proc-macro over the `*Kind` enums), or
- for the slot-remapping rewrite passes, a `Fold`-style trait that returns rewritten ops instead of mutating in place.

**Builders for IR construction.** `BlockLowering` is a 50+-field struct that emits
ops imperatively (§2). The repo already has one good builder — the fluent
`Diagnostic::error(..).with_span(..).with_note(..)` (`diagnostic.rs`) — and that
style should extend to IR nodes: an expr/op/block builder that owns the
counters + interner and exposes `expr(kind) -> ExprId`, `op(..)`, `block(..)`,
shrinking the field soup and centralizing slot/string interning. Research
options: hand-rolled typed builders, or the
[`bon`](https://docs.rs/bon) / [`typed-builder`](https://docs.rs/typed-builder)
crates for the struct-construction cases (e.g. `LirSignal`, `LirEffect`,
`LirSlotInfo`).

**Caveat:** introduce these incrementally and behind the existing No-Silent-Fallbacks
rule — a generated/default walker must still make "unhandled new variant" a loud
failure, not a silent skip. Start by unifying the THIR dependency-collection trio
(§6.1, lowest risk, highest duplication) and measure before generalizing.

## 7. Smaller / cosmetic

- **`yel-host` CLI name mismatch**: the clap derive still names the command `yel-run` in metadata though the binary is `yel-host` (noted in `yel-host/CLAUDE.md`).
- **`lir_rust.rs` dead path**: the LIR→Rust generator is commented out in `yel-wasm-codegen/lib.rs` (`// pub use lir_rust::generate_rust;`). Either revive or remove.
- **`test.wasm` committed at repo root** — a stray build artifact.
- **Large files generally**: `syntax/parser.rs` ~3.3k, `thir/typeck.rs` ~2.8k, `lower_to_lir/component.rs` ~1.1k, `hir/lower.rs` ~1.4k — all candidates for splitting.
- **README warns** "Highly WIP — broken builds on main are common." Don't assume `main` is green.

---

## How to use this doc

When you start a refactor, find the relevant section, then cross-reference the
plan in [`plans/`](../plans) and the architecture in
[`ARCHITECTURE.md`](ARCHITECTURE.md). When you *fix* one of these, delete its
entry here in the same change so the inventory stays trustworthy.
