# Yel Compiler Architecture

> Deep reference for the current state of the codebase. The per-crate
> `CLAUDE.md` files link here for detail. Last reviewed against `main` on
> 2026-06-29. When you change pipeline shape, IR fields, or crate boundaries,
> update this file in the same change.

Yel is a declarative, reactive UI language that compiles to a **WebAssembly
component** (component-model, GC + linear memory) talking to a host through the
WIT interface `yel:ui/dom@0.1.0`. See [`LANGUAGE.md`](../LANGUAGE.md) for the
surface language and [`README.md`](../README.md) for project goals.

---

## 0. Direction of travel (read this first)

**The codebase is mid-migration from a UI-specific compiler to a generic one
where the LIR and the entire back-end are a frontend-agnostic substrate shared
by both Yel (UI) and the visual flow language.** Almost every active refactor
listed in [§9](#9-in-progress-refactors-read-before-touching-lircodegen) and
every transitional bridge in [`TECH_DEBT.md §1`](TECH_DEBT.md) exists to serve
this single goal. Keep it in mind for *any* change to `lir/`, `lower_to_lir/`,
or `yel-wasm-codegen`.

- **Where we are:** the back-end was born inside the UI compiler. It still reads
  UI-component-specific state directly — the mount-tree `tree_shape`, symbolic
  `BoundaryField` slots, an implicit `(ref $Comp)` self-param, `yel:ui/dom`
  assumptions. `LirResource` (recently renamed from `LirComponent`) still carries
  UI-only fields that codegen consumes "during the transitional phases."
- **Where we're going:** LIR becomes a neutral IR of typed slots, blocks, GC
  struct/array types, and plain `LirOp`s. Codegen reads everything through the
  **arena traits** in `lir/arena.rs` (`LirExprArena` / `LirStringArena` /
  `LirSlotArena` / `LirComponentArena` / `LirFunctionLike`) so it never touches a
  concrete `LirResource`. The UI mount-tree synthesizer becomes *just one pass
  Yel chooses to run*, writing its output into the same generic structures a
  non-UI frontend uses. The flow frontend (`yel-flow-core`) already drives codegen
  via `generate_component` / `generate_function_module` through these traits.
- **How to tell which side a thing is on:** anything named `tree_shape`,
  `boundary`, `mount`, `BoundaryField`, `has_self_ref_param`, or `yel:ui/dom`,
  and anything using `LirSlotId::legacy_u32()`, is UI-coupled / transitional and
  is being pushed either *up* into a Yel-only lowering pass or *down* into generic
  LIR ops. New back-end code should depend only on the arena traits and generic
  ops, never on UI concepts.

The two plans that carry this work: [`plans/lir-resource-flatten.md`](../plans/lir-resource-flatten.md)
(generalize LIR) and [`plans/flow-frontend.md`](../plans/flow-frontend.md) (the
second frontend that proves it).

---

## 1. Workspace layout

The root `Cargo.toml` workspace members:

| Crate | Role | Entry points |
|-------|------|--------------|
| [`yel-core`](../crates/yel-core) | The compiler front-end + IRs. Parse → HIR → THIR → LIR. No target code. | `Compiler` (`compiler.rs`), `CompilerContext` (`context.rs`) |
| [`yel-wasm-codegen`](../crates/yel-wasm-codegen) | Back-end. LIR → WASM component, WIT, and DOT. | `generate_wasm*`, `generate_wit`, `generate_dot` (`lib.rs`) |
| [`yelc`](../crates/yelc) | CLI driver + shared lowering pipeline used by all drivers. | `main.rs` (CLI), `pipeline.rs` (`lower_all`) |
| [`yel-lsp`](../crates/yel-lsp) | Language server (tower-lsp): diagnostics, hover, completion, semantic tokens. | `server.rs`, `main.rs` |
| [`yel-smith`](../crates/yel-smith) | `wasm-smith`-style random **valid** Yel source generator for fuzzing. | `lib.rs`, `main.rs` — see its `CLAUDE.md` |
| [`yel-host`](../crates/yel-host) | Wasmtime dev host implementing `yel:ui/dom` as an in-memory stub. | `main.rs` — see its `CLAUDE.md` |

**Detached / experimental (NOT workspace members, gitignored):** the visual
flow-graph frontend — `yel-flow-core`, `yel-flow-lsp`, and the `floc` CLI. They
reuse `yel-core`'s LIR + `yel-wasm-codegen` directly. See [§7](#7-flow-frontend-experimental)
and [`plans/flow-frontend.md`](../plans/flow-frontend.md).

Non-crate dirs: `editors/vscode` (extension), `yel-viewer` (web playground),
`yel-flow-editor` (flow editor UI), `examples/`, `scripts/`, `plans/`
(refactor plans — read before large refactors).

---

## 2. The compilation pipeline

```
source (.yel)
  │  yel-core::syntax::parse  (pest grammar)
  ▼
AST            crates/yel-core/src/syntax/ast.rs        — strings, spans, no ids
  │  Compiler::lower_to_hir   (hir/lower.rs)
  ▼
HIR            crates/yel-core/src/hir/                  — tree; NodeId/LocalId; names still strings; no types
  │  Compiler::type_check     (thir/typeck.rs)  +  thir/signalck.rs
  ▼
THIR           crates/yel-core/src/thir/                 — typed tree; names resolved to DefId/FieldIdx; every expr has Ty + ExprId
  │  Compiler::lower_to_lir    (lower_to_lir/component.rs → blocks.rs)
  ▼
LIR            crates/yel-core/src/lir/                  — block-based; flat op stream; interned expr/string tables; GC + memory layout
  │  yel-wasm-codegen          (wasm/, wit.rs, dot.rs)
  ▼
WASM component (+ WIT, + DOT debug graph)
```

The phases are explicit methods on `Compiler` (`crates/yel-core/src/compiler.rs`):
`parse`, `lower_to_hir`, `type_check`, `lower_to_lir`, plus the globals variants
`type_check_globals` / `lower_globals_to_lir`. The whole loop is orchestrated
once in [`yelc/src/pipeline.rs::lower_all`](../crates/yelc/src/pipeline.rs) and
reused by every driver (CLI, native lib, WASI). Errors **accumulate** in
`ctx.diagnostics`; `lower_all` bails between phases via `compiler.has_errors()`
rather than `Result`-per-node.

### 2.1 The shared state: `CompilerContext`

One `TyCtxt`-style struct (`context.rs`) owns all global compiler state and is
threaded (`&` / `&mut`) through every phase:

- `interner: Arc<Interner>` — string interning → `Name(usize)` (`interner.rs`)
- `types: TypeInterner` — type interning → `Ty(u32)` (`types/interner.rs`)
- `defs: Definitions` — every definition, `IndexVec<DefId, DefItem>` + `(Name, Namespace)→DefId` map (`definitions.rs`)
- `source_map: SourceMap` + `Span` — byte-offset spans across files (`source.rs`)
- `diagnostics: Diagnostics` — accumulating error sink (`diagnostic.rs`)
- `known: KnownDefinitions` — cached `DefId`s for builtins (`known.rs`, populated by `stdlib_lookup.rs`)
- `dom_imports: Option<DomImports>` — `DefId`s of the `yel:ui/dom` host functions (`dom_imports.rs`)
- Interior-mutable side tables: `block_names: RefCell<…>`, `block_id_counter: Cell<u32>` (so `alloc_block_id(&self)` works through a shared ref), `component_lifecycle_blocks`, `global_fanout_blocks`.

---

## 3. The IR layers

### 3.1 AST — `syntax/`
- `syntax/parser.rs` (~3.3k lines) wraps a **pest** grammar; `syntax/ast.rs` (~940 lines) is the tree.
- Carries spans; identifiers are plain strings; `AstTyKind` includes `Named(..)` which is **not** resolved here.

### 3.2 HIR — `hir/`
- `HirExpr { kind: HirExprKind, span }`, `HirNode { id: NodeId, kind, span }`. Recursive variants box children (`Binary { lhs: Box<HirExpr>, … }`).
- `hir/lower.rs` (~1.4k lines) runs **register-then-lower** in phases so forward references resolve: (1) register type defs, (1b) register elements / imported components / globals, (2) register component decls without bodies, (3) lower bodies.
- Names are still strings here ("resolved in THIR"); `local_scope.rs` tracks locals with a push/pop scope stack.

### 3.3 THIR — `thir/`
- `ThirExpr { id: ExprId, kind: ThirExprKind, ty: Ty, span }`. Names are resolved: `Def(DefId)`, `Field { field_idx: FieldIdx, field_def: DefId, .. }`, etc.
- `thir/typeck.rs` (~2.8k lines) is **bidirectional**: a `Mode` enum (`Infer` synthesize / `Check(Ty)` against expected). It records a `span → Ty` `TypeMap` for the LSP, recovers from errors by assigning `Ty::ERROR` and continuing, and keeps numeric literals polymorphic.
- `thir/signalck.rs` is a **read-only** post-typeck analysis: walks the THIR producing `SignalDependencies` (`binding_reads`, `handler_writes`, `effects_by_signal` inverted index) that drives reactivity.

### 3.4 LIR — `lir/` (+ `lower_to_lir/`)
This is the largest and most actively-refactored area. THIR→LIR lowering lives in
`lower_to_lir/` (kept **outside** `lir/` so `lir/` has no HIR/THIR dependency).

Two-stage lowering (`lower_to_lir/component.rs`):
1. **Tree LIR** (`TreeLirResource { signals, effects, body: Vec<LirNode> }`) — `LirLowering` discovers signals/effects (`collect_dependencies` walks each expr, sorts+dedups deps).
2. **Block LIR** (`LirResource`) — `BlockLowering` (`lower_to_lir/blocks.rs`, **~8.5k lines** — the single biggest file) flattens the tree into blocks of flat `LirOp`s, allocating slots/blocks/strings and interning exprs/strings.

Key LIR data structures (`lir/`):
- `LirModule { components: Vec<LirResource>, global_defaults: HashMap<DefId, LirExpr>, package }` (`lir/module.rs`) — the whole compilation unit.
- `LirResource` (`lir/node.rs`) — one component/resource: `blocks`, `exprs` (interned), `strings` (interned + deduped), `slots`, `signals`, `effects`, `body_tree`, `tree_shape`, GC `struct_types`/`array_types`. (Renamed from `LirComponent`; see tech debt.)
- `LirBlock` / `LirOp` (`lir/block.rs`, ~1.6k lines) — block = params + flat op stream. Ops reference exprs by `ExprId`, strings by `StringId`, values by `LirSlotId`.
- `LirExpr { kind: LirExprKind, ty: Ty }` (`lir/expr.rs`).
- `LirTypeRef` (`lir/block.rs`) — **symbolic** type references (`ComponentStruct`, `OtherComponentStruct(DefId)`, `TreeBoundary(TreeBoundaryId)`, `GlobalsStruct(DefId)`, `GcVariantCase(Ty, u32)`) resolved to concrete wasm type indices only in codegen.
- Codegen reads LIR through **arena traits** (`lir/arena.rs`): `LirExprArena`, `LirStringArena`, `LirSlotArena`, aggregate `LirComponentArena`, and `LirFunctionLike` — so both `LirResource` and the flow frontend's per-function adapter feed the same emitter.
- Reactivity layout: `lir/signal.rs`, `lir/signal_layout.rs` (per-signal slot/memory layout), `lir/tree_shape.rs` (mount-tree boundaries), `lir/layout.rs` (~650 lines), `lir/struct_types.rs` (GC struct/array decls), `lir/boundary_rewrite.rs` (boundary-field slot resolution), `lir/dedupe.rs` (post-pass structural dedup of identical update blocks via slot-normalized hashing).

### 3.5 IDs, indices, interning (cross-cutting)
- `ids.rs`: one `u32` newtype per index space — `DefId`, `FieldIdx`, `VariantIdx`, `LocalId`, `ExprId`, `NodeId`, `BlockId`, plus the **stable-across-lowering** correlation ids `ForId`, `IfId`, `TreeBoundaryId`. `DefId::INVALID = u32::MAX`.
- `index_vec.rs`: `IndexVec<I: Idx, T>` — typed arena; `push` returns the id; only the matching id type indexes it.
- Interners cache-on-miss (look up first, insert on miss). The string and type tables dedup; the **LIR expr table does not** (documented TODO).

> These conventions are codified as a reusable skill: [`/compiler-skills`](../.agents/skills/compiler-skills/SKILL.md).

---

## 4. Reactivity model (signals & effects)

- A component **property** is a *signal*. Reads in bindings/text create *effects*; handler bodies *write* signals.
- `signalck.rs` builds the inverted `signal → [EffectSource]` index. `lower_to_lir` turns each reactive binding into an *update block*; writing a signal fans out to its dependent update blocks.
- Pointer-repr signals (records/tuples) are stored in **both** the component's `$Comp` GC struct **and** linear memory; `SignalStorage` models these as independent `gc` + `mem` backings. (See memory note `project_signal_storage_dual`.)
- `lir/dedupe.rs` merges structurally-identical per-(boundary, signal) update blocks to cut code size.

---

## 5. Back-end — `yel-wasm-codegen`

Reads LIR (via arena traits) and emits artifacts. Public API in `lib.rs`:
`generate_wasm` / `generate_wasm_module` / `generate_wasm_with_wit`,
`generate_component` / `generate_function_module`, `generate_wit`, `generate_dot`.
Errors are a typed `CodegenError` (never silent fallbacks — see crate `CLAUDE.md`).

> **Migration target (see [§0](#0-direction-of-travel-read-this-first)).** This
> crate is becoming a *generic* back-end shared by Yel and the flow language. It
> should consume LIR only through the `lir/arena.rs` traits and generic ops.
> Today some paths still reach for UI-component specifics — the `gc_types`
> emitter walks `tree_shape` boundaries, signal emission assumes the `$Comp`
> self-ref and `yel:ui/dom`, and several helpers branch on UI shapes. Those are
> being moved up into Yel-only lowering or down into generic LIR ops; don't add
> new UI assumptions to codegen. The `generate_function_module` path (no
> mount-tree, arbitrary return type) is the already-generic entry the flow
> frontend uses.

- `wasm/` — the encoder. `wasm/codegen/` splits emission by concern: `build.rs` (type-section + function-type interning), `op_emit.rs`, `block_fn.rs`, `dispatch.rs`, `signal_emit.rs`, `accessors.rs`, `record_list.rs`, `scratch.rs` (scratch-local allocation), `name_section.rs`, `function_type.rs`, `constants.rs`.
- `wasm/gc_types.rs` — emits WASM-GC `(struct …)` / `(array …)` types from the tree-shape boundaries.
- `wasm/repr.rs` — value representation (`InternalRepr`: scalar vs flat/GC).
- `wasm/runtime/` — `memory.rs` (linear-memory bump/layout) and `strings.rs` (string storage as `(ptr,len)`).
- `wit.rs` + `wit_ast.rs` — WIT world/interface emission (exported component interface, imported `yel:ui/dom`).
- `dot.rs` — Graphviz DOT of the reactive dependency graph (debug/snapshot).
- `lir_rust.rs` — (currently disabled) experimental LIR→Rust source path.

The host contract is `yel:ui/dom@0.1.0` (see `yel-host` `wit/` and its `CLAUDE.md`).

---

## 6. CLI & drivers — `yelc`

- `pipeline.rs` (`lower_all`, `wit_options`, `diagnostics`) is the shared, transport-neutral front-end loop used by the CLI binary, the native lib API, and the WASI component. Output of lowering is `Lowered { module: LirModule, hir }`.
- `main.rs` — clap CLI `yelc`. Subcommands: `compile -o <wasm|wit|dot|rust>` (with `--release`, `--opt`, `wasm-opt` passthrough + strip), `ir` (dump LIR, `--pretty`/`--json`), `check`. Debug dumps include `--hir`.

---

## 7. Flow frontend (experimental)

`yel-flow-core` (+ `yel-flow-lsp`, `floc`) compile a **node-based visual flow
graph** to WASM by reusing yel-core's LIR and yel-wasm-codegen. It does **not**
go through HIR/THIR. Flow modules: `graph.rs` (node graph), `flow_ir.rs`
(`FlowFunc`), `lower.rs` (graph→CFG→structured control flow), `to_lir.rs`
(FlowIR→LIR via the arena traits), `ty.rs`, `wire.rs`, `registry.rs`,
`wit_emit.rs`, `compile.rs`. The whole point of the in-progress
[`lir-resource-flatten`](../plans/lir-resource-flatten.md) refactor is to make
LIR fully frontend-agnostic so flow can emit it without inheriting UI/mount-tree
concepts. See [`plans/flow-frontend.md`](../plans/flow-frontend.md).

---

## 8. Testing

- **Snapshot/golden** (insta): `yelc/tests/snapshot.rs` pins generated WIT/DOT by running the real binary; `INSTA_UPDATE=always` or `cargo insta review` to update.
- **Diagnostic & golden fixtures**: `yel-wasm-codegen/tests/fixtures/{positive,diagnostics,known_bugs}` — `.yel` paired with `.wit`/`.dot`/`.expected`/`.failure`.
- **Execution (e2e)**: `yel-wasm-codegen/tests/execution.rs` runs the component under Wasmtime with recording DOM closures and asserts ordered DOM-op subsequences. **Correctness rule: never soften an assertion to match known-wrong output**; mark known bugs `#[ignore]` with a reference.
- **Fuzzing**: `yel-smith` generates valid sources; batch-validate with `wasm-tools validate` (see its `CLAUDE.md`). Baseline fuzz pass rate is tracked (see memory `project_typed_gc_migration_stage0`).
- **Determinism**: output must be byte-stable — collections derived from hash maps/sets are sorted+deduped before emission.

---

## 9. In-progress refactors (read before touching LIR/codegen)

These are live migrations; expect mixed old/new naming. Details and call-site
catalogs in [`docs/TECH_DEBT.md`](TECH_DEBT.md) and [`plans/`](../plans).

1. **Typed `SlotId` ladder** — `LirSlotId::legacy_u32()` is a bridge while slots migrate from raw `u32` to a typed enum; many call sites still use it. (Git log: "per-block slot ladder", "typed SlotId enum".)
2. **`LirComponent` → `LirResource` + flatten `tree_shape`** — make LIR frontend-agnostic: replace the `tree_shape` side-channel and symbolic `BoundaryField` slot resolution with explicit GC struct-type registrations and `StructGet`/`StructSet` ops. See `plans/lir-resource-flatten.md`.
3. **WASM-GC representation migration** — move values from canonical-flat (decomposed flat ABI valtypes in slots/memory) to typed WASM-GC structs/arrays (single ref), with `wasm/repr.rs::InternalRepr` as the single source of truth. Half-migrated today (records/tuples/typed lists on GC; option/result/variant still partly flat). Goal: kill the internal canonical-flat bridges (`lir/layout.rs::canonical_flat_valtypes`) and keep flattening only at WIT boundaries; collapse the dual GC+memory signal storage. Detail + fragile invariants in [`TECH_DEBT.md §1.5`](TECH_DEBT.md). (Memory `project_typed_gc_migration_stage0`, `project_signal_storage_dual`.)
4. **Inline lifecycle decomposition** — `MountComponent` etc. lowering to plain `LirOp`s; blockers tracked in memory `project_mount_component_wrappers`.
