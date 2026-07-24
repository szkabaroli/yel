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
>
> **Checklist convention:** each item below is a checkbox. `[x]` = resolved
> (kept for the record, with the resolution noted); `[ ]` = still open. When you
> fix one, tick its box and trim the body to the one-line resolution rather than
> deleting the entry outright.

Two crate-level invariants keep debt _loud_ rather than silent — keep them:

- **No silent fallbacks** — unimplemented paths use `todo!(...)` / `Err(CodegenError::…)`, never placeholder IR/instructions. (`crates/yel-core/CLAUDE.md`, `crates/yel-wasm-codegen/CLAUDE.md`.)
- **Never soften a test assertion** to match known-wrong output (`yel-wasm-codegen/tests/execution.rs`).

---

## 1. Big transitional bridges (highest leverage)

> These all exist because the back-end is mid-migration from UI-specific to
> generic (shared with the flow language) — see
> [`ARCHITECTURE.md §0`](ARCHITECTURE.md). Each bridge is debt _only_ until that
> migration lands; the fix is "finish the generalization," not "patch the bridge."

- [x] **1.1 `LirSlotId` slot ladder** — allocator flipped.
      Temps are per-block: `alloc_temp_slot_typed` allocates `LirSlotId::Block`
      into the current block's `slots` vec, every synth pass
      (`synth_export_lifecycle_blocks`, `synth_one_global_fanout_block`, …),
      `allocate_boundary_param_slots`, and the `boundary_rewrite` walk-slot
      allocators do the same, and each generated function declares only its own
      block's temps plus the few genuinely shared `Resource` temps. Cross-block
      Temp references panic loudly in codegen (`scratch::slot_info`). The
      `legacy_u32()` bridge itself is deleted (debug names render slot ids via
      `Display`). No flat index lookups survive. Measured on the complex fuzz fixture: 136,756 →
      1,573 declared locals; dev component 3.0 MB → 77 KB.

- [x] **1.2 `tree_shape` side-channel + `BoundaryField` chain walk** — resolved at IR level.
      The IR no longer carries the side-channel: `LirResource.tree_shape` is
      deleted (the resource holds only the `struct_types` / `array_types`
      registry, which codegen exclusively reads), `LirSlotKind::BoundaryField`
      and the symbolic chain walk are gone (boundary-field access is explicit
      `StructGet`/`StructSet` resolved by `lir/boundary_rewrite.rs`), and the
      `boundary_params: Vec<TreeBoundaryId>` field is deleted — blocks carry
      typed `boundary_param_slots` allocated by `LirBlock::set_boundary_params`,
      and ids derive from the slots' `RefNullForBoundary` val_tys.
      What remains is internal to the YEL-only synthesis pass: `tree_shape.rs`'s
      `ComponentTreeShape`/`TreeBoundary`/`TreeFieldDecl` survive as the
      synthesizer's scratch representation (projected into the registry by
      `struct_types::project_tree_shape` before the resource is built), and the
      lowerer's ~30 `self.tree_shape` reads consume that scratch. Collapsing
      synthesize+project so the registry is built directly (and `node_field`
      becomes a standalone lowering map) is the remaining — purely internal —
      cleanup; see `plans/lir-resource-flatten.md` Stage 5e.

- [x] **1.3 `LirComponent` → `LirResource` rename** — resolved (naming).
      The trait is `LirResourceArena` and the generic-layer docs (`lir/arena.rs`,
      `lir/node.rs`) frame a `LirResource` as a frontend-agnostic multi-block
      compilation unit (UI: a component; flow: a function package). What remains
      is §1.2's substance, not naming: the UI-only fields on the resource are
      still read by codegen during the transitional phases and disappear as
      THIR→LIR lowers them inline.

- [x] **1.4 Legacy reactivity emission path** — resolved (globals residue tracked in §1.5).
      The legacy ops are gone: `LirOp::SignalWrite` / `SignalWriteExpr` /
      `InitSignal` / `InitSignalDefault` deleted from the IR, their codegen arms
      and store helpers (`emit_signal_struct_store_*`) deleted from
      `op_emit.rs` / `signal_emit.rs`. Every signal write and init lowers inline
      through `blocks.rs::inline_signal_write_or_init_from_expr` to generic ops
      (`EvalExprToSlots` + `StructSetSym` / `GlobalFieldSet`); a shape the helper
      can't handle is a loud `todo!` at the emit site, not a fallback.
      Component-local triggering is direct `CallBlock`s
      (`emit_trigger_for_signal`); unobserved-signal writes emit no trigger at
      all. Global triggering is now single-path too: write sites emit a
      `LirOp::TriggerEffects` placeholder, and the module-level
      `resolve_global_triggers` pass (runs after every component is lowered —
      killing the old lowering-order dependence) synthesizes each observer's
      fanout block (a plain LIR block: registry loop → the same per-instance
      update fns locals call) and expands every placeholder into direct
      `CallBlock`s. The codegen-side duplicate fanout implementation
      (`generate_global_fanout_for`, `emit_boundary_chain_from_self_inline`,
      `global_fanout_func_idx` + its function pre-registration) is deleted;
      a `TriggerEffects` reaching codegen is a hard `InvalidIR` error.
      Codegen-synthesized global writers (binding setters) call the same LIR
      fanout blocks through `emit_trigger_effects`.
      Resolving this also fixed a family of lowering/codegen predicate mismatches
      (records-with-tuple-fields slot typing, `option<tuple|record>` gc-variant vs
      ref-collapse, payload-less `VariantCtor` i32-discriminant shortcut on
      GcVariant types) that accounted for all four long-standing fuzz baseline
      failures — the 100-seed set went 96/100 → 100/100.

### 1.5 WASM-GC representation migration (canonical-flat → typed GC)

Values are mid-migration from a **canonical-flat** representation (a type
decomposed into its flat ABI valtypes, spread across multiple slots / linear
memory) to **typed WASM-GC** structs and arrays (a single typed ref). The
intended single source of truth for "how is this Yel value represented on the
stack / in a block" is `wasm/repr.rs::InternalRepr` (`Zero`, `Scalar`,
`GcRef(struct_idx)`, `GcArrayRef(arr_idx)`, `GcVariant(idx)`). The
half-migrated state is the debt:

- [x] **Internal representation.** Every value shape is a single typed ref (or scalar) internally: records/tuples/lists are GC refs, option/result/variant are `GcVariant` subtype hierarchies (nullable-ref collapse for eligible `option<T>`). The WIT boundary is served by two recursive pairs in `wasm/codegen/accessors.rs` — `emit_member_lift_to_memory` (GC → canonical, over `GcRefSource`) and `emit_member_pack` (canonical → GC, over `CanonicalSource`) — instead of per-shape generators.
- [x] **The flat bridge that must shrink to the edges** — resolved. Canonical-ABI flattening is now type-enforced to WIT boundaries: both `canonical_flat_valtypes` and `flatten_core_valtypes` require a [`WitBoundary`] witness. The target (memory `project_typed_gc_migration_stage0`) is met.
  - **LIR side — resolved.** `LirLayoutContext::canonical_flat_valtypes` now has **zero internal LIR-pass callers**; it is reached only through codegen's `WasmPackageBuilder::canonical_flat_valtypes` boundary delegation (the two were unified — one algorithm, no drift; the canonical flattener never emits GC refs, so yel-core's ref-free join and codegen's ref-aware `join_flat_valtypes` provably coincide). The two former internal callers turned out to be **stale over-reservations, not correct mirrors**: (a) `populate_internal_lifecycle_scratch` sized the constructor's flat-scratch region from signal *types* (`canonical_flat_valtype_counts`), but the ctor's inline `EvalExprToSlots`+`StructSetSym` init only needs the *expression-shape* scratch that `compute_flat_scratch_counts` already computes — the type-based stamp just over-declared locals; (b) `expr_contains_composite_field_load`'s `Index` arm reserved a memory base-pointer scratch for composite list elements, but indexing is a single `array.get` yielding one GC ref (the legacy memory `list_get` path was deleted in Phase 7) and never feeds `emit_flat_slot_load_at_ptr`. Both removed; the `InternalLifecycleScratch` struct/field, `canonical_flat_valtype_counts`, and `max_flat_counts` are deleted. Verified: 200-seed fuzz-validate clean, execution + core suites green, all fixtures deterministic, and a `list<record>[i].field` round-trip probe validates.
  - **Codegen `canonical_flat_valtypes` — enforced.** All 25 call sites were audited: every one is genuinely boundary (WIT setters/getters, ABI function signatures, the callback-import direct/indirect-return decision, `cabi_post` / setter-spill decisions, canonical memory layout, and the canonical→GC pack materializers). The function now requires a zero-sized [`WitBoundary`] witness (`repr.rs`), so the type system enforces that every canonical flattening names itself as boundary code — a new internal caller must write `canonical_flat_valtypes(ty, WitBoundary::assert())`, which is greppable and reviewable rather than a silent reflex. Byte-neutral (ZST); execution + core + 100-seed fuzz green. Not hermetic (the witness is crate-constructible — distributed boundary callers rule out a single-module privacy wall), but it converts a silent convention into an explicit, checked act.
  - **Codegen `flatten_core_valtypes` — gated.** `flatten_core_valtypes(ty, WitBoundary)` now requires the same zero-sized boundary witness as `canonical_flat_valtypes`, so the type system enforces that every canonical-flattening call names itself boundary code. All 7 internal stack-arity uses had already moved to `internal_stack_slots`; the last non-boundary caller — the `VariantCtor` slot-count return in `emit_expr` — was the blocker. It's replaced by an explicit split: a collapsed `option<ref>` emits its single ref via `emit_variant_ctor_flat` and returns `internal_stack_slots` (==1); **any other** non-GcVariant `VariantCtor` reaching `emit_expr` is now a loud `InvalidIR` error, since genuine multi-slot flat variants (e.g. `attribute-value`) only cross the host boundary via `PushExpr` (op_emit). That converts the old "hit by zero seeds but not provably unreachable" worry into a proof: a 200-seed fuzz run + full fixture suite + execution suite never trips the guard. Every surviving `flatten_core_valtypes` caller is boundary (WIT lift/lower accessors, ABI signatures, callback lower/return, canonical memory + list-element layout, `emit_variant_ctor_flat`'s host-boundary path, the `ternary_block_types` registry consumed only by `canonical_block_type`). Byte-neutral (ZST witness + the one reachable arity is unchanged): snapshots, execution + core suites green, 200-seed fuzz-validate clean.
- [x] **Incomplete materialize paths.** Resolved by deletion: `lower_to_lir/signals_inline.rs` (the inline _memory_ signal-write helpers with their "bail until const-materialize" todos) is gone entirely — no signal or global write lowers to linear memory anymore, so there is nothing left to materialize into memory.
- [x] **Dual signal storage.** Pointer-repr signals (records/tuples) used to be stored in **both** the `$Comp` GC struct **and** a per-instance linear-memory cell. That backing is fully **removed**: records/tuples live solely on the GC struct, and boundary getters/setters lift/lower through a `cabi_realloc` scratch. The scaffolding is gone too — `SignalStorage.mem` / `MemSlot` / `memory_size` / `signal_memory_offset` deleted from `signal_layout.rs`, `MemoryLayout::signal_addr` + `signal_offsets` deleted from codegen, and every dead per-signal memory branch (`op_emit.rs` InitSignal/SignalWrite/InitSignalDefault, `expr.rs` SignalRead/Def, the `accessors.rs` scalar getter/variant setter fallthroughs, `blocks.rs` inline routing) removed or turned into `unreachable!`. Verified byte-for-byte behavior-neutral: full suite green + a 100-seed fuzz run with an identical failing-seed set (69/100). Only the WIT-boundary lift/lower shims and the memory-resident **globals** path survive — see below.
- [x] **Globals-in-memory.** Removed: record/tuple global properties are backed by ref-typed core wasm globals like every other property (`GlobalsBlockLayout` gives them a one-ref-slot field path; `globals_init` materializes defaults via `struct.new` + `global.set`). The whole memory path is deleted — `global_property_addrs`, the reservation loop, `signals_inline.rs`, `LirOp::MemConst` / `MemConstGlobalProp` and their codegen arms, the memory `SignalRead` fallback in `expr.rs`, and the dead store helpers (`emit_signal_store`, `emit_flat_slot_store`, `compute_slot_locals`, `is_pointer_repr`). Note this was latently **broken**, not just ugly: lowering still routed record globals to `MemConstGlobalProp` while codegen had stopped reserving memory for them, so any record/tuple global failed to compile. Pinned by the `record_global_roundtrip_through_core_globals` execution test (default-init render, handler write, read-after-write, fanout re-render) and the `global_record_tuple_props` snapshot fixture. Nothing value-shaped lives in linear memory anymore — remaining linear-memory users are the string/heap runtime, WIT-boundary lift/lower scratch, and DOM-handle slots.

> **Phased, with phase labels in comments:** e.g. `lir/block.rs:520` "Phase 2.2b switches the…". Expect `Phase N` markers; grep them to see what's done vs pending.

This is part of the same generic-back-end push as [§1.1–1.4](#1-big-transitional-bridges-highest-leverage) and `ARCHITECTURE.md §0`: a uniform typed-GC representation is what lets a non-UI frontend share codegen without inheriting the linear-memory flat ABI.

## 1.6 Globals are not first-class alongside components (the compilation unit is the file, not the component)

> Tracked in depth by [`plans/global-component-unify.md`](../plans/global-component-unify.md)
> ("Globals and components are both top-level compilation units, but today they
> run two parallel spines"). **Phases 1–4 are now done** — `LirModule` carries
> first-class `globals: Vec<LirGlobal>` and a single `imports: Vec<LirImport>`
> registry, and codegen + `wit_ast` derive from them. What remains open below is
> the front-end/lowering-driver residue (anemic `Hir`/`ThirGlobal`, the
> globals-only `resolve_global_triggers` pass, driver hand-matching) and the
> codegen `globals_init` shape. The claim this section backs: the real
> compilation unit is **the `.yel` file** (a module of top-level items), not "a
> component" — every top-level declaration should lower through one uniform item
> spine, with codegen differences (resource-with-registry vs.
> singleton-with-core-globals) expressed as a property of the item.

- [x] **HIR/THIR: globals are symmetric first-class items** — resolved.
      `HirItem`/`ThirItem` (`hir/node.rs`, `thir/node.rs`) are real
      `{Component, Global}` enums, and both now expose the **full symmetric
      accessor set** — `as_component`/`as_global`/`into_component`/`into_global`
      — so consumers no longer hand-match the enum (e.g. `pipeline.rs`'s HIR
      retention is now `filter_map(HirItem::into_component)`). The remaining
      apparent asymmetries turned out to be correct modeling, not debt, and the
      docs now say so: a global carries no node tree because a UI body is a
      *component-specific* shape (not a missing field), and its
      signal-dependency analysis lives in the `CompilerContext::signal_deps`
      side table keyed by `DefId` **exactly as a component's does** (signalck is
      read-only analysis whose output belongs with the other analysis tables for
      both kinds alike). A global's property/callback metadata living in the
      `GlobalDef` def-table entry mirrors a component's in its `ComponentDef` —
      both register a `Def`; neither carries that metadata on the HIR node.
- [x] **LIR: globals are first-class items** — resolved (phase 4). `LirModule`
      (`lir/module.rs`) now holds `globals: Vec<LirGlobal>` — a per-item struct
      (`def_id`, `name`, `is_export`, `package`, `properties: Vec<LirGlobalProperty>`
      with direction + default, `callbacks`) — the peer of `resources` for the
      non-instantiable half of the module. The old flat, item-less side-maps
      (`global_defaults: HashMap<DefId, LirExpr>` + `global_default_exprs`) are
      gone; a global is a unit again, not a bag of property `DefId`s. Default
      expressions share one module-scope `global_exprs` arena (all global defaults
      seed together at module start), and `LirModule::global_defaults_map()` is the
      one thin regrouping codegen's per-property init still consumes. The host
      import surface is likewise first-class: `imports: Vec<LirImport>` is the
      single ordered import registry (component callbacks, global callbacks, DOM),
      built once by `CompilerContext::build_import_contract`, off which both the
      core import section and the WIT import interfaces derive — killing the old
      independent `ImportLayout`-vs-`wit_ast` derivations that could drift.
- [x] **Module-scope emission no longer fabricates a `LirResource`** —
      resolved. The shared wasm emitter (`emit_expr` and its helpers —
      `emit_callback_arg`, `emit_variant_ctor_*`, `slot_local`/`slot_info`,
      `signal_index_in`, `comp_idx_of`, `emit_global_struct_store_from_expr`,
      `generate_filter_function`) now takes `&dyn LirResourceArena` instead of a
      concrete `&LirResource`. Component callers pass `&LirResource` (auto
      unsizing coercion — no call-site churn); module-scope emission
      (`generate_globals_init`, the module-scope filter loop) plugs in a
      purpose-built `lir::module::ModuleScope` arena that owns only the global
      default-expression arena — no `DefId::INVALID`, no placeholder block, no
      fake signals. The arena trait gained `exprs()` and a transitional
      `signals()` (defaulted empty for non-UI scopes); its `block()` default was
      made dedupe-robust (id-match + linear fallback) to match
      `LirResource::get_block`. The old fabricated `module_scope_carrier` is
      deleted; the minimal `LirResource::empty` shell survives only for the flow
      frontend's per-function packaging and boundary-rewrite test scaffolding —
      legitimate uses, not emission fabrication. Latent bug closed along the way:
      module-scope filter predicates used to get an **empty** expression arena
      (pinned by the `global_filter_default` fixture).
- [ ] **`resolve_global_triggers` — an entire extra compiler pass that exists
      solely because globals aren't lowered in the same one-pass-per-item flow as
      components** (`lower_to_lir/blocks.rs::resolve_global_triggers`, run once
      after every component is lowered — `pipeline.rs::lower_all`). It synthesizes
      per-(observing-component, global) fanout blocks and rewrites
      `LirOp::TriggerEffects` placeholders into `CallBlock`s; a `TriggerEffects`
      surviving to codegen is a hard `InvalidIR` error
      (`wasm/codegen/op_emit.rs:947`) that literally says "the
      resolve_global_triggers pass must run after lowering" — i.e. codegen's
      correctness depends on an out-of-band, globals-only pass with no equivalent
      for component-to-component signal propagation (handled inline, per-item).
- [x] **One shared module-lowering spine** — resolved.
      `Compiler::lower_items_to_module(items, package) -> LirModule`
      (`compiler.rs`) is the single entry every driver shares: it type-checks
      each item, lowers components + globals, and runs the module-level passes
      (`resolve_global_triggers`, global lowering, import contract). `pipeline.rs::lower_all`
      and all four codegen harnesses
      (`yel-wasm-codegen/tests/{runtime,execution,dump_wasm,integration}.rs`) now
      call it instead of each hand-rolling the `ThirItem` match plus the trailing
      module passes. The globals-only wrinkle in the fuzzer is closed too:
      `yel-smith`'s `test_generated_code_compiles` used to `continue` past
      `ThirItem::Global` and compile each component in isolation — it now lowers
      the whole module through the spine, so the global codegen paths
      (globals-init, cross-component fan-out, module-scope default expressions)
      are actually exercised. Note the module-level passes themselves are *not*
      debt: `resolve_global_triggers` is inherently a whole-module step (it needs
      every component lowered before it can wire fan-out), like a link phase —
      it lives inside the spine, not bolted onto each caller.
- [ ] **Codegen: globals get no registry/handle scaffolding, a separate
      `(start)` init function, and a separate layout pass.**
      `GlobalsBlockLayout` (`wasm/gc_types.rs`) is explicitly documented as
      "singletons — no registry / handle / array scaffolding," unlike the
      per-component `GcTypeLayout` (registry array + free-list + handle table,
      assigned in `build.rs`). Global seeding is `generate_globals_init`
      (`build.rs`), a dedicated function assembled directly in codegen via the
      `empty_module_carrier` hack, wholly outside the per-resource block-lowering
      path components use (their `constructor_block`/`internal_constructor_block`
      on `LirResource`). `compute_globals_block_layout` (`build.rs`) is also a
      separate loop over `ctx.defs.globals()` (the HIR-era side table), not a walk
      over `LirModule` items uniformly.
- [ ] **`docs/ARCHITECTURE.md` is stale relative to the in-progress unification**
      — it still describes `type_check_globals`/`lower_globals_to_lir` as parallel
      phase methods and `LirModule.components` (renamed `resources` in phase 4
      step 1). Update §2–3 alongside the next phase-4 sub-step, per the plan's own
      "Invariants" section.

---

## 2. `lower_to_lir/blocks.rs` — the 8.5k-line monster

`BlockLowering` is a single struct with **50+ fields** (output vecs, monotonic
counters `next_slot`/`next_block`/`next_memory_offset`, `current_ops` +
`ops_stack`, for-loop stacks, deferred-body queues, signal layout) and the file
is ~8,500 lines — by far the largest in the repo. It works but is the hardest
thing to modify safely. Notable shortcuts inside:

- [ ] **Expr table is not interned/deduped** — `intern_expr` "always add[s] - could deduplicate later" (`blocks.rs` ~6749). Identical exprs get distinct `ExprId`s. (Strings _are_ deduped.)
- [ ] **Deferred emission** with `pending_block_id_override` — block ids are pre-allocated and stashed so deferred handler/derived bodies can reference a block before it's emitted (`blocks.rs:~544`). Correct but subtle ordering dependency.
- [ ] **Hard-coded sizing:** `blocks.rs:705` "TODO: Compute this dynamically in codegen based on actual string/signal sizes."
- [ ] **`todo!()` cliffs:** unsupported for-loop iterable has "no LIR classifier" (`blocks.rs:3758`); other arms at `blocks.rs:2659`.

A safe-decomposition target: split by concern (slots, signals, control flow,
deferred bodies) the way `wasm/codegen/` is split.

---

## 3. Type system / front-end shortcuts

- [ ] **Named types unresolved in the type interner**: `AstTyKind::Named(_)` is interned as `Unknown` "for now" until name resolution (`types/interner.rs:331`); similarly HIR "keep[s] as identifier for now, will be resolved in THIR" (`hir/lower.rs:1068`).
- [ ] **Lambdas/closures incompletely typed**: `thir/typeck.rs:978,1652` "TODO: capture analysis"; `:1655` "TODO: infer function type from params and body". Closure capture and full function-type inference are stubbed.
- [ ] **`match` not real yet**: `lower_to_lir/component.rs:626` "TODO: Desugar to match expression" — conditional lowering is special-cased rather than general match.
- [ ] **Error expr reaching LIR is a crash, by design**: `component.rs:790` `todo!("Error expression reached LIR lowering")` — relies on typeck having stopped the pipeline first (see No-Silent-Fallbacks).
- [ ] **Two unreachable `ErrorCode` variants (dead code)**: `ErrorCode::UnknownUnitSuffix` (E0004) and `ErrorCode::MissingElement` (E0042) are defined and still have emission arms, but neither can fire — an unknown unit suffix and every `ParseError::Missing(...)` site are shadowed by an earlier `E0060` SyntaxError (the pest grammar rejects the malformed input before the semantic arm runs). Found by the diagnostics-fixture sweep: every other error code has a triggering fixture in `tests/fixtures/diagnostics/` (20 reachable codes, verified by actual `error[E00xx]`), but these two are untriggerable. Either delete them + their dead arms, or leave a note if a future grammar relaxation would surface them.
- [ ] **Two inconsistent idioms for emitting a coded diagnostic**: the concise `Diagnostics::error(span, code, msg)` convenience method (**33** call sites) coexists with the fluent `Diagnostic::error(msg).with_span(span).with_code(code)` builder (**13** sites). Only **4** of the builder sites actually need it — they attach `.with_note(...)`, which the convenience method can't express; the other ~9 are just verbose duplicates of what the one-liner does. Standardize: migrate the note-free builder sites to `Diagnostics::error(...)`, and add a note-capable convenience variant (e.g. `error_with_note` or a builder-returning helper) so "code + message + note" has one obvious form too. Low risk (pure call-site refactor; diagnostics are covered by `tests/fixtures/diagnostics/`).

---

## 4. Codegen (`yel-wasm-codegen`) shortcuts

- [ ] **Callbacks/host-imported handlers not wired**: `wasm/expr.rs:406` "TODO: Wire up actual host-imported callback calls." Event handlers that should call back into the host are incomplete.
- [ ] **No `[resource-drop]`** for freed handles: `wasm/codegen/signal_emit.rs:451` — freed component handles don't return to the registry → **handle leak** over a long-running session.
- [ ] **String/list callback-arg buffers never freed**: when a callback is invoked with a `string` / `list` argument (or a composite containing one), `emit_callback_arg` (`wasm/expr.rs`) materializes the value to a `(ptr, len)` linear-memory buffer via the per-array materializer and passes it to the host import, but nothing frees that buffer after the call returns → **per-invocation leak**. There is no `cabi_post`-equivalent on the _import_ (lower) side; the getter/return (lift) side already has `generate_cabi_post_getter`. The composite value→canonical-stack path (`accessors.rs::emit_value_to_canonical_stack`) allocates nothing itself, so only the string/list _leaf_ materializers leak. Fixing needs an import-side post-return free of every fresh buffer reachable from the lowered args.
- [ ] **Stringify fast-paths only**: `wasm/expr.rs:463,475,487` `*-to-string` arms `todo!()` on unexpected arity; numeric repr "hit[s] the scalar fast-path and fall[s] back to S32 for…" (`wasm/functions.rs:477`).
- [ ] **WIT version hard-defaulted**: package version "default[s] … to `0.1.0` for now" when a source omits it (`wasm/functions.rs:328`, mirrored in `yelc/pipeline.rs::wit_options`).
- [ ] **`color`/`brush` unsupported as property/signal types**: they work only as element _attribute values_ (`Text { color: #ff0000 }`). As a stored property (`c: color = #ff0000`) the surface primitive `InternedTyKind::Color` (4-byte, `lir/layout.rs:276`) and the ADT a hex literal / named case desugars to (`Adt(known.variants.color)`, `hir/lower.rs:1123`) have different storage shapes, so typeck rejects the assignment (`expected color, found Color`) and codegen can't flatten it (`variant ctor payload flattens to 4 slots but joined shape only has 0`). Fixing needs the two representations unified across typeck + layout + codegen. `yel-smith` deliberately does not emit them as property types (`crates/yel-smith/src/lib.rs`).

---

## 5. Layout / definitions stubs

- [ ] **Variant layout incomplete**: `lir/layout.rs:160` "TODO: Compute for user-defined variants", `:166` "TODO: Look up from definitions" — user-defined variant sizes are not fully computed.
- [ ] **Block-role classification placeholders**: `lir/block.rs:157,159` "TODO #105: classify"; `lir/function.rs:179` `FunctionRole` has a "Catch-all for less-classified blocks (legacy / migration)" variant.
- [ ] **Stdlib element callbacks missing**: `stdlib_lookup.rs:720,731,772,783` — `clicked`/`changed`/`submitted`/`toggled` callbacks are TODO on several builtin elements.

---

## 6. Missing abstractions — walker traits & builders (research)

The IRs have **no shared traversal or construction abstraction**, so the same
"match every expr/node/stmt variant and recurse into children" boilerplate is
hand-written in many places, and IR nodes are constructed imperatively with
ad-hoc counters. This is the single most pervasive structural debt.

### 6.1 Duplicated recursive walkers

- [x] **THIR walkers unified.** The typed-tree read / write / dependency walkers now share one traversal: [`thir::visit::ThirVisitor`] + the free `walk_expr` / `walk_stmt` functions hold the recursion in one place, and each analysis overrides only the arms whose _action_ it needs (plus a `visit_closure` hook for whether to descend into closure bodies). `signalck`'s `collect_expr_reads` / `collect_stmt_writes` (the `typeck` copies were already folded into these shared fns) and `lower_to_lir/component.rs`'s `collect_dependencies` are now thin `ThirVisitor` impls. `walk_expr`/`walk_stmt` are exhaustive with **no** `_` arm, so a new `ThirExprKind`/`ThirStatement` variant is a compile error in `thir/visit.rs` — the single place to teach the new shape.

Still hand-rolled:

- [ ] `lower_to_lir/blocks.rs` `collect_deps_recursive` / `collect_deps_from_stmt` (LIR). Now that exprs are a flat `LirExprId` arena (§3, done), these could fold into either a `LirVisitor` over the arena or a plain linear arena scan — the latter is exactly what the codegen aggregators in `wasm/mod.rs` (`collect_strings_from_expr`, `collect_runtime_needs`, …) now do, since every subexpression is its own arena entry.
- [ ] The lowering walkers (`hir/lower.rs`, `lower_to_lir/component.rs`/`blocks.rs` `lower_node`/`lower_expr`/`lower_statement`) repeat the same dispatch skeleton, and the rewrite passes (`lir/boundary_rewrite.rs`, `lir/dedupe.rs`) hand-walk op streams to remap slot ids. A `Fold`-style trait (returns rewritten nodes) fits the rewrite passes; the lowering walkers produce a _different_ IR so they don't map onto a same-IR visitor as cleanly.

### 6.2 What to research / adopt

**Visitor / walk traits (read-only + mut).** The proven pattern (rustc
[`rustc_hir::intravisit::Visitor`](https://rustc-dev-guide.rust-lang.org/hir.html#hir-visitors)
and `rustc_ast::visit`) splits _traversal_ from _action_: a `Visitor` trait whose
`visit_expr`/`visit_node` default to calling free `walk_expr(self, e)` functions
that recurse into children. An analysis overrides only the arms it cares about;
the recursion lives in exactly one place. `syn`'s
[`Visit`/`VisitMut`/`Fold`](https://docs.rs/syn/latest/syn/visit/index.html) are
the same idea, code-generated.

`thir/visit.rs` (`ThirVisitor` + `walk_expr`/`walk_stmt`) adopts the read-only
half of this for THIR. It follows intravisit's `visit_*` → `walk_*` split but
**not** intravisit's `NestedFilter`/`nested_visit_map` machinery: that exists to
let a HIR visitor decide, per nested _item/body_, whether to descend, and HIR
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

- [ ] **`yel-host` CLI name mismatch**: the clap derive still names the command `yel-run` in metadata though the binary is `yel-host` (noted in `yel-host/CLAUDE.md`).
- [ ] **`lir_rust.rs` dead path**: the LIR→Rust generator is commented out in `yel-wasm-codegen/lib.rs` (`// pub use lir_rust::generate_rust;`). Either revive or remove.
- [ ] **`test.wasm` committed at repo root** — a stray build artifact.
- [x] **Nondeterministic GC-type / string-data ordering** — fixed. Two independent `HashMap`/`HashSet`-iteration-order bugs made the emitted module vary run-to-run for globals-heavy fixtures (`global_option_result_defaults`): (1) sibling GC-variant types (`opt_string` vs `res_string_string`) reordered because `gc_types.rs::topo_sort_gc_variant_tys` preserved its `HashSet`-ordered input among mutually-independent nodes — fixed with a deterministic `Ty`-id tie-break; (2) global-default string literals reordered because `lower_globals` built the default-expression arena in `HashMap`-iteration order, and codegen's `collect_strings` interns string literals by walking that arena — fixed by lowering defaults in `DefId` order. Verified: every positive fixture now compiles byte-identically across repeated runs.
- [x] **Random-seed nondeterminism — fixed systemically (Fx + clippy guard).** Beyond the two site-specific bugs above, the compiler emitted different bytes **run-to-run** for ~35/200 fuzz seeds because `std::collections::HashMap`/`HashSet` use `RandomState` (a per-process random seed) and their iteration order leaked into output (WASM type-index assignment, for-loop update/fanout emission). Rather than chase each site, `yel-core` + `yel-wasm-codegen` were swept to `rustc_hash::FxHashMap`/`FxHashSet` (seedless hashing → deterministic iteration; both crates already depended on `rustc-hash`), plus the one non-`Ord` output boundary (`build.rs` ternary-block-type interning) sorts its shapes before assigning indices. Result: **0/200 run-to-run byte diffs**, snapshots byte-identical, suites green. Guarded against regression by a root `clippy.toml` (`disallowed-types` = std `HashMap`/`HashSet`, with a rule-#4 reason) denied via `[lints.clippy]` in both crates (`allow`ed in `yel-host`, the dev host). The lint is rustc's own `potential_query_instability` discipline: never iterate a random-seeded map for output.
- [ ] **Large files generally**: `syntax/parser.rs` ~3.3k, `thir/typeck.rs` ~2.8k, `lower_to_lir/component.rs` ~1.1k, `hir/lower.rs` ~1.4k — all candidates for splitting.
- [ ] **README warns** "Highly WIP — broken builds on main are common." Don't assume `main` is green.

---

## How to use this doc

When you start a refactor, find the relevant section, then cross-reference the
plan in [`plans/`](../plans) and the architecture in
[`ARCHITECTURE.md`](ARCHITECTURE.md). When you _fix_ one of these, tick its
checkbox and trim the entry to its one-line resolution in the same change, so
the inventory stays trustworthy at a glance.
