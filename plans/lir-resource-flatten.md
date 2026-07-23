# Plan: rename `LirComponent` → `LirResource` and flatten `tree_shape` into types + ops

Status: substantially complete — Stages 1–5d, 5e-1, 5e-2, 5e-4 done; ChildrenArray ops folded (5e-3, type-reference plumbing keeps the ForChildrenArray name); `LirResource.tree_shape` and `boundary_params` deleted. Remaining: collapse synthesize+project so the registry is built directly and delete the synthesizer-internal `ComponentTreeShape` scratch (final 5e/5f cleanup).

## Goal

Make the LIR a frontend-agnostic shared substrate by:

1. Renaming `LirComponent` → `LirResource` to drop the YEL-component-specific framing.
2. Eliminating the parallel `tree_shape: TreeShape` representation. The mount-tree synthesizer keeps existing as a YEL-frontend-only pass, but its output becomes:
   - GC struct-type registrations in a resource-level type pool, and
   - Explicit `StructNew` / `StructGet` / `StructSet` ops in blocks, and
   - Typed struct-ref params on block functions,
   instead of a side-channel `TreeShape` value that codegen reads.
3. Making `LirOp::BoundaryField` slot resolution disappear (no chain walk, just a typed ref + struct-get op pair).

After this, a non-YEL frontend (the planned visual flow language) can produce LIR using the same primitives without inheriting any UI / mount-tree concepts. The synthesizer is just one pass YEL chooses to run; the IR shape is unchanged whether you ran it or not.

## Non-goals

- Not changing wasm output semantics. Programs run identically; bit-level output may shift (type-section ordering, block IDs, slot IDs) so all snapshots regenerate.
- Not generalizing the WIT-export skeleton to non-component shapes. That's a separate (and necessary) piece for the flow language but not part of this refactor.
- Not touching THIR or HIR. This refactor sits entirely below the THIR→LIR boundary.
- Not removing the synthesizer. It still runs for YEL programs. It just writes its output into the same data structures non-UI frontends use, instead of into a separate `TreeShape`.

## Pre-read — what's entangled with `tree_shape`

Three things ride on it; all three need explicit replacements:

### 1. GC struct-type emission

`gc_types::emit_component_tree_types` walks `LirComponent.tree_shape.boundaries` and emits one wasm-GC `(struct ...)` type per `TreeBoundary`, plus a `(array ...)` per `ForAnchor`'s children list. The resulting type indices are stored on a per-component `GcTypeLayout` keyed by `TreeBoundaryId`.

After: a resource-level struct-type registry holds these directly. The synthesizer registers types as it encounters them; no separate walk.

### 2. `BoundaryField` slot resolution

`LirSlotKind::BoundaryField { boundary_id, field_idx }` is a *symbolic* pointer. At codegen, it resolves via:

- Find the in-scope boundary-ref local for `boundary_id` (via `current_boundary_locals`, which is populated by `BindBoundaryLocal` ops emitted by the lowerer).
- Emit `local.get <ref>` then `struct.get <struct_ty> <field_idx>` (read) or `struct.set` (write).

After: every BoundaryField use becomes an explicit `LirOp::StructGet { ref_slot, struct_ty, field_idx, result }` or `LirOp::StructSet { ref_slot, struct_ty, field_idx, value }`. The slot kind goes away. The `current_boundary_locals` walker disappears from codegen.

### 3. `boundary_params` on block functions

Block fns receive ancestor boundary refs as typed wasm params. Today the type comes from `tree_shape` via boundary-id lookup. Block-function-type interning (in `build.rs`) walks `block.boundary_params: Vec<TreeBoundaryId>` and looks up each id's struct-type-idx via the GC layout.

After: block params are just typed slots in `block.params` (the existing field, today restricted to i32 / typed-struct-ref / typed-array-ref slots). The boundary-id middleman disappears; block param types come from the slots' own `LirSlotValType`.

## Target architecture

```
yel-core::lir
├── LirResource {
│       name: StringId,
│       def_id: DefId,
│       signals: Vec<LirSignal>,
│       blocks: Vec<LirBlock>,
│       slots: Vec<LirSlotInfo>,
│       exprs: Vec<LirExpr>,
│       strings: StringInterner,
│       struct_types: Vec<LirStructTypeDecl>,   // NEW
│       array_types: Vec<LirArrayTypeDecl>,     // NEW
│       mount_block: BlockId,
│       update_blocks: Vec<…>,                  // unchanged
│       …
│   }
│
├── LirStructTypeDecl {
│       name: String,                            // for the wasm name section
│       fields: Vec<LirFieldDecl>,
│       supertype: Option<LirStructTypeIdx>,    // for sub-typing in flat-gc-struct
│   }
│
├── LirFieldDecl {
│       name: Option<String>,
│       val_ty: LirSlotValType,                  // already exists
│       mutable: bool,
│       packed: Option<PackedKind>,              // i8 / i16
│   }
│
├── LirSlotKind = Temp { local_idx } | Memory { … }   (BoundaryField removed)
│
└── LirOp ::= … existing variants … plus:
      ├── StructNew     { struct_ty, args, result }
      ├── StructNewDefault { struct_ty, result }
      ├── StructGet     { base, struct_ty, field_idx, result }
      ├── StructSet     { base, struct_ty, field_idx, value }
      └── (existing ChildrenArray* generalize to ArrayGet/Set/Copy/NewDefault if we
           fold them in this pass — see open question §1)
```

Block-fn signatures: `Vec<LirSlotId>` (already), each slot's `val_ty` is one of i32 / i64 / f32 / f64 / typed-struct-ref / typed-array-ref. No more `boundary_params: Vec<TreeBoundaryId>`.

The synthesizer's job becomes:

1. Walk the THIR/UI tree (same logic as today).
2. For each TreeBoundary/ForAnchor that would be created, register a struct-type or array-type in the resource's pool. Get back a struct-type-idx.
3. At each lowering site that today emits `BoundaryField` slot allocations + `LoadHandle` / `StoreHandle`, instead emit explicit `StructGet` / `StructSet` ops referencing the registered struct type and the in-scope ref slot.
4. For block fn params, set `block.params` to typed-struct-ref slots directly.

The synthesizer is still a 200-LOC YEL-only pass. Its inputs and outputs change, but its *role* doesn't.

## Migration plan

Each stage leaves a runnable, fully-tested tree. Stages are ordered so that the codegen path keeps working at every checkpoint and snapshots regenerate per stage. No "one big rewrite, hope it builds" — anything broken at the end of stage N is local to stage N.

### Stage 0 — baseline

- Run `cargo test -p yel-wasm-codegen --no-fail-fast`. Confirm 100/100.
- Run the fuzz harness to capture today's pass rate (for regression comparison).
- Snapshot the workspace warning count (we'll re-enforce at the end).

End of stage: nothing changed; baseline numbers recorded.

### Stage 1 — rename `LirComponent` → `LirResource`

Pure mechanical rename. No semantic change.

- Rename `LirComponent` struct in `yel-core::lir`.
- Rename every reference across both crates. Use `cargo check -p yel-wasm-codegen` to drive.
- Update field references that name "component" in their identifier (e.g. `comp_idx`, `current_comp`) only when they're conceptually about the resource — leave variable names alone otherwise; we're just renaming the *type*.
- Snapshot regen: should be zero diff. The DOT generator's labels include "component App" — only that label should change. (Unless we choose to keep "component" as a YEL-frontend concept that *labels* a LirResource — open question §2.)

End of stage: `cargo test --workspace` green, name section diffs only.

Estimated effort: 2 hours.

### Stage 2 — add the resource-level type registry, populate alongside `tree_shape`

Add the new fields to `LirResource` but leave `tree_shape` in place. The synthesizer writes to *both*. Codegen still reads `tree_shape`. Pure addition; no removal.

- Add `struct_types: Vec<LirStructTypeDecl>` and `array_types: Vec<LirArrayTypeDecl>` to `LirResource`.
- Modify `tree_shape::synthesize` to register each TreeBoundary's struct as a `LirStructTypeDecl` and each ForAnchor's array as a `LirArrayTypeDecl`. Cache the registry index on the boundary alongside the existing `TreeBoundary` data.
- No codegen changes yet. Just verify the registry contents match what codegen would emit from `tree_shape` at this point — assert in a unit test that for every TreeBoundary, the registered struct type's field shape matches what `emit_component_tree_types` would produce for it.

End of stage: registry populated, tests green, codegen unchanged.

Estimated effort: 4 hours.

### Stage 3 — replace `BoundaryField` slot uses with explicit struct ops at lowering time

This is the meatiest stage. `BoundaryField` slots disappear from emitted LIR; the lowerer emits explicit `StructGet` / `StructSet` ops with a typed-ref slot and a registered struct-type-idx.

- Add `LirOp::StructGet`, `StructSet`, `StructNew`, `StructNewDefault` if not already present. (Some may already exist for record/tuple/option lowering; reuse.)
- For every `BoundaryField` slot allocation site in `block_lower.rs`, replace with:
  - allocate a temp slot for the read result (typed appropriately),
  - emit a `StructGet` op against the in-scope boundary ref slot + struct-type-idx + field-idx.
- For every `LoadHandle` / `StoreHandle` against a `BoundaryField` slot, the lowerer no longer needs to do the chain walk. The replacement is the explicit struct op above.
- `LirSlotKind::BoundaryField` stays in the type definition for now, but no new uses are created.
- Codegen's `current_boundary_locals` chain-walker still exists but is dead code (no `BoundaryField` slots reach it).

End of stage: tests green, snapshots churn (block ID / slot ID shifts), no `BoundaryField` slots in output.

Estimated effort: 1 day.

### Stage 4 — replace `boundary_params` with typed struct-ref params in `block.params`

- For every block fn signature site, replace the `boundary_params: Vec<TreeBoundaryId>` mechanism with typed struct-ref slots in `block.params`.
- Update all `CallBlock` / `CallBlock2` / `CallBlock3` call sites: caller pushes typed struct refs as additional positional args; callee receives them as typed slots in its `params`.
- Block-fn-type interning in `build.rs` reads slot types directly from `block.params` — no boundary-id lookup.
- Codegen's `emit_boundary_ref` helper goes away.

End of stage: tests green, name section reflects typed param names only (no boundary-id-derived labels).

Estimated effort: half day.

### Stage 5 — delete the parallel TreeShape representation + fold ChildrenArray ops

**Status: in progress (sub-stage 5b partial).** Stage 5 split into 5a–5f:

- **5a — fold `ChildrenArray*` → `Array*`:** **partial — dead `Array*` variants deleted.** Investigation revealed the `LirOp::ArrayNewDefault / ArrayGet / ArraySet / ArrayCopy` variants (taking raw `ty_idx: u32`) had codegen handlers and dedupe arms but **no LIR-lowering site ever constructed them** — they were dead code variants. Deleted them from the IR (`block.rs`), their codegen arms (`op_emit.rs`), dedupe arms (`dedupe.rs`), and pretty-printer arms (`yel-flow-core/pretty.rs`). The actual array-mutation ops emitted by the lowerer remain `ChildrenArray*` keyed by `TreeBoundaryId`; array reads of typed-list signals continue through `ArrayGetItem` / `ArrayGetItemFat` / `ArrayGetItemFatToMem`. The proper "fold" — renaming `ChildrenArray*` and routing them through the resource's `array_types` registry — moves to **Stage 5e-3** (deferred; needs lowerer access to wasm-type-idx via the registry, which depends on projection-before-emit ordering).
- **5b — sweep remaining BoundaryField loads/stores so the codegen chain walk is dead:** partial. Rewriter now covers `LoadHandle`, `StoreHandle`, `LoadI32`, `StoreI32Slot`, `StoreI32` against BoundaryField slots (StoreI32 via new `BoundaryStructSetConst` op). Per-resource counts on real fixtures:
  - `counter_test.yel`: 29 rewritten, 8 remain
  - `list_append.yel`: 9 rewritten, 4 remain
  - `fuzz_nested_option.yel`: 0/0
  Remaining sites are LoadHandle/StoreHandle inside dispatch fns / handler bodies / update fns where the boundary reference isn't statically bound at the LIR layer (no `BindBoundaryLocal` / `Alloc*Boundary` / `boundary_param_slots` for that boundary id at the use site). Closing these requires structuring the lowerer to add explicit bindings for those contexts — separate piece of work. Until then, codegen's chain walk via `emit_boundary_ref` stays alive as the fallback.
- **5c — replace `boundary_params` consumers with `boundary_param_slots`:** **done.** Helper `LirBlock::boundary_param_ids_from_slots(slots)` derives the boundary id from each slot's val_ty (`RefNullForBoundary(b_id)`). Migrated consumers: `build.rs::dynamic block fn type` interner, all three `CallBlock`/`CallBlock2`/`CallBlock3` callers in `op_emit.rs`, `signal_emit.rs` update-fn fan-outs (×2 sites), `block_fn.rs` param-binding (length / iteration), `name_section.rs::build_block_func_name` (added a `slots` param) + line 737 length check, `dot.rs::block_label` and `boundary_kind_for_block`. Remaining `.boundary_params` reads after 5c: 5 lowerer-side writes (the field's producers) + Stage 4's `allocate_boundary_param_slots` post-pass + 3 dedupe.rs sites — all deleted in 5e.
- **5d — replace `tree_shape` consumers in codegen with `struct_types` / `array_types` reads:** **done.** Registry extended with `kind: TreeBoundaryKind`, `parent: Option<LirStructParentLink>`, and `LirStructFieldDecl::role: LirFieldRole` (DomHandle/LoopVar/SubBoundary/ChildrenArray/ActiveTag) so consumers can disambiguate same-shape scalar fields. Migrated consumers:
  - `name_section.rs` — type-name registration, field-name registration, ForAnchor companion-array naming. All three sites read `struct_types[i].name` / `.fields[j].name` / `.kind` directly.
  - `signal_emit.rs::emit_boundary_ref` — root check via `kind == Root`; parent chain walk via `struct.parent`.
  - `signal_emit.rs::emit_boundary_chain_from_self_inline` (global fan-out) — chain walk via registry.
  - `lifecycle.rs` self-walk unmount — `LirFieldRole::DomHandle` discriminator instead of `TreeFieldDecl::DomHandle` matching; root-stop via `kind == Root`; parent walk via registry.
  - `op_emit.rs::AllocSubBoundary` — parent link from registry.
  - `dot.rs::boundary_kind_for_block` — kind read from registry.
  - `gc_types::emit_component_tree_types` — full migration. Signature changed from `&ComponentTreeShape` to `&LirResource`. Walks `component.struct_types` for struct emission, `component.array_types` for array emission, derives `tree_struct_type_idx` / `tree_for_arr_type_idx` / `tree_root_type_idx` from the registry. New helper `build_struct_from_decls` translates `LirStructFieldDecl` (with `role` discriminator) into wasm `FieldType`.
  - **Verification**: zero `.tree_shape` reads remain in `crates/yel-wasm-codegen/`. All 100/100 tests pass. Zero fixture/snapshot drift — wasm output bytes are identical (same data, different field path).
- **5e — delete `TreeShape`, `TreeBoundary`, `TreeBoundaryKind`, `TreeFieldDecl`, `BoundaryField` slot kind, `boundary_params`, `ChildrenArray*` ops, `tree_shape.rs`:** **deferred — multi-session work.**

  After 5b/5c/5d, codegen no longer reads `tree_shape`. The remaining work to actually delete things is sequenced and non-trivial:

  **5e-1 — drive Stage 5b's "remaining BoundaryField loads/stores" count to zero. DONE (enforced).** Implemented via option (a): the `BoundaryRefFromSelf { boundary_id, result }` op + a per-block rewriter pre-pass that, for each unbound boundary, either prepends a `BoundaryRefFromSelf` (when reachable from `$self.tree` — registry `kind == Root` / static `parent` chain) or synthesizes a `BoundaryStructGet` chain from an already-bound ancestor (descendants of a `boundary_param_slot`). Remaining count is **0 across every positive fixture** (counter 37 rewritten / 0 remain, nested_for 29/0, nested_parent 20/0, …). The invariant is now enforced, not just observed: `lower_component` `debug_assert!`s `count_remaining_boundary_field_loadstore == 0` after the pass, so the whole lowering test suite is its regression guard, and codegen `unreachable!`s on any `BoundaryField` slot reaching `LoadHandle`/`StoreHandle`/`LoadI32`/`StoreI32`/`StoreI32Slot`. **`emit_boundary_ref`'s chain walk is no longer a fallback** — it survives only as the codegen of the explicit `BoundaryRefFromSelf` op and the `CallBlock` boundary-param calling convention. The remaining blocker for deleting `LirSlotKind::BoundaryField` is no longer coverage; it's that the lowerer still *produces* `BoundaryField` slots as the rewriter's input (see 5e-4) — eliminating the slot kind means moving `(boundary_id, field_idx)` onto explicit pre-rewrite ops, entangled with the `tree_shape`→registry migration.

  **5e-2 — delete `boundary_params` field. DONE.** Producers allocate the typed mirror slots directly via `LirBlock::set_boundary_params` (Block-variant, per-block local_idx) at block-finish time, so the `allocate_boundary_param_slots` post-pass and its ordering dependency with dedupe are gone. Dedupe hashes/compares boundary ids derived from the slots (`LirBlock::boundary_param_ids`), which are present before dedupe runs by construction. The field and its five producers are deleted; `boundary_rewrite` seeds its binding map from the slots.

  **5e-3 — fold `LirOp::ChildrenArrayGet/Set/Copy/NewDefault` → generic `Array*`.** The Array* ops use `ty_idx: u32` (a wasm type-section index). ChildrenArray* uses `anchor_boundary: TreeBoundaryId`. To fold, the lowerer needs to compute the wasm `ty_idx` at lowering time — but that index is determined by codegen's `gc_layouts[comp].tree_for_arr_type_idx[anchor]`. Either (a) move the wasm type idx computation into the lowerer (leaks codegen detail upward), or (b) introduce a `LirOp::ArrayByBoundary*` family that's structurally Array* with boundary-id resolution at codegen — same as today's ChildrenArray* with a renamed type. Path (b) is just rename without behavior change. Path (a) is the structurally clean answer but requires moving wasm-type-section sequencing into the lowerer. Probably defer until a separate "lower wasm-type-section computation into LIR" track.

  **5e-4 — delete `LirSlotKind::BoundaryField`. DONE.** The symbolic slot kind is gone. Persistent GC-struct fields are now accessed through three new pre-rewrite ops — `LirOp::StructFieldGet/Set/SetConst { struct_ty: TreeBoundaryId, field_idx, .. }` — emitted directly by the lowerer (every `alloc_boundary_field_slot_named` + `LoadHandle`/`StoreHandle`/`LoadI32`/`StoreI32`/`StoreI32Slot` site migrated, across the attr/dyntext/if-anchor/for-anchor clusters, incl. the threaded `outer_item_field_slots` map and the if/for helper signatures). `boundary_rewrite` resolves them to the **generic** `LirOp::StructGet/StructSet/StructSetConst { rec, field_idx, .. }` — no `boundary_id` on the resolved op; codegen recovers the wasm struct-type index from `rec`'s `val_ty` (`RefNullForBoundary` → `tree_struct_type_idx`, `RefNullForComponent` → `component_struct_type_idx`, `RefNull` → the index directly). The dead `StructGet/StructSet { ty_idx }` ops were deleted and their names reused. The internal-unmount detach list now derives from the `struct_types` registry (role == DomHandle, Root-reachable) instead of scanning slots. Codegen `unreachable!`s on any surviving symbolic op; the LIR-layer `debug_assert` is the regression guard. 284/284 tests green, snapshots byte-identical, wasm validates. **Still pending (separate track):** deleting `tree_shape.rs` / the `tree_shape` field — the lowerer's ~20 synthesis-time `tree_shape` reads must migrate to the registry first; the `StructField*` ops still source their `(struct_ty, field_idx)` from `tree_shape.node_field`.

  **Recommended ordering:** 5e-1 (~1 day, unblocks slot-kind deletion) → 5e-2 (~1 day, unblocks field deletion) → 5e-3 (~half day, contained rename) → 5e-4 (~1-2 days, the big mechanical sweep). Total: 3.5–4.5 focused days. Not in scope for one auto-mode session.
- **5f — bulk snapshot regen + doc sweep:** blocked on 5e. (5b–5d already produced no fixture drift, so the bulk regen described in the plan only fires once 5e-2 / 5e-4 actually delete things from the IR shape.)

**What landed in Stage 5b:**

- New ops: `LirOp::BoundaryStructSetConst { boundary_id, field_idx, rec, value: i32 }` for literal-i32 stores.
- Rewriter coverage extended to `LoadI32`, `StoreI32Slot`, `StoreI32`.
- Verification helper `count_remaining_boundary_field_loadstore(&LirResource) -> usize`.
- Opt-in tracing: set `YEL_DEBUG_BOUNDARY_FIELD=1` to print per-resource rewrite counts during compile.
- All 100 wasm-codegen tests pass; end-to-end `list_append` REPL smoke verified.

(Original Stage 5 description below — kept for the eventual cleanup pass.)

- Delete `TreeShape`, `TreeBoundary`, `TreeBoundaryKind`, `TreeFieldDecl`, `LirSlotKind::BoundaryField`, `boundary_params` field on `LirBlock`.
- Synthesizer's outputs are now exclusively the `LirResource` registry + emitted ops. Its public API drops the `TreeShape` return value.
- Fold `LirOp::ChildrenArrayGet/Set/Copy/NewDefault` into the generic `LirOp::ArrayGet/Set/Copy/NewDefault` family. Every site rewrites to operate on a typed array-ref slot directly. The for-anchor's array type lives in the resource's `array_types` registry by this stage, so the implicit "resolve through ForAnchor's children field" behavior is now an explicit slot reference. Delete the `ChildrenArray*` ops.
- Bulk snapshot regen via `UPDATE_SNAPSHOTS=1`. Sample 5–6 fixtures manually (counter_test, list_for_loop, list_filter, list_append, fuzz_nested_option, dot_every_signal_kind) and eyeball the diffs. If those look right, accept the rest.
- Doc-comment sweep: replace mentions of "boundary" / "tree shape" with "struct type" / "resource type pool" where appropriate, leaving doc-archaeology comments where the original intent was UI-specific.

End of stage: `git grep TreeShape`, `git grep BoundaryField`, `git grep ChildrenArray` all return 0 hits.

Estimated effort: 1 day (was half day; +ChildrenArray fold).

### Stage 6 — verification + cleanup

- Run the full fuzz suite. Compare pass rate against Stage 0 baseline. Any regression is a bug introduced by this refactor; fix before merging.
- Workspace warning count back to baseline.
- Update `crates/yel-core/CLAUDE.md` and `crates/yel-wasm-codegen/CLAUDE.md` to reflect the new IR shape.
- Update `LANGUAGE.md` if it references the old field names.

End of stage: ready to merge.

Estimated effort: 2-4 hours.

**Total estimated effort: 3–3.5 days of focused work.** (+0.5 day vs. initial estimate to absorb the ChildrenArray fold into Stage 5.)

## Risk areas

1. **Structural dedupe (Phase 3a)** leans on boundary identity for some equivalence checks. After Stage 4 the equivalence relation needs reformulation in terms of typed struct-ref slot equivalence. Need to audit `crates/yel-core/src/lir/dedupe.rs` and verify that the post-refactor IR can still be deduped with the same precision. Risk: dedupe regresses, output bloats. Mitigation: dedupe unit test at Stage 3 boundary, before deletion.

2. **`gc-dump` walker in yel-host** uses `tree_shape` to give pretty type names. After Stage 5, it switches to the resource's `struct_types` registry. The output should look the same (same type names, same hierarchy) but the data path is different. Mitigation: regenerate yel-host gc-dump golden output, manually inspect for the canonical fixture (counter_test).

3. **Type-section bit stability**. The type section's traversal order today is determined by `tree_shape`'s boundary order. After the refactor it's the registration order in `struct_types`. These probably differ. The regenerated wasm is *semantically equivalent* but *byte-different*. If anything outside the workspace caches binary outputs (e.g. cached compiler artifacts in yel-viewer), it'll need a clean.

4. **Forward-reference dependencies between struct types**. Today the synthesizer's tree-walk order naturally puts parent boundaries before children, so wasm type indices come out topologically sorted. The flat registry approach needs to preserve that, or we need to use rec groups for the whole pool. Mitigation: emit all resource struct types in one big rec group. Cheap; eliminates ordering anxiety.

5. **Snapshot churn cascades**. Each stage churns DOT, WAT, .wit snapshots. Easy to lose track of what's an *expected* diff vs. an *unintentional* one. Mitigation: at each stage end, verify a sampled diff manually before bulk-accepting `UPDATE_SNAPSHOTS=1`.

## Resolved decisions

1. **Fold `ChildrenArrayGet/Set/Copy/NewDefault` into regular `ArrayGet/Set/Copy/NewDefault`.** In scope for this refactor; done as part of Stage 5 once the for-anchor's array type lives in the resource's `array_types` registry. The `ChildrenArray*` op family is deleted; every site rewrites to the generic `Array*` op against a typed array-ref slot.
2. **No `ResourceKind` discriminator.** `LirResource` is the same shape regardless of frontend. The YEL frontend's WIT emitter and DOT generator can label things "component App" by reading the resource's name and frontend-supplied metadata; no enum tag required.
3. **No byte-stability constraint.** Type-section reordering is fine; downstream consumers regenerate. Workspace snapshots regenerate as part of each stage.
4. **Snapshot strategy: bulk regen at Stage 5.** Sample 5–6 fixtures manually before mass-accepting `UPDATE_SNAPSHOTS=1`; the rest fall under "expected churn from refactor" once the sample looks right.

## Verification

- `cargo test --workspace --no-fail-fast` green at every stage end.
- Fuzz pass rate at Stage 6 ≥ Stage 0 baseline (regressions = bugs to fix before merge).
- Manual smoke: every fixture in `crates/yel-core/examples/` compiles via `--release` and `wasm-tools validate` clean.
- yel-host: `run` / `dump` / `gc-dump` / `repl` all work on `counter_test.yel` and `list_append.yel`.
- The DOT generator's output for a sample fixture (e.g. `list_for_loop.dot`) shows the same fns / signals / call edges as Stage 0.

## What this enables

After Stage 6, the visual flow language can produce a `LirResource` directly from its JSON / graph input:

```rust
fn graph_to_lir_resource(graph: &FlowGraph) -> LirResource {
    let mut r = LirResource::new(graph.name);
    for node in &graph.nodes {
        // Allocate slots for input/output pins.
        // Emit LirOps for the node's behavior (Call, EvalExpr, If, Loop, …).
        // Register any struct types it needs (e.g. for stateful nodes).
    }
    r
}
```

No reference to `TreeShape`, `TreeBoundary`, `BoundaryField`, or `boundary_params` is needed. The resource is just a flat structure of typed slots, blocks of ops, and a type pool.

The same `LirResource` then flows through the existing wasm-codegen pipeline (with a new WIT-export skeleton for the flow-language exports — separate work).

## Out of scope (followup tracks)

- WIT-export skeleton generalization — flow-language programs export `run(input) -> output` rather than `[constructor]` / `[method]mount`. Needs a parallel emitter in `wit_ast.rs`.
- Reactivity for the flow language — separate decision, see prior conversation. Probably not signal-based; needs its own scheduler if signals don't fit.
