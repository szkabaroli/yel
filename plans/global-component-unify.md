# Unifying globals & components onto one pipeline spine

**Status:** phases 1–4 **DONE**. One typecheck + codegen path; no
surface-language changes; WIT/DOT byte-identical on existing fixtures and the
execution suite green throughout. `LirModule` now carries first-class
`globals: Vec<LirGlobal>` (replacing the `global_defaults`/`global_default_exprs`
side-maps) and `imports: Vec<LirImport>` (the single host-import registry);
codegen and `wit_ast` derive from these instead of re-walking `ctx.defs`. Phases
5–6 (the `@annotation` free-fn/interface toggle and DOM-as-global) remain out of
scope but are not precluded.

## Why

Globals and components are both top-level compilation units, but today they run
two parallel spines (HIR → THIR → LIR → codegen). The deeper truth — and the
North Star (`docs/ARCHITECTURE.md` §0) — is that **a global is a host-boundary
declaration**: its functions are host imports; in-tree reactive state
(CounterStore-style defaults) is a *secondary* facet. Components are the
*exported* boundary the host drives; globals are an *imported* boundary plus
optional state. They should share one spine. Later, `yel:ui/dom` itself becomes
"a built-in global" (phase 6, deferred).

## Current divergence (as mapped)

- **Import resolution is fragmented across 3 mechanisms:**
  1. DOM imports — 18 hardcoded `DefId`s (`dom_imports.rs`), emitted from
     `yel:ui/dom@0.1.0` (`build.rs:1004`), resolved by the 18-entry linear scan
     `wasm_import_index_for_dom_def` (`mod.rs:775`).
  2. Component callbacks — `ImportLayout.callback_indices: HashMap<DefId,u32>`
     (`mod.rs:837`), indices start at `NUM_DOM_IMPORTS` (18).
  3. Global callbacks — `GlobalCall` flows THIR→LIR, but codegen **drops the
     args** as a `// TODO` (`expr.rs:406`). Not wired at all.
- **HIR asymmetry:** there is no `HirGlobal`; globals are registered in
  `ctx.defs` and never lowered to HIR. THIR pulls them from `ctx.defs.globals()`
  in a batch (`typeck.rs:179`), separate from the per-component `type_check`.
- **THIR/LIR:** `ThirComponent` vs `ThirGlobal`; `lower_component -> LirResource`
  vs `lower_globals -> (HashMap<DefId,LirExpr>, Vec<LirExpr>)`; `LirModule` holds
  `components: Vec<LirResource>` plus `global_defaults` side-maps.
- **Codegen:** separate memory-layout pass (globals reserved before components),
  separate `$globals_<name>` GC types, a `globals_init` start function, and
  separate global-section emission.

## Decisions (locked)

- **Item model:** `HirItem`/`ThirItem` as an **enum** `{ Component, Global }`,
  explicit dispatch (matches the already-split `signalck`). No body-less
  components carrying dead UI fields.
- **Keystone:** one host-import registry keyed by `DefId` (`HashMap<DefId,u32>`)
  that resolves DOM ops, component callbacks, and global callbacks uniformly.
  Order preserved (DOM 0–17, then callbacks, then `[resource-new]`) so existing
  snapshots stay byte-identical.

## Phases

### Phase 1 — Unified import registry  ✅ DONE (snapshot-identical)
Seeded the 18 DOM `DefId`s into `ImportLayout` so it owns the *whole* import
index space (`callback_indices` → `import_indices`; `find_callback_index` →
`import_index`). Routed `LirOp::CallFunction` (`op_emit.rs`) and the callback
`Call` path (`expr.rs`) through `import_index`; deleted
`wasm_import_index_for_dom_def`. Indices unchanged → WASM/WIT/DOT byte-identical
(yel-wasm-codegen + yelc suites green, no snapshot rewrites).

### Phase 2 — Wire global callbacks  ✅ DONE (new feature, snapshot-identical)
Global callback `DefId`s now join `ImportLayout` (no `self` handle — globals are
freestanding, not resources); their imports are emitted from the global's own
WIT interface (`{ns}:{pkg}/{global-kebab}@{ver}`) and func types interned
without a self param. Name section labels them `[global-callback]{g}.{fn}`.

**Collapsed `LirExprKind::GlobalCall` into the generic `Call { func, args }`**
(per review): now that the registry resolves any callee by `DefId`, a global
call and a component callback differ only in whether codegen pushes a receiver
handle — derived from `Definitions::is_global_callback(fn_id)`, no separate op.
`ThirExprKind::GlobalCall` (meaningful surface sugar) still exists but lowers to
`LirExprKind::Call`. The `globals.yel` fixture now genuinely imports + calls
`theme/toggle-dark-mode`; WASM validates; `.wit`/`.dot` unchanged.

Remaining (not yet done): the `in`/`out` property setter/notifier imports are
still WIT-only (unimplemented in core, as before) — that state-sync belongs to
Phase 4, not here.

### Phase 3 — Unified item spine  ✅ DONE (front-end, snapshot-identical)
Added `HirGlobal` + `HirItem`/`ThirItem` enums (`Component | Global`, with
`as_component`/`into_component` accessors). `lower_to_hir` now returns
`Vec<HirItem>` (components then globals); globals flow through HIR instead of a
side table. **Collapsed the four type-check entries into one** `type_check(&HirItem)
-> ThirItem` (per review) — `type_check_global`/`type_check_globals`/
`type_check_globals_structured` deleted; the global arm is a private helper.
`pipeline.rs::lower_all` and every test harness (`integration`, `execution`,
`dump_wasm`, `runtime`) now run a single item loop, accumulating global defaults
from `ThirItem::Global`; LSP/smith/example/CLI callers updated. Global LIR
lowering still goes through `lower_globals_to_lir` (Phase 4).

Note: `type_check_with_map` (a tooling/type-map variant) remains and is currently
dead — separate axis from the globals/components split; candidate for removal.

### Phase 4 — Frontend-agnostic `LirModule` + global state as core wasm globals
**Reframed (per review): a global is NOT a resource.** A resource is the
component concept (handle + GC struct + lifecycle, exported to the host).
Globals are singletons — not instantiable — so they need no struct: their
*functions* are host imports (freeform or interface-grouped) and their *state*
lives in **core wasm mutable globals**.

Target `LirModule` (frontend-agnostic; mirrors the wit-parser world model):

```rust
// As landed (lir/module.rs):
LirModule {
    resources:   Vec<LirResource>,           // exported instantiable units (components).
    globals:     Vec<LirGlobal>,             // first-class global items; their default
                                             //   exprs' children index into global_exprs.
    global_exprs: Vec<LirExpr>,              // shared module-start default-expr arena.
    imports:     Vec<LirImport>,             // the host-import registry (single source of truth).
    interfaces:  IndexVec<InterfaceId, LirInterface>,
    package:     Option<PackageId>,
}
struct LirGlobal { def_id, name, is_export, package, properties: Vec<LirGlobalProperty>, callbacks }
struct LirGlobalProperty { def_id, direction: GlobalPropDirection, default: Option<LirExpr> }
struct LirImport { def_id, name: Name, interface: InterfaceId, params, result, receiver: LirReceiver }
enum   LirReceiver { None, Borrow(DefId) }   // Borrow => leading borrow<resource> / core i32 self.
```

Deviations from the original sketch, and why:
- **`globals` keeps a module-shared `global_exprs` arena** rather than a per-global
  one. All global defaults lower through one module scope and seed together at
  module start; a shared arena keeps the module-scope `.filter()` path and the
  `globals_init` carrier working with no `LirExprId` rebasing. `LirGlobal` is
  still a first-class per-item struct (identity, properties, callbacks, package).
- **`LirImport.interface` is a non-optional `InterfaceId`** (not
  `Option<InterfaceId>`): every host import today belongs to an interface (the
  freeform `WorldKey::Name` path is still unused). It can widen to `Option` if a
  freeform import ever appears.

Alignment notes (verified against `wit-parser` 0.248.0):
- "Freeform" = `WorldItem::Function` keyed by `WorldKey::Name` (function directly
  in the world). Grouped = `WorldKey::Interface(id) → WorldItem::Interface`.
  `interface: Option<InterfaceId>` encodes exactly this. (Today `wit_ast` only
  ever emits interfaces; the freeform path is unused.)
- `FunctionKind::Freestanding` (no resource `self`) is orthogonal to placement.
- Resources are never freeform — surfaced through an export interface owning
  their `Constructor`/`Method` functions; a `LirResource` references a
  (non-optional) export `InterfaceId`.
- `globals` are module state, never world items; only a global's *functions*
  become `LirImport`s. DOM-as-a-global then needs no special case.

Sub-steps (each green; WASM bytes change once state moves to core globals, but
`.wit`/`.dot` stay stable — globals **execution** tests are the guard):
1. ✅ `LirModule.components` → `resources` (mechanical, byte-identical). Done;
   method `exported_components` → `exported_resources`.
2. ✅ Introduce `InterfaceId` (ids.rs) + `LirInterface`/`InterfaceDirection` +
   `LirModule.interfaces: IndexVec<InterfaceId, LirInterface>` (scaffold, empty).
   Also removed vestigial serde: `LirModule` is never (de)serialized — only
   `LirResource` is (for `ir --json`), so `LirModule`/`IndexVec` carry no serde.
3. ✅ **DONE (behaviour-preserving; WIT/DOT byte-identical, execution green).**
   Introduced `LirImport` (`lir/module.rs`) as the module's **single host-import
   registry** (`LirModule.imports`) — every function the core module imports
   (component callbacks, global callbacks, DOM), in import-index order, each
   referencing its `InterfaceId`. One frontend producer,
   `CompilerContext::build_import_contract(component_def_ids) -> (interfaces,
   imports)`, now builds **both** the import registry and the full set of import
   `LirInterface`s (component-callbacks, local globals with setter/notifier
   funcs, foreign DOM) — replacing the foreign-only `build_import_interfaces`.
   `LirIfaceFn` gained a `receiver: LirReceiver` (`None` | `Borrow(component)`),
   so a `borrow<resource>` self-param is contract data, not a per-kind code path.
   - **Codegen routed off `imports`:** `ImportLayout::new(imports, all_components)`
     derives the index space + `[resource-new]` slots from the registry (its
     `unique_callbacks`/`global_callbacks`/`components` fields are gone); the
     import section, per-import type interning (`import_types` keyed by `DefId`),
     the callback return-area scratch sizing, and the name section all iterate
     `module.imports` + `module.interfaces` instead of re-walking `ctx.defs`.
   - **`wit_ast` routed off the contract:** one `render_import_contract` renders
     every import interface (receiver → `borrow<self>`; foreign package → inline
     types, local → shared-types `use` aliases); the per-kind
     `create_per_component_callbacks_interfaces` / `create_globals_interfaces`
     are deleted, and `create_world` takes one unified `import_interface_ids`
     list. The WIT world is unchanged (byte-identical fixtures).
   - Kebab-casing is now shared: `yel_core::naming::to_kebab_case` is the single
     source of truth the frontend contract and the backend renderer both use, so
     a frontend-computed interface name can't drift from its backend re-derivation.
4. Global state → core wasm globals, in two halves:
   - **4a ✅ DONE (green).** Consolidated the global-*write* lowering onto a new
     `LirOp::GlobalFieldSet { block, field, value }` (replacing the explicit
     `GlobalGet{GlobalBlockSelf} + StructSetSym{GlobalsStruct}` pair).
     Codegen still lowers it via the GC struct → behavior-preserving. Reads were
     already codegen-resolved `SignalRead`s, so no read-side change. `GlobalFieldGet`
     proved unnecessary (reads are expr-position) and was dropped.
   - **4b ✅ DONE (green).** Storage flipped to per-field core wasm globals.
     `GlobalsBlockLayout` gained `field_valtypes` + `field_core_globals`; the
     global section declares one `(mut <valtype>)` per storage field. Reads
     (`emit_global_struct_read`) → `global.get`; writes (`GlobalFieldSet`,
     `emit_global_struct_store_from_expr/_from_slot`, dispatch) → `global.set`
     (multi-slot writes drop the scratch spill — `global.set` in reverse stack
     order). `globals_init` seeds via the flipped store helpers. Deleted the dead
     `resolve_global_struct_target`. The `$globals_<name>` struct + self-global
     are still emitted (dead) — step 5 removes them. The runtime start-function
     test was updated to assert `global.set` seeding (new correct behavior).
     WASM bytes change for global fixtures; `.wit`/`.dot` unchanged; 51 execution
     tests are the runtime guard.
### Step 5 — Delete the dead `$globals_` struct machinery  ✅ DONE (green)
Removed: the `$globals_<name>` GC struct-type emission (`emit_globals_struct_type`
→ `compute_globals_block_layout`, no type emitted), the per-block self-global,
`globals_init`'s struct-alloc loop, `GlobalsBlockLayout.{struct_type_idx,
self_global_idx}`, `resolve_global_struct_target`, and the three now-dead LIR
variants `LirTypeRef::GlobalsStruct`, `LirGlobalRef::GlobalBlockSelf`,
`LirSlotValType::RefNullForGlobalBlock` (+ their codegen resolvers in `op_emit`,
`function_type`, `gc_types`). `name_section` now names the core globals instead
of the struct/self-global. `GlobalsBlockLayout` is retained as a pure layout
record (`property_field_paths`/`field_valtypes`/`field_core_globals`).

Global state is now **purely core wasm globals** — no GC struct, no self-global,
no allocation. All suites green (yel-core 161 + yel-wasm-codegen incl. 51
execution + yelc); `globals.yel` validates with `$theme-global-0 (mut i32)` and
no `$globals_` struct type; no `.wit`/`.dot` churn; lsp/smith build clean.

Follow-up (minor, not done): `global_in_struct` is now a stale name (no struct) —
it gates migrated-core-global vs linear-memory props; rename to e.g.
`global_in_core_globals`. And the `global_property_addrs` linear-memory path
still backs pointer-typed (record/tuple) global props.

## Phase 6 — DOM as a built-in global (frontend-agnostic backend)

Goal: the backend has **no** DOM knowledge; DOM is a built-in global whose
functions are host imports (riding the Phase 1/2 import machinery), so the
hardcoded import section + type table in `build.rs` can be deleted.

### 6.1 + 6.2 — DOM ABI types + real signatures in the frontend  ✅ DONE (green)
- Registered the **`AttributeValue`** variant (14 cases, WIT order; `color` case
  reuses the existing builtin `Color` variant) via the known-types mechanism
  (`stdlib_lookup::register_builtin_variants`, slot `known.variants.attribute_value`).
  `node` is `u32` (matches the WIT — not a resource).
- Replaced `dom_imports.rs`'s placeholder `() -> ()` signatures with the **real**
  `yel:ui/dom` signatures (`alloc_dom_fn`: typed `ParameterDef`s + return + `Func`
  type). Verified each flattens (canonical ABI) to the hardcoded wasm type — incl.
  `set-attribute` → Type 31 `(i32,i32,i32,i32,i64,i32,i32,i32,i32)→()`.
- **Architectural correction (per review):** the validation test belongs in the
  frontend, not codegen — the back-end must not reference DOM. The codegen test
  was removed; `dom_imports.rs` now has frontend structural tests (signatures +
  the 14-case variant). The back-end has **zero** DOM-aware tests.
- Inert for current codegen (backend still hardcodes the types); only effect is
  `DefId` renumbering → re-baselined 63 `.dot` fixtures (raw-DefId node ids; pure
  number shifts, structure identical) + the yelc dot snapshot. `.wit` unchanged.

### 6.3 (in progress) — route the backend off the frontend signatures
- ✅ **`wit_ast` DOM function table derived from the frontend.** Replaced the
  hardcoded `dom_funcs` literal (~131 lines, the third copy of the signatures)
  with a derivation from `ctx.dom_imports()` — reads each function's name, params,
  and result from its frontend `FunctionDef`, mapping the param yel-types to WIT
  (`string`/`u32`/`attribute-value`). **Byte-identical WIT** (integration fixtures
  pass with no `.wit` rewrite). The `color`/`attribute-value` WIT *variant types*
  stay built in-interface (must match the host's `dom.wit`, which keeps them
  inside `interface dom`) — `use_type_in` would relocate them to the shared
  canonical-ADT interface, changing the published WIT.

### 6.3 tail — DefId-keyed import emission (DOM no longer special on the import side)  ✅ DONE (green)
- Deleted the `IMPORT_*` + `NUM_DOM_IMPORTS` magic constants; the import-index
  allocation assigns DOM slots by position (computed `num_dom_imports`).
- Deleted the hardcoded `imports.import("yel:ui/dom@0.1.0", …, Function(5/31/…))`
  section in `build.rs`. The import section now **iterates the registry**: DOM
  imports are emitted by a loop reading each function's name from its frontend
  `FunctionDef` and its wasm type from `dom_import_types` (interned from the
  frontend signature via `canonical_flat_valtypes`, no `self` handle — exactly
  the global-callback path). The DOM import index is "whatever slot the registry
  gave it," like any callback.
- Centralized the DOM enumeration on `DomImports::all() -> [DefId; 18]` (one
  source of truth for the order, owned by `DomImports`), removing the duplicate
  array in `ImportLayout`.
- WASM bytes change (DOM imports now reference dynamic `type-dom-*` types, not the
  static `Function(5/31)`); **the 51 host-linking execution tests validate it**,
  and `.wit`/`.dot` are byte-stable. The static DOM-only wasm types (e.g. Type 31)
  are now dead in the type section — a follow-up cleanup; the rest stay (shared
  with alloc/free/realloc).

### 6.3 epilogue — intern the whole static function-type vocabulary  ✅ DONE (re-baselined, validated)
Eliminated the fixed 0–33 static function-type block in `build.rs` and the
computed index bases (`15 + arity`, getter dispatch, the `runtime::types`
constant module). All function types are now interned via `intern_type` into a
named **`FuncTypes`** registry on the builder; every consumer reads a
compiler-checked field instead of a literal/computed index. In the process,
**12 dead types** fell out for free (the 3 DOM-only 7/8/31, the 3 redundant
setter types 11/13/15, and 6 redundant ctor/if-block types 24/25/26/28/29/30 —
all of which were already shadowed by dynamically-interned per-shape types).
Deleted: the static type block, the `runtime::types` module (~70 lines), the
manual `name_section` type-name table (names now come from `intern_type`), and
the `NEXT_DYN_TYPE_IDX` magic (`gc_type_base = dyn_types.len()`). WASM type
indices fully renumber; **the 51 host-linking execution tests + `wasm-tools
validate` + the integration suite confirm correctness**; `.wit`/`.dot` unchanged.
This is the rustc-shaped end state: no fixed type-index vocabulary, every type
keyed/allocated.

### 6.4 — DOM is a built-in global; imports flow through the generic path  ✅ DONE (re-baselined, execution-verified)
DOM is now registered as a **built-in foreign-package global** (`Dom`, package
`yel:ui@0.1.0`, its 18 functions are the global's `callbacks`), so its host
imports ride the **same generic global-callback machinery** as any user global —
no DOM-specific import path remains in the backend.
- `GlobalDef.package: Option<PackageId>` added (`None` = module package; `Some` =
  foreign). `register_dom_imports` allocates the 18 fns then registers the `Dom`
  global over them (not name-registered → not user-resolvable).
- The global-callback import **emission** + **type interning** now read the
  global's package, so DOM declares against `yel:ui/dom@0.1.0`.
- Deleted: the `build.rs` DOM import-emission loop, the DOM type-interning loop,
  the `ImportLayout` DOM seeding + `num_dom_imports` field, and the
  `name_section` `dom_func_names` block (DOM is now named `[global-callback]dom.*`
  alongside other globals). DOM imports get whatever slots the registry hands out.
- `create_globals_interfaces` + `register_types_for_globals` **skip** foreign
  globals, so DOM's WIT (the `dom` interface + its inline `attribute-value`/`color`
  types) still comes from `create_dom_interface` (host-compat) and is NOT also
  hoisted into a `shared-types` interface.
- `DomImports` is kept as a typed accessor backed by the global's callback DefIds,
  so the THIR→LIR lowering is unchanged.
- WASM re-baselined (DOM imports moved out of the leading 0–17 block into the
  global-callback range); **the 51 host-linking execution tests confirm DOM still
  links to `yel:ui/dom@0.1.0`**. `.wit` structurally unchanged (a trailing blank
  line); `.dot` re-baselined for the +1 DefId shift from the new global.

### 6.4b — DOM functions ARE the global's callbacks (single source of truth)  ✅ DONE
The `Dom` global's `callbacks` are now the only stored copy of the 18 DOM
`DefId`s. `CompilerContext` holds just `dom_global: Option<DefId>` (was a
separate `dom_imports: Option<DomImports>` table); `register_dom_imports` returns
the global id, and `ctx.dom_imports()` reconstructs the typed `DomImports` view
on demand from the global's callbacks via `DomImports::from_callbacks` (the strict
inverse of `all()`, guarded by a round-trip test). `DomImports` survives only as a
type-safe lens over those callbacks — there is no parallel DOM registry. All ~28
lowering/backend call sites are unchanged (`ctx.dom_imports().create_element`,
now reading the derived view). No snapshot churn (DefId allocation unchanged).

### 6.5 — WIT collapse: DOM rendered from the LIR boundary contract  ✅ DONE (import side)
The hardcoded `create_dom_interface` (~290 lines) is gone. The boundary
contract is now **data in the LIR**: `LirInterface` carries `package`,
`owned_types` (ADTs defined inline), `resources` (by `DefId`), and `functions`
(`LirIfaceFn` = name + signature + callable `def`) — all frontend-agnostic (no
UI roles; a function is just a signature). `CompilerContext::build_import_interfaces`
populates `LirModule.interfaces` (one foreign-package entry per foreign global —
DOM today, owning `attribute-value`/`color` inline). The WIT backend renders that
table via `render_import_interface`: it allocs the interface in its package and
defines `owned_types` inline driven by the contract (`inline_types_owner` honoured
by `register_type` — contract data, not a DOM special-case). Threaded through every
WASM/WIT entry point (`generate_wit`, `generate_wasm_module`, `generate_wasm_with_wit`
auto-builds it from ctx). WIT is byte-identical bar a trailing blank line;
**the 51 host-linking execution tests confirm DOM still links to `yel:ui/dom@0.1.0`**.

### 6.6b — `set-text-content` value is a generic `to_string` Call  ✅ DONE (execution-verified)
Same treatment for the string push: `PushExprAsString` and the bespoke
type-dispatching `emit_expr_as_string` codegen are deleted. The lowering wraps
the value in a real `to_string` `Call` (`wrap_as_string`) — or pushes it
unchanged if already a `string` — and emits the generic `PushExpr`. The
`to_string`-function-for-type mapping is now a shared `CompilerContext`
method (`to_string_func_for`); interpolation's `get_to_string_func_for_type`
delegates to it, and codegen's existing `Call` arm dispatches the helper. All
51 execution tests pass; `.wit`/`.dot` unchanged. Both stringify-on-the-boundary
ops (`PushExprAsString`, `PushExprAsAttrValue`) are now generic value
construction + a single generic `PushExpr`.

### 6.6 — `set-attribute` value is generic variant construction  ✅ DONE (execution-verified)
The bespoke `PushExprAsAttrValue` LIR op and the `emit_expr_as_attr_value`
codegen flattener are deleted. The lowering now wraps the value in a real
`attribute-value` `VariantCtor` (`wrap_as_attr_value` picks the case by matching
the value's type to the case payload — the DOM knowledge lives in the frontend),
and emits a generic `PushExpr { expr }` op. Codegen routes a `VariantCtor`
through the shared `emit_variant_ctor_flat` (forcing the flat boundary form, not
the GC ctor). To make that path handle the attribute-value cases the bespoke op
used to, `emit_variant_ctor_flat` was generalized to reuse existing helpers:
the **string** fat-pointer non-terminal promotion via `pack_fat_ptr_to_i64`
(general — any variant with a string case + i64 slot 0), and the **color** case
via the existing `emit_attr_value_color_arm`/`pack_color_to_attr_slots`. The
`attribute-value` `Ty` is cached on `known.variants` so the immutable-`ctx`
lowering can build the ctor. WASM re-baselines (different instruction sequence);
**all 51 host-linking execution tests pass** (narrow ints, string, color via the
grid/checker fixtures); `.wit`/`.dot` unchanged. This removes the last
UI-specific DOM op from codegen.

Remaining (6.7): migrate the **export side** onto the contract — component
resource interfaces + dispatch (the `resources` field is in the model; the
resource must own its constructor/method surface, today synthesized by
`create_component_interface` with UI lifecycle baked in) — and local globals
(shared-types). The `color` branch in `emit_variant_ctor_flat` is a guarded reuse
that generalizes to a nested-variant lift when a second nested-variant case appears.

Note: `.dot` encoding raw `DefId` numbers makes all 63 fixtures churn on any
builtin-registration change — brittle; a follow-up could use stable per-component
signal indices instead.

## Invariants / guards

- Snapshot fixtures `globals_only`, `globals`, `global_inout`,
  `global_out_property`, `for_global_list` must stay byte-identical through
  phases 1, 3, 4; phase 2 only adds new fixtures.
- No silent fallbacks: unimplemented paths use `todo!()` / `CodegenError`.
- Each phase keeps `main` green (full `cargo test`).
- Update `docs/ARCHITECTURE.md` §2–3 and `docs/TECH_DEBT.md` when the spine
  changes (phase 3/4).
