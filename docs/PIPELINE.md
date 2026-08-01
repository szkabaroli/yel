# The Yel compiler pipeline, stage by stage

> **What this doc is for:** answering "which stage owns this?" and "where does
> *X* get desugared/resolved/allocated?" without reading 30k lines.
> [`ARCHITECTURE.md`](ARCHITECTURE.md) describes the *system* (crates, shared
> state, migrations); this file describes the *stages* — what each one
> establishes, what it deliberately leaves undone, and its entry points.
> Known hacks: [`TECH_DEBT.md`](TECH_DEBT.md). Last reviewed against `main` on
> 2026-07-24.

```
source (.yel)
  │  parse                     pest grammar
  ▼
AST     syntax/ast.rs          strings, spans. Nothing resolved.
  │  Compiler::lower_to_hir    hir/lower.rs
  ▼
HIR     hir/                   ids assigned, scopes tracked, first desugarings. No types.
  │  Compiler::type_check      thir/typeck.rs (+ thir/signalck.rs)
  ▼
THIR    thir/                  every expr has a Ty; every name is a DefId/FieldIdx/LocalId.
  │  Compiler::lower_to_lir    lower_to_lir/component.rs → lower_to_lir/blocks.rs
  ▼
LIR     lir/                   flat LirOp blocks, interned exprs/strings, slots, GC layout.
  │  yel-wasm-codegen          wasm/, wit.rs, dot.rs
  ▼
WASM component  (+ WIT, + DOT)
```

Each arrow is one explicit method on `Compiler` (`yel-core/src/compiler.rs`).
The whole loop is orchestrated exactly once, in
[`yelc/src/pipeline.rs::lower_all`](../crates/yelc/src/pipeline.rs), and every
driver (CLI, native lib, WASI, fuzzer, tests) shares it.

---

## At a glance

| Stage | Entry point | Input → Output | The one thing it establishes |
|-------|-------------|----------------|------------------------------|
| **Parse** | `Compiler::parse` / `parse_with_source_id` | `&str` → `ast::File` | The text is syntactically well-formed and spanned. |
| **AST→HIR** | `Compiler::lower_to_hir` → `hir::lower_file` | `&File` → `Vec<HirItem>` | Every definition is *registered* and every node/local has an **id**. |
| **HIR→THIR** | `Compiler::type_check` → `thir::type_check` | `&HirItem` → `ThirItem` | Every expression has a **`Ty`**; every name is a **`DefId`**. Plus reactivity deps (signalck). |
| **THIR→LIR** | `Compiler::lower_to_lir` → `lower_to_lir::lower_component` | `&ThirComponent` → `LirResource` | Structure becomes **blocks of flat ops** over **allocated slots**. |
| **Module passes** | `Compiler::lower_items_to_module` | `&[HirItem]` → `LirModule` | Cross-item facts: global fanout, globals init, the import/export boundary contract. |
| **LIR→target** | `generate_wasm_module` / `generate_wit` / `generate_dot` | `&LirModule` → bytes | Symbolic type refs become **concrete wasm type indices**; ops become instructions. |

---

## 1. Parse — `syntax/`

**Entry:** `Compiler::parse(source)`, `Compiler::load_file(path)`,
`Compiler::parse_with_source_id(source, id)` → `CompileResult<ast::File>`.
Implementation: `syntax/parser.rs` (~3.3k lines, a **pest** grammar wrapper);
tree types in `syntax/ast.rs` (~950 lines).

**Establishes**
- A spanned tree. Every node carries `Spanned<T> { node, span }`; `Span` is a
  byte range plus a `SourceId` into `ctx.source_map`.
- Nothing else. Identifiers are `String`s. `TyKind::Named(..)` is **not**
  resolved. `Expr::Ident` doesn't know whether it's a local, a property, or a
  component.

**Also does here (and nowhere else)**
- **Package-name validation** — `Compiler::validate_package` rejects non-kebab
  package identifiers *at parse time*, because the grammar is deliberately
  permissive and `wit-component` would otherwise fail much later with an opaque
  `decoding custom section component-type` error.

**Error behaviour** — the only stage that can return `Err`. A hard
`ParseError` aborts (and is also pushed to `ctx.diagnostics`); recoverable
`catched_errors` are pushed as diagnostics and parsing continues. Everything
downstream accumulates instead of returning `Result`.

---

## 2. AST → HIR — `hir/`

**Entry:** `Compiler::lower_to_hir(&File)` → `hir::lower::lower_file` →
`Vec<HirItem>` (`HirItem::Component | HirItem::Global` — one per top-level unit).

### The defining pattern: register-then-lower

`lower_file` runs in **phases** so forward references resolve regardless of
declaration order (`hir/lower.rs`):

1. register type definitions — `register_record` / `register_enum` / `register_variant`
2. register `element`s, `extern component`s, `global`s — `register_element`,
   `register_import_component`, `register_global`
3. register component *declarations* (name, properties, callbacks) — **without** bodies (`register_component`)
4. lower bodies — `lower_component` → `lower_node` → `lower_expr` / `lower_statement`

By the end, `ctx.defs` holds every `DefId` in the program, and `ctx.types` holds
every type those definitions mention.

### Establishes
- **Ids.** `NodeId` per UI node (`fresh_node_id`), `LocalId` per binding, tracked
  through a push/pop scope stack (`hir/local_scope.rs`).
- **Types are interned but not checked** — `ctx.intern_ast_ty` turns
  `AstTyKind` into a `Ty` handle for annotations. Expressions still have no type.
- **Partial name resolution.** `lower_expr` on `Expr::Ident` resolves against
  locals first, then `Namespace::Value` / `Type` / `Component`. Names that don't
  resolve are **kept as syntax** (`HirExprKind::Call { func: name, .. }`) for
  THIR to deal with — HIR never errors on an unknown name.

### Desugarings performed here
| Surface | Becomes | Why here |
|---|---|---|
| `#ff0000` color literal | `Color.rgba((r,g,b,a))` — a `PathCall` to a variant ctor | Nothing downstream needs a primitive color repr. |
| `x += 1` | `x = x + 1` (`HirStatement::Assign` over a `Binary`) | Compound assignment never reaches typeck. |
| `Foo.bar(args)` | `PathCall` if `Foo` is a **type** or **global**, else `MethodCall` | Needs the def tables, which only exist post-registration. |
| `Enum.case` / `Global.prop` member access | `HirExprKind::Path { segments }` | Distinguishes a namespaced path from a real field access. |

### Does NOT do
Type checking, arity checking, field-index resolution, or any reactivity
analysis. `if` / `for` / `Element` stay **structured** as `HirNodeKind` — the UI
tree is not flattened until LIR.

---

## 3. HIR → THIR — `thir/`

**Entry:** `Compiler::type_check(&HirItem)` → `thir::type_check` → `ThirItem`.
The single type-check entry; it dispatches on item kind internally
(`type_check_global` for globals — they have no UI body, so only their property
defaults are checked). `type_check_with_map` is the LSP variant that also
returns the `span → Ty` map.

**Core:** `thir/typeck.rs` (~2.9k lines).

### Bidirectional
Every expression is checked in a `Mode`:
- `Mode::Infer` — synthesize a type from the expression.
- `Mode::Check(Ty)` — check against an expected type.

This is what keeps **numeric literals polymorphic**: `2` has no type until a
`Check(f32)` arrives, at which point the coercion is authorised. (The literal is
only *materialised* at the LIR stage — see §4.)

### Establishes
- `ThirExpr { id: ExprId, kind, ty: Ty, span }` — **every** expression has a `Ty`
  and a stable `ExprId`.
- **Full name resolution.** `HirExprKind::Call { func: String }` becomes
  `ThirExprKind::Call { func: DefId }`; `Field { field: String }` becomes
  `Field { field_idx: FieldIdx, field_def: DefId }`; `Path` splits into
  `EnumCase { ty_def, case_idx }`, `VariantCtor { ty_def, case_idx, payload }`,
  `GlobalRead { global, field, prop }`, or `GlobalCall { global, function, args }`.
- **Field reordering** — `RecordLiteral.fields` are stored in *definition* order,
  not source order.
- **Element resolution** — `ThirNodeKind::Element { component: Option<DefId>, tag }`;
  `None` marks a built-in element.
- **A `span → Ty` `TypeMap`** for LSP hover.

### Boundary checks that live here
`ty_reaches_empty_aggregate` walks every boundary-carried position
(option/result/list/tuple payloads, record fields, variant payloads) and rejects
empty `record {}` / `enum {}` / `variant {}` — the component model forbids them,
and catching it here gives a source span instead of an encoder error.

### signalck — the reactivity analysis
`thir/signalck.rs` runs **immediately after** typeck, from inside `type_check`
(`check_component` / `check_global`), and stores its result on the context via
`ctx.set_signal_deps(def_id, deps)`.

It is **read-only** — it does not mutate THIR. It produces `SignalDependencies`:
- `binding_reads` — which signals each binding reads
- `handler_writes` — which signals each handler writes
- `effects_by_signal` — the **inverted index** `signal → [EffectSource]` that
  drives the whole update-block fanout in LIR

### Error behaviour
Recovers with `Ty::ERROR` and **keeps traversing**, so one file reports all its
type errors at once. `lower_items_to_module` type-checks every item before
lowering any, then bails on `has_errors()` — LIR lowering assumes well-typed input.

---

## 4. THIR → LIR — `lower_to_lir/` (output in `lir/`)

**Entry:** `Compiler::lower_to_lir(&ThirComponent)` →
`lower_to_lir::lower_component` → `LirResource`.

> **Layering rule.** Lowering lives in `lower_to_lir/`, *outside* `lir/`, so
> `lir/` has **no HIR/THIR dependency**. LIR is a frontend-neutral target — the
> flow frontend emits it directly, without HIR or THIR. Anything in `lir/` may
> only reach `ops`, `ids`, `types`, `interner`, `source`, `definitions`,
> `context`. See [`ARCHITECTURE.md §0`](ARCHITECTURE.md#0-direction-of-travel-read-this-first).

This is **two stages in one call**:

```rust
pub fn lower_component(component: &ThirComponent, ctx: &CompilerContext) -> LirResource {
    let mut lowering = LirLowering::for_component(ctx, component.def_id, &component.locals);
    let tree = lowering.lower_component_to_tree(component);   // stage A
    let mut lowering = BlockLowering::new(ctx, &tree);
    lowering.lower_component(&tree)                            // stage B
}
```

### Stage A — tree LIR (`lower_to_lir/component.rs`, ~1.2k lines)
Output: `TreeLirResource { signals, effects, body: Vec<LirNode>, exprs }`.

- **Discovers signals and effects.** Component properties become signals;
  reactive bindings/text become effects. `collect_dependencies` walks each
  expression, then **sorts + dedups** the dependency list — determinism is a
  correctness requirement, not a nicety (snapshots are byte-compared).
- **Starts the expression arena.** `LirExpr { kind, ty }` values are pushed to
  `exprs`; everything downstream references them by `LirExprId`.
- **Materialises numeric literals.** `lower_primitive_literal` uses the THIR
  `Ty` to pick the exact representation — `y: f32 = 2` lowers to
  `LirLiteral::F32(2.0)`, *not* `S32(2)`. This is where polymorphic literals die.
- **Desugars string interpolation** — `ThirExprKind::Interpolation` becomes a
  `Call` to the known `concat` function, with each non-string part wrapped in a
  type-specific `to_string` conversion. Single-part interpolations skip `concat`
  entirely.
- The UI tree is still a **tree**: `LirNodeKind::{Element, StaticText,
  DynamicText, If, For, ChildrenSlot}`.

### Stage B — block LIR (`lower_to_lir/blocks.rs`, **~8.4k lines — the biggest file in the repo**)
Output: `LirResource`.

- **Flattens the tree into blocks.** Each `LirBlock` is params + a flat stream of
  `LirOp`. Control flow becomes explicit ops (`If`, `Loop`, `CallBlock`,
  `Return`).
- **Allocates storage.** `LirSlotId` is a typed enum — `Block { block, idx }` for
  per-block temporaries, `Resource { idx }` for resource-wide slots. The
  allocator is per-block; cross-block temp references panic loudly in codegen.
- **Interns.** Strings are deduped into a string table (`StringId`).
  Expressions are appended to the arena but **not yet deduped** (documented TODO).
- **Computes layout.** GC struct/array type declarations (`lir/struct_types.rs`),
  per-signal layout (`lir/signal_layout.rs`), the mount-tree boundaries
  (`lir/tree_shape.rs`), scratch-local counts.
- **Synthesises lifecycle blocks** — `synth_internal_constructor_block`,
  `synth_internal_unmount_block`, `synth_export_lifecycle_blocks`.
- **Post-passes** — `lir/dedupe.rs` merges structurally-identical per-(boundary,
  signal) update blocks via slot-normalised hashing; `lir/boundary_rewrite.rs`
  resolves symbolic boundary-field slots.

### What stays symbolic on purpose
`LirTypeRef` (`lir/block.rs`) — `ComponentStruct`, `OtherComponentStruct(DefId)`,
`TreeBoundary(TreeBoundaryId)`, `GcVariantCase(Ty, u32)`, `TupleStruct(Ty)`, … —
is **not** a wasm type index. Resolution to concrete indices happens only in
codegen. Same for `BlockId`, `StringId`, `LirExprId`.

### Stable correlation ids
`ForId`, `IfId`, `TreeBoundaryId` must stay **stable across the tree→block
transition** — later passes (dedupe, boundary rewrite, codegen) key off them.

---

## 5. Module-level passes — `Compiler::lower_items_to_module`

Some facts are not knowable from one item. `lower_items_to_module(items, package)`
type-checks everything, lowers each component, then runs these **in order** to
produce the `LirModule`:

| Pass | Function | What it needs the whole module for |
|---|---|---|
| Global trigger resolution | `resolve_global_triggers` | Expands every `LirOp::TriggerEffects` placeholder into direct `CallBlock`s and synthesises per-observer fanout blocks. Must run after **every** component is lowered — codegen **rejects any surviving `TriggerEffects`**. |
| Global lowering | `lower_globals_to_lir` | Produces `Vec<LirGlobal>` + the shared default-expression arena they index into. Iterates in declaration order (an `IndexVec` walk, never a `HashMap`) so the arena is byte-stable. |
| Module-start init | `synth_globals_init_block` | Plans the globals-init as an **LIR block**, so the backend transcribes it rather than building it imperatively. |
| Import contract | `ctx.build_import_contract` | One registry of every host import (component callbacks, global callbacks, DOM) plus the `Import`-direction WIT interfaces that declare them. |
| Export contract | `ctx.build_export_interfaces` | `Export`-direction `LirInterface` per exported component: the resource plus `LirIfaceFn`s for the constructor, mount/unmount, and a get/set pair per non-callback signal. |
| Extern components | `ctx.build_extern_component_interfaces` | `extern component X` declarations as `Import`-direction resource interfaces. |

Result — the single compilation unit handed to the backend:

```rust
LirModule {
    resources: Vec<LirResource>,     // instantiable components
    globals: Vec<LirGlobal>,         // module-scope singletons
    global_exprs: Vec<LirExpr>,      // shared default-expr arena
    global_init_block: Option<..>,   // module-start init, planned in LIR
    imports: Vec<LirImport>,         // the one host-import registry
    interfaces: IndexVec<InterfaceId, LirInterface>,  // boundary contract, both directions
    package: Option<PackageId>,
}
```

Both the core wasm import section and the WIT import interfaces derive from
`imports`/`interfaces` — there is no second source of truth.

---

## 6. LIR → target — `yel-wasm-codegen`

Three independent consumers of the same `LirModule`:

| Target | Entry | File |
|---|---|---|
| WASM component | `generate_wasm_module` (primary), `generate_wasm` / `generate_wasm_with_wit` (shims), `generate_component` / `generate_function_module` (generic, no mount-tree) | `wasm/` |
| WIT | `generate_wit` | `wit.rs`, `wit_ast.rs` |
| DOT (debug graph) | `generate_dot` | `dot.rs` |

### The wasm path, in order (`generate_wasm_module` → `generate_wasm_module_with_wit`)
1. **Build the WIT AST** — `WitAstBuilder`, seeded with the module's boundary
   contract (`set_import_contract`). Yields a `(Resolve, WorldId)`.
2. **Empty-module short-circuit** — a module with no resources *and* no global
   defaults emits a `dummy_module`; a globals-only library still emits a real
   core module (allocator, memory, start function).
3. **Build the core module** — `WasmPackageBuilder`: `set_globals`,
   `set_imports`, pre-intern common strings, `collect_strings`, `init_heap`,
   then `build_core_module`.
4. *(optional)* pipe through `wasm-opt` — **must** happen before metadata
   embedding, since it strips custom sections.
5. **Embed WIT metadata** — `wit_component::embed_component_metadata`.
6. **Encode the component** — `ComponentEncoder::validate(true).encode()`.

Set `YEL_DEBUG_WASM=1` for pre-validation plus `/tmp/debug_core_module.wasm` and
`/tmp/debug_module_with_metadata.wasm` dumps. It's off by default because the
test harness compiles dozens of fixtures per run and the `/tmp` writes race.

### What codegen resolves that nothing else could
- `LirTypeRef` → concrete wasm type indices.
- Function types → a single **growing interned registry** (`func_types`); there
  is no fixed vocabulary and no computed index bases. Consumers read a *named*
  index, so the indices themselves are pure allocation artifacts. One `concat`
  type is interned per distinct arity the program actually uses.
- `wasm/repr.rs::InternalRepr` — the single source of truth for value
  representation (scalar vs flat vs GC).
- `wasm/runtime/` — linear-memory bump allocation and string storage as `(ptr,len)`.

`wasm/codegen/` splits emission by concern: `build.rs` (type section + interning),
`op_emit.rs`, `block_fn.rs`, `dispatch.rs`, `signal_emit.rs`, `accessors.rs`,
`record_list.rs`, `scratch.rs`, `name_section.rs`, `function_type.rs`.

### Error behaviour
Typed `CodegenError` (`UnsupportedType`, `UnsupportedExpr`, `MissingDefinition`,
`InternalError`, `EncodingError`, `InvalidIR`, `LayoutMissing`). **Never a silent
fallback** — an unimplemented path is `todo!("descriptive msg")` or an `Err`.
A placeholder instruction produces type-incorrect wasm that is near-impossible to
trace back from a hex dump.

---

## Where does X happen?

| Question | Answer |
|---|---|
| Where do names become `DefId`s? | Mostly **HIR** (`lower_expr` against `ctx.defs`); everything left over resolves in **THIR**. |
| Where do expressions get types? | **THIR** only. HIR interns *annotations*, not expression types. |
| Where does `2` become `f32`? | Typeck **authorises** it (`Mode::Check`); LIR stage A **materialises** it (`lower_primitive_literal`). |
| `#ff0000` → variant ctor? | **HIR** (`lower_expr`). |
| `x += 1` → `x = x + 1`? | **HIR** (`lower_statement`). |
| `"a {b} c"` → `concat(...)`? | **LIR stage A** (`lower_expr`, `ThirExprKind::Interpolation`). |
| Record fields reordered to definition order? | **THIR** (`RecordLiteral.fields`). |
| Where is the `signal → effects` index built? | **signalck**, right after typeck, stored on `ctx`. |
| Where does the UI tree stop being a tree? | **LIR stage B** (`blocks.rs`) — stage A is still `LirNodeKind`. |
| Where are slots allocated? | **LIR stage B**, per block. |
| Where are strings deduped? | **LIR stage B** (expressions are **not** deduped — known TODO). |
| Where do wasm type indices appear? | **Codegen only.** LIR type refs are symbolic (`LirTypeRef`). |
| Where is the host-import list built? | **Module pass** `build_import_contract` — one registry feeding both the import section and WIT. |
| Where is `@children` spliced? | Marked `ChildrenSlot` through HIR/THIR/LIR-tree; resolved in **LIR stage B**. |
| Where are empty `record {}` etc. rejected? | **THIR** (`ty_reaches_empty_aggregate`) — with a source span. |
| Where is the package name validated? | **Parse** (`validate_package`), to beat `wit-component`'s opaque error. |

---

## Cross-cutting rules (they apply at every stage)

1. **No silent fallbacks.** Never emit a placeholder value/instruction for an
   unimplemented path. `todo!("msg")` or `Err(CodegenError::…)`. A
   `Literal(Bool(false))` standing in for an unlowered string produces
   type-incorrect IR that costs hours to trace.
2. **Diagnostics accumulate.** Push to `ctx.diagnostics` and keep going (recover
   with `Ty::ERROR`). The driver bails *between* phases via `has_errors()` —
   never early-return on the first user error.
3. **Typed ids + interning.** One `u32` newtype per index space (`ids.rs`) stored
   in `IndexVec` (`index_vec.rs`). Never pass a raw `usize` index.
4. **Determinism.** Anything derived from a `HashMap`/`HashSet` is sorted and
   deduped before it reaches output. Snapshot and golden tests compare bytes.
5. **Tests assert correct behaviour.** Never weaken an assertion to match a known
   bug — mark it `#[ignore]` with a reference.

---

## Inspecting each stage

```bash
cargo run -p yelc -- check path.yel          # parse + HIR + typecheck only, diagnostics
cargo run -p yelc -- ir --pretty path.yel    # dump LIR (also --json)
cargo run -p yelc -- compile -o wit  path.yel
cargo run -p yelc -- compile -o dot  path.yel
cargo run -p yelc -- compile -o wasm path.yel > out.wasm && wasm-tools validate out.wasm
YEL_DEBUG_WASM=1 cargo run -p yelc -- compile -o wasm path.yel   # + /tmp core-module dumps
```

There is **no HIR or THIR dump** on the CLI today: `OutputFormat` is
`rust | wit | wasm | dot` only, and `check` renders diagnostics without printing
IR. `Lowered` still retains its `hir` field for a `--hir` dump that no longer
exists as a flag. (`docs/ARCHITECTURE.md` §6 and `crates/yelc/CLAUDE.md` both
still claim `--hir` — stale.) To inspect HIR/THIR, go through the library API or
a unit test.
