# Definition arenas — completing ark's `program_parser_alt.rs` model

Decided 2026-07-31, at the user's direction: adopt the dora model that ark's
`program_parser_alt.rs` started and never finished, and dissolve
`yelc-sema`'s `Definition`/`DefKind` into per-kind definition arenas.

## What the alt file actually is

`arkc-frontend/src/program_parser_alt.rs` imports `PackageDefinition`,
`PackageDefinitionId`, `ModuleDefinition`, `ModuleDefinitionId`,
`PackageName`, `SourceFile`, `SourceFileId`, `FnDefinition` from `crate::hir`
— **none of which exist anywhere in ark**. The file cannot compile; it is
dora's architecture (`dora-lang/dora`, ark's ancestor) mid-transplant. So the
reference for the missing types is dora:

- `Sema` carries **arenas**: `packages`, `modules`, `source_files`, plus one
  arena per declaration kind (`fcts`, `structs`, `enums`, `globals`, …), each
  with its own id type (`FctDefinitionId`, …).
- `PackageName::{Stdlib, Boots, Program, External(name)}` — packages are
  *roles*, not just names.
- `ModuleDefinition { package_id, parent_module_id, name, table:
  OnceCell<Rc<SymTable>> }` — the symbol table hangs off the module.
- `SourceFile { package_id, module_id, path, content }` — every file knows
  its place in the tree.
- `ProgramParser` with a `files_to_parse: VecDeque` work queue:
  `add_stdlib_package` → `add_program_package` → `add_dependency_packages`,
  each file parsed and scanned off the queue.

## Step 1 — the compilation structure (landed with this plan)

`yelc-sema/src/compilation.rs`, ported: `PackageName::{Std, Program,
External}`, `PackageDefinition`, `ModuleDefinition`, per-file assignment of
`SourceId → (package, module)`. `yelc-hir`'s `check_package` becomes the
completed `ProgramParser::parse_all` shape: program package from the
directory, std packages from the embedded registry, dependency packages from
`--include` — every package and module a real arena row rather than a bare
`PackageId(u32)` ordinal.

`Definitions` survives step 1 untouched as the per-package *declaration*
table; the arenas describe the *compilation's* structure above it.

## Step 2 — dissolving `Definition`/`DefKind` (contracted, not yet built)

Per-kind arenas replace the one `Vec<Definition>`:

| today | becomes | id |
|---|---|---|
| `Definition { kind: Type }` (record) | `RecordDefinition { name, fields: Vec<Member-shaped rows>, ty }` | `RecordDefinitionId` |
| `…Type` (enum / variant) | `EnumDefinition` / `VariantDefinition` | per-kind ids |
| `…Component` (component / element / extern) | `ComponentDefinition` / `ElementDefinition` / `ExternComponentDefinition` | per-kind ids |
| `…Global` | `GlobalDefinition { properties, callbacks }` | `GlobalDefinitionId` |
| `Member` rows | fields **on** the per-kind definition (dora keeps fields on `StructDefinition`) | index |
| `Sym` / coarse `DefKind` | `SymbolKind` grows per-kind variants (already ported in `yelc-hir/src/sym.rs`) | — |

Cascades that make this its own step, each a mechanical sweep once the types
land:

1. **`TyKind::Adt(DefId)`** → per-kind variants (`Record(id)`, `Enum(id)`,
   `Variant(id)`, `Component(id)`) — dora's `SourceType` shape. Touches the
   interner, `type_of`, and every `Adt` match.
2. **The artifact** — `SerializedDefPath.kind` becomes the fine-grained kind;
   `Stamp::FORMAT` 3 → 4. The wire gets *more* faithful: the frozen
   compiler's `DefKind` was always fine-grained, and the coarse table is the
   thing that had to carve out a mapping in the differential.
3. **HIR** — `Def(DefId)` / `Prop { owner: DefId }` / `StateRef.owner` carry a
   cross-kind reference; either a small `DefinitionId` enum over the per-kind
   ids (dora's `SymbolKind` minus the scope-only variants) or per-kind
   variants at each site. Decide when writing; prefer the enum, it keeps HIR
   nodes small.
4. **The frozen differential and the checkpoint harness** re-map — and get
   *simpler*: frozen `DefKind::Record(_)` compares against `RecordDefinition`
   directly instead of both sides coarsening.

**Sequencing rule:** step 2 lands as one change with the differential green on
both sides of it, or it does not land. The 2000-program definition-table
comparison is the safety net that makes the sweep safe; it must be re-pointed
at the arenas in the same change.

## What dies at the end

`Definition`, coarse `DefKind`, `Sym` (already shadowed by the ported
`SymbolKind`), `Definitions.modules` (superseded by the module arena), and
`OverloadKey` moves onto the per-kind function definition where dora keeps
it.

## Step 2 working design (2026-08-01, from reading dora's source directly)

Read for this: dora-frontend's `element_collector.rs` (per-kind `alloc` +
backpatch), `sym.rs` (`SymTable`/`ModuleSymTable`), `useck.rs` (fixpoint
import resolution), `typeck.rs` (symtable construction per body). Sequenced
**after** the pending worktree commits land — the sweep is one atomic change
on a clean tree.

### The arena API — behind a `Sema` on the context (decided 2026-08-01)

Dora names its whole god-object `Sema`; yel keeps `CompilerContext` for the
infrastructure (interner, sources, diagnostics — things every stage touches)
and adds a **`Sema` field holding everything the passes own**: the per-kind
definition arenas, the compilation structure (`compilation.rs`'s
package/module arenas move in), the type interner, intrinsics, known items,
symbol tables. The split is *infrastructure vs. semantic model*; a pass
signature that takes `&mut Sema` cannot accidentally be about I/O.

```rust
// yelc-base — one generic arena over IndexVec
pub struct Arena<Id: Idx, T> { rows: IndexVec<Id, T> }
impl<Id: Idx, T> Arena<Id, T> {
    pub fn alloc(&mut self, row: T) -> Id;      // dora: sa.classes.alloc(class)
    // iter / index / iter_enumerated as IndexVec has them
}

// yelc-sema — per-kind arenas, per-kind ids, on the Sema
context.sema.records:    Arena<RecordDefinitionId, RecordDefinition>
context.sema.enums:      Arena<EnumDefinitionId, EnumDefinition>
context.sema.variants:   Arena<VariantDefinitionId, VariantDefinition>
context.sema.components: Arena<ComponentDefinitionId, ComponentDefinition>  // + element/extern rows
context.sema.globals:    Arena<GlobalDefinitionId, GlobalDefinition>
context.sema.functions:  Arena<FunctionDefinitionId, FunctionDefinition>    // OverloadKey moves here
```

Members become fields **on** the per-kind row (dora keeps fields on
`StructDefinition`), indexed by the existing index newtypes.

**Naming rule (decided 2026-08-01): `Hir*` stays.** The HIR vocabulary keeps
its prefix (`HirExpr`, `HirBoundary`, `HirModule`, …) — dora's bare names are
not adopted there. Dora is the reference for *architecture*, not for what
yel's IR types are called.

### Why per-kind arenas beat one `enum Definition` arena

Recorded because the alternative looks equivalent and is not:

1. **Ids carry proof of kind.** `RecordDefinitionId` cannot name a global, so
   the wrong-kind match arm disappears from every consumer instead of being
   repeated (today: ~60 `DefKind::` matches, most asserting a kind the
   caller already knew; `TyKind::Adt(DefId)` forcing typeck to consider
   nonsense). The enum survives only at genuinely polymorphic sites
   (symbol table, `HirExprKind::Def`, hover, artifact path) as
   `DefinitionId`.
2. **Each kind gets its natural columns.** The single table forced
   everything kind-specific through the one `Member` row shape in a side
   table. Per-kind rows dissolve it: `RecordDefinition.fields`,
   `EnumDefinition.cases`, `ComponentDefinition { properties, functions }`.
3. **Passes iterate their kind.** Dora's `enumck` walks `sa.enums`,
   `fctdefck` walks `sa.fcts` — the pass signature is the filter, not a
   convention over one big table.
4. **Borrow locality.** `&mut sema.functions` while reading `&sema.records`
   coexists; one `Vec<Definition>` cannot split that borrow.

### The cross-kind reference

The plan's stated preference stands: one `DefinitionId` enum (dora's
`SymbolKind` minus the scope-only variants) —

```rust
pub enum DefinitionId {
    Record(RecordDefinitionId), Enum(EnumDefinitionId),
    Variant(VariantDefinitionId), Component(ComponentDefinitionId),
    Global(GlobalDefinitionId), Function(FunctionDefinitionId), // …
}
```

— carried by `HirExprKind::Def`, `Prop.owner`, `StateRef.owner`, `TyKind`
(per-kind variants replacing `Adt`), and the artifact's `SerializedDefPath`.

### Measured sweep inventory (2026-08-01, worktree)

~110 direct call sites: `DefKind::` ×60, `.defs.` ×45, `TyKind::Adt` ×7,
`context.definition` ×4 — across `yelc-sema` (definitions, artifact
wire/load/write, context, known, intrinsics, stdlib), `yelc-hir` (lower/*,
sym, emit_hir, signalck, module), `yelc-lsp` (symbols). Plus `Stamp::FORMAT`
3 → 4 and re-pointing the frozen differential (`frozen_parity`,
`single_namespace`, the corpus sweeps).

### Dora patterns to adopt alongside (not gating step 2)

- **Prelude layer.** `ModuleSymTable`'s lookup chain is levels → module →
  dependencies → **prelude** (`sym.rs:65`); `stdlib_lookup::setup_prelude`
  fills the last slot. That slot is where yel's builtin element inventory
  belongs — the structural fix for `?VStack` once `std:ui` declares
  elements, with no surface-syntax change.
- **Unused-use marking.** Dora's `Symbol` carries `use_info` with a
  `used: Cell<bool>` flipped by every `get`; `warn_unused_uses` scans at the
  end. Free unused-include warnings for yel, surfaced by the LSP.
- **Fixpoint useck.** Needed only when includes can chain (re-exports);
  yel's flat includes resolve in one pass today. Recorded so the loop shape
  (`ignore_unknown_symbols` rounds until no progress, then one strict pass)
  is reached for instead of reinvented.
- **Frozen tables.** `ModuleDefinition.table: OnceCell<Rc<SymTable>>` — the
  table is mutable during collection + useck, then frozen; resolution
  afterwards is a read-only service. Matches the register-then-lower
  invariant already in place.
