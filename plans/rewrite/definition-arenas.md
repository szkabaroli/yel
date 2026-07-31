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
