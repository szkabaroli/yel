# yel-wasm-codegen — test coverage baseline

Generated with:

    scripts/coverage.sh

(requires `cargo install cargo-llvm-cov`).

## Baseline snapshot

Date of snapshot: session closing 2026-04-19. Re-run after substantial
codegen changes and update the table below.

| File | Regions | Lines | Functions | Grade |
|---|---:|---:|---:|---|
| wasm/runtime/list.rs      | 100.00% | 100.00% | 100.00% | 🟢 |
| wasm/runtime/memory.rs    |  99.70% |  99.26% |  94.44% | 🟢 |
| wasm/runtime/strings.rs   |  99.53% |  99.28% |  94.44% | 🟢 |
| wit.rs                    |  90.62% |  94.44% |  66.67% | 🟢 |
| wit_ast.rs                |  78.80% |  81.10% |  69.70% | 🟡 |
| wasm/runtime/mod.rs       |  73.39% |  80.68% |  50.00% | 🟡 |
| wasm/core_module.rs       |  68.70% |  68.55% |  65.22% | 🟡 |
| wasm/mod.rs               |  56.36% |  57.46% |  70.49% | 🟡 |
| wasm/expr.rs              |  30.17% |  29.75% |  41.67% | 🟠 |
| **TOTAL**                 | **64.42%** | **65.06%** | **65.86%** | |

## Reading the numbers

- **Regions** are fine-grained control-flow regions (LLVM instrumentation).
  Region-coverage best reflects whether every decision branch has been
  hit, which is what we actually care about for codegen correctness.
- **Lines** are less sensitive to match-arm coverage — a single `match`
  with 40 unreached arms still shows a few "uncovered lines" even
  though 40 behaviours are missing. Use regions as the primary metric.
- **Functions** counts which top-level items executed at all; not
  sensitive to internal branches.

## Blackest spot: `wasm/expr.rs` (13.25%)

Both `emit_expr` variants (line 18+ and 852+) plus `emit_expr_count` /
`emit_expr_as_string` match over `LirExprKind`. Many variants never fire
from current fixtures: `VariantCtor`, `RecordConstruct`,
`TupleConstruct`, `Closure`, `Range`, deep ternaries, the full `Literal`
matrix (f32, f64, unit, list, tuple, record), `OptionalField`, complex
`PathCall` shapes.

Realistic paths to raise this:

1. Add fixtures that use the listed features. Each fixture is cheap and
   lifts coverage by several percent at a time.
2. Write fine-grained unit tests that build tiny `LirExpr` trees in
   memory and drive `emit_expr` directly. This requires exposing
   `WasmPackageBuilder` state (runtime_funcs, import_layout, allocator
   state) to tests — medium-effort refactor.

Tier 4 fixtures already pushed a few of these variants into the
executed set; the rest need either more fixtures or the builder-state
refactor above.

## Second-blackest: `wasm/mod.rs` (52.59%)

Most of the file is the `WasmPackageBuilder` bookkeeping methods
(`collect_strings_from_expr`, `collect_concat_arities`,
`collect_record_types`, `collect_list_constructs`). Each has a deep
match over `LirExprKind` that mirrors `emit_expr`. Same fixture-driven
coverage story — they go up together.

## Keeping it honest

Run `scripts/coverage.sh` before merging codegen-touching PRs. If a
file drops more than a few % expect a paragraph in the commit
explaining why. A real regression should SHOW UP here first (whole
paths no longer reached), well before it ships to a user.

## Rerun frequency

- Local: on-demand when working on codegen.
- CI: consider `scripts/coverage.sh --lcov` uploaded to codecov once CI
  exists. Not wired up yet.
