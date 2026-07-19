# CLAUDE.md - yelc

The **CLI driver** for the Yel compiler plus the **shared front-end pipeline**
reused by every output driver (CLI binary, native lib API, WASI component).
Architecture overview: [`docs/ARCHITECTURE.md`](../../docs/ARCHITECTURE.md) §6.

## Modules

| File | What it is |
|------|-----------|
| `pipeline.rs` | Transport-neutral front-end loop. `lower_all(compiler, sources) -> Lowered { module: LirModule, hir }` runs parse→HIR→typeck→LIR over every source, then type-checks/lowers global-singleton defaults once at the end. Also `wit_options(package)` and `diagnostics(compiler) -> Vec<DiagnosticData>` (flattens diagnostics to transport-neutral records). Drivers differ only in how they render diagnostics and emit output. |
| `main.rs` | clap CLI `yelc`. |
| `lib.rs` | Native library API surface. |

## CLI surface (`main.rs`)

- `yelc compile <files> -o <wasm|wit|dot|rust>` — with `--release`, `--opt`, `--package`, and `wasm-opt` passthrough args; the wasm path can post-process/strip via `wasm-tools`.
- `yelc ir <file> [--pretty] [--json]` — dump the lowered LIR.
- `yelc check <files>` — parse + type-check, report diagnostics, no output.
- Debug dumps include `--hir` (why `Lowered` retains `hir`).

## Conventions

- Keep `pipeline.rs` free of target/transport-specific types — it's shared by the CLI, the native lib, and the WASI component. Add new shared front-end logic here, not in `main.rs`.
- Errors **accumulate** in the compiler context; `lower_all` returns `Err(LoweringFailed)` between phases via `compiler.has_errors()` and the caller renders `ctx.diagnostics` itself.
- WIT package defaults to `yel:app@0.1.0` when no source declares a package (`wit_options`).

## Tests

- `tests/snapshot.rs` — insta snapshots of generated **WIT/DOT** by running the real `yelc` binary. Update intentional changes with `INSTA_UPDATE=always cargo test -p yelc --test snapshot` (or `cargo insta review`); snapshots in `tests/snapshots/`.
- `tests/compile.rs` — compile-outcome helpers (`expect_success`/`expect_failure`).
