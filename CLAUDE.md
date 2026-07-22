# CLAUDE.md — yel (workspace root)

Yel is a declarative, reactive UI language that compiles to a **WebAssembly
component** (component-model, GC + linear memory) that talks to a host through
`yel:ui/dom@0.1.0`. Surface language: [`LANGUAGE.md`](LANGUAGE.md). Goals:
[`README.md`](README.md). ⚠️ Highly WIP — `main` is often red.

## North star (active migration)

We are moving from a **UI-specific compiler** to a **generic one**: the LIR and
the whole `yel-wasm-codegen` back-end are becoming a frontend-agnostic substrate
**shared by Yel (UI) and the visual flow language**. Most live refactors and
every transitional bridge exist to serve this. New back-end / LIR code should
depend only on the `lir/arena.rs` traits and generic `LirOp`s — never on UI
concepts (`tree_shape`, `boundary`/`mount`, `$Comp` self-ref, `yel:ui/dom`).
Full framing: [`docs/ARCHITECTURE.md` §0](docs/ARCHITECTURE.md).

## Start here

- **Deep architecture (current state):** [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) — pipeline, every IR layer, key types, codegen, flow frontend, testing. Read before non-trivial work.
- **Hacks / shortcuts / ugly corners:** [`docs/TECH_DEBT.md`](docs/TECH_DEBT.md) — read before refactoring LIR or codegen.
- **Refactor plans:** [`plans/`](plans) — `lir-resource-flatten.md`, `flow-frontend.md`.
- **Coding rules as a skill:** [`/compiler-skills`](.agents/skills/compiler-skills/SKILL.md) (compiler patterns), [`/rust-skills`](.agents/skills/rust-skills/SKILL.md) (general Rust).
- **Hunting miscompilation bugs with the fuzzer:** [`/fuzzer-debugging`](.agents/skills/fuzzer-debugging/SKILL.md) — the yel-smith triage loop (measure → categorize → minimize → narrow → locate) and verify-with-round-trips discipline. The [`fuzzer-bug-hunter`](.claude/agents/fuzzer-bug-hunter.md) agent runs this end-to-end.

## The pipeline (one line)

`source → AST (syntax/) → HIR (hir/) → THIR (thir/, typed) → LIR (lir/, block-based) → WASM/WIT/DOT (yel-wasm-codegen)`

Phases are explicit methods on `Compiler` (`yel-core/src/compiler.rs`); the loop
is orchestrated in `yelc/src/pipeline.rs::lower_all`. All global state lives on
one `CompilerContext` (`yel-core/src/context.rs`) threaded through every phase.

## Crate map

| Crate | Role | `CLAUDE.md` |
|-------|------|-------------|
| `crates/yel-core` | Front-end + IRs (parse→HIR→THIR→LIR). No target code. | [yes](crates/yel-core/CLAUDE.md) |
| `crates/yel-wasm-codegen` | Back-end: LIR → WASM component, WIT, DOT. | [yes](crates/yel-wasm-codegen/CLAUDE.md) |
| `crates/yelc` | CLI `yelc` + shared lowering pipeline. | [yes](crates/yelc/CLAUDE.md) |
| `crates/yel-lsp` | Language server (tower-lsp). | — |
| `crates/yel-smith` | Random valid-Yel generator for fuzzing. | [yes](crates/yel-smith/CLAUDE.md) |
| `crates/yel-host` | Wasmtime dev host (`yel:ui/dom` stub). | [yes](crates/yel-host/CLAUDE.md) |
| `crates/yel-flow-*`, `floc` | Experimental visual flow frontend (detached, gitignored). | — |

## Cross-cutting conventions (apply everywhere)

1. **No silent fallbacks.** Never emit placeholder IR/instructions for unimplemented paths — use `todo!("descriptive msg")` or return `Err(CodegenError::…)`. Placeholders produce type-incorrect IR / wrong WASM that's near-impossible to trace. (See `yel-core/CLAUDE.md`, `yel-wasm-codegen/CLAUDE.md`.)
2. **Diagnostics accumulate.** Push to `ctx.diagnostics` and keep going (recover with `Ty::ERROR`); the driver bails between phases via `has_errors()`. Don't early-return on the first user error.
3. **Typed ids + interning.** One `u32` newtype per index space (`ids.rs`), stored in `IndexVec` (`index_vec.rs`); intern strings (`Name`) and types (`Ty`). Never pass raw `usize` indices.
4. **Deterministic output.** Sort + dedup anything derived from a `HashMap`/`HashSet` before it reaches output — snapshot/golden tests depend on byte-stability.
5. **Tests assert correct behaviour.** Never weaken an assertion to match a known bug; mark it `#[ignore]` with a reference instead.
6. **Expect in-progress migrations.** Mixed old/new naming is normal right now (`LirComponent`↔`LirResource`, `legacy_u32()` slot bridge). See `docs/TECH_DEBT.md` §1.

## Build / test quick reference

```bash
cargo build                       # workspace
cargo test                        # all
cargo test -p yelc --test snapshot          # WIT/DOT snapshots
INSTA_UPDATE=always cargo test -p yelc --test snapshot   # accept snapshot changes
cargo run -p yelc -- compile -o wasm path.yel > out.wasm
cargo run -p yelc -- compile -o wit  path.yel
cargo run -p yelc -- ir --pretty path.yel   # dump LIR
wasm-tools validate out.wasm
```

## When you change things

- Pipeline shape, IR fields, or crate boundaries → update `docs/ARCHITECTURE.md` in the same change.
- Fix a documented hack → delete its entry in `docs/TECH_DEBT.md` in the same change.
