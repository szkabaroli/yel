# Keep-list — what carries over intact

> Rule: [`keep-diagnostics-infrastructure`](../../.agents/skills/compiler-rewrite/rules/keep-diagnostics-infrastructure.md)
>
> Ships in every agent brief, next to the [anti-spec](anti-spec.md). The
> anti-spec says what may not be reproduced; this says what may not be replaced.

"The internals are garbage" is not uniformly true. The items below are better
than what a rewriting agent would produce, and they are the parts with the least
defence — nobody has written down why they are good, so they get replaced by
whatever the reference implementation uses.

**Two consequences worth stating in every brief:**

1. **A reference implementation is a source of structure, not of policy.** Where
   [ark](https://github.com/szkabaroli/ark) and yel disagree about error
   handling, interning, or id discipline, **yel wins**. Ark is consulted for how
   to build a parser, not for how to report errors.
2. **Adding to this list is cheap; discovering it late is not.** When review
   catches a good thing an agent nearly deleted, it is added here before the next
   stage is briefed.

---

## 1 · Diagnostics — `yel-core/src/diagnostic.rs` (285 lines)

**Carried over unchanged. Your stage REPORTS through this.**

| Item | Why it stays |
|---|---|
| `Severity`, `ErrorCode` | A real error-code enum with stable `E00xx` rendering (`code()`). Documentable, testable, greppable. 20 codes are reachable and fixture-covered. |
| `Diagnostic` builder — `error(msg)` / `warning(msg)` `.with_span()` `.with_code()` `.with_note()` | Composable, and `with_note` carries the secondary explanation a flat enum cannot express. |
| `Diagnostics` accumulating sink — `push`, `error(span, code, msg)`, `has_errors`, `error_count`, `iter` | One channel every phase reports on. |
| `render(&SourceMap)` | Single error format for CLI, LSP, and tests. |

**Allowed:** adding a new `ErrorCode` variant, improving message *wording*
(recorded in `goldens-changed.md`). **Not allowed:** changing the shape of the
API, introducing a per-stage `ParseError`-style enum, or returning errors out of
band. Either is a seam-change request, not an agent decision.

The specific failure this prevents: an agent porting ark's frontend adopts ark's
flat `ParseError` + `fn message(&self) -> String` + `Vec<ParseErrorWithLocation>`
return channel. That loses error codes, structured notes, the shared sink, and
`SourceMap`-aware rendering in one commit whose title says "parser" — and the
resulting fixture failures look like wording drift, so the "fix" is a re-bless.

**Two open cleanups the rewrite should land (from `TECH_DEBT.md` §3), not
carry:** the two unreachable codes (`UnknownUnitSuffix` E0004, `MissingElement`
E0042) go away or gain fixtures — see anti-spec B5 — and the two coexisting
emission idioms collapse to one, with a note-capable convenience form — B6.

## 2 · Source & spans — `yel-core/src/source.rs` (201 lines)

`SourceMap`, `Source`, `SourceId`, `Span` (`new` / `point` / `merge`),
`line_col`, `snippet`. Byte offsets, file ids, snippet rendering with context.

Every IR node's span must map into this. Ark's `Span { file_id, start, len }` is
the same idea with a different arithmetic convention — **use yel's**
(`start`/`end`); the divergence is not worth the churn across five stages.

## 3 · Interning — `yel-core/src/interner.rs` (165 lines)

`Interner` / `Name(u32)` / `ArcStr`, plus the type interner (`Ty`) under
`types/`. **No `String` survives past lowering.**

Ark keeps `name_as_string` on the ident; yel interns. Yel wins — this is the
explicit divergence from the reference, and it is in the brief so the agent does
not "fix" it back.

## 4 · Typed ids + `IndexVec` — `ids.rs` (294) + `index_vec.rs` (163)

One `u32` newtype per index space — `DefId`, `NodeId`, `ExprId`, `BlockId`,
`LocalId`, `FieldIdx`, `VariantIdx`, `ParamIdx`, `InterfaceId`, … — stored in
`IndexVec<I, T>` with `push -> I` and `iter_enumerated`.

**Never pass a raw `usize` index.** New index spaces get new newtypes; they do
not reuse an existing one because the integer happens to fit. (UI-specific ids
such as `TreeBoundaryId` are *not* on the keep-list — they are frontend
vocabulary and must not appear below the seam; see anti-spec C1.)

## 5 · Context threading — `yel-core/src/context.rs`

**One `CompilerContext`, threaded `&`/`&mut` through every phase.** All global
state lives on it: interners, `SourceMap`, `Diagnostics`, `Definitions`, and
analysis side tables. Phases are explicit methods on `Compiler`
(`yel-core/src/compiler.rs`); the loop is orchestrated in one place.

`CompilerContext::signal_deps` — an analysis side table keyed by `DefId`, shared
by components and globals alike — is **explicitly correct modeling, not debt**
(`TECH_DEBT.md` §1.6), and is the positive precedent for anti-spec B3.

Note the file is 963 lines and growing; splitting it is fine, but the *threading
model* — one context, no globals, no thread-locals, no ambient state — is frozen.

## 6 · Accumulate-and-continue error policy

Push to `ctx.diagnostics` and **keep going**. Recover with `Ty::ERROR` (or the
stage's equivalent recovery node) and lower the rest of the program. The driver
bails between phases via `has_errors()`.

**Never early-return on the first user error.** The parser's version of this is
ark's `parse_list` recovery discipline (recovery sets, `Error` nodes, and the
`assert!(token_idx > pos_before)` no-progress guard) — port the mechanism, route
the errors into `ctx.diagnostics`.

## 7 · No-silent-fallbacks invariant

`todo!("descriptive msg")` or `Err(CodegenError::…)` — never placeholder IR or
instructions. This is a crate-level invariant in both
`yel-core/CLAUDE.md` and `yel-wasm-codegen/CLAUDE.md`, and it is what has kept
the debt loud enough to inventory. Restated as anti-spec A5 because it is the
first thing softened under pressure.

## 8 · Determinism infrastructure

- `rustc_hash::FxHashMap` / `FxHashSet` throughout; std `HashMap`/`HashSet`
  denied by the root `clippy.toml` (`disallowed-types`) and wired via
  `[lints.clippy]` in the compiler crates.
- Sort + dedup anything derived from a map before it reaches output.

**The new crates inherit the lint.** This is load-bearing, not hygiene: it is
the fix for ~35/200 seeds that used to emit byte-different modules run-to-run.
See anti-spec A6.

## 9 · The test corpus itself

- 91 positive fixtures (WIT + DOT byte-for-byte, WASM validates)
- 23 diagnostic fixtures
- `known_bugs` fixtures — programs that *should* work and don't; the rewrite's
  free wins
- **85 execution tests** under Wasmtime — the only semantic oracle. Unmodified,
  by any stage, for any reason.
- insta snapshots at the CLI level (`yelc/tests/snapshot.rs`)
- `yel-smith` — the generator that makes the differential possible at all

`yel-smith` is kept and **kept honest**: where it generates a construct typeck
accepts but codegen rejects, that stays. It is a real front/back mismatch, and
removing the generator case hides it rather than fixing it.

## 10 · The `WitBoundary` witness (stage 7)

The zero-sized witness that forces `canonical_flat_valtypes(ty, WitBoundary::…)`
and `flatten_core_valtypes(ty, WitBoundary::…)` to name themselves as boundary
code. Not hermetic — the witness is crate-constructible — but it converts a
silent convention into an explicit, greppable, reviewable act, and it is what
finished the typed-GC migration. Carry the mechanism; see anti-spec C2.

## 11 · Documentation discipline — the habit, not the documents

The rule that a change updates its documentation **in the same commit**, and the
habit of keeping an honest debt inventory. `docs/TECH_DEBT.md` being accurate is
the reason this rewrite is scopeable at all — anti-spec D2.

To be explicit about what is *not* on this list: `docs/ARCHITECTURE.md` and
`docs/PIPELINE.md` describe the **frozen** compiler. They are useful as a
description of what the old code does — read them that way, the same way you read
the old source — but they are **not binding on the new design**. The rewrite's
own architecture is defined by this directory: the [scope table](scope.md), the
[anti-spec](anti-spec.md), the crate graph in [README](README.md), and each
stage's seam contract. Where the old docs and this directory disagree, this
directory wins, and the old docs get rewritten at cutover rather than honoured.
