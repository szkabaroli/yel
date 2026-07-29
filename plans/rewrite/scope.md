# Scope — frozen vs. free

> Rule: [`scope-frozen-vs-free`](../../.agents/skills/compiler-rewrite/rules/scope-frozen-vs-free.md)
>
> **This table is the first thing in every agent brief.** An agent that wants to
> move an item from frozen to free *asks*; it does not decide. The decision is
> logged in [`seam-changes.md`](seam-changes.md).

"Rewrite the internals, keep the language and the stages" is clear to someone
holding the project in their head and ambiguous to an agent at exactly the
boundaries that matter. Every ambiguity resolves toward scope growth, because
redesigning is more interesting than transcribing — and a rewrite that also
changes the language **cannot be differentially tested**, because the corpus
stops compiling. The moment that happens, every other rule loses its teeth
simultaneously.

## The table

| Frozen — changing it is a separate, approved decision | Free — expected to change |
|---|---|
| Surface syntax as specified in [`LANGUAGE.md`](../../LANGUAGE.md) — **one planned exception, see below** | Parser implementation, grammar technology (pest → hand-written lexer + recursive descent), AST node shapes |
| Stage names and their order: **AST → HIR → LIR → WASM** — frozen *because differential attribution depends on it*, not because the frozen compiler drew it there. Moving a boundary is an integrator decision logged in [`seam-changes.md`](seam-changes.md) ([2026-07-28](seam-changes.md#log)), and must say which stages lose independent attribution and what replaces the differential for them. THIR was merged into HIR under that rule — one IR, phases 3/4. | Every type, pass, and helper *inside* a stage |
| Exported WIT world and the `yel:ui/dom@0.1.0` host contract | How WIT is constructed and emitted (`wit_ast.rs` internals) |
| Observable DOM-op behaviour asserted by the **85 execution tests** | The lowering and codegen that produce it |
| Diagnostic *meaning* for the 23 diagnostic fixtures, and the `diagnostic.rs` API | Which stage reports what; new `ErrorCode` variants; message wording (with a recorded diff) |
| CLI surface: `yelc compile -o {wasm,wit,dot}`, `ast`, `ir`, `check` | Driver internals, `pipeline.rs` structure, the new `yelc2` stage-selection seam |
| Determinism of all output (byte-stable across runs) | Which data structures produce it (Fx maps, sorted derivations) |
| The keep-list items ([`keep-list.md`](keep-list.md)): diagnostics, `SourceMap`/`Span`, interning, typed ids + `IndexVec`, context threading, accumulate-and-continue, no-silent-fallbacks, the determinism lint | Everything else in `yel-core` and `yel-wasm-codegen` |
| The frozen tree itself — `crates/yel-core`, `crates/yel-wasm-codegen`, `crates/yelc` are **never edited** | New crates beside them |
| The 2000-seed corpus and its provenance ([`corpus.md`](corpus.md)) | — (regenerated from the old compiler only, never from the new one) |

**Anything not in this table defaults to frozen.**

## Clarifications that come up every time

### Diagnostic wording may improve; diagnostic *meaning* may not.

A fixture asserting `cannot infer type` must still reject that program, for that
reason, at a span covering the same construct. Improved wording is a golden
update with the diff **read and justified** in
[`goldens-changed.md`](goldens-changed.md). The diagnostic *infrastructure* is
frozen outright — see [`keep-list.md`](keep-list.md) §1.

### The stage boundary is frozen; the stage's data model is not.

"Keep LIR" means a block-based low IR exists — its arena traits, op set, and
resource model are all free, and stage 3 is expected to change them
substantially.

**THIR is no longer a stage.** It merged into HIR on 2026-07-28
([`seam-changes.md`](seam-changes.md)): one node vocabulary, `types: NodeMap<Ty>`
empty after phase 3 and total after 4. What that decision preserved is the
*obligation* — a typed form still exists before LIR, with a documented contract —
not the second IR that used to carry it.

### Parser technology is free, and expected to change.

Dropping pest for a hand-written lexer + recursive-descent parser over a lossless
green tree is **the plan**, not a scope violation — see
[`frontend-follow-ark-reference`](../../.agents/skills/compiler-rewrite/rules/frontend-follow-ark-reference.md).
What stays frozen is the *grammar it accepts*: every one of the 91 positive
fixtures, every corpus program, and every `LANGUAGE.md` construct parses, and
nothing new parses that did not before. "It simplifies the grammar" is never a
reason to require a trailing comma, reserve a new keyword, or tighten a
whitespace rule.

### The LIR's UI vocabulary is free — in one direction only.

Removing `tree_shape`, `boundary`/`mount`, `$Comp` self-ref, and `yel:ui/dom`
concepts from below the frontend seam is *mandated*, not merely permitted —
anti-spec C1. What is frozen is the **observable result**: the same DOM ops in
the same order, byte-identical WIT.

### The WIT world is frozen at its current *content*, not its current *code*.

`wit_ast.rs` may be rewritten freely. The emitted `.wit` text for the 91 positive
fixtures must be byte-identical, and the `yel:ui/dom@0.1.0` import surface must
match instruction-for-instruction, because the host on the other side is not part
of this rewrite. Note the hard-coded `0.1.0` default for sources that omit a
package version (`TECH_DEBT.md` §4) is **frozen behaviour** for now — fixing it
changes output and is a separate approved decision.

### Known bugs are free to fix, and that must be recorded.

The `known_bugs` fixtures and any latent bug the rewrite happens to fix are wins,
not divergences — but an unexplained corpus divergence is a **failure** even when
the new output looks better. Every divergence is enumerated with a reason. "The
new one is correct" is a reason; it still gets written down.

## Out of scope entirely (not frozen — simply not this project)

- `crates/yel-lsp` — consumes the frontend; it is a *beneficiary* of the green
  tree, not a stage. It is rewired after stage 1 lands, tracked separately.
- `crates/yel-host` — the Wasmtime dev host. Frozen because the execution tests
  run against it; not otherwise touched.
- `crates/yel-flow-*`, `floc` — the experimental flow frontend (gitignored). It
  is the *reason* stage 3 must be frontend-agnostic, but it is not rewritten
  here.
- `yel-viewer`, `yel-flow-editor` — not compiler code.
- Language features that do not exist yet (`match`, closures/capture analysis,
  `color`/`brush` as property types). The rewrite must not *foreclose* them —
  anti-spec B4, C4 — but it does not implement them.


## The surface freeze has one planned break: `match`

**Recorded 2026-07-29.** `match` is being added to the language
([`LANGUAGE.md` § Match](../../LANGUAGE.md#match),
[directions §9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it)).
That is a **surface language change**, which this table otherwise forbids, so it
is written down rather than left as an inconsistency for a reviewer to find.

Why it is allowed: variants are currently **write-only** — `LANGUAGE.md`
documents how to construct `some(v)` and `none` and no way to take either apart.
That is a hole in the language, not a wart in the implementation, and the three
unrelated conditional constructs it sits beside ([F18](findings.md#f18)) are
about to be lowered into a general form. Deciding that general form *after*
stages 5–7 are built on the current arrangement is the expensive order.

What the exception does **not** license:

- **It is not a precedent.** Surface changes remain out of scope; this one is
  named, dated and bounded to `match` and its patterns.
- **It does not move now.** Stage 1 is closed and in the ratchet. The design lands
  now because stages 3/4 need it; the grammar lands as a scoped stage-1 reopening
  after stage 4 closes, or at cutover, with its own ratchet row.
- **It does not get a differential.** The frozen compiler will never parse
  `match`, so it has no oracle — see directions §9. `yel-smith` must learn to
  generate it *before* it lands.
