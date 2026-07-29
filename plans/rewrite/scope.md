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


## The surface freeze has planned breaks

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


### 2026-07-29 — `<T>`, explicit type parameters

**Decided.** Functions may declare type parameters
([`LANGUAGE.md` § Type Parameters](../../LANGUAGE.md#type-parameters)).
Inference at the call site, no constraints, no generic user-defined types.

**Why now.** [§2](directions.md#2--the-stdlib-is-yel-source-embedded-in-the-binary)
wants the stdlib written in `.yel`, and its valuable half is generic — every
`list`, `option` and `result` operation. `filter` is
`(list<T>, func(T) -> bool) -> list<T>`, and there was **no way to write that
signature**: `function_decl` and `func_type` carry no parameter list, and the `T`
in `list<T>` was prose in a documentation table. Without this the stdlib move is
limited to `min`, `max`, `starts-with` and the `*-to-string` family — the rows
whose bodies are one intrinsic call each, which is the half worth the least.

**Why declared rather than inferred.** Grain gets a generic stdlib with no
`<T>` syntax at all, because ML-family let-generalization turns a `let` binding
into a polymorphic scheme automatically. That path was available and was not
taken, because it reopens
[A2](open-decisions.md#a2--how-much-inference-sits-inside-the-bidirectional-checker) —
decided the same day as option 2, unification **without** generalization.

A2's evidence was sound and is worth re-reading in this light: it established
that yel has no polymorphic bindings *as the language stands*. §2 changes how the
language stands. So the choice was which of the two to move, and declared
parameters move the smaller thing — new grammar, no change to the checking
algorithm — where generalization would move the algorithm and leave `E0002`'s
behaviour to be re-derived.

It also composes with [A1](open-decisions.md#a1--how-are-parameterized-types-represented):
monomorphization is Rust's representation strategy and pairs with declared
parameters directly. (Grain's path would also have composed — MLton monomorphizes
whole-program HM — but via a longer argument.)

**What it retires.** [S7](stage-3-hir-build.md#s7--does-ty-gain-a-non-concrete-variant)
justified `TyKind::Param` by "a generic body is checked once, generically, so
errors land in the stdlib rather than at the user's call site". That needs
template *bodies*, which needed §2, which needed this. `Param` was machinery
ahead of its feature; this is the feature.

**Deliberately excluded, and each is a separate decision if wanted later:**
constraints/bounds · generic user-defined types · explicit type arguments at a
call site (turbofish).

### The freeze now carries three breaks — read them together

`match`, `primitive` ([§2](directions.md#2--the-stdlib-is-yel-source-embedded-in-the-binary),
still unspent with two options), and `<T>`. Three breaks decided one at a time,
each when it became urgent, is how a freeze stops meaning anything.

They are not independent: `<T>` gates the stdlib, the stdlib motivates
`primitive`, and all three are additive — everything that parsed before still
parses. What they share is a cost the differential cannot absorb: **the frozen
compiler parses none of them**, so every program using them is outside the
oracle. `yel-smith` must learn each construct *before* it lands, or it is tested
only by the cases someone thought of
([A13](anti-spec.md#a13--the-generator-ships-not-its-instances)).

None of the three moves now. Stage 1 is closed and in the ratchet; they land as
one scoped reopening after stage 4, with one ratchet row.
