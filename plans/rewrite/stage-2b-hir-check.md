# Stage 2b — `yelc-hir`, check                      status: brief written

Replaces (frozen, never edited): `crates/yel-core/src/thir/`.
Phase **2b** of the merged HIR stage; phase 2a is
[`stage-2a-hir-build.md`](stage-2a-hir-build.md). Same crate, run after 2a.

Base: — · Started: — · Landed: —

> **Gate.** Not briefed until 2a lands. 2b is a separate pass over 2a's output,
> so a divergence is bisected by dumping after 2a.

## What this phase is

Bidirectional type checking over 2a's nodes, filling `types: NodeMap<Ty>` from
empty to total. **No new IR** — the merge decision
([`seam-changes.md`](seam-changes.md), 2026-07-28) means there is one node
vocabulary and one walker, both owned by
[2a](stage-2a-hir-build.md#the-shape-shared-with-2b).

Already true when this phase starts, and not to be redone: **declared types are
already `Ty`** in the definition tables (2a phase 2 —
[F5](findings.md#f5)). This phase types **expressions**, nothing else.

## Brief

- **Identical diagnostic *meaning* on the 23 diagnostic fixtures.** Same
  rejection, same reason, same construct. Wording may improve with the diff read
  and recorded in [`goldens-changed.md`](goldens-changed.md) — never re-blessed.
- **Accumulate and continue.** Recover with `Ty::ERROR` and keep checking; the
  driver bails between phases on `has_errors()` —
  [keep-list §6](keep-list.md#6--accumulate-and-continue-error-policy).
- **Carry the frozen visitor split forward.** `thir/visit.rs` (`ThirVisitor` +
  `walk_expr`/`walk_stmt`, exhaustive, with a `visit_closure` descent hook) is
  **the model**, not debt — the one place the duplicated-walker problem was
  actually solved. In the merged design it becomes *the* walker, shared with 2a,
  not a second one ([A3](anti-spec.md#a3--no-duplicated-walkers)).
- **Split `typeck.rs`** (2.8k in the frozen tree) —
  [A2](anti-spec.md#a2--no-god-pass).
- **Type-directed lowerings land here**, not in 2a: string interpolation →
  `concat` needs each part's type; `MethodCall` resolution (`len`, `filter`)
  likewise. See [2a § What lowerings belong here](stage-2a-hir-build.md#what-lowerings-belong-here)
  for the full split.

## Decisions this phase must make

Prefix `T` so they do not collide with 2a's `D` or `yelc-sema`'s `S`.

| # | decision | recommendation |
|---|---|---|
| T1 | Bidirectional (`Infer`/`Check`), or unification with inference variables? | **Bidirectional** — [below](#t1--bidirectional-checking-not-unification) |

### T1 · Bidirectional checking, not unification

**This was inherited, not decided.** "Bidirectional" is asserted across six plan
documents because the frozen tree does it (`Mode::{Infer, Check(Ty)}`,
`typeck.rs`). That is evidence about the old compiler, not an argument about the
new one — the same mistake [S7](infra-sema.md#s7--does-ty-gain-a-non-concrete-variant)
was written to correct. The argument, made properly:

**Keep bidirectional.**

1. **The surface is heavily annotated, so inference power buys little.** Every
   property declares its type (`count: s32 = 0`), every function parameter and
   global is declared. There is very little a solver would recover that an
   annotation has not already stated.
2. **[§3](directions.md#3--generics-are-monomorphization-by-name) needs the
   `Check` direction.** Type-directed instantiation at a call site *is* an
   expected type pushed downward. Monomorphization by name composes with
   bidirectional checking; with a constraint solver it becomes a second
   mechanism.
3. **[§5](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger)
   option B requires it outright** — the trigger rides on the slot's function
   type and is delivered by `Check`. Choosing unification would foreclose that
   option before it is decided.
4. **Diagnostic meaning is frozen on 23 fixtures.** Bidirectional produces
   "expected `X`, found `Y`" *at the construct*. A solver reports "cannot unify"
   wherever the constraint happened to fail, which is a different span and a
   different sentence. Matching the frozen fixtures is materially harder, and
   diagnostic meaning is not free to change.
5. **There are no polymorphic functions** (§3), so the thing unification is
   actually for does not arise.

**What would push the other way**, so the decision is revisitable: fixing
**function-type inference** (stubbed below) in its general form — `{ x -> x + 1 }`
with no expected type anywhere. But
[§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one)
establishes that closures only occur where an expected type *exists* (a `filter`
argument, a `func()`-typed slot), so the gap is **propagation, not inference
power**. Widening propagation is cheap; adding a solver is not.

**This answers half of [S7](infra-sema.md#s7--does-ty-gain-a-non-concrete-variant).**
Bidirectional checking needs no `Infer(var)` variant in `Ty`, because
`Mode::Infer` means *synthesize now*, not *unknown, to be solved later*. T1 and
S7 are the same decision seen from two crates and must be answered together.

## Gaps inherited as decisions, not copies

Each needs a written call before briefing. Implementing any of them **changes
output** and lands as its own enumerated divergence set with fixtures — never as
a side effect.

| gap | frozen behaviour | question |
|---|---|---|
| Closure capture | `captures` always `vec![]`; no LIR counterpart; capturing a local **panics** ([F6](findings.md#f6)) | model the value form regardless ([§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one)); implementing is a separate scope call — no corpus program, no output to match |
| Function-type inference | stubbed | same |
| Generics | none ([F1](findings.md#f1)) | adopt [§3](directions.md#3--generics-are-monomorphization-by-name), or keep the `Ty::ERROR` placeholder? |
| `match` | does not exist; conditionals special-cased | model the general form now so lowering has one path — [B4](anti-spec.md#b4--no-special-cased-control-flow-where-a-general-form-exists) |
| `color`/`brush` as property types | rejected — two storage shapes for one name | unify, or keep rejecting *with the same diagnostic*? — [C4](anti-spec.md#c4--no-type-whose-storage-shape-depends-on-where-it-appears) |

## Directions in play

- [§1](directions.md#1--builtins-are-a-table-not-a-field-per-builtin) — typeck
  asks a table for the type scheme instead of holding 24 `ctx.known.*` field
  references ([F12](findings.md#f12)).
- [§3](directions.md#3--generics-are-monomorphization-by-name) — **decided
  here.** Adds no `Ty` variant, no unification, nothing below the frontend seam.
  Adopting it moves `len`/`some`/`none`/`list.get`/`append` out of "blocked" for
  §2; `filter` stays blocked on §4.
- [§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one) —
  model the value form; do **not** implement capture. The design obligation is
  unconditional, the implementation is a separate scope call.
- [§5](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger) —
  **changes no output.** The node merge is 2a's shape question; the sub-decision
  here is *how the trigger is determined*. §5 records options A (a keyword — a
  frozen-surface change) and B (the trigger on the slot's function type, carried
  by the `Check` direction). Picking by default is how a language change gets
  made without anyone deciding to make one. **Either option owes the same test:**
  the dependency set of a body of each trigger kind, asserted on a fixture, in an
  execution test.
- [§6](directions.md#6--modules-are-serializable-artifacts) — the artifact this
  phase ends at.

## Contract

2b **adds nothing to the seam** except making `types` total. Everything else —
`HirId`, `HirMap`, `HirModule`, `NodeMap`, `TypeId`, `type_of`,
[H1–H5](stage-2a-hir-build.md#invariants-this-phase-establishes) — is owned and
established by [2a](stage-2a-hir-build.md#contract) and assumed here.

**Postcondition:** `types` has an entry for every expression node. A missing
entry after 2b is a bug, not a "not inferred" state — if a type could not be
determined, the entry is `Ty::ERROR` and a diagnostic was emitted
([A5](anti-spec.md#a5--no-silent-fallback)).

### What stage 3a may NOT assume

- **No `HirId` stability** across reparses, same as `NodeId`.
- **No classification of element props** into bindings vs handlers, if
  [D1](stage-2a-hir-build.md#d1) lands as recommended.
- **No capture sets** unless [§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one)
  is adopted into this brief.

## Verification

This is where the stage gets a real artifact. After 2b the module is
**serializable and typed** ([§6](directions.md#6--modules-are-serializable-artifacts)),
so it is **byte-comparable** — recovering the artifact-level differential that
neither the old stage-2 boundary nor 2a alone could provide
([F14](findings.md#f14)).

Plus, unchanged from 2a and re-run here:

1. **Diagnostics** — meaning, span, and order — over the 2000-seed corpus, 91
   positive and 23 diagnostic fixtures, via frozen `yelc check`. This is the
   phase where most of the 23 diagnostic fixtures actually fire.
2. **No panic** over the corpus.

## Definition of done

- [ ] `types` total after 2b; asserted by a walk over every corpus program that
      every expression node has an entry.
- [ ] Diagnostic *meaning* identical on all 23 diagnostic fixtures; any wording
      diff read and recorded in [`goldens-changed.md`](goldens-changed.md).
- [ ] Accumulate-and-continue verified: a program with three independent type
      errors reports three, not one.
- [ ] One walker shared with 2a — no second visitor
      ([A3](anti-spec.md#a3--no-duplicated-walkers)).
- [ ] No pass over ~800 lines without written justification
      ([A2](anti-spec.md#a2--no-god-pass)).
- [ ] The serialized typed module is byte-stable across runs
      (determinism, [keep-list](keep-list.md)).
- [ ] Each gap in the table above answered in writing.
- [ ] §3 accepted or rejected, with reasoning.
- [ ] §5's trigger option chosen — **and its dependency-set test written**
      whichever way it went.
- [ ] Adversarial review panel, read-only, one lens each.
- [ ] Surprises written — [D3](anti-spec.md#d3--a-stage-documents-what-surprised-it).

## Reference

- **Frozen** `yel-core/src/thir/` — `typeck.rs` (2.8k), `visit.rs` (the model to
  carry forward), `node.rs`, `expr.rs`.
- **2a's contract** — [`stage-2a-hir-build.md`](stage-2a-hir-build.md#contract).

## Numbers · Decision log · Surprises

*To be written at close-out.*
