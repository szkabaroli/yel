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

| # | decision | status |
|---|---|---|
| T1 | How much inference sits inside the bidirectional checker? | ✅ **bidirectional skeleton + unification variables**, no generalization (A2, 2026-07-29) — [below](#t1--bidirectional-checking-not-unification) |

### T1 · Bidirectional checking, not unification

**Decided 2026-07-29: bidirectional skeleton, *with* unification variables, no
let-generalization.** Option 2 of three
([A2](open-decisions.md#a2--how-much-inference-sits-inside-the-bidirectional-checker)).

**The question this replaced was badly posed.** It asked "bidirectional *or*
unification", which is a false dichotomy: bidirectional says *where* type
information flows (⇒ / ⇐), unification says *how* unknowns get resolved. Rust,
Swift, Scala and TypeScript do both. What is genuinely in tension with a
bidirectional skeleton is **let-generalization**, not unification — and GHC has
spent years restricting exactly that. Generalization is declined here because
[§3](directions.md#3--generics-are-monomorphization-by-name) leaves yel with no
polymorphic functions, so it would have nothing to generalize.

**What carries over from the earlier argument** (all still true, none of it
argued against a solver): the surface is heavily annotated · §3 needs the `Check`
direction for call-site instantiation · §5 option B is *delivered* by `Check`.

**What is now an obligation rather than an avoided cost.** The earlier draft's
fourth argument was diagnostics, and adopting a solver takes that cost on
deliberately:

> Bidirectional-only yields "expected `X`, found `Y`" **at the construct**. A
> solver reports a conflict wherever unification happened to fail — a different
> span and a different sentence.

Diagnostic *meaning* is frozen on the 23 diagnostic fixtures. So the solver must
be built to **report at the construct, not at the point of failure**: keep the
expected type on the obligation, and when unification fails, blame the site that
introduced the expectation. This is a design requirement on the checker, not a
wording exercise — and it is the single most likely way this phase fails its
diagnostic gate.

**`Infer` must not survive this phase.** See
[S7](infra-sema.md#s7--does-ty-gain-a-non-concrete-variant) for the full
obligation set; the part this phase owns is the postcondition below.

## Gaps inherited as decisions, not copies

Each needs a written call before briefing. Implementing any of them **changes
output** and lands as its own enumerated divergence set with fixtures — never as
a side effect.

| gap | frozen behaviour | question |
|---|---|---|
| Closure capture | `captures` always `vec![]`; no LIR counterpart; capturing a local **panics** ([F6](findings.md#f6)) | model the value form regardless ([§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one)); implementing is a separate scope call — no corpus program, no output to match |
| Function-type inference | stubbed | same |
| Generics | none ([F1](findings.md#f1)) | ✅ **adopted** — [§3](directions.md#3--generics-are-monomorphization-by-name), monomorphization by type, with a `Param` variant so bodies check once generically (A1 + A3, 2026-07-29) |
| `match` | does not exist; conditionals special-cased | model the general form now so lowering has one path — [B4](anti-spec.md#b4--no-special-cased-control-flow-where-a-general-form-exists) |
| `color`/`brush` as property types | rejected — two storage shapes for one name | unify, or keep rejecting *with the same diagnostic*? — [C4](anti-spec.md#c4--no-type-whose-storage-shape-depends-on-where-it-appears) |
| **Coercions are not materialized** | `types_compatible` returns `bool` and discards *which* conversion applies; no `Coerce` node exists. `list<s32>` → `list<s64>` typechecks and the encoder rejects it ([F17](findings.md#f17)) | materialize an explicit conversion node — see below |

### Materialize coercions — the rustc THIR lesson

rustc makes **adjustments explicit** at THIR (auto-deref, auto-ref, unsizing,
overloaded operators) precisely so MIR building never re-derives them. Yel does
the opposite: [F17](findings.md#f17) — typeck decides a coercion is legal and
records nothing, so every consumer re-derives it from the use site, and the
conversions are *not* uniform (`s32→s64` sign-extends, `u32→u64` zero-extends,
`Color→Brush` changes representation).

**The strongest argument is not tidiness, it is that the bug becomes
unconstructible.** With an explicit node, typeck must *build* the conversion —
and there is no conversion from `list<s32>` to `list<s64>` short of an
element-wise map, so failing to build it **is** the rejection, at the right span,
with a message. Today the same program reaches the encoder and dies there.

This phase owns it: coercion is type-directed, so it cannot live in 3a.

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
- [§8](directions.md#8--the-reactive-plan-is-an-artifact-and-its-shape-is-open) —
  **this phase emits the reactive plan.** Dependency sets already exist
  (`thir/signalck.rs` is the model); §5 adds the trigger; §4 adds the capture set.
  The plan is those three per body, as a **declared output in the seam** — not a
  `CompilerContext` side table, which would be
  [A1](anti-spec.md#a1--no-side-channel-ir) side-channel IR.

  **It carries reactive *units*, not function identities.** A unit is "this body
  runs when these signals change". How units are packaged into functions is
  [4b's](stage-3b-lower.md) choice, and naming functions here would silently make
  it. Ids are frontend ids — `BlockId` is a `yelc-lir` type this crate cannot
  reach.

## Contract

2b **adds nothing to the seam** except making `types` total. Everything else —
`HirId`, `HirMap`, `HirModule`, `NodeMap`, `TypeId`, `type_of`,
[H1–H5](stage-2a-hir-build.md#invariants-this-phase-establishes) — is owned and
established by [2a](stage-2a-hir-build.md#contract) and assumed here.

**Postcondition, strengthened by [T1](#t1--bidirectional-checking-not-unification):**
`types` has an entry for every expression node, **and no entry contains an
unresolved `Infer` variable**. A missing entry is a bug, not a "not inferred"
state; an *unresolved* entry is a worse bug, because it type-checks and then
miscompiles downstream. If a type could not be determined, the entry is
`Ty::ERROR` and a diagnostic was emitted
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

**`yelc2 --emit-hir` gains the type map.** The same dump 2a lands
([why it is yel-flavoured and not round-trippable](stage-2a-hir-build.md#yelc2---emit-hir--the-dump-is-a-deliverable-not-a-convenience))
now annotates every expression with its `Ty`, plus the trigger kind and capture
set this phase settles.

That makes this phase's central postcondition **directly visible**: `types` is
total, so a rendered expression with no type is a bug the dump shows rather than
a bug a later stage trips over. It is the cheapest possible check on the DoD line
below, and it doubles as the artifact 2a could not have
([F14](findings.md#f14)).

Plus, unchanged from 2a and re-run here:

1. **Diagnostics** — meaning, span, and order — over the 2000-seed corpus, 91
   positive and 23 diagnostic fixtures, via frozen `yelc check`. This is the
   phase where most of the 23 diagnostic fixtures actually fire.
2. **No panic** over the corpus.

## Definition of done

- [ ] `types` total after 2b; asserted by a walk over every corpus program that
      every expression node has an entry — and **visible in `--emit-hir`**, which
      renders an untyped expression as such rather than omitting it.
- [ ] **No `Infer` variable survives the phase** — a `has_infer()`-style assert
      over the whole map, not a spot check ([T1](#t1--bidirectional-checking-not-unification),
      [S7](infra-sema.md#s7--does-ty-gain-a-non-concrete-variant)).
- [ ] **Unification failures report at the construct that introduced the
      expectation**, not where the solver noticed. Verified against the 23
      diagnostic fixtures.
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
