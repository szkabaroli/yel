# Stage 6 — `yelc-lower`, HIR → LIR                 status: not started

Replaces (frozen, never edited): `crates/yel-core/src/lower_to_lir/`.
Phase **6**; phase 5 is [`stage-5-lir.md`](stage-5-lir.md). Briefed only
after 5 lands — [why they are separate](stage-5-lir.md#why-stages-5-and-6-are-separate-crates).

Base: — · Started: — · Landed: —

> **Stub.** Written out fully before the phase is briefed.

## Brief

*To be written.* Replaces `lower_to_lir/blocks.rs` — **8,500 lines, one struct,
50+ fields**: output vectors, monotonic counters (`next_slot`, `next_block`,
`next_memory_offset`), `current_ops` + `ops_stack`, for-loop stacks,
deferred-body queues, signal layout, all mutating together.

This file is the single strongest reason the rewrite exists, and it is the
canonical [A2](anti-spec.md#a2--no-god-pass) case. It was not written by someone
aiming for 8,500 lines; it grew one locally-reasonable decision at a time. **An
agent under the same local pressures will make the same decisions** unless the
brief forbids the shape explicitly.

Decomposition target — split by concern, the way `wasm/codegen/` already is:
slots · signals · control flow · deferred bodies. No pass over ~800 lines without
a written justification in this file.

`yelc-lower` is where UI vocabulary legitimately lives; it depends on `yelc-hir`
+ `yelc-sema` + `yelc-lir`. Nothing it knows may leak downward into
[5](stage-5-lir.md) ([C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam)).

## Shortcuts inside not to reproduce

- **Deferred emission** via `pending_block_id_override` — block ids pre-allocated
  and stashed so deferred handler/derived bodies can reference a block before it
  is emitted. Correct, but a subtle ordering dependency that must be modelled
  explicitly rather than re-derived.
- **Three deferred-body mechanisms where there is one concept** —
  `DeferredHandlerBody`, `DeferredDerivedBody`, and the inlined filter predicate.
  `DeferredHandlerBody`'s six env-snapshot fields **are capture analysis**, done
  here because the frontend declined to do it ([F9](findings.md#f9)). See
  [directions §5](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger):
  one body node, one capture analysis, one lowering, trigger as a field. It emits
  the same blocks, so it **changes no output** — but the capture set must cover
  all six fields or it is a narrowing that presents as for-loop handlers breaking
  ([A10](anti-spec.md#a10--an-allow-list-entry-is-characterized-by-evidence-about-the-other-implementation)).
- **`func_name != "filter"`** — the back end recognising a frontend builtin by
  string ([F7](findings.md#f7)). Does not survive under any option; lowering
  emits a generic op and codegen transcribes it.
- `todo!()` cliffs at unsupported for-loop iterables ("no LIR classifier") — keep
  them **loud**; do not soften to a fallback
  ([A5](anti-spec.md#a5--no-silent-fallback)).
- Hard-coded string/signal region sizing.

## Inherited obligations

**From [3's D7](stage-3-hir-build.md#d7--flatten-else-if-chains) — this phase
must recognise the `else if` chain.** A nested `If` whose `else` branch holds
exactly one `If` and nothing else lowers as the **flat N-way selector at one
anchor**. Uniform IR, smart lowering. Without it, every `else if` in the corpus
diverges ([F10](findings.md#f10)).

Consequence accepted in advance: explicit nested `if` then also gets the flat
shape, because after flattening the two are indistinguishable. That is the
enumerated divergence — toward one anchor and one effect subscription instead of
two — and every diverging program gets a line in
[`goldens-changed.md`](goldens-changed.md).

## Directions in play

[§1 — builtins are a table](directions.md#1--builtins-are-a-table-not-a-field-per-builtin),
to accept or reject when this phase is briefed. It pairs with
[4](stage-4-hir-check.md) — the lowering target and the type scheme come from
one row — so if 4 rejects it, 4 inherits the rejection rather than adopting
half of it. Whichever way it goes, it must not create a second path by which UI
names reach `yelc-lir`
([C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam)).

[§5 — handlers and closures are one concept](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger)
is the single lowering this phase owns; the frontend owns the node.

[§8 — the reactive plan](directions.md#8--the-reactive-plan-is-an-artifact-and-its-shape-is-open).
This phase **consumes** the plan and **owns the granularity decision**: how the
plan's reactive units are packaged into emitted functions — one per site (frozen
shape), one `update(mask)` per component, or inlined at the write site.

Two things that narrow it:

- **Dispatch is not in question.** [F16](findings.md#f16): a signal write already
  emits **direct `CallBlock`s** resolved at compile time — no runtime registry, no
  dirty mask. That is the right mechanism for a closed-world AOT compiler and it
  carries over. Only packaging is open.
- **Components are small** — measured across 83 fixture components/globals: max
  14 reactive properties, median 2, p90 4. There is little for a mask to
  amortise over, which argues the frozen per-site granularity is closer to right
  than it first appears.

Whatever is chosen **changes output** and lands as its own enumerated divergence
set — the 85 execution tests pin DOM-op behaviour, so the comparison is module
bytes and update cost, not correctness.

## Not debt

`resolve_global_triggers` is a **legitimate whole-module pass** — it must run
after every component is lowered because a global's fan-out targets do not exist
until then. It is a link step. It lives inside the shared module spine, and the
new design keeps that property.

## Contract · Reference · Definition of done · Numbers · Decision log · Surprises

*To be written.*
