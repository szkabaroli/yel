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

## Where many packages become one wasm

**This phase is the join point.** Above it everything is per-package; below it
nothing is. A compilation reads a local package plus every package it imports —
`yel:std` at minimum — and emits **one** wasm module, and this is the only step
at which that reduction can happen.

Why here and nowhere else, from the types rather than from taste:

- Everything above LIR is **named**. `DefId` carries a `PackageId`,
  `Definitions` discriminates by it, and each package is a WIT package at the
  boundary. `HirModule.id` *is* a `PackageId`, and `HirItemId`/`BodyId` are
  `IndexVec` indices **local to one module** — so a cross-package reference
  structurally cannot be a `HirItemId`; it must be a `DefId{package, index}`.
  Separate HIR per package is not a preference, the id spaces already assume it.
- LIR is flat and frontend-agnostic — functions, blocks, ops. So is a wasm
  module. Neither has a package concept and nothing below could reconstruct one.

So the package dimension must be **gone** by the time LIR exists, which puts the
seam at this phase exactly.

### Lower from roots — not merge, then lower

Start at the local package's exports (the world's exports, plus
`cabi_realloc`), walk reachable `DefId`s, and when one lands in another package
read that package's `HirModule`. **Nothing is merged.** The lowerer reads from a
`PackageId → HirModule` map, and the other package is simply another place it
looks. Three reasons this is the shape, not one of two options:

1. **Dead code.** stdlib is ~360 lines across four files today and will grow.
   Merge-then-lower emits all of it and asks the wasm optimizer to prove the rest
   unreachable. Root-walking does the elimination at the one place that knows the
   roots.
2. **Monomorphization already forces demand-driven lowering.** The recommendation
   in [`open-decisions.md`](open-decisions.md) is monomorphize-by-type, and
   [F15](findings.md#f15) records that the frozen compiler *already*
   monomorphizes `filter` per call site. A monomorphizing lowerer is
   demand-driven by construction — a generic body cannot be lowered until its
   instantiation is known, and that is known only at the call site. **The
   machinery that turns N packages into one wasm is the same machinery generics
   need.** Building merge-then-lower means building it twice and deleting one.
3. It is rustc's monomorphization collector, walking from roots across crates,
   for these reasons.

### Determinism is not free here

A demand-driven walk emits functions in **discovery order**, so the emitted order
is a function of how roots and memo entries are iterated. A `HashMap` anywhere in
that path makes the output vary between builds and the goldens flap for no source
change ([A6](anti-spec.md#a6--no-random-seeded-iteration-reaching-output)).
The roots must be walked in a stable order and the instantiation memo must
iterate deterministically. This is the phase's most easily-missed A6 obligation,
because unlike a sorted output list the hazard is in the *control flow*, not in a
data structure someone will notice.

### What must exist before this phase can be written

Something has to hold `PackageId → HirModule`. `CompilerContext` owns
`Definitions` and is the natural owner. **Today nothing holds it** —
`lower_files` returns one module to its caller and `yelc-driver` drops it.
Stage 3 does not owe this; it is named here so the phase that needs it does not
discover it mid-write.

### The precompiled-package cap, which is [B1](open-decisions.md) wearing a different hat

The artifact carries `SerializedDef { path, ty, is_export }` — **signatures, no
bodies**. So a precompiled `yel:std` lets a call to `min` typecheck but not
be emitted, and `min` is not a WIT import: it is compiled into the output
([`plans/desugar/counter.yelir`](../desugar/counter.yelir) calls it inside
`cabi-realloc`). Cross-package bodies must therefore come from source, lowered
in-process.

That reads as a build-time cost and is not. **Under monomorphization a generic
function cannot be precompiled at all** — there is nothing to compile until the
instantiation is known. rustc ships MIR in rlibs for exactly this reason. So
while bodies stay out of the artifact, the precompiled-package story is capped at
non-generic, non-inlined functions *permanently*, not until someone optimizes it.

B1 kept `Serialize` off `Ty` on other grounds and settled this one in passing.
The alternative that does **not** reopen it: cache stdlib below HIR, as a lowered
LIR or wasm object, which sidesteps type serialization entirely. That is a
separate decision and is not made here — it is recorded so that "precompiled
stdlib" is never planned as though the artifact already supports it.

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
