# Anti-spec — shapes the rewrite may not reproduce

> Derived from [`docs/TECH_DEBT.md`](../../docs/TECH_DEBT.md) at freeze SHA `ccf2086`.
> Rule: [`anti-spec-from-tech-debt`](../../.agents/skills/compiler-rewrite/rules/anti-spec-from-tech-debt.md)
>
> **Append-only.** When review finds a *new* failure shape, add a rule — never
> delete or renumber one. This list is the accumulated memory of the rewrite.

Ships in every agent brief alongside the contract and the
[keep-list](keep-list.md). **Violating any rule below fails review**, regardless
of whether the differential is clean — the differential checks behaviour, this
checks whether the result was worth having.

The rules name **shapes**, not incidents. `blocks.rs` will not exist in the new
tree; the pressure that produced it will.

---

## A. Universal — apply to every stage

### A1 · No side-channel IR

Everything a later stage needs is either **in the IR node** or in an **explicit
side table keyed by a typed id**. Never a parallel structure the consumer has to
know to consult, and never a field that is authoritative for one consumer and
stale for another.

*The shape it came from:* `LirResource.tree_shape` (§1.2) — a synthesized
description of component tree structure that lowering wrote, codegen read, and
neither owned. Boundary-field access resolved by walking a symbolic chain
through it. Killing it took three phases and it is still alive as an internal
scratch representation inside the YEL-only synthesis pass.

*Test to apply:* if a reviewer must read two files to know where a fact lives,
it is a side channel.

### A2 · No god pass

A pass does not simultaneously allocate identifiers, resolve names, decide block
structure, and emit output. Split into passes with **named inputs and outputs**.
No pass over ~800 lines without a written justification in the stage file.

*The shape it came from:* `lower_to_lir/blocks.rs` (§2) — 8,500 lines, one
`BlockLowering` struct with 50+ fields: output vectors, monotonic counters
(`next_slot`/`next_block`/`next_memory_offset`), an ops stack, for-loop stacks,
deferred-body queues, and signal layout, all mutating together. Also
`syntax/parser.rs` ~3.3k, `thir/typeck.rs` ~2.8k, `hir/lower.rs` ~1.4k (§7).

*Test to apply:* can you name this pass's input type and output type in one
sentence each? If the answer involves "and also mutates", it is a god pass.

### A3 · No duplicated walkers

**One visitor owns recursion per IR**; passes override arms. The walk/visit split
(`visit_expr` defaults to free `walk_expr`) is mandatory, and `walk_*` is
**exhaustive with no `_` arm** so a new variant is a compile error at the single
place that must learn the new shape.

*The shape it came from:* §6.1 — four hand-written copies of one THIR descent
(read collectors, write collectors, dependency collection, lowering), each
independently maintained. THIR was unified behind `ThirVisitor`; HIR lowering,
LIR `collect_deps_recursive`, and the `boundary_rewrite`/`dedupe` op-stream
walkers are still hand-rolled at the freeze.

*Caveat that is part of the rule:* a generated or defaulted walker must still
make an unhandled variant a **loud failure**, never a silent skip.

### A4 · No permanent bridge

An adapter between old and new representations **ships with its deletion
commit** in the same PR series. It is named in the stage file. An adapter still
present one stage after the stage that needed it is an anti-spec violation, not
a backlog item.

*The shape it came from:* all of §1 — a catalogue of "transitional" bridges
(`legacy_u32()` slot bridge, `LirComponent`↔`LirResource`, the canonical-flat
↔ typed-GC dual representation) that outlived their transitions by long enough
to need their own documentation. The rewrite exists partly *because* those
bridges accumulated.

### A5 · No silent fallback

An unimplemented path is `todo!("descriptive msg")` or a typed `Err(...)`. Never
placeholder IR, a default instruction, a zero slot, or a "reasonable guess".

*Why it is in the anti-spec and not just the keep-list:* this is the one
existing invariant that kept the old codebase's debt **loud** rather than
silent, and it is the first thing a rewriting agent softens when a match arm is
inconvenient. Type-incorrect IR emitted by a placeholder is near-impossible to
trace back from the WASM validator error it eventually causes.

### A6 · No random-seeded iteration reaching output

Anything derived from a hash map or set is **sorted and deduped** before it can
influence emitted bytes, type-index assignment, or golden text. `yel-core` and
`yel-wasm-codegen` use `rustc_hash::FxHashMap`/`FxHashSet`; std `HashMap`/
`HashSet` are denied by `clippy.toml` `disallowed-types`. **The new crates
inherit that lint** — this is not optional infrastructure.

*The shape it came from:* §7 — ~35/200 fuzz seeds produced byte-different
modules run-to-run because `RandomState` iteration order leaked into WASM type
indices and for-loop emission order. Two site-specific bugs were fixed before
anyone noticed the systemic one.

### A7 · No weakened assertion

A test is never softened to match known-wrong output, and a golden is never
re-blessed from the new compiler. If behaviour must change, the diff is read,
justified, and recorded in `goldens-changed.md`. An expected-to-fail test is
`#[ignore]` **with a reference**, and the ignored count is a tracked ratchet
metric.

---

## B. Front-end shapes (stages 1–3)

### B1 · No lossy parse

The parser produces a **complete tree for broken input**. Trivia lives in the
tree. A `Result<Ast, Vec<Error>>` that discards the tree on failure is a
regression even though it is smaller — it forecloses the LSP permanently.

*The shape it came from:* the pest-based parser gives a parse or a failure, and
"a file that does not parse" is the state a file is in most of the time in an
editor.

### B2 · No deferred name resolution encoded as a lie

A type or name that is **not yet resolved** is represented as an explicitly
unresolved thing (a lazily-filled cell, an `Unresolved` variant) — never as a
plausible-looking wrong answer that a later pass is expected to overwrite.

*The shape it came from:* §3 — `AstTyKind::Named(_)` interned as `Unknown` "for
now until name resolution", and HIR "keep as identifier for now, will be
resolved in THIR". Both are values that type-check as legitimate and are simply
incorrect until something fixes them, with nothing enforcing that something ran.

### B3 · No analysis result stored on the node it describes

Analysis output lives in **side tables keyed by typed id**, not as mutable
fields fattening the IR node. Nodes describe the program; tables describe what
passes learned about it.

*Positive precedent to follow:* `CompilerContext::signal_deps` keyed by `DefId`
is already the right shape and is explicitly documented as correct modeling
(§1.6), not debt.

### B4 · No special-cased control flow where a general form exists

If the language has a general construct, lowering goes through it. Conditionals
do not get a bespoke path that a later `match` implementation will have to
duplicate.

*The shape it came from:* §3 — `lower_to_lir/component.rs:626` "TODO: Desugar to
match expression"; conditional lowering is special-cased and `match` is not real
yet, so the two will need reconciling exactly once someone implements `match`.

### B5 · No unreachable diagnostic

Every `ErrorCode` variant has a triggering fixture, or it does not exist.

*The shape it came from:* §3 — `ErrorCode::UnknownUnitSuffix` (E0004) and
`ErrorCode::MissingElement` (E0042) are defined with live emission arms that
**cannot fire**: an earlier `E0060` SyntaxError shadows both because the grammar
rejects the input first. Dead code that looks load-bearing.

### B6 · One idiom per diagnostic shape

There is **one** obvious way to emit a coded diagnostic, one way to add a note.
Not two coexisting idioms where the choice is historical.

*The shape it came from:* §3 — `Diagnostics::error(span, code, msg)` (33 sites)
coexisting with `Diagnostic::error(msg).with_span().with_code()` (13 sites), of
which only 4 actually need the builder's `.with_note()`.

---

## C. Back-end shapes (stages 4–5)

### C1 · No domain vocabulary below the frontend seam

Nothing named `mount`, `boundary`, `component`, `dom`, `signal`, or `$Comp` may
appear in LIR-facing or codegen-facing code. The LIR and the whole back-end are
a **frontend-agnostic substrate** shared by Yel (UI) and the visual flow
language. New back-end code depends only on the `lir/arena.rs` traits and generic
`LirOp`s.

*This is the north star, not a style preference.* Stage 4 carries it, and the
crate graph enforces it: `yelc-lir` and `yelc-codegen` have no dependency path to
any frontend crate, so a UI reference below the seam is a build failure rather
than a review finding.

### C2 · One representation, chosen at the seam

A value has **one** internal representation. Where a second representation is
genuinely required (the canonical ABI at the WIT boundary), the conversion is
confined to named boundary code and the type system enforces it.

*The shape it came from:* §1.5 — values were half-migrated between
canonical-flat (decomposed into flat ABI valtypes across multiple slots and
linear memory) and typed WASM-GC. Two representations coexisting meant every
consumer had to know which one it was looking at, and lowering/codegen predicate
mismatches between them accounted for every long-standing fuzz failure.

*The mechanism to carry over, not reinvent:* the zero-sized `WitBoundary`
witness. `canonical_flat_valtypes(ty, WitBoundary::assert())` makes "I am
boundary code" an explicit, greppable, reviewable act instead of a silent
reflex. Note it is **not hermetic** (the witness is crate-constructible) — that
is a known limit, not a bug to fix by weakening the rule.

### C3 · No resource acquired without a release path

Every allocation, handle, or buffer that crosses the host boundary has a
matching free, designed at the same time. "Add the free later" produces a leak
that only shows up in long-running sessions and never in a test.

*The shape it came from:* §4 — freed component handles never return to the
registry (no `[resource-drop]`), and string/list callback-argument buffers
materialized into linear memory are never freed after the import returns
(no `cabi_post` equivalent on the lower side). Both are live leaks at the freeze.

### C4 · No type whose storage shape depends on where it appears

A type has one storage shape. If a surface type works as an element attribute
but not as a stored property, that is two types wearing one name.

*The shape it came from:* §4 — `color`/`brush`. The surface primitive
`InternedTyKind::Color` (4-byte) and the ADT a hex literal desugars to
(`Adt(known.variants.color)`) have different storage shapes, so `c: color =
#ff0000` fails typeck *and* fails to flatten in codegen. `yel-smith` deliberately
avoids generating them, which means the fuzzer cannot find the next instance of
this shape.

### C5 · No hard-coded sizing

Sizes are computed from the thing being sized. A constant with a "TODO: compute
this dynamically" comment is a bug with a due date nobody set.

*The shape it came from:* §2 (`blocks.rs:705`, string/signal region sizing) and
§5 (`lir/layout.rs:160,166` — user-defined variant sizes not computed, "TODO:
look up from definitions").

### C6 · No classification placeholder

An enum that classifies something has no `Other` / `Unclassified` / `Legacy`
catch-all variant. If a case cannot be classified, that is a `todo!`, not a
variant.

*The shape it came from:* §5 — `lir/block.rs:157,159` "TODO #105: classify" and
`FunctionRole`'s "catch-all for less-classified blocks (legacy / migration)"
variant, which is where every unclassified block quietly accumulated.

### C7 · No output-format generator that is not wired up

Dead generation paths are deleted, not commented out.

*The shape it came from:* §7 — `lir_rust.rs`, the LIR→Rust generator, is
commented out of `lib.rs` and has been maintained-by-accident ever since.

---

## D. Process shapes

### D1 · The compilation unit is the file, not the component

Every top-level declaration lowers through **one uniform item spine**. Codegen
differences (resource-with-registry vs. singleton-with-core-globals) are a
*property of the item*, not a parallel pipeline.

*The shape it came from:* §1.6 — globals and components ran two parallel spines
for most of the project's life. The consequences were not cosmetic: record/tuple
globals were **latently broken** (lowering routed them to a memory path codegen
had stopped reserving), module-scope expressions got an empty arena, and the
fuzzer skipped globals entirely, so none of it was caught.

### D2 · Debt is documented at the moment it is created

A shortcut lands with its entry in the tech-debt inventory, in the same change.
The inventory is the input to the *next* rewrite; the fact that it existed and
was accurate is why this rewrite is possible at all.

### D3 · A stage documents what surprised it

Behaviour discovered in the old compiler that nobody knew about goes in the
stage file's **Surprises** section, even when it changes nothing. Costs thirty
seconds to write at stage 3 and a week to rediscover at stage 5.
