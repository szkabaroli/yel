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

### A8 · An invariant is asserted, not observed

A test that collects counterexamples into a `Vec` and prints the count is not a
test of the property. If the property has known-permitted exceptions, they are an
**exact allow-list that fails in both directions** — it fails when an entry stops
being an exception, too. If it has none, it is an `assert!`.

A test whose *name* states a property its *body* does not check is a weakened
assertion under A7, whether or not it ever passed in the stronger form. So is a
count-based assertion loose enough to pass vacuously: `assert!(checked > 0)` over
a file list built by swallowing `read_dir` errors into an empty `Vec` reports
"120/120 ✓" when it should have checked 2118.

*Found by:* the stage-1 review panel. `truncation_inside_a_construct_always_reports`
asserted only that the tree round-tripped, never the diagnostic half of S5 it was
named for; the sibling sweep `eprintln!`d that 89 of 750 mutated inputs produced
no diagnostic and passed green. Both blocking S5 violations were sitting inside
the tests written to catch them.

### A9 · A ported construct is load-bearing or it is deleted

When a stage ports a design from a reference implementation, every construct it
brings over has a **live use site** in the new tree. A `const` set with no
consumer, a builder method called only by its own unit test, or a function
parameter that takes the same value at every call site is a **shape-only port**:
it makes review believe a mechanism is present when only its declaration is.

The stage file lists such items and either wires them up or removes them.
"Unused" is a finding, not a footnote.

*Found by:* the stage-1 review panel. `token::EMPTY` (documented as a recovery
set, zero references, used twice in the reference), `GreenTreeBuilder::abandon_node`
(only caller was its own test — and its absence changed the green shape of every
failed declaration), and `parse_list`'s `code: ErrorCode` parameter (`SyntaxError`
at all 8 call sites).

### A10 · An allow-list entry is characterized by evidence about the *other* implementation

A differential allow-list records where the new implementation is permitted to
differ from the frozen one. Its per-entry check must therefore assert something
about **the frozen side** — what it produced, what it skipped, what it did not
report. A check that inspects the *new* side restates the divergence instead of
justifying it, and **cannot fail while the divergence exists**.

The tell is that the check reads like the bug's own description.

*Found by:* the stage-1 review panel, round 2. An entry claiming the frozen
parser silently dropped a statement was guarded by "the new parser produced a
`Recovered::Missing` let-name". The frozen parser had in fact **kept** the
statement — the entry was a regression in the new parser (it reserved `let` and
`if` in statement position, which the frozen grammar does not), allow-listed
under a check that could only ever confirm it. This was the single case where the
differential caught a silently-tightened grammar, and the check turned it into an
exemption.

The companion rule: **the characterization must be falsifiable by a plausible
regression.** In the same review, the *main* allow-list's check
(`new_member_count > frozen_member_count`) was satisfied by a deliberately
introduced grammar tightening, because the recovery model always materialises an
unreadable element as a member — so `new > frozen` held by construction for the
entire class the check claimed to characterize. If you cannot state a check a
tightening would fail, you do not have a characterization; you have a list.

### A11 · A bound measures the quantity that actually fails

A guard against resource exhaustion counts the thing that runs out. A limit on
*recursion depth in one pass* does not bound *the depth of the structure that
pass builds*, and every later consumer — a walker, a `Drop` glue chain, a
serializer — recurses over the structure, not over the pass.

The compounding failure is that the guard's own tests then measure the guarded
counter, so they report healthy headroom on precisely the inputs that abort.

*Found by:* the stage-1 review panel, round 2, independently by both reviewers.
`MAX_NESTING_DEPTH = 256` bounded parser recursion, but `parse_binary` and
`parse_postfix` are iterative loops that enter and leave nesting per operand — so
`a.b.b.b…` built a 12,000-deep `Box` chain from a **valid, diagnostic-free** 6 KB
file while the depth counter read **2**. `parse()` returned; the walker aborted at
n=3144, `Drop` at n=5058, and the round-trip check itself at n=13164. The
invariant "parsing always returns" was true and worthless.

### A12 · An assertion holds at the granularity of the property

A property that is true *per construct* is asserted per construct. Aggregated to
the file or the run, two violations in opposite directions cancel and the
assertion passes.

*Found by:* the stage-1 review panel, round 2. S5 ("ill-formed input produces a
diagnostic **and** a recovery node") was asserted as
`(diagnostics > 0) != (error_nodes > 0)` over a whole file. Deleting a recovery
diagnostic left that test green across all 2225 mutated inputs — the file's
*other* recovery positions supplied the missing count. Only a hand-written
per-construct list caught it.

### A13 · The generator that found a bug class is what ships, not its instances

When randomized or generated input exposes a class of defect, the **generator**
is committed with a fixed seed. Freezing the specific counterexamples it happened
to find converts a mechanism that can find the *next* member of the class into a
regression test for the members already fixed.

Watch for the shape where a sweep's stated strength grows while its actual
coverage shrinks — a claim of "zero violations" backed by fewer, narrower inputs
than the run that found the violations in the first place.

*Found by:* the stage-1 review panel, round 2. The 300,000-random-input sweep
that found 446 S5 counterexamples in round 1 was not committed; the shipped
generator sampled ~1,200 truncations and split on **whitespace**, so it could not
construct `"{}"` from `"v={value}"` — and four S5 clusters survived underneath a
passing test.

### A14 · Test inputs are verified present, not merely counted

A count assertion over a directory proves the directory has entries, not that the
entries are the intended content. Content-addressed or lazily-fetched test data
(git-lfs pointers, submodules, downloaded archives) reads as **present and
wrong**, not as absent.

Assert something only the real content satisfies.

*Found by:* the stage-1 review panel, round 2. The corpus is git-lfs tracked; an
unpulled checkout leaves 2000 ~130-byte pointer stubs, so the pinned
`CORPUS_COUNT` passed. **Four of the stage's six headline numbers reproduced over
pointer stubs**, including "2118/2118 round-trip" and "deepest real program = 21".
The comment defending the assertion said an unpulled corpus "reads as empty" — it
does not, and that mistaken belief was what made the count look sufficient.

### A15 · A fix to a decision boundary is validated in **both** directions

When a fix moves the line between accept and reject, include and exclude, match
and skip, it is verified on **both sides** of the new line before it lands. A fix
validated only against the cases that motivated it reliably overshoots: the
inputs that used to fall on the correct side of the boundary are exactly the ones
nobody re-tested.

State the *class* the fix ranges over, enumerate its members mechanically, and
check every member — not the handful that prompted the change.

*Found by:* the stage-1 review panel, in **three consecutive rounds**, each time
as a fix relocating its own defect rather than removing it. Round 1's fabricated
values became round 2's fabricated *list elements* one level up. Round 2's
recursion guard bounded parser depth, moving the overflow into the consumers.
Round 3's keyword-prefix fix — added so `recordFoo { }` would parse, since the
frozen grammar has no word boundary — omitted the check that the remainder is a
valid `identifier`, so `record0 { }`, `component8A { }` and `package-a:b;` became
accepted too. The defect moved from under-accepting to over-accepting, and the
fix's own test list covered only the under-accepting half.

### A16 · A generator is asserted against the strongest property it can check

Shipping the generator ([A13](#a13--the-generator-that-found-a-bug-class-is-what-ships-not-its-instances))
is necessary and not sufficient. A generator wired to a weak property while a
stronger oracle sits unused in the same test crate is a mechanism that *looks*
load-bearing and finds nothing.

When a differential oracle exists, generated input is run against **it**, not
merely against a self-consistency check.

*Found by:* the stage-1 review panel, round 3. The committed mutation generator
was asserted only on S5 and byte round-trip; the frozen-parser oracle was
consulted only over a separate deterministic sweep. Pointing the existing
generator at the existing oracle produced **81 divergences, 7 of them genuine
blocking defects, in under four seconds** — three of that round's blocking
findings were reachable by the stage's own committed code.

Corollary: match the generator to the property. Random token soup exercises
recovery (both implementations reject nearly all of it, so it finds no
divergences); mutations of *real programs* exercise the grammar boundary.

### A17 · Test-input selection is stable under renames

Fixture sets sampled by position — `take(n)` over a sorted directory listing,
every k-th file, a hash of the path — silently re-point when a file is renamed,
added, or removed. The suite then measures a different population while reporting
the same metric name, and any number keyed to that population becomes a fact
about the filesystem rather than about the code.

Select by explicit list or by content, and make the count exact.

*Found by:* the stage-1 review panel, round 3. Renaming one fixture
(`imported_components.yel` → `extern_components.yel`, from unrelated work on the
frozen tree) changed which files a name-sorted strided sampler picked, turning
three of the stage's headline numbers red and shifting a pinned floor from 586 to
568. Nothing about the parser had changed.


### A18 · A number is produced by a command

A figure reasoned onto the page is indistinguishable, once written, from one that
was run. Both are bold, both are cited, both get relied on — the difference
surfaces only when something depends on it being true. Arithmetic is the usual
route (*"315 workspace tests plus the ~80 the new crate adds, so 395"*);
estimation is the other (*"duplication is a non-issue at this scale"*).

**Every number carries the command that produced it, near enough to re-run
without hunting.** A number without one is a claim, and is labelled as a claim or
deleted.

Two corollaries, both learned the hard way:

- **Where a number lives decides whether it can be skipped.** A row in
  [`ratchet.md`](ratchet.md) is a gate that fails loudly; the same figure as a
  line of prose in a stage's Numbers is a note that can be quietly not-taken.
  Moving a measurement from a row to a line is a real loss of rigour even when
  the number is identical, and is stated as a cost when it happens.
- **A measurement of the wrong population is worse than none**, because it
  reports a plausible number under the right name — see
  [A14](#a14--test-inputs-are-verified-present-not-merely-counted) and
  [A17](#a17--test-input-selection-is-stable-under-renames).

This is [A8](#a8--an-invariant-is-asserted-not-observed)'s sibling: A8 governs
invariants a **test** asserts, A18 governs figures a **document** reports. Same
failure, two surfaces, and the second has no compiler to catch it.

*Found by:* the baseline row in `ratchet.md`. Its first draft read **395**
workspace tests — derived from the old count plus the new crate's expected
additions rather than from `cargo test --workspace`. Caught before it landed, and
the row now names the command beside every column. The freeze check had the same
disease in a different costume: `git status … | wc -l` printed the expected
number while matching nothing at all, and went undetected for the whole of stage
1 *because the number it printed was the number expected*.

*Numbering note:* cited as `A19` in `ratchet.md` before it existed — the citation
was written from a miscount, and the reference was corrected to A18 rather than
leaving a permanent hole at A18. Nothing was renumbered; A18 had never been used.

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

### B7 · No unbounded recursion on user-controlled input

A recursive-descent front end carries an **explicit nesting limit** and reports
exceeding it as a diagnostic. Nesting depth is controlled by whoever typed the
file; a stack overflow is `abort()`, not a catchable panic, so no
accumulate-and-continue policy survives it and no recovery node can be produced.

A parser whose stated invariant is "always terminates and always returns" must
have a bound that makes that true, plus a test that **finds** the bound rather
than a corpus that never reaches it. The guard trips with real headroom below the
actual stack limit, not near it — and the limit that matters is the debug build's,
because that is what `cargo test` and a dev LSP run.

*Found by:* the stage-1 review panel, independently by both reviewers. ~1500
nested `(` — a ~3 KB file — SIGABRTed a debug build, across five independent
productions, including the *unclosed* case that is an ordinary editor state. The
robustness sweep missed it because truncations and single-token deletions of real
programs never generate deep nesting.

### B8 · Disambiguating lookahead is bounded by the construct, not the file

Where a parser resolves an ambiguity by scanning ahead, the scan terminates at
the **enclosing construct's boundary even when that boundary is missing**. A scan
that falls through to end-of-input on unterminated input turns every nesting
level into a full-tail rescan — and unterminated input is the *normal* state in
the editor a lossless tree exists to serve.

*Found by:* the stage-1 review panel. `has_depth_zero_arrow` ran to
end-of-token-stream when the `{` had no match: 1.1 / 3.8 / 14.6 ms for 500 / 1000
/ 2000 nested opens, a clean 4× per doubling on a 10 KB file.

### B9 · A recovery hole is a node, not a sentinel value

Extends [B2](#b2--no-deferred-name-resolution-encoded-as-a-lie) from types down to
names and other leaf data. Interning `""` for a name the parser could not read,
substituting an empty `Vec` for an unparsed parameter list, or truncating an
over-long argument list to the arity that fits are all the same shape one layer
down: a value that type-checks as legitimate and is simply wrong.

The IR needs a representation that makes the hole **unrepresentable-as-valid** —
a `Missing` variant, an `Option`, or an error node — so a consumer cannot
accidentally treat it as real. "A diagnostic was emitted" is not a substitute,
because nothing forces the consumer to have read the diagnostics.

The corollary: every recovery position needs an error representation in the data
model. If a `RecordField` has no error variant, then reporting a bad field and
pushing nothing is a **silently-dropped subtree**, no matter how good the
diagnostic was.

*Found by:* the stage-1 review panel. `synthetic_ident` interned `""`, so
`package ;` produced a package whose namespace and name were equal to each other;
446 of 300,000 random inputs produced a diagnostic and zero error nodes.

---

## C. Back-end shapes (stages 3–4)

### C1 · No domain vocabulary below the frontend seam

Nothing named `mount`, `boundary`, `component`, `dom`, `signal`, `effect`, or
`$Comp` may appear in LIR-facing or codegen-facing code. The LIR and the whole back-end are
a **frontend-agnostic substrate** shared by Yel (UI) and the visual flow
language. New back-end code depends only on the `lir/arena.rs` traits and generic
`LirOp`s.

*This is the north star, not a style preference.* Stage 3 carries it, and the
crate graph enforces it: `yelc-lir` and `yelc-codegen` have no dependency path to
any frontend crate, so a UI reference below the seam is a build failure rather
than a review finding.

**`signal` was always on this list, and the frozen tree violates it** —
`yel-core/src/lir/signal.rs` and `signal_layout.rs` sit in the backend beside
`tree_shape.rs`. `effect` was added 2026-07-29 for completeness; both were
already forbidden in spirit.

**The reactive graph is lowered away before LIR** (decided 2026-07-29). Signals
become cells, effects become registered callbacks plus a dispatch table, and LIR
sees only data, functions, calls and that table. Reactivity is *generated code*,
not IR vocabulary.

This is cheaper than it sounds and the evidence is checkable: the only file in
`yel-wasm-codegen` naming `LirSignal` / `LirEffect` / `LirBlockEffect` is
`lir_rust.rs`, which is **dead** (commented out of `lib.rs`). The WASM path does
not consume the reactive types at all — they sit in the generic substrate without
it needing them.

**Consequence worth stating, because it is load-bearing:** reactive
optimisations (dead effects, static regions, narrowing an update to the ops that
depend on a changed input) must therefore run in
[stage 6](stage-6-lower.md), *before* the graph is lowered away — the same rule
as signal deps on HIR before the tree is desugared. **Analyse at the layer where
the structure exists; lower it away after.**

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
seconds to write at stage 2 and a week to rediscover at stage 4.

### D4 · A doc comment describing usage is a claim under review

"Used as the recovery set where a caller has no synchronising tokens" on a `const`
with zero references. "Bounded scan" on a scan that runs to end-of-input. "It does
not guess at what was meant" one line above `intern("")`.

These are **false statements in the artifact reviewers read to decide whether a
pattern was ported**. A doc comment that asserts *how* something is used is
checked against its call sites in review, and a divergence is a finding at the
same severity as the code being wrong — because the doc is what made the code
look right.

*Found by:* the stage-1 review panel, which caught all three of the above in one
pass.
