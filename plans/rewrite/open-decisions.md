# Open decisions — the questionnaire that unblocks phase 2

> **How to use it.** Every entry is a question with concrete options. Tick one,
> or write your own under *Other*. The recommendation is listed **last, and
> labelled**, so it does not lead the answer — several of these have a defensible
> second choice.
>
> **Answering is the deliverable.** No entry needs research: the evidence is
> gathered and cited. If you pick the recommendation, the tick is enough. If you
> pick anything else, add one sentence of *why* — that sentence is what the next
> agent reads.
>
> **When a cluster is done**, copy the answers into the owning file's Decision log
> ([`infra-sema.md`](infra-sema.md), [`stage-2a-hir-build.md`](stage-2a-hir-build.md),
> [`stage-2b-hir-check.md`](stage-2b-hir-check.md)). This file is a worksheet, not
> the record.

**10 open.** Cluster E (2a's HIR shape) was answered in full on 2026-07-29 and
is recorded in [`stage-2a-hir-build.md`](stage-2a-hir-build.md#decision-log) as
D1–D6 — that file is the record, this one is the worksheet. Also decided:
[D7](stage-2a-hir-build.md#d7--flatten-else-if-chains) (flatten `else if`),
[D8](stage-2a-hir-build.md#d8--a-module-is-identified-by-itself-not-by-a-file)
(module identity).

| cluster | # | blocks | parallel? |
|---|---|---|---|
| [A · Type representation](#cluster-a--type-representation) | 4 | **everything** | no — answer first |
| [B · Identity & serialization](#cluster-b--identity--serialization) | 3 | sema's contract | after A |
| [C · Builtins](#cluster-c--builtins) | 2 | sema's bulk | after A |
| [D · Context shape](#cluster-d--context-shape) | 1 | sema's API | after B, C |
| [E · HIR shape](#cluster-e--hir-shape-2a) | ~~6~~ **0** | 2a's seam types | ✅ answered 2026-07-29 |
| [F · Trigger](#cluster-f--trigger-2b) | 1 | 2b | with E1 |

Not asked here, deliberately: whether to **implement** closure capture
([§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one)). The
design obligation is already binding; implementing is a scope move that does not
gate phase 2.

---

## Cluster A · Type representation

Answer all four in one sitting — each constrains the others.

### A1 · How are parameterized types represented?

- [ ] **Monomorphization by type.** `list<T>` instantiates to concrete
      `$list_s32`, `$list_Person`. No type variables, no unification. Internal
      only — `list<s32>` stays the surface. Key: `(template, concrete args)`.
- [ ] **Monomorphization by GC shape** (Go 1.18's stenciling). Same machinery,
      **coarser key**: `(template, shapes(args))`. On WASM-GC the partition is
      roughly `{i32, i64, f32, f64, ref}`, so `list<Person>` and `list<Address>`
      share one copy and every generic is bounded at ~5 instantiations regardless
      of user-type count. Costs a second concept (shape ≠ type) and interacts
      with [B3](#b3--where-does-the-overload-discriminator-live).
- [ ] **Real generics.** Add `TyVar` + substitution + unification +
      generalization, and a polymorphic-representation decision at the LIR seam.
- [ ] **Neither yet.** Keep the frozen `Ty::ERROR` placeholder; parameterized
      stdlib items stay unavailable.
- [ ] Other: ______

*Hangs on it:* A3, B3, C1, and whether §2 tier C (`len`, `some`, `list.get`,
`append`) is reachable at all.

*Evidence:* [F1](findings.md#f1) — no type variables exist today; `option` is
registered with `payload: Ty::ERROR`. [F15](findings.md#f15) — `filter` is
**already** monomorphized per *call site*, so per-type is a **reduction** against
the real baseline, not an increase.

*On code size*, which matters for a web target: erasure ships one general copy
that fights dead-code elimination, while a specialized instantiation feeds the
`--gufa --closed-world -Oz` pipeline already in the release path. The two
remaining alternatives are closed — erasure reintroduces a second value
representation ([C2](anti-spec.md#c2--one-representation-chosen-at-the-seam)),
witness tables need funcrefs ([§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one)).

**Recommendation:** monomorphization, **by type first** — options 1 and 2 share a
memo table and differ only in the key function, so coarsening to shapes later is
a local change, not a redesign. Measure before coarsening: build one generic at
two same-shape instantiations and see whether `--gufa --type-merging` already
merges them. [§3](directions.md#3--generics-are-monomorphization-by-name).

**Answer:**

---

### A2 · What algorithm does 2b type-check with?

- [ ] **Bidirectional** — `Mode::{Infer, Check}`, no solver, types concrete at
      every step.
- [ ] **Unification** — generate constraints with fresh variables, solve.
- [ ] **Bidirectional now, solver later** behind the same API.
- [ ] Other: ______

*Hangs on it:* A3's `Infer` half; F1's option B; whether "expected X, found Y"
diagnostics survive on the 23 fixtures.
*Note:* this was **inherited from the frozen tree, never argued**, until it was
written up — so treat it as genuinely open.
**Recommendation:** bidirectional —
[T1](stage-2b-hir-check.md#t1--bidirectional-checking-not-unification).

**Answer:**

---

### A3 · Does `Ty` get a `Param` variant?

The `T` in `list<T>` — a placeholder in a *declaration*.

- [ ] **No** — templates are carried as **syntax** (AST `TypeRef` + a
      substitution) and interned only once concrete.
- [ ] **Yes** — templates are represented as `Ty`, so a parameter needs a variant.
- [ ] Other: ______

*Hangs on it:* structural equality, interner uniquing, and what B1 has to write.
**Recommendation:** no — [S7](infra-sema.md#s7--does-ty-gain-a-non-concrete-variant).

**Answer:**

---

### A4 · Does `Ty` get an `Infer` variant?

A placeholder during *checking*, solved later. **Distinct from A3** — conflating
the two is the error this pair exists to prevent.

- [ ] **No** — bidirectional needs none; `Mode::Infer` means *synthesize now*.
- [ ] **Yes** — inference variables, solved during checking.
- [ ] Other: ______

*Hangs on it:* whether function-type inference can be fixed in its general form
later without a `Ty` change.
**Recommendation:** no, **if A2 is bidirectional** — the two answers must agree.

**Answer:**

---

## Cluster B · Identity & serialization

`yelc-sema`'s. Depends on A.

### B1 · How does `Ty` cross a module boundary?

`pub struct Ty(pub u32)` **already derives `Serialize`/`Deserialize`**
(`types/interner.rs:13`), so a naive derive writes the interner index.

- [ ] **Structurally, and delete the derive** — the wrong thing stops compiling.
- [ ] **Structurally, keep the derive** for in-memory/debug use, rely on review.
- [ ] **As a handle plus a remap table** applied on load.
- [ ] Other: ______

*Evidence:* Swift — *"types are always serialized with enough info to regenerate
them at load time."*
**Recommendation:** structurally, delete the derive —
[S2](infra-sema.md#s2--ty-must-not-serialize-as-its-handle).

**Answer:**

---

### B2 · Is `DefId` module-qualified from day one?

- [x] **Yes** — `DefId { module, index }`; `DefPath` is derivable from it.
- [ ] **No** — plain index now, qualify when serialization actually lands.
- [ ] **No module concept yet** at all.
- [ ] Other: ______

*Hangs on it:* retrofitting touches every downstream holder of a `DefId`, which
is the whole compiler.
**Recommendation:** yes — [S5](infra-sema.md#s5--defid-shape).

**Answer:**

---

### B3 · Where does the overload discriminator live?

A name does not identify a definition under overloading: `len` is both
`list<T> -> s32` and `string -> s32`. Swift's `XREF_VALUE_PATH_PIECE` carries the
*type* for exactly this.

- [x] **`yelc-sema`** — one `OverloadKey`, consumed by both `DefPath` and A1's
      mangling.
- [ ] **`yelc-hir` (2b)** — it is a resolution concern, sema just stores it.
- [ ] **Two mechanisms**, one per consumer.
- [ ] Other: ______

**Recommendation:** `yelc-sema` — [S6](infra-sema.md#s6--overloadkey).

**Answer:**

---

## Cluster C · Builtins

### C1 · How are builtins registered?

- [x] **One table** — `name → { arity, type scheme, lowering target }`, replacing
      `stdlib_lookup.rs` (1,029 lines) and `known.rs` (413).
- [ ] **Field per builtin**, rewritten but structurally as today.
- [ ] **Table for functions, fields for elements/enums/variants.**
- [ ] Other: ______

*Evidence:* [F12](findings.md#f12) — one builtin is currently four things that
must agree, checked by nothing.
*Not blocked on* [§2](directions.md#2--the-stdlib-is-yel-source-embedded-in-the-binary):
the table is filled from Rust now; §2 later changes where rows come from.
**Recommendation:** one table — [§1](directions.md#1--builtins-are-a-table-not-a-field-per-builtin).

**Answer:**

Three sub-questions, only if C1 lands as a table:

**C1a · One table, or two projections?** Typeck wants the type scheme, 3b wants
the lowering target, and `yelc-lir` must see neither.
- [ ] One table, two accessors · [ ] Two tables + a key-alignment test · [ ] Other: ______

**C1b · Do builtin *elements* go in it?** `KnownElements` is 15 fields of UI
vocabulary with no "lowering target" in the same sense.
- [ ] Yes · [ ] No, separate home · [ ] Other: ______

**C1c · Variadics.** `concat` is registered with an empty parameter list and a
comment saying it is really variadic. A table with a declared arity must answer.
- [ ] Arity gains a variadic form · [ ] `concat` becomes N fixed arities ·
      [ ] Other: ______

---

### C2 · What happens to builtin elements, enums and variants?

C1 settles *functions*. This is the rest of `known.rs`.

- [ ] **Same table** as functions.
- [ ] **A separate table** — they have no lowering target.
- [ ] **Delete** — they resolve through the normal definition tables.
- [ ] Other: ______

*Note:* the `Option<DefId>` wrapper is load-bearing nowhere — every read is an
unwrap-or-diagnostic for a case that cannot occur once registration has run
([A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)).
**Recommendation:** separate home; do not port 413 lines because they exist —
[S3](infra-sema.md#s3--does-known-survive-at-all).

**Answer:**

---

## Cluster D · Context shape

### D0 · What does `CompilerContext` hold?

[keep-list §5](keep-list.md#5--context-threading--yel-coresrccontextrs) keeps
context *threading*, not the frozen 963-line struct.

- [ ] **Six fields** — interner, type interner, definitions, builtin table,
      source map, diagnostics.
- [ ] **Fewer** — pass some explicitly instead. Name which: ______
- [ ] **More** — name what and why: ______

*Already settled by the crate graph, not by preference:* `block_id_counter`,
`block_names`, `component_lifecycle_blocks` and the fanout table are `yelc-lir`
types. `sema → lir` is forbidden, so they **cannot compile here** — they belong
to [3a](stage-3a-lir.md)/[3b](stage-3b-lower.md).
**Recommendation:** the six — [S4](infra-sema.md#s4--what-stays-on-the-context).

**Answer:**

**D0a · Where does `signal_deps` live?** Cited as the *positive* precedent for
side tables, but it is reactivity analysis — a frontend concern, not a sema one.
- [ ] `yelc-sema` · [ ] `yelc-hir` · [ ] Other: ______

---

## Cluster E · HIR shape (2a)

Runs in parallel with A–D. Only E1 couples outward.

### E1 · Does HIR keep bindings and handlers as separate lists?

- [x] **No — one uniform prop list.** 2b classifies, using the declared type.
- [ ] **Yes** — classify syntactically in 2a (value is a closure literal ⇒
      handler).
- [ ] **One list plus a classification side table** filled by 2b.
- [ ] Other: ______

*Evidence:* stage 1's AST already unified them into `NamedProp`;
[F8](findings.md#f8) shows a handler-shaped block typechecks against a `func()`
prop, so the split is not a type-system fact.
*Caveat if you pick option 1 — this is the real work:* the frozen lowering uses
the split to decide scoping, and `LocalId` ordinals reach the type checker.
**Local allocation order must be verified unchanged.**
**Recommendation:** one uniform list.

**Answer:**

---

### E2 · Does the `For` node carry the item type?

- [x] **No** — remove `item_ty: Ty`; a side table if 2b needs it keyed by node.
- [ ] **Yes** — keep it on the node.
- [ ] Other: ______

*Evidence:* a typeck result on an untyped IR node —
[B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes).
**Recommendation:** remove.

**Answer:**

---

### E3 · Does `For` carry the loop-variable name?

Frozen comment: *"stored directly to avoid LocalScope lookup issues."*

- [x] **No** — fix the scope structure so the lookup works.
- [ ] **Yes, as a side table** (`NodeMap`).
- [ ] **Yes, on the node** — keep frozen behaviour.
- [ ] Other: ______

**Recommendation:** fix the scope structure.

**Answer:**

---

### E4 · Does `HirGlobal` carry a body?

Today it does not; property defaults live in `GlobalDef`, which is asymmetric
with `HirComponent`.

- [ ] **Yes** — symmetric with components, one uniform spine.
- [x] **No** — defaults stay in the definition table. `HirGlobal` carries only
      the functions declared on that global.
- [ ] Other: ______

*Note:* the frozen doc comment claims the asymmetry is deliberate. Either answer
is defensible; the requirement is that it is **chosen**, not inherited.
**Recommendation:** none — this one genuinely wants your call.

**Answer:**

---

### E5 · What order do items lower in?

- [ ] **All components, then all globals** — frozen behaviour, which preserves
      type-check and therefore diagnostic order.
- [x] **Globals then components** — dependency order: components reference
      globals, globals cannot reference components. Also makes *lowering* agree
      with *registration*, which is already globals-first.
      ⚠️ Carries an obligation; see the log entry.
- [ ] **Source order, with diagnostics sorted before rendering.**
- [ ] Other: ______

*Constraint:* diagnostic order is asserted by the 23 diagnostic fixtures. This is
the one place "uniform item spine" and "match frozen output" pull apart.
**Recommendation:** components-then-globals, preserved exactly.

**Answer:**

---

### E6 · How is a doc comment attached to an item?

Stage 1 explicitly did **not** decide this; 2a owns it.

- [x] **Nearest preceding comment run, no blank line between.**
- [ ] **Not attached yet** — 2a records trivia positions only.
- [ ] Other: ______

**Recommendation:** either, but stated — not left implicit.

**Answer:**

---

## Cluster F · Trigger (2b)

### F1 · How is a body's trigger determined?

Reactive bodies' reads join a dependency set; event bodies' reads must not.

- [ ] **The slot's function type**, delivered by the `Check` direction. No surface
      change. Cost: a function type gains a component, so type equality,
      inference and any function-typed WIT surface must account for it — global
      callbacks are `func(...)`-typed and cross the boundary, so "internal only"
      must be *arranged*, not assumed. **Requires A2 = bidirectional.**
- [ ] **A required keyword** on the closure. Cost: every `clicked: { … }` stops
      parsing — all 91 fixtures and the differential with them.
- [ ] **An optional keyword.** Cost: the unmarked case still needs positional
      inference, so the mechanism is supplemented rather than replaced.
- [ ] **Positional inference**, frozen behaviour made explicit.
- [ ] Other: ______

*Decide with [E1](#e1--does-hir-keep-bindings-and-handlers-as-separate-lists).*
**Whichever wins owes the same test, and it is not optional:** the dependency set
of a body of each trigger kind, asserted on a fixture, in an execution test.
Neither failure mode — spurious re-renders, stale UI — is caught by "it compiles".
**Recommendation:** the slot's function type —
[§5](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger).

**Answer:**

---

## After the questionnaire

Two mechanical items, independent of every answer above. Both must land **before
any measurement** — fixing the fixture changes what reaches HIR.

- [ ] `global_filter_default.yel` rewritten to `{ x -> x > 2 }` and re-blessed,
      with a line in [`goldens-changed.md`](goldens-changed.md). *Still broken as
      of 2026-07-28.*
- [ ] The two silent `_ => {}` parser arms filed as a `known_bugs` entry.
      *Still absent.*

Then: land `yelc-sema` → land 2a's seam types → brief 2a.
