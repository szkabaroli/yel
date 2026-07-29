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
> ([`stage-3-hir-build.md`](stage-3-hir-build.md), [`stage-3-hir-build.md`](stage-3-hir-build.md),
> [`stage-4-hir-check.md`](stage-4-hir-check.md)). This file is a worksheet, not
> the record.

**1 open — F1**, and it is stage 4's. Every `yelc-sema` question (B1, C1a–c,
C2, D0, D0a) was answered on 2026-07-29 and is recorded in
[`stage-3-hir-build.md`](stage-3-hir-build.md#decisions) under S1–S4 — that file
is the record, this one is the worksheet. The brief says decide F1 with D1, so it
is worth closing before stage 4 is briefed rather than after.

Cluster A was answered in full on 2026-07-29 (A2's misplaced tick corrected the
same day). Cluster E (3's HIR shape) was answered in full on 2026-07-29 and
is recorded in [`stage-3-hir-build.md`](stage-3-hir-build.md#decision-log) as
D1–D6 — that file is the record, this one is the worksheet. Also decided:
[D7](stage-3-hir-build.md#d7--flatten-else-if-chains) (flatten `else if`),
[D8](stage-3-hir-build.md#d8--a-module-is-identified-by-itself-not-by-a-file)
(module identity).

| cluster | # | blocks | parallel? |
|---|---|---|---|
| [A · Type representation](#cluster-a--type-representation) | ~~4~~ **0** | **everything** | ✅ answered 2026-07-29 |
| [B · Identity & serialization](#cluster-b--identity--serialization) | ~~1~~ **0** | sema's contract | ✅ answered 2026-07-29 |
| [C · Builtins](#cluster-c--builtins) | ~~4~~ **0** | sema's bulk | ✅ answered 2026-07-29 |
| [D · Context shape](#cluster-d--context-shape) | ~~2~~ **0** | sema's API | ✅ answered 2026-07-29 |
| [E · HIR shape](#cluster-e--hir-shape-stage-3) | ~~6~~ **0** | 3's seam types | ✅ answered 2026-07-29 |
| [F · Trigger](#cluster-f--trigger-stage-4) | 1 (F1) | 4 | with E1 |

Not asked here, deliberately: whether to **implement** closure capture
([§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one)). The
design obligation is already binding; implementing is a scope move that does not
gate phase 2.

---

## Cluster A · Type representation

Answer all four in one sitting — each constrains the others.

### A1 · How are parameterized types represented?

- [x] **Monomorphization by type.** `list<T>` instantiates to concrete
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

### A2 · How much inference sits inside the bidirectional checker?

**Bidirectional is the skeleton either way.** An earlier draft posed this as
"bidirectional *vs* unification", which is a false dichotomy — and the tick that
landed on "Bidirectional" under that wording could have meant either row below.
The two are orthogonal: bidirectional says *where* type information flows
(synthesize ⇒ / check ⇐); unification says *how* unknowns get resolved. Rust,
Swift, Scala and TypeScript all do both.

- [ ] **1 · None.** Every type concrete at every step, no metavariables. Where
      neither direction determines a type, emit a diagnostic — the frozen tree's
      *"cannot infer the type of this closure without an expected type."*
- [x] **2 · Metavariables + unification, no generalization.** Mint an inference
      variable where nothing determines a type, unify, resolve before the phase
      ends. Rust-like. ← **CHOSEN**
- [ ] **3 · Full Hindley-Milner** — option 2 plus let-generalization.
- [ ] Other: ______

*Hangs on it:* [A4](#a4--does-ty-get-an-infer-variant) follows directly — option 1
⇒ A4 no; options 2/3 ⇒ A4 yes. It does **not** constrain
[A3](#a3--does-ty-get-a-param-variant): metavariables and type parameters are
independent holes.

*Why not 3:* generalization exists to infer *polymorphic* types, and
[A1](#a1--how-are-parameterized-types-represented) establishes yel has none — it
would have nothing to generalize. It is also the one part genuinely in tension
with a bidirectional skeleton; GHC has been *restricting* let-generalization for
years for that reason. Unification is not in tension; generalization is.

**Answer: 2 — bidirectional skeleton with unification variables.** "Bidirectional
with HM" in the colloquial sense: HM-*style* inference, minus the
let-generalization yel has no use for. Recorded 2026-07-29.

> **The tick sat on row 3 until 2026-07-29 while the prose said 2.** Corrected to
> row 2, which is what every other line in this entry already said — the row's own
> `← CHOSEN` marker, the *Why not 3* paragraph, and the answer sentence. Recorded
> rather than quietly fixed because rows 2 and 3 are materially different work and
> [A4](#a4--does-ty-get-an-infer-variant) says it *follows from A2*, so a
> misreading propagates.

**Why "no use for it" is a fact about yel, not a preference.** Generalization
needs two things — a binding whose inferred type still has free variables, and
that binding used at more than one type. Yel has neither, and both halves are
checkable rather than argued:

1. **No type-parameter syntax exists.** `function_decl = { export_modifier? ~
   identifier ~ ":" ~ func_type ~ ";" }`, `func_type = { "func" ~ "(" ~
   func_params? ~ ")" ~ func_return? }` — there is nowhere to write `<T>`. The
   `T` in `list<T>`, and in `filter: (list<T>, func(T) -> bool) -> list<T>`, is
   **prose in `LANGUAGE.md`'s builtin table**, not syntax a user can write.
2. **A `let`-bound closure never reaches a generalizable state.** The shape that
   would need it:

   ```yel
   let id = { x -> x };
   let a: s32 = id(1);
   let b: string = id("hi");
   ```
   ```
   error[E0002]: cannot infer the type of this closure without an expected type
   ```

   A closure only checks where the expected type is already concrete. There is no
   moment at which `id : a → a` with `a` free, so the quantify-at-`let` step would
   have nothing to quantify.

Measured 2026-07-29 against the frozen compiler. It returns if yel ever gets
user-written generics — a language change that would reopen
[A1](#a1--how-are-parameterized-types-represented) first.

**What this accepts.** [T1](stage-4-hir-check.md#t1--bidirectional-checking-not-unification)
argued against a solver on four grounds; three are unaffected, one is a real cost
now taken on deliberately: **diagnostics.** Bidirectional-only yields "expected
`X`, found `Y`" *at the construct*; a solver reports a conflict wherever
unification failed — different span, different sentence. Diagnostic meaning is
frozen on 23 fixtures, so this becomes an **explicit obligation on 4**, not an
accident.

---

### A3 · Does `Ty` get a `Param` variant?

The `T` in `list<T>` — a placeholder in a *declaration*.

- [ ] **No** — templates are carried as **syntax** (AST `TypeRef` + a
      substitution) and interned only once concrete.
- [x] **Yes** — templates are represented as `Ty`, so a parameter needs a variant.
- [ ] Other: ______

*Hangs on it:* structural equality, interner uniquing, and what B1 has to write.
**Recommendation:** no — [S7](stage-3-hir-build.md#s7--does-ty-gain-a-non-concrete-variant).

**Answer:**

---

### A4 · Does `Ty` get an `Infer` variant?

A placeholder during *checking*, solved later. **Distinct from A3** — conflating
the two is the error this pair exists to prevent.

- [ ] **No** — no metavariables; `Mode::Infer` means *synthesize now*.
- [x] **Yes** — inference variables, solved during checking. ← **follows from A2**
- [ ] Other: ______

**Answer: yes.** Not independent — [A2](#a2--how-much-inference-sits-inside-the-bidirectional-checker)
option 2 requires it. Recorded 2026-07-29.

**Three obligations this creates**, none of which exist under "No":

1. **`Infer` must not survive the phase.** It is legal *during* 4 and illegal
   after. 4's postcondition strengthens from "`types` is total" to "`types` is
   total **and contains no unresolved variable**" — rustc's `has_infer()` check.
2. **It must never be serialized.** A module artifact containing a hole is a bug,
   not a state ([B1](#b1--how-does-ty-cross-a-module-boundary),
   [§6](directions.md#6--modules-are-serializable-artifacts)).
3. **Structural equality and interner uniquing must account for it** — two
   distinct variables are not the same type. Decide whether variables live in the
   interner at all or in a side unification table
   ([S7](stage-3-hir-build.md#s7--does-ty-gain-a-non-concrete-variant)).

---

## Cluster B · Identity & serialization

`yelc-sema`'s. Depends on A.

### B1 · How does `Ty` cross a module boundary?

`pub struct Ty(pub u32)` **already derives `Serialize`/`Deserialize`**
(`types/interner.rs:13`), so a naive derive writes the interner index.

- [x] **Structurally, and delete the derive** — the wrong thing stops compiling.
- [ ] **Structurally, keep the derive** for in-memory/debug use, rely on review.
- [ ] **As a handle plus a remap table** applied on load.
- [ ] Other: ______

*Evidence:* Swift — *"types are always serialized with enough info to regenerate
them at load time."*
**Recommendation:** structurally, delete the derive —
[S2](stage-3-hir-build.md#s2--ty-must-not-serialize-as-its-handle).

**Answer: structurally, no derive.** Confirmed by implementation 2026-07-29
(`crates/yelc-sema/src/artifact/`) rather than by argument. Types are written
into an artifact-local table and re-interned on load; the enforcement holds —
simulating the bug needed four edits including a new `Ty::from_raw_index`. The
test loads into a **differently populated** interner, because a same-interner
round trip passes with raw handles on the wire and proves nothing; that control
is kept in the suite under that name.

**The derive is still live one crate down.** `Name` and `SourceId` derive
`Serialize` in `yelc-base`, and a `Name` is an interner index with exactly this
failure mode. The artifact writes strings instead, but by convention, not by
type error. Decide before stage 3 grows the wire surface.

---

### B2 · Is `DefId` module-qualified from day one?

- [x] **Yes** — `DefId { module, index }`; `DefPath` is derivable from it.
- [ ] **No** — plain index now, qualify when serialization actually lands.
- [ ] **No module concept yet** at all.
- [ ] Other: ______

*Hangs on it:* retrofitting touches every downstream holder of a `DefId`, which
is the whole compiler.
**Recommendation:** yes — [S5](stage-3-hir-build.md#s5--defid-shape).

**Answer: yes.** Confirmed 2026-07-29. A `DefId` crossing a package boundary
becomes a path and resolves to the consumer's own index, which is not the
producer's.

**`DefPath` as recorded does not do the job**, on two counts found by building
it: it holds `Name`s and `Ty` handles, so it is not serializable at all (it is
the resolution-independent *in-process* form, one step short of the wire); and it
has **no namespace**, so it cannot distinguish a record from a component of the
same name, which `Definitions` explicitly permits. See
[`seam-changes.md`](seam-changes.md), 2026-07-29.

---

### B3 · Where does the overload discriminator live?

A name does not identify a definition under overloading: `len` is both
`list<T> -> s32` and `string -> s32`. Swift's `XREF_VALUE_PATH_PIECE` carries the
*type* for exactly this.

- [x] **`yelc-sema`** — one `OverloadKey`, consumed by both `DefPath` and A1's
      mangling.
- [ ] **`yelc-hir` (4)** — it is a resolution concern, sema just stores it.
- [ ] **Two mechanisms**, one per consumer.
- [ ] Other: ______

**Recommendation:** `yelc-sema` — [S6](stage-3-hir-build.md#s6--overloadkey).

**Answer: `yelc-sema`** — one `OverloadKey`, as recorded.

~~**Untested, and currently unreachable:** `Definitions` keys names by
`(Name, Namespace)` with no discriminator, so `stdlib.rs`'s two `len`s cannot
both be registered.~~ **Two things wrong with that, corrected 2026-07-29.**

- `stdlib.rs` never registered into `Definitions`. It registers into
  `BuiltinTable`, whose `by_name` has always been `Name → Vec<BuiltinId>`; both
  `len`s register, and
  `stdlib::tests::len_has_two_overloads_that_lower_differently` has always
  asserted it. The conclusion was right; the evidence named the wrong table.
- The real blocker — `Definitions`' key — is **gone**. It is a single-namespace
  symbol table keyed by `Name` with `SmallVec<[Sym; 1]>` values, and
  `register_overload` takes an `OverloadKey`
  ([`scope.md`](scope.md), [`seam-changes.md`](seam-changes.md), 2026-07-29).

**What is still blocked, and it is not a table.** The artifact **loader**
registers definitions in pass 1 and resolves the type table in pass 2 — a
declared type may name an ADT that only exists once the definitions do — so a
`Ty`-valued key is unavailable at the moment registration needs it.
`SerializedDefPath.overload` stays empty and an artifact holding an overload set
is **rejected**, not half-loaded; that rejection is now reachable from a real
registration rather than only from a hand-built artifact. Filling the field wants
a key that does not depend on the type table (Swift mangles one into the path),
which is a separate decision.

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

**C1a · One table, or two projections?** Typeck wants the type scheme, 4 wants
the lowering target, and `yelc-lir` must see neither.
- [x] One table, two accessors · [ ] Two tables + a key-alignment test · [ ] Other: ______

**C1b · Do builtin *elements* go in it?** `KnownElements` is 15 fields of UI
vocabulary with no "lowering target" in the same sense.
- [ ] Yes · [x] No, separate home · [ ] Other: ______

**C1c · Variadics.** `concat` is registered with an empty parameter list and a
comment saying it is really variadic. A table with a declared arity must answer.
- [x] Arity gains a variadic form · [ ] `concat` becomes N fixed arities ·
      [ ] Other: ______

---

### C2 · What happens to builtin elements, enums and variants?

C1 settles *functions*. This is the rest of `known.rs`.

- [ ] **Same table** as functions.
- [x] **A separate table** — they have no lowering target. Shaped as resolved
      lang-items: `DefId`, not `Option<DefId>`.
- [ ] **Delete** — they resolve through the normal definition tables.
- [ ] Other: ______

*Note:* the `Option<DefId>` wrapper is load-bearing nowhere — every read is an
unwrap-or-diagnostic for a case that cannot occur once registration has run
([A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)).
**Recommendation:** separate home; do not port 413 lines because they exist —
[S3](stage-3-hir-build.md#s3--does-known-survive-at-all).

**Answer:**

---

## Cluster D · Context shape

### D0 · What does `CompilerContext` hold?

[keep-list §5](keep-list.md#5--context-threading--yel-coresrccontextrs) keeps
context *threading*, not the frozen 963-line struct.

- [x] **Six fields** — interner, type interner, definitions, builtin table,
      source map, diagnostics.
- [ ] **Fewer** — pass some explicitly instead. Name which: ______
- [ ] **More** — name what and why: ______

*Already settled by the crate graph, not by preference:* `block_id_counter`,
`block_names`, `component_lifecycle_blocks` and the fanout table are `yelc-lir`
types. `sema → lir` is forbidden, so they **cannot compile here** — they belong
to [5](stage-5-lir.md)/[6](stage-6-lower.md).
**Recommendation:** the six — [S4](stage-3-hir-build.md#s4--what-stays-on-the-context).

**Answer:**

**D0a · Where does `signal_deps` live?** Cited as the *positive* precedent for
side tables, but it is reactivity analysis — a frontend concern, not a sema one.
- [ ] `yelc-sema` · [x] `yelc-hir` · [ ] Other: ______

---

## Cluster E · HIR shape (stage 3)

Runs in parallel with A–D. Only E1 couples outward.

### E1 · Does HIR keep bindings and handlers as separate lists?

- [x] **No — one uniform prop list.** 4 classifies, using the declared type.
- [ ] **Yes** — classify syntactically in 3 (value is a closure literal ⇒
      handler).
- [ ] **One list plus a classification side table** filled by 4.
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

- [x] **No** — remove `item_ty: Ty`; a side table if 4 needs it keyed by node.
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

Stage 1 explicitly did **not** decide this; 3 owns it.

- [x] **Nearest preceding comment run, no blank line between.**
- [ ] **Not attached yet** — 3 records trivia positions only.
- [ ] Other: ______

**Recommendation:** either, but stated — not left implicit.

**Answer:**

---

## Cluster F · Trigger (stage 4)

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

Then: brief 3. `yelc-sema` and the seam types are no longer separate landings —
as of 2026-07-29 they are phases 1 and 2 of stage 3
([`stage-3-hir-build.md`](stage-3-hir-build.md#work-in-scope)), so Cluster A is
the last thing standing between here and briefing.
