# Stage 3 — `yelc-hir`, build + resolve            status: phases 0–2 landed

Replaces (frozen, never edited): `crates/yel-core/src/hir/` (1,995 lines) **plus
the `yelc-sema` inventory** (`context.rs`, `definitions.rs`, `known.rs`,
`stdlib_lookup.rs`, `types/` — ~3,536 lines), which became
[phase 1](#phase-1--yelc-sema-3536-lines) rather than a separate landing.
Phase **3** of the merged HIR stage; phase 4 is
[`stage-4-hir-check.md`](stage-4-hir-check.md). Same crate, run in sequence.

Base: — · Started: 2026-07-29 · Landed: —

> **Gate: open, and it is no longer Cluster A.** Stage 1 landed (`33e5c71`, 0
> corpus divergences); D1–D6 answered 2026-07-29;
> [Cluster A](open-decisions.md#cluster-a--type-representation) **was answered in
> full on 2026-07-29** — A1/A3/A4 are recorded in this file's own
> [S7](#s7--does-ty-gain-a-non-concrete-variant), so the banner that used to
> stand here was already contradicted 700 lines below it. What is left is
> [phase 3's own gate](#gate).
>
> **Scope grew on 2026-07-29.** `yelc-sema` (~3.5k lines), the seam types, and
> the two oracle-hygiene items were prerequisites; they are now
> [phases of this stage](#work-in-scope). Phases 0, 1 and 2 have landed
> (`1d12250`, `9a54ad1`/`ca905d0`, and this reconciliation's commit); **phase 3
> is what remains.**
>
> ## ⚠️ Reconciled 2026-07-30 — read this before the body
>
> This file was written against the tree as it stood on 2026-07-28. Since then
> phase 1 landed, seven surface/lexer changes landed, and two design sessions
> (`plans/modules.md`, [directions §9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it))
> moved decisions this brief states. Every correction below is made **in place,
> with the wrong statement left visible**, which is this directory's rule. The
> corrections cluster:
>
> | | what moved | where the body is now right |
> |---|---|---|
> | `Namespace` is **deleted** | `ca905d0` — one namespace, a two-level symbol table (`Sym`/`DefKind`/`Module`) | [contract](#designed-for-serialization--what-stage-3-owes-6), [phase-1 contract](#phase-1-contract--what-yelc-sema-exports) |
> | `ModuleId` **means something else** | `fbaa95e` — the old one is `PackageId`; `ModuleId` now indexes symbol-table module nodes | [D8](#d8--a-package-is-identified-by-itself-not-by-a-file), [Multiple files](#multiple-files) |
> | the UI lowers **before** checking | [directions §9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it) + [D1's revision](#phase-placement-revised-2026-07-29--classification-is-a-table-lookup) | [What lowerings belong here](#what-lowerings-belong-here), [the shape](#the-shape-shared-with-stage-4) |
> | the artifact **exists** | `9a54ad1` — stage 3 implements two traits and bumps a constant | [Designed for serialization](#designed-for-serialization--what-stage-3-owes-6), [DoD](#definition-of-done) |
> | five surface constructs now **parse** — and two lexer rules changed in *both* compilers | `8daa4b9`, `a68e127`, `7899c12`, `da8cbfa`; `3ef3568`, `d27bab2` | [What arrives from stage 1](#what-arrives-from-stage-1-and-what-does-not) |
> | the seam types are **not landable as written** | found by landing them | [Contract](#contract) |
>
> The last row is the one to read first: **three statements in the contract code
> block are wrong**, two of them contradict each other inside the same block, and
> one of them cannot be written in Rust at all. They are marked ⚠️ at the point of
> use.

## The shape (shared with stage 4)

**One IR, two phases, types in a side table.** One node vocabulary, one walker.
`types: NodeMap<Ty>` is empty after 3 and total after 4 —
[`seam-changes.md`](seam-changes.md), 2026-07-28.

| phase | does | produces |
|---|---|---|
| **3** *(this file)* | AST → HIR; register items; resolve names; collect declared types; **desugar the UI tree to functions and calls** | HIR + `Definitions` typed |
| **4** | bidirectional type checking over the same nodes | `types` map total |

⚠️ **The third clause was added 2026-07-30 and is the largest single change to
this brief.** The UI desugaring was written here as a *stage 4* job when the file
was drafted; [D1's phase-placement revision](#phase-placement-revised-2026-07-29--classification-is-a-table-lookup)
and [directions §9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it)
moved it, because handler-vs-binding classification reads a **declared** type out
of `Definitions` and never an inferred one. Consequence: **phase 4 never sees
UI**, and `typeck.rs`'s ~2.8k lines of element/property/handler/children cases
evaporate rather than being ported. The exception is
[binders](#the-one-exception-binders).

**3's output is a public surface, not an internal intermediate.** Yel will have
lints, and early (syntactic) lints run here while type-aware lints run after 4.
Same nodes, same walker, a lint written once. Lint results are side tables like
every other analysis output
([B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)).

## Brief

Port `arkc-hir`'s structure to yel's item vocabulary: name-resolved, bodies
separated from items by id, analysis results in side tables.

- **Three phases, across the whole file set** — invariant [H1](#h1).
- **Bidirectional `HirId ↔ NodeId` map** (ark `hir_map.rs`: `map` + `rev_map`,
  `next_hir_id(node_id)` allocating and recording in one call). What lets a
  diagnostic point at source, and what the LSP needs. ⚠️ **The ark shape does not
  transfer unchanged** — see [the contract](#the-hirmap-key-is-not-a-nodeid).
- **Side tables, not fattened nodes** (ark `NodeMap<V>`, `assert!(old.is_none())`
  on insert) — [B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes).
  `CompilerContext::signal_deps` keyed by `DefId` is the existing positive
  precedent for the *shape*. ⚠️ **Not for the address**: it does not stay on the
  context — [D0a](#s4--what-stays-on-the-context) moved it into `yelc-hir`,
  which this bullet contradicted for as long as both were in the file.
- **Types are not re-represented** — [see below](#why-there-is-no-parsedtype).
- **Bodies separated from items by id** (ark `Module { node_types, bodies,
  elements }`).
- **One walker**, exhaustive, no `_` arm ([A3](anti-spec.md#a3--no-duplicated-walkers)).
  The frozen tree has a second, hand-rolled one: `collect_children_slots`
  (`lower.rs:52`) re-walks the node tree with its own match over every kind.
  4 shares this walker; it does not get its own.
- **One uniform item spine** for globals and components
  ([D1](anti-spec.md#d1--the-compilation-unit-is-the-file-not-the-component)).
  The frozen tree already models this correctly — `HirItem` is a real
  `{Component, Global}` enum, not two parallel pipelines. That is the *shape* to
  arrive at, reached by writing it, not by copying `node.rs`
  ([read, do not port](README.md#read-the-frozen-tree-do-not-port-it)).
- **No `String` survives.** Stage 1 established S4 and the frozen HIR breaks it
  in three places: `HirNodeKind::Element.name`, `HirBinding.name`,
  `HirHandler.name`. Also [keep-list §3](keep-list.md).

## Inherited from stage 1

S1–S6 hold; assume them, re-verify none. Notably: `green.text() == content`,
every AST node has `NodeId` + `Span`, names interned, ill-formed input yields **a
diagnostic AND an `Error` node**, parsing always returns a `ParsedFile`.

Not assumable: sibling ordering beyond source order, `NodeId` stability across
reparses, any green-tree reuse API. **Trivia attachment is this phase's problem.**

Four of stage 1's eight Surprises change what arrives here:

| # | consequence |
|---|---|
| **1** | The frozen *parser* silently discarded malformed `global`/`record` members; stage 1 reports them. **More programs now reach HIR, carrying `Error` nodes the frozen HIR never saw.** Every `Error` variant and `Recovered::Missing` must lower to something — never a panic, never a skip. |
| **2** | `if {` parses as an **element named `if`** (keywords are not reserved). Element resolution will see `if`/`for`/`else` as names. Reproduced deliberately; do not "fix" it here. |
| **5** | `name: func(…)` is a **property** in a component and a **callback** in a global. Registration must preserve the asymmetry. |
| **6** | `extern component`, legacy `callback name(…);`, and the `bind` modifier are real, are in fixtures, and are absent from `LANGUAGE.md`. All three lower here. |

### What arrives from stage 1, and what does not

**Added 2026-07-30.** The body of this brief was written against the AST as it
stood on 2026-07-28 and assumes the constructs below are absent. **Five of them
now parse**, so HIR must lower them; six more are designed and do not parse, so
HIR must not be built around them. The distinction is the point — a brief that
lists both together invites lowering code for a node type that does not exist.

**Parses today. Lowering is owed.** Each is an approved surface break in
[`scope.md`](scope.md); the frozen compiler parses none of them, so **none is
covered by the differential** and each needs hand-written fixtures here.

| construct | landed | AST |
|---|---|---|
| `func<T>(…)` type parameters | `8daa4b9` | `FuncSignature::type_params: Vec<Recovered<TypeParam>>` |
| `@name(key = value)` attributes on declarations | `a68e127` | `AttributeList` / `Attribute` / `AttributeArg`, on **ten** declaration types |
| function **bodies** | `7899c12` (design `5ac81f3`) | `FunctionDecl::body: Option<Block>`; `Block { id, span, stmts, tail }` |
| `for` in **statement** position | `7899c12` | `Stmt::For(Box<ForNode>)`, with `ForBody::Statements` |
| `return` | `da8cbfa` | `Stmt::Return(ReturnStmt)` |

Two consequences that are not obvious from the table:

- **`Block` is shared by four statement-block positions**, not two —
  `ClosureExpr`, `FunctionDecl`, `IfStmt` and `ForNode` ([`scope.md`
  correction 5](scope.md)). One HIR body construct covers all four, and `tail`
  is what distinguishes statement position from expression position. Do not
  re-derive that distinction from the node kind.
- **`ForNode` is one node in two positions.** A UI `for` and a statement `for`
  are the same AST type with a different `ForBody`. [D2](#d2--for-does-not-carry-the-item-type)
  and [D3](#d3--for-does-not-carry-the-loop-variable-name) apply to both.

**Designed, does not parse. Do not build for it, do not foreclose it.**

| construct | design | status |
|---|---|---|
| `match` + patterns | [directions §9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it), `LANGUAGE.md` | the **target form** every conditional lowers into; grammar lands as a scoped stage-1 reopening after stage 4 |
| `impl T { … }` | [`modules.md` §2](../modules.md) | method scope; needs a receiver type, so its *lookup* is stage 4 |
| `module M { … }` | [`modules.md` §3](../modules.md) | → WIT `interface`; the symbol table's second level already exists and nothing populates it |
| `from "…" include X` / `use X.{ … }` | [`modules.md` §4.1](../modules.md) | ⚠️ **one open question, and it is stage 3's**: does `include` name a package or a module? [`modules.md` §7](../modules.md) says *decide before HIR is built on it* |
| `primitive name: type = "@op";` | [`scope.md` correction 3](scope.md) | an **item form**, not an attribute — the "attributes subsume `primitive`" collapse was withdrawn |
| `ref` opaque type | [`scope.md`](scope.md) | one type name |

**`match` is the one to design around now.** Everything else can be added later
without moving what phase 3 builds; `match` is the general conditional that
`Ternary`, statement `if` and UI `if` are all supposed to collapse into
([F18](findings.md#f18)), and shaping HIR for it after phase 4 is built is the
expensive order.

**Two lexer changes also landed, in *both* compilers, and are on neither list.**
Not surface breaks — the corpus was regenerated after each and all 8000 artifacts
came back byte-identical — but they change what reaches HIR, and one of them
touches this brief's Surprise 2 without invalidating it:

| | landed | what changed |
|---|---|---|
| **a keyword ends at a word boundary** | `3ef3568` | `ifa { … }` was `if a { … }`, `recordFoo { … }` was a record named `Foo`, `input: s32;` in a `global` was an `in` property named `put`. None of it was designed, and it is gone from both parsers. Keywords are still **not reserved** — `KEYWORD_FIRST ⊆ NAME_FIRST` — so Surprise 2 stands: `if {` still has two live readings, and element resolution still sees `if`/`for`/`else` as names |
| **a hyphen joins a name only before a name character** | `d27bab2` | `{ p: s32->p }` was a *record* on one side and a *closure* on the other; `count-=1` was an assignment to a variable named `count-`. Kebab names (`starts-with`, `in-out`, `font-size`) are untouched |

## What lowerings belong here

**HIR is name-resolved before it is typed, so a desugaring belongs in 3 iff it
is decidable from names alone.**

| needs | belongs in |
|---|---|
| nothing but the syntax tree | 3, any phase |
| the definition tables | 3, **phase 2 or 3** — never phase 1 ([H1](#h1)) |
| a *type* to choose the target | [4](stage-4-hir-check.md) |
| the whole **package** (fan-out, ordering) | [6](stage-6-lower.md) — e.g. `resolve_global_triggers` |

⚠️ **The second row is where the UI went.** *"Which property is a handler"* reads
a **declared** type out of `Definitions`; it is this row, not the third. That is
the whole content of [D1's phase-placement revision](#phase-placement-revised-2026-07-29--classification-is-a-table-lookup)
and it is why phase 4 never sees UI.

**The five the frozen tree performs** (`docs/PIPELINE.md` lists four):

| surface | becomes | needs |
|---|---|---|
| `x += 1` | `x = x + 1` | nothing |
| `#ff0000` | `Color.rgba((r,g,b,a))` | the `Color` def |
| `Foo.bar(args)` | `PathCall` if `Foo` is a type/global, else `MethodCall` | def tables |
| `Enum.case`, `Global.prop` | `Path { segments }` | def tables |
| `bind value: x` | getter + empty setter, merging same-named props ([F13](findings.md#f13)) | nothing |

F13 is undocumented and interacts with **D1**: it is the one place two source
props genuinely fold into one entity. Its `HashMap<String,_>` + parallel
`binding_order` both go — an order-preserving structure does this directly.

**Must not move in.** Anything type-directed (interpolation → `concat` needs each
part's type; `MethodCall` survives into HIR by design) · ~~**UI tree flattening**
(`if`/`for`/`Element` stay structured; flattening is LIR's job)~~ · name errors
(HIR never errors on an unknown name — resolution is *partial* on purpose).

⚠️ **The struck clause is wrong as written, and the correction is not "delete
it".** Two different things were being called flattening:

| | where | still true? |
|---|---|---|
| UI **desugaring** — element/property/handler/child become builder functions and calls | **3** ([D1](#phase-placement-revised-2026-07-29--classification-is-a-table-lookup), [§9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it)) | moved here; the clause denied it |
| UI **tree-shape flattening** — the anchor/boundary/mount layout the back-end walks | [6](stage-6-lower.md) | unchanged; still not this stage's |

So `Element` does **not** stay structured, and `if` goes **directly to `match`**
rather than to a statement `if` first — routing it through a form that is itself
sugar is two lowerings where one suffices, and a second place for the reactive
keying to be attached inconsistently ([§9](directions.md#why-ui-if-goes-straight-to-match-not-via-if)).

#### The one exception: binders

**State it plainly in any brief built from this: a generated region function does
not have a complete signature at construction.** *"Everything becomes functions"*
invites the opposite assumption and it is wrong for exactly two constructs.

| construct | binder | type comes from |
|---|---|---|
| `for item in items` | `item` | the iterable's element type — **phase 4** |
| `match v { some(x) -> … }` | `x` | the scrutinee's case payload — **phase 4** |

This is not a new mechanism. `hir/lower.rs:1152` already writes
`item_ty: Ty::ERROR, // Will be inferred` and `thir/typeck.rs:559–575` fills it
through `locals.set_ty` — a local outliving its unknown type is what the frozen
tree already does. The desugaring emits the **structure**; checking fills the
slot. Note this is also why [D2](#d2--for-does-not-carry-the-item-type) deletes
`item_ty` rather than moving it: the slot belongs in a phase-4 side table, not on
the node.

**Pattern *resolution* is type-directed too**, and therefore phase 4's: a bare
lowercase name is a case pattern when it names a case of the scrutinee's type and
a binding otherwise. Same shape — the arm's structure lowers early, the pattern's
meaning resolves late.

### Candidates, and what blocks each

Not proposals — a list so nobody re-derives it. The rustc rule that generates it:
**desugar to a general form early, then lower the general form once.** Six Rust
constructs (`for`, `while`, `while let`, `if let`, `?`, `let else`) collapse to
`loop` + `match` before HIR exists, so MIR building handles two, not six.

| candidate | today | blocked on |
|---|---|---|
| `Ternary` → a general conditional | carried by **all four IRs**; `ExprKind` has `Ternary` and no `If`, so yel has *three* unrelated conditional constructs — expression, statement, UI node ([F18](findings.md#f18)) | ⚠️ ~~the `match` / general-conditional decision … **The target form does not exist yet.**~~ **Answered 2026-07-29: `match` is the target form** ([§9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it)). All three surface conditionals lower into it, UI `if` **directly**. What is left is sequencing: the grammar lands as a scoped stage-1 reopening after stage 4, so phase 3 designs for `Match` and cannot yet be handed one to lower. |
| `Range` → a struct literal | carried by all four IRs | **not blocked — scheduled.** There is no `Range` type to desugar into *yet*, and [§2 § What the stdlib must provide](directions.md#what-the-stdlib-must-provide-not-just-what-can-move-into-it) now carries it as a requirement. Not generic, so it does **not** wait on §3. |
| `MethodCall` → `Call` | already desugars — gone by the typed layer | — |
| `Interpolation` → `concat` | already desugars — gone by LIR | — |

**Neither is a "someday" item.** `Ternary` waits on a real open decision (the
general conditional form). `Range` waits on nothing but sequencing — the stdlib
is planned, and the desugaring is now a **requirement on its contents** rather
than a request blocked by it. Revisit this table when either lands, rather than
rediscovering it.

The general rule, worth applying to every future candidate: **a desugaring
desugars *into* something, and that something is a design requirement on whatever
provides it.** Record the requirement when the desugaring is decided.

Note the scale: F18's three conditional forms across four IRs is
[B4](anti-spec.md#b4--no-special-cased-control-flow-where-a-general-form-exists)
at four times [D7](#d7--flatten-else-if-chains)'s, and D7 was judged worth an
enumerated divergence.

**Every desugaring moves diagnostic spans.** `x += 1` → `x = x + 1` means a later
error reports against a *synthesized* `Binary`. The five carried over are safe by
construction (the 23 fixtures pin them); a **new** one is checked against those
fixtures before it lands.

### The artifact — and the stage boundary through it

**Added 2026-07-30.** [`plans/desugar/counter.yel`](../desugar/counter.yel) is a
one-property component; [`counter.yelir`](../desugar/counter.yelir) is what
lowering makes of it, written out concretely, with
[its README](../desugar/README.md) carrying the evidence, the fifteen corrections
that checking it produced, and the compiler bugs it turned up. Read those for
detail; what belongs *here* is only the boundary — because **the artifact is not
this stage's output.** It spans four stages, and reading it as one would put that
conflation into the brief.

**The stage-3 slice** — the only part writable today, since it needs no types:

| in the artifact | why 3 |
|---|---|
| the component as a `record`; every UI node gone | definition tables only ([D1](#d1--bindings-and-handlers-are-one-uniform-prop-list)) |
| `mount` / `unmount` / the per-region update functions, as ordinary functions with the record as first parameter | UFCS; no method concept needed |
| the UI `if` as `match`, **directly** | [§9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it) |
| resolved names; dependency sets | `signalck` reads only `Def`/`Local`, never a type |
| the five desugarings above | as tabled |

At stage 3 the interpolation is **still an interpolation node** — the artifact
writes `concat("Count: ", s32-to-string(count))`, which is ahead of this stage on
that one line and says so.

**What is in the artifact and is not this stage's:**

| | whose | why |
|---|---|---|
| types on every node; interpolation → `concat`; **which `*-to-string`** | [4](stage-4-hir-check.md) | type-directed |
| `for` / `match` binder types; pattern resolution | 4 | [the binder exception](#the-one-exception-binders) |
| `Component`→`TreeRoot`→`If0`→`If0Then`; the handle registry; effects collapsed to one `update` per signal | [6](stage-6-lower.md) | tree-shape flattening + reactive lowering, exactly the row above |
| `concat` → `$concat2`; the mangled `@export` names; `cabi_realloc`; the return area; `cabi_post`; the packed handler id | codegen | canonical ABI and arity monomorphization are the back-end's |

Two corrections to the obvious reading of that table, both from the artifact's
README:

- **`concat` → `$concat2` is codegen's, not LIR's.** The arity scan feeds
  `RuntimeFunctions::new`, and the monomorphized function is emitted in the wasm
  builder. The candidates table above says interpolation is *"gone by LIR"*; the
  `concat` **call** is, the arity family is not.
- **There is no `concat` overload to pick.** `yelc-sema` declares one variadic
  `concat` and **eight** `*-to-string` rows, one per source type. So the
  type-directed choice inside an interpolation is *which `to-string`*, and
  `concat` needs no type at all. `len` is the name with overloads; `concat` is
  not.

**What the artifact shows this brief does not say.** It predates the
UI-lowers-before-typecheck move, and three obligations fall out of writing the
output down:

1. **`mount` and `unmount` are a pair, and the pairing is this stage's.** Every
   handle, region and registration the desugaring *creates* is one the teardown
   must undo. Writing `mount` alone is what hid the obligation — and in the
   frozen back-end it stayed hidden: there is no effect deregistration and
   `remove-event-listener` is imported into every module and never called.
2. **Teardown order is a correctness property, not a style choice.** Effects come
   off before nodes go away; an effect firing between a node's removal and its
   own deregistration writes to a node that is gone. Invisible in source, visible
   the moment the IR is explicit.
3. **A region lowers to a function *pair* per branch**, not one builder —
   `if-branch-mount` and `if-branch-unmount`. The [binder exception](#the-one-exception-binders)
   names the `for`/`match` signature problem; it does not name the pairing.

**Provenance, now demonstrable.** [The obligation](#the-desugarings-diagnostic-obligation)
argues from a hypothetical `Button { label: 42 }`. The artifact supplies the real
count: **nine generated functions** in one one-property component — `new`,
`constructor`, `mount`, `unmount`, two region updates, the `if-branch` pair, and
the click handler — none of which the user wrote, and every one of which is a
name a type error can be reported against. Cite it rather than the hypothetical.

**A risk for phase 4, found by writing phase 3's output.** The artifact's
allocator calls `min(old-size, new-size)`. `min` is registered
`vec![Ty::S32, Ty::S32] -> S32` (`yelc-sema/src/stdlib.rs:102–108`) and
`stdlib/num.yel:10` records why — *"Monomorphic on s32 today because there are no
numeric constraints"* — while the canonical ABI's sizes are **`u32`**. There is
no `u32` `min`, no constraint to write one generically
([§3](directions.md#3--generics-are-monomorphization-by-name) is what unblocks
that), and no coercion. **The first stdlib call in the first concrete lowering is
one the checker will reject.** Expect it; it is not a lowering bug.

#### ⚠️ Two things in this brief the artifact contradicts

Recorded rather than reconciled, per this directory's rule.

- **[`--emit-hir` says the dump must not round-trip](#yelc2---emit-hir--the-dump-is-a-deliverable-not-a-convenience);
  the artifact asserts the opposite** (*"the HIR dump must parse as yel"*). The
  brief's three supporting examples do not support it: `x = x + 1`,
  `Color.rgba((r,g,b,a))` and a flattened `else if` chain are **all valid yel**,
  so "a renderer emitting valid `.yel` would have to lie or refuse" does not
  follow from them. The real obstacles are five *parser* gaps, each measured
  against `yelc2 --emit-ast` in the artifact's README §1 — top-level `func`,
  `module M { }`, module-level mutable state, `@export`/`@import` absent from
  `KNOWN_ATTRIBUTES`, and no variadic syntax. That is a different argument with a
  different answer, and the decision should be re-taken on it.
- **[Provenance deliverable 4](#what-is-owed) cannot be written as specified.**
  It says to grep rendered diagnostics for "the generated-name prefix" and assert
  zero hits. There is no prefix: `__mount_*` and `__ui_*` appear **nowhere** in
  the frozen tree — they are invented in this brief's own example — and the
  back-end's actual scheme is `{comp}-{kind}` (`counter-mount`,
  `counter-if-update-b0`), which is shape-indistinguishable from a user-written
  name. Either this stage mandates a prefix for generated functions, or
  deliverable 4 needs a mechanism that does not depend on one.

## Decisions

**All decided 2026-07-29.** Reasoning in the [Decision log](#decision-log).

| # | decision | answer |
|---|---|---|
| D1 | Do bindings and handlers stay split? | **No — one uniform prop list**, classified from the *declared* prop type. ⚠️ Phase placement revised 2026-07-29. [log](#d1--bindings-and-handlers-are-one-uniform-prop-list) |
| D2 | `For.item_ty: Ty` on the node | **Remove.** [log](#d2--for-does-not-carry-the-item-type) |
| D3 | `For.item_name` *"stored directly to avoid LocalScope lookup issues"* | **Remove; fix the scope structure.** [log](#d3--for-does-not-carry-the-loop-variable-name) |
| D4 | Do globals get a body? | **No.** `HirGlobal` carries only its functions; defaults stay in `GlobalDef`. [log](#d4--hirglobal-has-no-body--only-its-functions) |
| D5 | Item and diagnostic ordering | ⚠️ **Globals then components** — reverses the frozen lowering order, and carries a measured obligation. [log](#d5--globals-lower-before-components) |
| D6 | Trivia / doc-comment attachment | **Attach** — nearest preceding comment run, no blank line. [log](#d6--doc-comments-attach-to-the-nearest-preceding-comment-run) |
| D7 | Flatten `else if` into nested `If`? | **Decided: yes** — [log](#d7--flatten-else-if-chains) |
| D8 | What identifies the compilation unit — one `SourceId`, or the file set? | **Decided: `PackageId` + `Vec<SourceId>`** — [log](#d8--a-package-is-identified-by-itself-not-by-a-file) |

### D1

Recommendation: **one uniform prop list.** Stage 1's AST already unified them
into `NamedProp { modifier, name, value }`; re-deriving two lists is an analysis
result on the node (B3). HIR *cannot* classify correctly anyway — whether
`bumped: { … }` is a handler depends on `bumped`'s declared type, which
~~[4](stage-4-hir-check.md) owns~~ ⚠️ **this stage owns** — it is a `Definitions`
lookup, not an inference ([F8](findings.md#f8), and
[the revision](#phase-placement-revised-2026-07-29--classification-is-a-table-lookup)).
The sentence is left because *"HIR cannot classify"* is the claim that moved, and
it moved without the **answer** moving. And the payload param
falls out: the frozen `HirHandler.param` exists so `drop: (payload) { … }` binds
a body-scoped local, which in the landed AST is just `ClosureExpr { params, body }`.

**Caveat, and it is the real work:** the frozen lowering *uses* the split to
decide scoping. A uniform lowering must produce the same locals in the same
order — `LocalId` ordinals reach the type checker, and `HirHandler`'s doc says
typeck re-defines the param "to produce the THIR `LocalId` with matching arena
parity." Verify local allocation order is unchanged before declaring D1 free.

## Contract

> ~~**Proposed.**~~ **Landed 2026-07-30** as `crates/yelc-hir`, types only, no
> lowering body ([`contract-before-fanout`](../../.agents/skills/compiler-rewrite/rules/contract-before-fanout.md)).
> A needed change is a request in [`seam-changes.md`](seam-changes.md).
>
> **3 owns this contract**; 4 assumes it and adds only the `types` map.

**Input:** `&[yelc_syntax::ParsedFile]` — the **whole file set** — plus
`&mut CompilerContext` (from `yelc-sema`: interner, `Definitions`, diagnostics).
**Output:** `HirModule` with `Definitions` typed and `types` empty.

⚠️ **Three of the declarations below are wrong, and landing them is how that was
found.** Two contradict each other *inside this block*. They are struck and
corrected in place; the reasoning is under
[What the seam could not be written as](#what-the-seam-could-not-be-written-as).

```rust
pub struct HirId(u32);          // distinct from syntax::NodeId and from DefId
pub struct BodyId(u32);

// ⚠️ WRONG — a `NodeId` is unique within ONE file, and this map spans the set.
// pub struct HirMap {
//     map:     FxHashMap<HirId, NodeId>,
//     rev_map: FxHashMap<NodeId, HirId>,
// }

/// A `NodeId` qualified by the file it was allocated in. `yelc-syntax`
/// allocates per file, starting at zero, so this pair — not `NodeId` — is what
/// identifies an AST node across the set `lower_files` is handed.
pub struct SourceNodeId { pub source: SourceId, pub node: NodeId }

pub struct HirMap {                       // ark hir_map.rs, both directions
    map:     FxHashMap<HirId, SourceNodeId>,
    rev_map: FxHashMap<SourceNodeId, HirId>,
}
impl HirMap {
    pub fn next_hir_id(&mut self, node: SourceNodeId) -> HirId;  // alloc + record
    pub fn node_of(&self, hir: HirId) -> Option<SourceNodeId>;
    pub fn hir_of(&self, node: SourceNodeId) -> Option<HirId>;
}

pub struct HirModule {         // ⚠️ the noun is questioned below — not renamed
    pub id:      PackageId,        // identity of the package, not of a file
    pub sources: Vec<SourceId>,   // the file *set* — see "Multiple files"
    pub items:   IndexVec<HirItemId, HirItem>,
    pub bodies:  IndexVec<BodyId, HirBody>,
    pub map:     HirMap,
    pub types:   NodeMap<Ty>,     // empty after 3, total after 4
}

/// Side table. One value per HirId, write-once.
pub struct NodeMap<V>;
impl<V> NodeMap<V> {
    pub fn insert(&mut self, id: HirId, v: V);   // asserts no prior value
    pub fn get(&self, id: HirId) -> Option<&V>;
}

/// Types are NOT re-represented. A HIR entity refers to the AST `TypeRef` it
/// was written as, by `NodeId`.
pub struct TypeId(SourceNodeId);   // ⚠️ was `TypeId(NodeId)` — same defect

/// ⚠️ NOT LANDED. Three things are unresolved and all three are contract:
///   - `&mut self` names no receiver, and the brief never says what owns it;
///   - the memo cannot be a `NodeMap<Ty>` — `NodeMap::insert` takes a `HirId`
///     and a `TypeId` is not one, which the two declarations above contradict
///     each other about, 20 lines apart;
///   - the DoD requires it "structurally unreachable from phase 1 (the
///     collector does not exist yet)", which is a claim about a type that is
///     never named.
/// The one syntax→`Ty` function: resolves `TypeKind::Named` against the
/// definition tables and interns. Memoized.
/// **Callable only after H1 phase 1.** Calling it earlier is F3, not a variant of it.
// pub fn type_of(&mut self, ty: TypeId) -> Ty;

/// Each of H1's three phases sweeps every file before the next begins.
pub fn lower_files(parsed: &[ParsedFile], ctx: &mut CompilerContext) -> HirModule;
```

### What the seam could not be written as

Found by landing it, 2026-07-30. Recorded here rather than fixed silently,
because each was wrong for a reason that generalises.

<a id="the-hirmap-key-is-not-a-nodeid"></a>
#### 1 · The `HirMap` key is not a `NodeId` — ark's shape does not transfer

`rev_map: FxHashMap<NodeId, HirId>` is copied from ark's `hir_map.rs`, which the
Brief cites as the model. It works **there** and cannot work **here**, and the
difference is one the citation hides:

| | ark | yel |
|---|---|---|
| `NodeId` allocation | one process-global `AtomicUsize` (`arkc-parser/src/parser.rs:28`) | **per file, from zero** (`yelc-syntax/src/lib.rs:47`) |
| so a `NodeId` is | unique across the compilation | unique **within one `ParsedFile`** |
| a `NodeId → HirId` map over N files | correct | **silently collides on every file after the first** |

Stage 1 chose per-file allocation *deliberately*, rejecting exactly ark's design:
a process-global counter makes a node's id depend on how many files were parsed
earlier, which is the determinism hazard
[A6](anti-spec.md#a6--no-random-seeded-iteration-reaching-output) forbids and
would make any golden containing node ids unstable. That was the right call. It
also means **the one ark construct this brief names as a model is the one that
had to change**, and nothing in either file said so.

Both directions are qualified, not just the reverse one: `node_of(hir)` returning
a bare `NodeId` would hand back a number the caller cannot interpret without
already knowing the file.

**Why it would not have been caught.** [H2](#h2) is *"the map is total and
bidirectional"*, asserted by `hir_of(node_of(h)) == h`. That round-trip **passes
under the collision** — the last writer wins in `rev_map`, and the forward map
still answers. It is [A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)
with a test attached: the property is real, the test is real, and neither can see
the bug. H2 has been restated below to be observable.

#### 2 · `type_of` has no receiver, and its memo has the wrong key space

Three defects, listed in the code block above. The middle one is the interesting
one: the same 20-line block declares `NodeMap::insert(&mut self, id: HirId, …)`
and *"`type_of` … memoized in a `NodeMap<Ty>`"* taking a `TypeId`. One of those
is wrong and the block does not say which — so `type_of` is **the one seam type
that did not land**, deliberately, rather than being landed under a guess.

What it needs before it can be: a decision on **what owns it**. The DoD line
*"structurally unreachable from H1 phase 1 (the collector does not exist yet)"*
already assumes an owner — a type constructed between phase 1 and phase 2 — and
that owner is what has never been named. Pick it and both other defects close:
the receiver is that type, and the memo is a field on it keyed by `TypeId`.

##### Decided 2026-07-30 — the owner is stage 3's lowering context, in `yelc-hir`

A struct in `yelc-hir` holding `&mut CompilerContext` plus the memo. `type_of` is
a method on it; the memo is keyed by **`TypeId`**, not a `NodeMap<Ty>` — a
`NodeMap` keys `HirId`, a different index space, and that mismatch was the
contradiction.

**`CompilerContext` was not an option**, which is worth stating because it is the
obvious home and the reason it fails is structural rather than a preference:
`type_of` looks up by `TypeId`, `TypeId` is `yelc-hir`'s, and `yelc-sema` sits
**below** `yelc-hir` in the crate graph. It cannot see the type it would have to
key on. Not a style call — a build error.

The third option, moving `TypeId` down into `yelc-sema` so the context could own
it, was rejected: it buys one convenience by giving `yelc-sema` knowledge of
syntax-tree numbering, which is the layer above it. That is the leak this crate
graph exists to make impossible, and today has been spent tidying instances of it.

**What it costs**, stated so it is not a surprise: only stage 3 can call it
directly. Stage 4 must be handed the context — one parameter — and the memo dies
with the lowering, so nothing carries stale entries between compilations. Both
are acceptable; the second is arguably a feature.

**This closes the gate on phase 3.** Phase 2 stopped here deliberately rather
than landing `type_of` under a guess, which was the right call — the guess would
have been `CompilerContext`, and it would not have compiled.

#### 3 · `HirModule` is the noun `ModuleId` → `PackageId` was renamed away from

Not corrected — **flagged**, because it is a rename and this file does not get to
make one unilaterally.

`fbaa95e` renamed `ModuleId` → `PackageId` on the grounds that *"the noun was one
level off"*: the thing compiled, versioned and serialized is the **package**, and
`module` was about to become a surface keyword meaning *WIT interface*
([`modules.md` §6](../modules.md)). `ModuleId` now means something else entirely
— an index into the symbol table's module arena, one node per `include`.

The identical argument applies to `HirModule`: it holds `id: PackageId`, it spans
`sources: Vec<SourceId>`, and [D8](#d8--a-package-is-identified-by-itself-not-by-a-file)
says it *is* the package. So three things want the word again, exactly as
`modules.md` §6 predicted, and one of them is this type. `HirPackage` is the
name the same reasoning produces.

It landed as `HirModule` because that is what the seam list said, and inventing
contract while implementing it is what phase 2 exists to prevent. **Decide before
phase 3 writes the lowering**, which is when the name becomes expensive to move.

### Designed for serialization — what stage 3 owes §6

[§6](directions.md#6--modules-are-serializable-artifacts) needs the **package** to
be writable and re-readable. ~~**None of that is implemented here**~~ — the format
landed on 2026-07-29 (`9a54ad1`), so what 3 owes is now *implementation*, below.
Swift's mechanism is the reference — see §6 for the `XREF` citation.

**The rule: two ID classes, and only one of them is ever written.**

| | internal | external |
|---|---|---|
| what | `DefId`, `HirId`, `BodyId`, `Ty` | `DefPath` |
| shape | dense index into this package's table | package id + a path of name pieces |
| resolved by | array index — O(1), used everywhere in-process | **lookup** in the target package |
| on mismatch | undefined behaviour, silently wrong | a **diagnostic**, loudly |
| serialized? | **never** | yes — this is the only thing that crosses |

⚠️ **The sketch that stood here is superseded on every line. Read `wire.rs`, not
this.** It is kept because two of its errors are instructive and are recorded
under *Two statements above are wrong* below.

```rust
// ⚠️ SUPERSEDED by `yelc_sema::{DefPath, artifact::wire::SerializedDefPath}`.
// Wrong four ways: `Namespace` no longer exists (`ca905d0`); there is no
// `PathPiece` type and never was; the `module` field is a `PackageId` where the
// landed wire form carries a `PackageName` (a `PackageId` is an index into the
// *consumer's* dependency list, which is the bug this whole section is about);
// and `overload` is an `OverloadKey`, not an `Option<OverloadKey>` — the empty
// key already means "unoverloadable", so the `Option` was a second way to spell
// the same absence.
//
// pub struct DefPath { pub module: PackageId, pub pieces: Vec<PathPiece> }
// pub enum PathPiece {
//     Item   { name: Name, ns: Namespace },
//     Member { name: Name, ns: Namespace, overload: Option<OverloadKey> },
// }

// What landed. Two representations, and the split is the point: `DefPath` is
// resolution-independent but still full of interner handles, so it is the
// *in-process* form; `SerializedDefPath` is the wire.
pub struct DefPath {                     // yelc-sema/src/ids.rs
    pub package:  Name,
    pub segments: Vec<Name>,
    pub overload: OverloadKey,           // `params: Vec<Ty>`; empty ⇒ unoverloadable
}

pub struct SerializedDefPath {           // yelc-sema/src/artifact/wire.rs
    pub package:  PackageName,           // ns:name@version, as strings
    pub kind:     DefKind,               // so the loader rebuilds the right thing
    pub segments: Vec<String>,
    pub overload: Vec<TypeIndex>,        // always empty — the loader cannot rebuild a set
}
```

**`kind` is not the old `Namespace` wearing a new name, and the difference
matters here.** A `Namespace` was part of the **key**: `Point` could be a record
*and* a component and a path needed the discriminator to say which. Single
namespace killed that job. `kind` survives for a different one — a loaded
definition has to be **rebuilt** as the right thing, and `register` takes a
`DefKind`. Same four values, opposite direction of use.

**Three things this pins down.**

1. **`Ty` is structural on the wire, a handle in memory.** Serialization writes
   the `TyKind` shape recursively (the type is `yelc_sema::TyKind`; this file
   called it `InternedTyKind` throughout and no such name landed); loading
   re-interns into the host interner. There is no `Ty` remap table, so there is
   none to forget.
   Swift: *"types are always serialized with enough info to regenerate them at
   load time."*
2. **`overload` exists because the prelude needs it.** No *user-written* yel can
   declare two `len`s — but [§3](directions.md#3--generics-are-monomorphization-by-name)
   keeps `len` as both `list<T> -> s32` and `string -> s32`, and the prelude is
   the motivating precompiled module. A name alone cannot name one of them. This
   is Swift's `XREF_VALUE_PATH_PIECE` carrying the type, and it is the **same
   key** §3 uses to resolve `(name, argument types)` — settle it once, use it
   twice.
3. **Only `export`ed items get a `DefPath`.** Yel is *simpler* than Swift here:
   Swift needs private discriminators because private decls are still
   referenceable from inlinable code. Yel has no cross-module inlining, so a
   non-exported item is unreferenceable across a boundary **by construction** —
   no discriminator needed, and an attempt is a lookup that fails rather than a
   name that collides.

**No longer deferred, and no longer stage 3's to design.** The serializer was
built on 2026-07-29 — `crates/yelc-sema/src/artifact/`, postcard, with the
version stamp. So this section's obligation changes shape, and it is now a
**deliverable with DoD lines**, not a design constraint to keep satisfiable:

> **Stage 3 implements two traits; it does not design a format.**
>
> ```rust
> impl ToArtifact   for HirNode { type Wire = …; fn to_artifact(&self, w: &mut ArtifactWriter<'_>) -> Self::Wire }
> impl FromArtifact for HirNode { type Wire = …; fn from_artifact(w: &Self::Wire, p: &LoadedPackage<'_>) -> Result<Self, LoadError> }
> ```
>
> The `Wire` type must contain no `Ty` and no `DefId`. The only way to obtain a
> wire value for either is the `ArtifactWriter` / `LoadedPackage` handed to those
> methods — a `Ty` becomes a `TypeIndex`, a `DefId` becomes a
> `SerializedDefPath` — so **implementing the trait is simultaneously the
> mechanism and the constraint**. Blanket impls cover `Option<T>` and `Vec<T>`.
>
> A `HirId` needs neither: the whole HIR travels together, so its ids only have
> to agree with themselves. The `types` `NodeMap` is a `Vec<(HirId, TypeIndex)>`
> written in `HirId` order — a map derived from a hash map must be sorted before
> it reaches bytes ([A6](anti-spec.md)), and serialized bytes are output.
>
> Then bump `Stamp::FORMAT`. postcard writes struct fields by position and enum
> variants by index, so *any* shape change in `artifact::wire` is invisible in
> the bytes and must move the stamp by hand.

**Two things the round-trip must be tested against, both already established by
`yelc-sema`'s own suite and both easy to lose.** They are stated here because
stage 3 writes the *second* set of impls and will be tempted to copy the first
set's tests:

1. **Load into a differently populated interner.** A same-interner round trip
   passes whether or not `Ty` was written structurally, so it proves nothing.
   `yelc-sema` keeps that control in the suite under its own name; stage 3's
   HIR round trip needs the same asymmetry or it is vacuous.
2. **`SourceNodeId` is a wire hazard the trait cannot catch.** `SourceId`
   *does* derive `Serialize` in `yelc-base` — the asymmetry `wire.rs` records —
   so a `SourceNodeId` in a `Wire` type compiles and writes an index into the
   **producer's** `SourceMap`. `HirMap` is the obvious thing to want in an
   artifact and it is made entirely of that hazard. Decide whether the map
   crosses at all; if it does, it needs the treatment `Ty` got, and the type
   system will not help.

Still deferred: the lazy-load offset table (Swift's index block — the flat
`Artifact` struct forecloses it, and reopening costs a `format` bump), and an
input hash, which `directions.md` §6's envelope has and §6.6's stamp does not.

**Two statements above are wrong, found by building it** — see
[`seam-changes.md`](seam-changes.md), 2026-07-29:

1. **"There is no `Ty` remap table, so there is none to forget."** There is one.
   Re-interning a recursive structure needs each already-converted entry to be
   addressable by its children, so the load side builds artifact-index → `Ty`
   and resolves every reference through it. The claim describes the *format*
   (no handles on the wire) and denies the *loader*, which are different things.
2. **The `DefPath` sketched above is not writable.** `Name` and `Ty` are both
   interner indices; a `DefPath` holding them has the exact problem it exists to
   solve. The wire form uses `String` segments and type-table indices.

~~**One trap is already armed in this crate's dependencies.** `Ty` is
`pub struct Ty(pub u32)` and **already derives `Serialize`/`Deserialize`**~~ —
⚠️ **disarmed 2026-07-29 (B1).** `yelc_sema::Ty` deliberately does **not** derive
either, so writing a handle is now a *type error* rather than a review finding.
The sentence was true of the frozen `yel-core/src/types/interner.rs:13` and is
false of the crate stage 3 actually depends on.

**The trap that is still armed is `Name` and `SourceId`.** Both derive
`Serialize` in `yelc-base`, so for those the rule rests entirely on
`artifact::wire` being the only place wire types are declared. That asymmetry is
recorded in [`seam-changes.md`](seam-changes.md) and is the reason the
`SourceNodeId` note above exists. What is *not* deferred is the `DefId`/`DefPath`
split, because retrofitting it means touching every downstream consumer that
holds a `DefId`.

### Why there is no `ParsedType`

An earlier draft proposed a `ParsedType` enum mirroring the AST with an
`OnceCell<DefId>`. Wrong three ways:

1. **Incomplete mirror** — `ast::TypeKind` has four compound types
   ([F2](findings.md#f2)); a hand-written re-listing kept one.
2. **It would have narrowed `Result`** — stage 1 stores `args` *as written*
   because `result<a,b,c>` is real input (S5). An `{ok, err}` re-representation
   reintroduces exactly the narrowing stage 1 avoided.
3. **The `OnceCell` was a B3 violation** — a resolution result on the node it
   describes, mutable after construction. ark's `ty.rs` (the actual reference;
   there is no `parsety.rs`) has no cell either.

**So HIR does not re-represent types.** It refers to the AST `TypeRef` by
`NodeId`, and one memoized `type_of` does resolution and interning together. A
duplicated enum **is** the "second tree" the stub said to avoid.

### How anything becomes typed

| | typed by | mechanism |
|---|---|---|
| **declared types** — fields, property types, params/returns, variant payloads | **3, phase 2** | `type_of` → `Ty` into the definition tables |
| **expressions** | **[4](stage-4-hir-check.md)** | bidirectional inference → `types` map |

The definition tables carry real `Ty` and always have ([F5](findings.md#f5)) —
rustc's `type_of(def_id)`-before-body-check split, not a deviation. **The frozen
bug is phase ordering only** ([F3](findings.md#f3)): same function, same output
type, wrong moment.

Keeping the written syntax around exactly long enough for there to be something
to resolve against is the whole point of not re-representing types, and is what
makes [H4](#h4) achievable rather than aspirational.

### Multiple files

This phase merges them. **There are no includes *yet*:** `ItemKind` has no
`Import` variant, and `ExternComponent` declares a component implemented
*elsewhere* — an import **contract**.

⚠️ **Corrected 2026-07-30 in two ways.**

1. ~~`LANGUAGE.md` has no `import`/`use`~~ — `include` and `use` are **designed**
   ([`modules.md` §4.1](../modules.md)), with `use` taking WIT's own grammar
   verbatim. They do not parse, so nothing lowers them here; the reason to say so
   is the *open question attached to them*, which is stage 3's:
   **does `include` name a package or a module?** [`modules.md`
   §7](../modules.md) is explicit — *"not decided; decide before HIR is built on
   it."* The symbol table already has the two-level shape either answer needs
   (`Sym::Module(ModuleId)`, `bind_in_module`), and **nothing populates it**.
2. ~~Multi-file means the files on the command line~~ — a **package is a
   directory** ([`modules.md` §4](../modules.md), matching WIT's and Go's rule),
   its dependencies are vendored in `deps/`, and there is no manifest. The
   file-oriented CLI (`yelc2 [OPTIONS] <FILE>`) cannot name that unit;
   §6.5 records this as *"a consequence for stage 2 that nobody recorded"* and
   the signature becomes `yelc2 build [./dir]`. Not urgent — nothing consumes
   packages yet — but `lower_files(&[ParsedFile])` is the seam that will be
   handed a directory's files, not a command line's.

The frozen driver merges fully-lowered files inside a loop, so cross-file
references resolve in one direction only ([F4](findings.md#f4)). The fix is H1's
phases sweeping the file set. This does not conflict with
[D1](anti-spec.md#d1--the-compilation-unit-is-the-file-not-the-component), which
is about one *item spine* for globals and components, not about file count. The
unit of *parsing* stays the file; the unit of *resolution* is the file set.

**The divergence is the cheap kind:** a strict widening. Programs that fail on
argument order begin compiling; nothing that compiles today changes output. Two
caveats: the **corpus proves nothing** (yel-smith generates single files;
multi-file is effectively untested — needs new fixtures in both orders,
[A14](anti-spec.md#a14--test-inputs-are-verified-present-not-merely-counted)),
and **diagnostic order moves for multi-file inputs** (per-file interleaving
becomes phase-major; single-file inputs — the entire corpus — are unaffected).

Also decide: the frozen loop bails *inside* the loop, so a broken first file
means the second is never parsed and its diagnostics never appear — accumulate-
and-continue violated at file level. Phase-major sweeping fixes it for free, but
it is a separate observable change.

### Invariants this phase ESTABLISHES

<a id="h1"></a>
**H1 · Register names, collect types, lower bodies — three phases, not two.**
Each sweeps **every file** before the next begins.

| phase | does, across all files | may not |
|---|---|---|
| 1 · register | a `DefId` + name for every item | call `type_of` — no name is guaranteed to exist |
| 2 · collect | `type_of` every **declared** type into the definition tables | look at any body |
| 3 · lower | lower bodies; expressions stay untyped until 4 | register new items |

A body may reference any item regardless of source order, **and so may a declared
type, and so may either across file boundaries.** *Asserted by* two fixtures: a
record whose field is typed as a record declared **last** in the same file, and a
component referencing a record declared in a file passed **second**. Both fail
today ([F3](findings.md#f3), [F4](findings.md#f4)).

<a id="h2"></a>
**H2 · The `HirId ↔ SourceNodeId` map is total, bidirectional and injective.**
Every `HirId` maps to an AST node present in the input, `hir_of(node_of(h)) == h`,
**and no two `HirId`s map to the same node**. *Asserted by* a walk over every
corpus program, and by a **multi-file** fixture — single-file inputs cannot
observe the third clause.

⚠️ **Restated 2026-07-30, and the addition is the point.** As originally written
(*"maps to a `NodeId`"*, round-trip only) H2 **passes under the defect that
motivated the restatement**: with a per-file `NodeId` as the reverse key, file 2's
node 7 overwrites file 1's, the last writer wins, and `hir_of(node_of(h)) == h`
still holds for the survivor. The round trip is a real property asserted by a real
test that cannot see a real bug — [A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)
in its most convincing form. Injectivity plus a multi-file input is what makes it
observable; see [the contract](#the-hirmap-key-is-not-a-nodeid).

**H3 · No analysis result on the node it describes.** No node field is written by
a later pass — no `Ty`, no capture set, no resolution outcome. Those are
`NodeMap`s, including `types`. *Asserted by* the absence of a `Ty` field on any
node type.

<a id="h4"></a>
**H4 · An unresolved name is unresolved.** Never interned as `Unknown` and hoped
over. *Asserted by* a record field of user-record type resolving to that record's
`DefId` — which fails against the frozen behaviour.

**H5 · Nothing is silently dropped.** Every AST item, member, node, and
`Recovered::Missing` produces a HIR entity or a diagnostic. No `_ => {}` arm in
lowering. *Asserted by* exhaustive matches and a fixture per `Error` variant.
Carries stage 1's S5 forward — the invariant the frozen parser broke for its
entire life.

## The desugaring's diagnostic obligation

**Binding, not aspirational.** The UI tree is lowered to functions and calls here,
which means every error about a UI construct is reported against code the user
never wrote. Preserving spans is not sufficient — it gets the *location* right and
the *sentence* wrong.

```
Button { label: 42 }        ⟶   __mount_button(label: 42)

  what the checker sees:  argument 1 of `__mount_button` expects string, found s32
  what the user needs:    property `label` on `Button` expects string, found s32
```

Same span. One of those names a function nobody wrote.

This is a known tax on desugaring early, not a yel problem: rustc added
`DesugaringKind` precisely because desugared `for` / `?` / `await` produced errors
phrased in terms of generated code. It is payable, and the price is that the
lowering records **provenance**, not just spans.

### Why this is an obligation rather than a quality goal

**The oracle is silent here.** Of the 23 diagnostic fixtures, **not one is both
UI-shaped and type-level** — the UI-shaped ones (`children_no_slot`,
`duplicate_children`, `invalid_value_binding`, `recursive_instantiation`) are
structural or resolution checks, and every type-level one is about expressions,
records, enums or numbers.

So this stage can lower the UI tree, regress **every** UI type-error message to
name generated functions, and the entire frozen suite stays green. The
differential cannot see it either — both compilers accept the same programs, and
these are programs that compile. There is no measurement that fails.

That is the exact shape [A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)
exists for, so it is written as a deliverable with tests attached rather than
left to review.

### What is owed

1. **Provenance recorded for every generated node**, by the lowering that
   generates it — element, property, handler, child, `for` region, match arm.
   Not reconstructed downstream; the lowering is the only place that knows.
2. **Diagnostics name the written construct.** A type error on a UI property says
   *property `X` on `Y`*. No generated function name (`__mount_*`, `__ui_*`)
   appears in any user-visible message, ever.
3. **New fixtures**, because none exist: at minimum a property type mismatch, a
   handler signature mismatch, and a `for`-bound item used at the wrong type.
   These are additions to the diagnostic corpus, which is a **gain** in oracle
   coverage rather than a golden change — nothing existing is re-blessed
   ([`oracle-never-rebless`](../../.agents/skills/compiler-rewrite/rules/oracle-never-rebless.md)).
4. **A test that no generated identifier reaches a diagnostic.** Grep the rendered
   output of the whole diagnostic corpus for the generated-name prefix and assert
   zero hits. Cheap, and it fails loudly the first time someone adds a lowering
   that forgets provenance.

### Open sub-decision · where provenance lives

Not settled; decide before the lowering is written, because it is the lowering
that populates it.

- **On `Span`** — rustc's choice: an interned context id, propagating
  automatically, so the *renderer* rewords without every emit site knowing.
  Costs a `u32` on a type copied everywhere.
- **On `SourceMap`** — it already maps spans to source text; mapping spans to
  provenance is the same shape, and `Diagnostic::with_span` stays unchanged.
- **A `NodeMap<Origin>` side table** — cheapest, and already this stage's idiom
  ([B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)), but
  every emit site must look it up, which means every emit site must remember to.

The first two put the rewording in one place; the third distributes it. Prefer
one place — a diagnostic obligation discharged at N sites is discharged at N−1
sites within a year.

**Note this is provenance, not analysis.** B3 forbids storing an *analysis
result* on the node it describes. Where a node came from is established at
construction and never recomputed, so it is not the shape B3 is about.

### Re-checked 2026-07-30 against D1's phase-placement revision — it still reads

This section was written (`046ff17`) alongside the revision and **presupposes**
it: *"the UI tree is lowered to functions and calls here"* is only true because
classification moved into this stage. Nothing in it needs correcting, and the
rest of the brief has now been moved to agree with it rather than the other way
round.

Two clauses gain force from the move, worth naming so they are not read as
softer than they are:

- **"Every error about a UI construct"** now means *every* one, not the type-level
  subset. Phase 4 sees only generated calls, so a UI diagnostic can only be
  phrased correctly from recorded provenance — there is no fallback path where
  the checker still has the element in hand.
- **The `for` region** in *what is owed* item 1 is the
  [binder exception](#the-one-exception-binders). Its provenance is recorded at
  construction in phase 3 while its binder's *type* arrives in phase 4, so the
  two halves of one diagnostic are established in different phases. That is the
  case most likely to be got wrong, and it is the third of the three new fixtures.

## Verification

**3 has no artifact of its own** ([F14](findings.md#f14)) — and pretending
otherwise is the failure mode this section prevents. What is comparable,
strongest first:

1. **The `Definitions` table** — contents **and order**, since `DefId`s are
   ordinals that reach output ordering. Shape-independent, so it works across two
   different IR designs. Compare via a **read-only oracle harness** that depends
   on frozen `yel-core` as a `dev-dependency` and drives it: this reads the frozen
   tree, does not edit it, and is allowed by
   [`greenfield-never-touch-old-code`](../../.agents/skills/compiler-rewrite/rules/greenfield-never-touch-old-code.md).
   ⚠️ **Not a "new crate" — the pattern already exists twice**, in
   `yelc-syntax/tests/parity.rs` and `yelc-sema/tests/single_namespace.rs`, both
   as `dev-dependencies` that vanish at cutover phase 4
   ([A4](anti-spec.md#a4--no-permanent-bridge)). A third crate would be a third
   thing to delete.

   ⚠️ **The comparison needs a stated mapping, and one carve-out.** The frozen
   table keys `(Name, Namespace)`; ours keys `Name` and tags `DefKind`. The four
   values line up one-to-one, so the mapping is mechanical — but the **30
   cross-kind programs** enumerated in `single_namespace.rs` are *deliberately*
   not comparable, because we reject what the frozen tree accepts
   ([`scope.md`](scope.md), 2026-07-29). No corpus program contains one
   (measured: not one of 2117 checked-in `.yel` files reuses a top-level name
   across kinds), so the differential is unaffected in practice — which is
   exactly why it must be written down rather than discovered as a mismatch.
2. **Diagnostics** — meaning, span, **and order** (D5) — over the 2000-seed
   corpus, 91 positive and 23 diagnostic fixtures, via frozen `yelc check`.
   ⚠️ **90 positive**, not 91, since `1d12250` (phase 0) moved
   `global_filter_default.yel` to `known_bugs/`. Both numbers appear in this
   file; the DoD has been corrected too.
3. **No panic, total lowering** over the corpus.

### `yelc2 --emit-hir` — the dump is a deliverable, not a convenience

The driver already emits `--emit-ast`, `--emit-green`, `--emit-green-text`; the
established pattern is one `--emit-<ir>` per stage, added as a line in the
straight-line `run()`. **`--emit-hir` lands with this phase.**

**Render it yel-flavoured, like rustc's MIR dump — and do not make it
round-trippable.** MIR dumps look like Rust and are not Rust; the same applies
here, for a concrete reason: HIR is *post-desugaring*. `x += 1` is already
`x = x + 1`, `#ff0000` is already `Color.rgba(…)`, an `else if` chain is already
nested ([D7](#d7--flatten-else-if-chains)). A renderer emitting valid `.yel`
would have to either lie about the input or refuse.

**The round-trip need is already met one layer up** — stage 1's S1 guarantees
`green.text() == content`, and `--emit-green-text` exposes it. That frees this
dump to be *readable* instead of *faithful*, which is the more useful of the two.

**What it must show is everything source cannot** — the analysis, not the syntax:

| | |
|---|---|
| resolved names | `count#12` — the `DefId` the name bound to |
| declared types | from H1 phase 2; expressions stay untyped until 4 |
| desugarings, made visible | the five in [What lowerings belong here](#what-lowerings-belong-here) |
| dependency sets | `thir/signalck.rs` is the model |
| trigger kind | [§5](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger) |
| capture sets | [§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one) |

Every analysis this brief argues for becomes **observable and testable** the
moment it is in the dump. That is most of the reason to build it early.

> **It is a change-detector, not an oracle.** A golden HIR dump is a snapshot of
> the *new* compiler, so it can tell you something moved — it can never tell you
> the output is *correct*.
> [`oracle-never-rebless`](../../.agents/skills/compiler-rewrite/rules/oracle-never-rebless.md)
> applies to the frozen artifacts and the 85 execution tests; it does not make a
> HIR snapshot into an oracle. Keep the two straight or the suite goes green while
> the compiler is wrong.

The dump is output, so [A6](anti-spec.md#a6--no-random-seeded-iteration-reaching-output)
applies: stable ordering, byte-identical across runs.

The artifact-level differential arrives after
[4](stage-4-hir-check.md#verification).

## Gate

1. ~~Stage 1 closed out~~ ✅ `33e5c71`, 2026-07-28.
2. ~~**D1–D6 answered in writing**~~ ✅ 2026-07-29. D7 and D8 were already
   decided. **D5 carries an obligation into the stage** — the corpus item-order
   differential; see its log entry.
3. ~~**Cluster A answered**~~ — [`open-decisions.md`](open-decisions.md#cluster-a--type-representation).
   ✅ **answered in full 2026-07-29** (A1 monomorphization by type, A2 unification
   without generalization, A3 `Param`, A4 `Infer`), and recorded in this file's
   own [S7](#s7--does-ty-gain-a-non-concrete-variant). The banner at the top of
   this file claimed it was open for a day after S7 said otherwise — the two
   statements were 700 lines apart and neither knew about the other, which is the
   argument for one status line rather than two.
4. ~~**Phase 2 landed**~~ ✅ 2026-07-30, `crates/yelc-hir`, types only.
   `type_of` was the exception and the residual gate; **closed 2026-07-30**
   when its owner was named — stage 3's lowering context, in `yelc-hir`
   ([decision](#decided-2026-07-30--the-owner-is-stage-3s-lowering-context-in-yelc-hir)).
   The code lands with phase 3, deliberately: the owner is a type phase 3
   constructs.
5. ~~**`include`'s level decided**~~ ✅ **2026-07-30** — an `include` names a
   **module**, and the question dissolved rather than being answered directly:
   the Go model ([`modules.md`](../modules.md), *"every file declares, and
   disagreement is an error"*) makes the package the directory, so a file has no
   top module of its own for `include` to mean. §4.1's reading stands, now with
   the mechanism (`E0071`/`E0072`, `yelc-hir/src/packageck.rs`) landed and tested
   ahead of the lowering.

That is the whole gate. Everything that used to sit here is now *work*, below.

## Work in scope

**Changed 2026-07-29.** Four items were previously prerequisites — things that had
to land before the stage could be briefed. They are now **phases of the stage
itself**. The stage is bigger and there are fewer handoffs; the ordering
constraints that made them prerequisites still hold, and are stated per phase.

Phases run in order. Phase 0 must complete before any number is taken.

### Phase 0 · Oracle hygiene — ✅ **done 2026-07-29** (`1d12250`)

1. ~~**`global_filter_default.yel` resolved.**~~ ✅ **It could not be re-blessed.**
   Corrected to `{ x -> x > 2 }` the program **panics the frozen compiler** at
   `hir/local_scope.rs:73`, so there is no output to bless. It moved to
   `known_bugs/` instead, and the panic is scoped by experiment: the same closure
   is fine in a component property default and in a global *function* body, and
   panics only in a **global property default** — exactly the path the fixture
   claimed to guard. Positive fixtures 91 → 90; no coverage lost, `.filter` is
   exercised by three other positive fixtures. Full log in
   [`goldens-changed.md`](goldens-changed.md); new baseline row `1d12250` in
   [`ratchet.md`](ratchet.md).

2. ~~**The two silent `_ => {}` arms** filed as a `known_bugs` entry.~~
   ✅ **Filed, with a new harness.** They are an **under**-rejection — the frozen
   parser *accepts* a `global` body containing garbage and drops the member — and
   the existing `.failure` harness asserts compilation **fails**, so a fixture
   there would compile cleanly and report *"the bug appears to be fixed,
   graduate this to positive/"*.

   **I first concluded no fixture was owed. That was the wrong call**, and the
   reasoning shows why: I checked whether the bug fit the harness, found it did
   not, and stopped — instead of noticing that `known_bugs/runtime/` already
   establishes the pattern for "compiles cleanly but is wrong" (a subdirectory
   the non-recursing lister cannot see, plus its own test). The gap was fillable
   in about sixty lines.

   What landed: `known_bugs/silent_discard/global_member.yel` +
   `.dropped`, and `known_bugs_silently_discarded_members` in
   `tests/integration.rs`. Each `.dropped` line names an identifier the **source
   writes** and the **AST must not contain**; the harness additionally rejects a
   `.dropped` entry absent from the source, so a typo cannot make the assertion
   vacuously pass. Mutation-checked in both directions before landing — making
   the member valid trips *"IS present in the AST"*, and a bogus `.dropped` entry
   trips *"as written this assertion is vacuous"*.

   The `yelc-syntax` record stands alongside it and is still the stronger of the
   two: `support::catch_all::DIVERGENCES` lists 18 cases and
   `explains_our_report` proves each **causally** — it excises exactly the bytes
   the frozen parser discarded, re-parses, and requires our diagnostic to
   disappear. The fixture pins the bug in the frozen tree where a reader of that
   tree will find it; the divergence list pins it against the new parser.

**The ordering constraint that made this a prerequisite held, and cost nothing.**
Both items were done before any 3 measurement. The corpus did not need
regenerating: `git status --porcelain` over every frozen `src/` and `Cargo.*`
came back empty, so the frozen binary is byte-identical and the oracle cannot
have moved.

<a id="phase-1--yelc-sema-3536-lines"></a>

### Phase 1 · `yelc-sema` — ✅ **landed 2026-07-29** (`9a54ad1`, `ca905d0`, `fbaa95e`, `bbe6cfa`)

**3,314 lines**, against the ~3,536 estimated below — and the comparison is not
like-for-like in either direction, which is worth stating because a line count
that happens to match invites the reading that the file was ported.

| | frozen | landed | |
|---|---|---|---|
| builtins | `stdlib_lookup.rs` + `known.rs` = **1,442** | `builtins.rs` + `stdlib.rs` + `known.rs` = **722** | S1/C2: one table, two accessors, plus resolved lang-items |
| the rest | `context.rs` 963 · `definitions.rs` 742 · `types/interner.rs` 389 | `context.rs` 174 · `definitions.rs` 823 · `types.rs` 270 · `ids.rs` 151 | the god object shed 789 lines; the symbol table **grew**, because it now holds two levels and an overload set |
| — | — | `artifact/` **+1,096** | not in the frozen inventory at all — the format did not exist |

**What landed that this section did not anticipate**, and phase 3 depends on all
three: the **package artifact** (`artifact.rs` + `wire`/`write`/`load`/
`encoding`), the **two-level symbol table** (`Sym`, `Module`, `bind_in_module`),
and the **single-namespace narrowing**, which is the rewrite's first
non-additive surface break.

Was a separate landing (`stage-3-hir-build.md`, now merged here). Now this stage's
first build phase; everything that brief said lives below. Frozen equivalent, minus what `yelc-base` already carries:

| frozen file | lines |
|---|---|
| `context.rs` | 963 |
| `stdlib_lookup.rs` | 1,029 |
| `definitions.rs` | 742 |
| `known.rs` | 413 |
| `types/interner.rs` | 389 |
| **total** | **~3,536** |

`ids.rs`, `index_vec.rs`, `interner.rs`, `source.rs`, `diagnostic.rs` are
**already done** in `yelc-base`.

It transforms no IR, but it carries real design decisions —
[§1](directions.md#1--builtins-are-a-table-not-a-field-per-builtin) lives
entirely inside it, as do the [`DefId`/`DefPath` split](#designed-for-serialization--what-stage-3-owes-6)
and `Ty`'s structural serialization. ~~Its open questions are Clusters A–D of
[`open-decisions.md`](open-decisions.md), and **Cluster A gates the phase**.~~
✅ **All of A–D were answered 2026-07-29**; the only open decision left in that
worksheet is F1, which is stage 4's.

**Take its checkpoint even though it is no longer a landing.**
`lookup_known_definitions` registers builtins from *no input at all*, so the
resulting `Definitions` table is comparable against the frozen one before a
single source file is parsed. That was the argument for giving sema its own
ratchet row, and folding it into 3 does not make the checkpoint less real — it
makes it easier to skip. Compare the table, and record the result in Numbers.

⚠️ **Still owed. Phase 1 landed without it**, which is exactly the risk this
paragraph named — *"folding it into 3 makes it easier to skip"* — and then the
fold happened and it was skipped. It is not blocked on anything: `BuiltinTable`
is populated from Rust, the frozen `KnownDefinitions` is reachable from a
`dev-dependency`, and the comparison needs no source file. Take it before
phase 3's first measurement.

#### `context.rs` is a cross-pipeline god object and cannot be ported

Measured: **40 functions, 7 of them LIR-flavoured**, plus this state —

```rust
block_names:                RefCell<HashMap<(DefId, BlockId), BlockDebugName>>,
block_id_counter:           Cell<u32>,
component_lifecycle_blocks: RefCell<HashMap<DefId, ComponentLifecycleBlocks>>,
// + the per-(observer, global signal) fanout block table
```

That is **LIR state living on the shared context** because it was convenient —
[A1](anti-spec.md#a1--no-side-channel-ir) and
[A2](anti-spec.md#a2--no-god-pass).

It is also a hard crate-graph error. `BlockId`, `BlockDebugName` and
`ComponentLifecycleBlocks` are `yelc-lir` types, so porting them here creates
`sema → lir`, which the [dependency graph](README.md) forbids:

```
base ← sema ← hir, lower          sema must NOT reach lir
base ← lir  ← lower, codegen
```

**So the LIR half is not descoped for tidiness — it does not compile here.**
It belongs to [5](stage-5-lir.md) (the ids and their allocation) and
[6](stage-6-lower.md) (the lifecycle and fanout tables, which are lowering
bookkeeping). That is a note *for those briefs*, and it is written down here
because this is where someone will first notice.

#### Decisions this phase must make

Numbered `S` so they do not collide with this stage's `D`.

Answers come from [`open-decisions.md`](open-decisions.md); this table is the
record.

| # | decision | status |
|---|---|---|
| S1 | Adopt [§1](directions.md#1--builtins-are-a-table-not-a-field-per-builtin)'s builtin table? | ✅ **yes** — one table, replacing `stdlib_lookup.rs` + `known.rs` (C1, 2026-07-29) |
| S2 | How does `Ty` serialize? | ~~⬜ open~~ ✅ **structurally, and the derive is deleted** (B1, 2026-07-29); **tested** by `artifact/` since `9a54ad1` |
| S3 | Does `known.rs` survive? | ~~⬜ open~~ ✅ **yes, as resolved lang-items holding `DefId` not `Option<DefId>`** (C2, 2026-07-29) |
| S4 | What stays on `CompilerContext`? | ~~⬜ open~~ ✅ **six fields, plus `known` as a projection** (D0/D0a, 2026-07-29) |
| S5 | `DefId` shape, given `DefPath` | ✅ **package-qualified from day one** — `DefId { package, index }` (B2, 2026-07-29). ⚠️ Written `{ module, index }` here and landed as `{ package, index }`: `fbaa95e` renamed the level |
| S8 | One namespace, or four? | ✅ **one** (`ca905d0`) — **not in this table when the table was written**, and the largest thing phase 1 decided: `Definitions` keys by `Name` alone, so a record and a component may no longer share a name. The rewrite's first non-additive surface break; ledger entry in [`scope.md`](scope.md) |
| S6 | Who owns `OverloadKey` — here or 4? | ✅ **here** — one key, consumed by `DefPath` and §3's mangling (B3, 2026-07-29) |
| S7 | Does `Ty` gain a non-concrete variant? | ✅ **yes — both** `Param` *and* `Infer` (A3, A4, 2026-07-29). **Reverses this file's recommendation** — see below |

### S1 · Adopt the builtin table (§1)

`stdlib_lookup.rs` (1,029 lines) and `known.rs` (413) implement one builtin as
four things that must agree and are checked by nothing
([F12](findings.md#f12)). §1 replaces them with one row per builtin:
`name → { arity, type scheme, lowering target }`.

This is the single largest simplification available to this crate, and it is
**not** blocked on [§2](directions.md#2--the-stdlib-is-yel-source-embedded-in-the-binary)
(the source stdlib) — the table is populated from Rust now, and §2 later swaps
*where the rows come from*, not what they are.

Open, from §1: one table or two projections · do builtin **elements** belong in
it (`KnownElements` is 15 fields of UI vocabulary with no lowering target) ·
variadics (`concat` is registered with an empty param list and a comment saying
it is really variadic).

**Decided 2026-07-29.**

**C1a — one table, two accessors.** Not two tables with a key-alignment test.
[F12](findings.md#f12) is exactly the failure of *"four things that must agree,
checked by nothing"*; replacing four unchecked things with two unchecked things
is the same bug at smaller scale, and the alignment test is the part that rots
first because it passes for years. One row, two accessors makes misalignment
**unrepresentable** rather than tested. The constraint that `yelc-lir` must see
neither is already enforced by the crate graph — lir has no dependency path to
sema, so it cannot name either accessor — so it does not need to be paid for in
table shape.

**C1b — no, builtin elements do not go in it.** The row is
`{ arity, type scheme, lowering target }`. An element has no arity, no type
scheme in that sense, and no lowering target — three dead columns for every
element row. A table whose columns are meaningless for half its rows is two
tables sharing a name. They go to C2's home.

**C1c — arity gains a variadic form.** `concat` is genuinely variadic; the frozen
registration says so in a comment it cannot enforce
(`stdlib_lookup.rs:293`, `// concat: func(string...) -> string`). Interpolation
desugars to `concat` with **one argument per part**, and a 10-part interpolation
compiles today — so "N fixed arities" has no principled N, and the N+1 case would
fail on a call **the user never wrote**, since the desugaring is compiler-
generated. The diagnostic would name `concat` at a source position containing a
string literal. So: `Arity::{ Fixed(n), Variadic { min, element } }`, and the
table can state the property instead of commenting it.

### S2 · `Ty` must not serialize as its handle

`pub struct Ty(pub u32)` **already derives `Serialize`/`Deserialize`**
(`types/interner.rs:13`). Carrying that derive forward means every struct
containing a `Ty` silently writes an **interner index** — the exact bug
[§6](directions.md#6--modules-are-serializable-artifacts) exists to prevent, and
one that compiles perfectly.

Required: serialized positions write the type's *structure*; loading re-interns.
Swift's rule — *"types are always serialized with enough info to regenerate them
at load time"*. The cheapest enforcement is to **not derive `Serialize` on `Ty`
at all** and make the structural writer the only path, so the wrong thing does
not typecheck.

**Decided 2026-07-29 (B1): structurally, and delete the derive.**

Deleting `Serialize`/`Deserialize` from `Ty` is the decision, not merely the
recommended enforcement. Keeping the derive "for debug use" leaves a path where
the wrong thing compiles; removing it makes writing a `Ty` handle a **type
error**, which is the only version of this that survives contact with a
contributor who has not read this file.

What it costs, stated so it is not a surprise: any inspection path that wanted a
cheap `Ty` dump now goes through the structural writer. That is slower and it is
correct, and `yelc2` is where such a dump belongs anyway.

### S3 · Does `known` survive at all?

`KnownDefinitions` is five sub-structs of `Option<DefId>`. S1 removes the
*functions* half outright. What is left is the question of builtin **elements**,
**enums** and **variants** — UI vocabulary with no lowering target, which may
genuinely want a different home than a call table. Decide; do not port 413 lines
because they exist.

Note the `Option` is load-bearing nowhere: every read is an unwrap-or-diagnostic
for a case that cannot occur once registration has run
([A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)).

**Decided 2026-07-29 (C2): a separate home, shaped as resolved lang-items.**

Neither "same table" (wrong shape — see C1b) nor "delete". Delete is tempting and
wrong: these **are** registered in `Definitions`, which is how ordinary name
lookup finds them, but the compiler *itself* also needs to say "the `Color`
record" or "the `option` variant" while lowering. A cache of the `DefId`s the
compiler references by name is rustc's `lang_items`, and it is a real pattern
rather than inherited clutter.

**The defect to fix is the `Option`, not the existence.** There are **47**
`Option<DefId>` fields across the six `Known*` structs. Every read is an
unwrap-or-diagnostic for a case that cannot occur once registration has run
([A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)). So the new home
holds `DefId`, resolved once at construction, failing **there** if a builtin is
missing — one assertion at the point the invariant is established, instead of 47
re-checks at points that cannot observe it.

### S4 · What stays on the context

Keep-list §5 keeps context *threading*, not the god object. The test for each
field: **is it produced and consumed within sema, or is it a later stage's state
parked here?** The LIR fields fail that test and cannot compile here anyway.

Watch for the same shape re-forming: `signal_deps` keyed by `DefId` is cited as
the *positive* precedent for side tables
([B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)) — but
it is reactivity analysis, which is a frontend concern, not a sema one. Decide
where it lives rather than inheriting its address.

**Decided 2026-07-29.**

**D0 — the six fields:** interner, type interner, `Definitions`, the builtin
table, source map, diagnostics. Nothing else. The LIR fields
(`block_id_counter`, `block_names`, `component_lifecycle_blocks`, the fanout
table) are settled by the crate graph rather than by preference — `sema → lir` is
forbidden, so they **cannot compile here**.

**D0a — `signal_deps` moves to `yelc-hir`.** It is cited as the positive
precedent for side tables and it stays one, keyed by `DefId`
([B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)) — but
a side table is a shape, not an address. Sema's test is *produced and consumed
within sema*; reactivity analysis is produced by the frontend and consumed by
lowering, and fails it. Keeping it on the context because that is where it
happens to live today is how the god object re-forms one justified field at a
time.

### S5 · `DefId` shape

`DefId` stays a dense module-local index — it is used everywhere in-process and
must stay O(1). What changes is that it is **module-qualified from day one**, so
that `DefPath` (the serialized form) is derivable and nothing downstream has to
be retrofitted later. See
[3 § Designed for serialization](#designed-for-serialization--what-stage-3-owes-6).

### S6 · `OverloadKey`

Needed twice: by [§6](directions.md#6--modules-are-serializable-artifacts)'s
`DefPath` (Swift's `XREF_VALUE_PATH_PIECE` carries the type, because a name does
not identify a decl under overloading) and by
[§3](directions.md#3--generics-are-monomorphization-by-name)'s mangling
(`len` is both `list<T> -> s32` and `string -> s32`). **Same key, settle once
here.**

### S7 · Does `Ty` gain a non-concrete variant?

**Decided 2026-07-29: yes, both.** ~~`InternedTyKind`~~ `TyKind` gains `Param`
(A3) *and* `Infer` (A4), and both landed. **This reverses the recommendation
previously written here**, which
was "no" on both. The reasoning that recommendation rested on is recorded below,
along with why it did not survive — a recommendation that loses is more useful
kept than deleted.

Two holes, two lifetimes, and they are **not interchangeable**:

| variant | means | legal | must be gone by |
|---|---|---|---|
| `Param(idx)` | the `T` in a declaration | in a template's stored signature | substitution at instantiation |
| `Infer(var)` | unknown, to be solved | during 4 checking | the end of 4 |

##### Why "no" lost

The recommendation assumed templates would be carried as **syntax** (an AST
`TypeRef` plus a substitution), interned only once concrete. That works, and it
keeps `Ty` entirely concrete — but it forces **checking at instantiation**: a
template body cannot be typechecked until a concrete type is substituted, so an
error inside `filter` reports at the *user's* call site. That is the C++ template
error-message problem.

`Param` buys the opposite: the body is checked **once, generically**, against the
parameter. Errors land in the stdlib, where they belong. Combined with
[A1](open-decisions.md#a1--how-are-parameterized-types-represented)'s
monomorphization, this is Rust's arrangement — generic bodies checked once, then
specialized per instantiation — and it was dismissed here too quickly on the
grounds that "there are no type variables today" ([F1](findings.md#f1)), which is
evidence about the frozen compiler, not an argument about the new one.

##### What both variants now oblige

1. **Neither may ever be serialized.** A module artifact containing a `Param` or
   an `Infer` is a bug, not a state. This tightens
   [S2](#s2--ty-must-not-serialize-as-its-handle): the structural writer must
   *refuse* them, not merely encode them faithfully.
2. **`Infer` must not outlive 4.** 4's postcondition strengthens from "`types`
   is total" to "`types` is total **and contains no unresolved variable**" —
   rustc's `has_infer()` check, asserted rather than assumed.
3. **`Param` must not outlive substitution.** A `Param` reaching 5 is the same
   class of error: the instantiation did not happen.
4. **Structural equality must distinguish them.** Two `Param(0)`s from different
   templates are not the same type; two distinct `Infer` variables are never
   equal. Decide whether variables live in the interner at all or in a side
   unification table — interning a value that is *about to change* is the usual
   mistake here.
5. **The interner's uniquing invariant weakens.** Today equal types share a
   handle. With `Infer`, two handles may become equal *later*, which every
   `Ty == Ty` comparison in the checker must be written knowing.

#### Phase-1 contract — what `yelc-sema` exports

> ~~Lands on `main` as compiling Rust before Phase 3 starts~~ ✅ **landed
> 2026-07-29** ([`contract-before-fanout`](../../.agents/skills/compiler-rewrite/rules/contract-before-fanout.md)).

⚠️ **Every line of the sketch below is stale. `crates/yelc-sema/src/lib.rs`'s
re-export list is the contract; this is kept for the diff.** Seven differences,
each an ordinary consequence of a decision recorded elsewhere in this file, and
together an illustration of why a contract written twice is a contract that
disagrees with itself.

```rust
// ⚠️ AS WRITTEN — superseded on every line. See the corrected block below.
// pub struct DefId  { module: PackageId, index: u32 }
// pub struct DefPath { module: PackageId, pieces: Vec<PathPiece> }
// pub enum   InternedTyKind { … }
// pub struct TyInterner;
// pub struct Definitions;   // alloc, register_name, lookup, span, as_*
// pub enum   Namespace { Type, Value, Component, Global }
// pub struct CompilerContext {
//     pub interner: Arc<Interner>, pub types: TyInterner, pub defs: Definitions,
//     pub builtins: BuiltinTable, pub source_map: SourceMap,
//     pub diagnostics: Diagnostics,
// }
```

```rust
// AS LANDED — crates/yelc-sema/src/lib.rs

// identity
pub struct PackageId(pub u32);                    // the compilation unit
pub struct ModuleId(pub u32);                     // NEW MEANING: one symbol-table
                                                  // module node, one per `include`
pub struct DefId { package: PackageId, index: u32 }
pub struct DefPath { package: Name, segments: Vec<Name>, overload: OverloadKey }
pub struct OverloadKey { params: Vec<Ty> }

// types
pub struct Ty(u32);                               // handle; NOT Serialize — S2/B1
pub enum   TyKind { … Param(u32), Infer(u32), … } // not `InternedTyKind`
pub struct TypeInterner;                          // not `TyInterner`

// definitions — a two-level symbol table, ONE namespace
pub struct Definitions;      // register · register_overload · register_module ·
                             // bind_in_module · lookup · lookup_def ·
                             // lookup_in_module · module(s) · span_of · get ·
                             // set_ty · len · iter
pub enum   DefKind { Type, Value, Component, Global }   // a TAG, not a key
pub enum   Sym { Type(DefId), Value(DefId), Component(DefId), Global(DefId),
                 Module(ModuleId) }
pub struct Definition { id, name, kind, span, ty: Option<Ty>, is_export, overload }
pub struct Module { name, package, span, /* private scope */ }
pub struct Collision { name, existing, existing_span, attempted, span }

// builtins
pub struct BuiltinTable;     // §1: name → { arity, type scheme, lowering target }
pub enum   Arity { Fixed(usize), Variadic { min, element } }   // C1c
pub struct Known; pub struct KnownItems;   // C2: resolved lang-items, DefId not Option

// artifacts — NOT ANTICIPATED HERE AT ALL, and stage 3 implements against it
pub struct Artifact; pub struct Stamp; pub struct PackageName;
pub trait  ToArtifact; pub trait FromArtifact;
pub struct ArtifactWriter<'a>; pub struct LoadedPackage<'a>;

// threading — six fields plus a projection
pub struct CompilerContext {
    pub names:       Interner,      // NOT Arc<Interner>
    pub types:       TypeInterner,
    pub defs:        Definitions,
    pub builtins:    BuiltinTable,
    pub sources:     SourceMap,     // NOT `source_map`
    pub diagnostics: Diagnostics,
    known:           Option<KnownItems>,   // private; `resolve_known` then `known()`
}
```

**Depends on:** `yelc-base` only. **Must not depend on:** `yelc-syntax`,
`yelc-hir`, `yelc-lir` — a `use yelc_lir::BlockId` here is the error this brief
exists to prevent, and cargo will say so. ✅ Holds; `yel-core` appears as a
`dev-dependency` for the differential harness only.

### Phase 2 · 3's seam types on `main` — ✅ **landed 2026-07-30**, one exception

`HirId`, `BodyId`, `HirMap`, `HirModule`, `NodeMap`, `TypeId`, ~~`type_of`~~,
`lower_files` — as compiling Rust, before the lowering body is written.
`crates/yelc-hir`, depending on `yelc-base`, `yelc-syntax`, `yelc-sema` and no
other workspace crate.

Previously a separate landing under
[`contract-before-fanout`](../../.agents/skills/compiler-rewrite/rules/contract-before-fanout.md),
whose purpose is that parallel authors cannot each invent a plausible `Ty`. With
one agent owning the stage end to end there are no parallel authors, so this
becomes internal sequencing rather than a handoff. **The sequencing still
matters**: types first, body second, because a seam discovered while writing the
body gets shaped by the body's convenience.

**And that is exactly what it bought.** Writing the types with no body to serve
found three defects in the contract in one sitting — a map key that cannot
distinguish two files, a function with no receiver, and a memo whose key space
its own neighbour contradicts. Each of the three would have been *invisible* while
writing the lowering, because the lowering knows which file it is in, knows what
`self` is, and would have picked a memo key without noticing it was choosing.
[Details](#what-the-seam-could-not-be-written-as).

**One type is not landed:** `type_of`. It was the gate on phase 3; the gate
**closed 2026-07-30** when its owner was named — a lowering context struct in
`yelc-hir` holding `&mut CompilerContext` and a memo keyed by `TypeId`
([decision](#decided-2026-07-30--the-owner-is-stage-3s-lowering-context-in-yelc-hir)).
The decision is made and the code is not written, deliberately: the owner is a
type phase 3 constructs, so `type_of` lands **with** the lowering rather than
before it. Nothing blocks phase 3.

### Phase 3 · HIR build + resolve — 🚧 **core landed 2026-07-31**, uncommitted

The stage proper — [Brief](#brief), [Contract](#contract), [D1–D6](#decisions).

**What landed** (staged, not committed, per standing instruction):

- **The vocabulary** — `expr.rs` (~40 node kinds), `module.rs` items/bodies.
  `Match` is the only conditional (§9); `MethodCall` does not exist (pure UFCS,
  `modules.md` §8); no `String`, no `Ty` on any node; `Prop { owner, member }`
  replaces the frozen props-as-locals hack (`lower.rs:894–911`) — D3's fix
  applied one level up.
- **Member rows on `Definitions`** (`yelc-sema`) — fields, cases, properties,
  member functions as `(owner, index)`-addressed rows, because under one
  namespace a member name cannot be a root binding. Seam change, logged.
- **`lower_files`** — H1's three phases, kind-major registration in the frozen
  order, `type_of` on the lowering context (memo keyed by `TypeId`), D5
  globals-then-components, the five desugarings, D7 nesting, F13 merge,
  recovery lowering everywhere (H5).
- **The walker** (`visit.rs`, one, exhaustive, `thir/visit.rs`'s shape) and
  **signal deps** (`signalck.rs`) — the frozen `signalck` one stage earlier:
  per-body *and* per-site read/write sets over `Prop`/global-field references,
  computed after the desugaring and before checking, needing names only.
  Ordering argument recorded in `signalck.rs`'s module doc. Pass files
  are named `<pass>ck.rs` by convention — `packageck`, `signalck`, stage 4's
  `typeck`; the lowering constructs and is not a pass in this sense.
- **`yelc2 --emit-hir`** — definition table with resolved declared types, then
  items with bodies, deps lines, and every desugaring visible.
- **44 `yelc-hir` tests** (invariants H1/H2/H4/H5 by name, desugarings, D5,
  scoping, deps) and the **frozen differential**
  (`tests/frozen_parity.rs`): definition table contents-and-order identical
  over **2000/2000** corpus programs, mapping stated in the file, E0071 the
  only diagnostic carve-out.

**Numbers** — see [Numbers](#numbers--surprises). **Still owed** — the list at
the end of that section; each entry says why it is not in this landing.

### What this restructuring costs

Stated because it is a real cost, not a free simplification:

- **The stage is now ~3.5k lines of `yelc-sema` plus the whole HIR build.** The
  skill's guidance is that a stage which will not fit in one agent's context
  contains an internal seam worth contracting. Phases 1 and 2 *are* that seam,
  now written down — if the stage has to split, it splits there, and the split is
  already drawn.
- **Sema loses its own ratchet row**, so its independent checkpoint is now a line
  in this stage's Numbers rather than a gate that cannot be passed silently.
  Phase 1 says to take it anyway; that instruction is the mitigation, and it is
  weaker than a gate.

## Reference

- **ark** `~/Documents/Code/ark/compiler/arkc-hir/src/`: `hir_map.rs` ·
  `hir/hir_id.rs` · `hir/hir_node.rs` · `hir/module.rs` · `hir/visit.rs` ·
  `ty.rs`. ⚠️ An earlier stub cited `parsety.rs`; **no such file** — verify every
  reference path before quoting it. ⚠️ And verify the *premises*, not only the
  path: `hir_map.rs` is correct in ark and wrong here because ark's `NodeId`
  comes from a process-global counter and yel's does not
  ([details](#the-hirmap-key-is-not-a-nodeid)). A reference is a source of
  **shape**, and a shape carries assumptions.
- **Frozen** `yel-core/src/hir/` — `lower.rs` 1,510 lines, an
  [A2](anti-spec.md#a2--no-god-pass) case.
- **Landed stage 1** `yelc-syntax/src/ast.rs` — `ItemKind`, `ComponentMember`,
  `GlobalMember`, `UiNode`, `NamedProp`, `Recovered<T>`, `MaybeIdent`, and (added
  since this list was written) `Block`, `Braced<T>`, `AttributeList` /
  `Attribute` / `AttributeArg`, `TypeParam`, `ForBody`, `Stmt::{For, Return}`.
  Read the doc comments: several record frozen grammar behaviour lowering must
  respect.
- **Landed stage 1** `yelc-syntax/src/ast/visit.rs` — **the walker precedent**,
  and closer to hand than ark's. Exhaustive, no `_` arm, one place; and its
  `walk_expr` carries a `stacker` guard because expression spines are unbounded
  in valid input. HIR's walker inherits that problem and the reasoning behind the
  fix, which is written out in `yelc-syntax/Cargo.toml`.
- **Landed phase 1** `yelc-sema/src/lib.rs` — the re-export list **is** the
  phase-1 contract. Read it in preference to the sketch above.
- **Landed phase 2** `yelc-hir/src/lib.rs` — this stage's own seam.

## Definition of done

- [x] **Phase 0 landed before any measurement** — `global_filter_default.yel`
      ~~re-blessed from the frozen compiler~~ moved to `known_bugs/` because it
      panics the frozen compiler and there is no output to bless; `_ => {}` arms
      filed. ✅ `1d12250`, 2026-07-29
- [ ] **`yelc-sema` exists**, and its builtin `Definitions` table is compared
      against the frozen one — the standalone checkpoint, recorded in Numbers.
      ⚠️ **Half done.** The crate exists (`9a54ad1`); **the comparison was never
      taken**, and no test in `yelc-sema/tests/` drives the frozen
      `KnownDefinitions`. Exactly the skip this line's own paragraph predicted.
- [ ] **Seam types landed as compiling Rust before the lowering body was
      written.** ⚠️ **All but `type_of`**, 2026-07-30 — and the exception is the
      deliverable, not a shortfall: it did not land because it *cannot be written*
      as specified. [Details](#2--type_of-has-no-receiver-and-its-memo-has-the-wrong-key-space).
- [x] `yelc-hir` compiles; depends on `yelc-base`, `yelc-syntax`, `yelc-sema`
      and no other **workspace** crate (third-party is not what this clause is
      about — see the stacker precedent in [`seam-changes.md`](seam-changes.md)).
      ✅ 2026-07-30
- [ ] H1–H5 each asserted by a named test, not by review. **H2 in its restated
      form**, over a **multi-file** input — the round-trip alone passes under the
      key collision it exists to rule out.
- [ ] One walker, exhaustive, no `_` arm; no `collect_children_slots` counterpart.
- [ ] 2000 corpus programs + **90** positive + 23 diagnostic fixtures build and
      resolve without panic. *(91 → 90 in phase 0.)*
- [ ] `Definitions` identical to the frozen tree's — contents **and order** —
      over the full corpus, via the read-only oracle harness, with the
      `Namespace`→`DefKind` mapping stated and the 30 cross-kind programs of
      `single_namespace.rs` carved out as a **deliberate** narrowing.
- [ ] Diagnostic set identical in meaning, span, and order over the full corpus.
- [ ] No `String` in any HIR type. No `Ty` on any HIR **node** — the definition
      tables and the `types` side table carry `Ty` by design.
- [ ] `type_of` structurally unreachable from H1 phase 1 (the collector does not
      exist yet), not merely commented. ⚠️ **Naming the collector is a
      prerequisite**, not a consequence — this line is the DoD assuming a type the
      contract never declares.
- [ ] **`yelc2 --emit-hir` renders this phase's output**, yel-flavoured, showing
      resolved `DefId`s, declared types, and the desugarings. Byte-stable across
      runs. Explicitly **not** round-trippable — see above.
- [ ] **`ToArtifact` / `FromArtifact` implemented for every HIR node type**, no
      `Ty` and no `DefId` in any `Wire`, the `types` map written as a
      `Vec<(HirId, TypeIndex)>` sorted by `HirId`, and **`Stamp::FORMAT` bumped**
      — postcard writes fields by position, so a schema change is invisible in
      the bytes. Round-tripped through a **differently populated** interner; a
      same-interner round trip proves nothing.
- [ ] **No `DefId` is reachable from a serializable position.** `DefPath` is the
      only identity that crosses a **package** boundary
      ([§6](directions.md#6--modules-are-serializable-artifacts)); a `DefId` in a
      would-be-serialized struct is the bug this split exists to prevent. Same
      question answered for `SourceNodeId`, which `Ty`'s type-level guard does
      **not** cover — `SourceId` derives `Serialize`.
- [x] `HirModule` carries a `PackageId` and a *set* of `SourceId`s — not one
      source. The **package** is built from the file set (H1), so a single-source
      field is a category error. ✅ 2026-07-30 — but see
      [the noun](#3--hirmodule-is-the-noun-moduleid--packageid-was-renamed-away-from).
- [ ] **Provenance recorded for every node the UI desugaring generates**, and no
      generated identifier appears in any rendered diagnostic — asserted over the
      whole diagnostic corpus, not reviewed.
- [ ] **New UI type-error fixtures** (property mismatch, handler signature,
      `for`-bound item) — the frozen suite has none, so this area currently has
      no oracle at all.
- [x] D1–D6 recorded with reasoning. ✅ 2026-07-29
- [ ] **D5's item-order divergence measured, not asserted.** The 815 corpus
      programs containing both a global and a component are byte-identical, or
      diverge *only* in item order with every divergence enumerated in
      [`goldens-changed.md`](goldens-changed.md). "We expected only item order to
      change" is a claim; an unmeasured claim is how a miscompile ships as an
      expected reordering ([D5](#d5--globals-lower-before-components)).
- [ ] Adversarial review panel, read-only, one lens each.
- [ ] Surprises written — [D3](anti-spec.md#d3--a-stage-documents-what-surprised-it).

## Decision log

### D7 · Flatten `else if` chains

**Decided 2026-07-28, before briefing. Adopted: yes.** `If` drops
`else_if_branches`; an `else if` chain becomes a nested `If` in the `else` branch.

**Free parts, both checked first.** The nested `If` is *not* synthesized — stage
1 gives every `ElseIfBranch` its own `NodeId` and `Span`, so it maps to a real
AST node: [H2](#h2) holds, spans point at the actual `else if`, no diagnostic
moves. And visit order is unchanged (`a, b, c` either way), so D5 is unaffected.

**Not output-neutral** — [F10](findings.md#f10). The frozen lowering treats
`else if` as a flat N-way selector at one anchor and nested `if` as two
independent 1-way selectors.

**Obligations.**
1. **[Stage 6](stage-6-lower.md) must recognise the chain** — a nested `If`
   whose `else` holds exactly one `If` and nothing else lowers as the flat N-way
   selector. Uniform IR, smart lowering. Without it, every `else if` in the
   corpus diverges.
2. **Explicit nested `if` then also gets the flat shape**, because after
   flattening the two are indistinguishable — which is the point. A real,
   enumerated divergence, toward one anchor and one subscription instead of two.
3. Recorded in advance, and every diverging program gets a line in
   [`goldens-changed.md`](goldens-changed.md) — *"an unexplained corpus
   divergence is a failure even when the new output looks better"*.

**Blast radius.** 1 positive fixture uses `else if`, 2 use explicit nesting.
**Corpus-level unmeasured — measure before implementing.** If yel-smith does not
generate `else if`, the corpus proves nothing here and those 3 fixtures are the
only evidence ([A13](anti-spec.md#a13--the-generator-that-found-a-bug-class-is-what-ships-not-its-instances)).

**Why adopt anyway.** `else_if_branches` is carried by three IRs
([F11](findings.md#f11)) and every consumer of each. The alternative matches
output by construction and propagates the special case through the whole rewrite
([B4](anti-spec.md#b4--no-special-cased-control-flow-where-a-general-form-exists)).

### D8 · A package is identified by itself, not by a file

<a id="d8--a-module-is-identified-by-itself-not-by-a-file"></a>

**Decided 2026-07-28, before briefing.** `HirModule` carries `id: PackageId` and
`sources: Vec<SourceId>`. It does **not** carry a single `SourceId`.

⚠️ **Retitled 2026-07-30, and the retitle is the correction.** This entry was
written when the compilation unit was called a *module*. `fbaa95e` renamed that
level to **package**, `ca905d0` gave `ModuleId` an unrelated new meaning (one
symbol-table node per `include`), and [`modules.md` §4](../modules.md) settled
that a package is a **directory**. Read every *"module"* below as *"package"* —
the argument is unchanged and only the noun moved, which is itself the entry's
own point about nouns. The old anchor is kept so existing links resolve.

**And the noun is still one level off in one place:** the type is called
`HirModule`. See
[the contract](#3--hirmodule-is-the-noun-moduleid--packageid-was-renamed-away-from).

**The error this corrects.** The first draft of the contract in this file had
`HirModule { source: SourceId, … }` alongside
`lower_files(parsed: &[ParsedFile])`. Those cannot both be right: H1 makes each
phase sweep the **whole file set** before the next begins, so the thing being
built spans N files and has no single source. A `SourceId` field on it is a
category error — it names one input of many as though it were the identity of
the output.

**Why it survived review until now.** It read as harmless because every consumer
that touched it wanted a span, and any source in the set would have produced
*a* span. Nothing asked the question that breaks it —
**"what is this module's identity?"** — until
[§6](directions.md#6--modules-are-serializable-artifacts) needed a `PackageId` to
put at the head of a `DefPath`. A serialized cross-module reference cannot say
"the module whose first file was `foo.yel`".

**The transferable form**, worth stating because this stage has more chances to
make it: **a field that identifies the thing it is on is different from a field
that happens to be available on it.** `sources` is the second kind (provenance,
for diagnostics); `id` is the first. Conflating them is invisible until something
needs to *refer* to the whole, which is exactly what serialization does and what
in-process compilation never did.

### D1 · Bindings and handlers are one uniform prop list

**Decided 2026-07-29: one uniform prop list.** No `HirBinding`/`HirHandler`
split; HIR carries what stage 1's AST already carries — `NamedProp { modifier,
name, value }` — and [4](stage-4-hir-check.md) classifies using the declared
type.

The rationale is above under [D1](#d1) and does not repeat here. The **caveat is
binding and is 3's to discharge, not to note**: the frozen lowering uses the
split to decide scoping, `LocalId` ordinals reach the type checker, and
`HirHandler`'s doc says typeck re-defines the param "to produce the THIR
`LocalId` with matching arena parity." A uniform lowering must produce the same
locals in the same order. That is a test — locals of a closure-valued prop
enumerated in allocation order, against the frozen tree's — not a review remark
([A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)).

Couples to [F1](open-decisions.md#f1--how-is-a-bodys-trigger-determined), which
is 4's and still open.

#### Phase placement revised 2026-07-29 — classification is a table lookup

The answer above said *"4 classifies, using the declared type."* The **answer**
stands; the **placement** was wrong, and it matters because the whole UI lowering
hangs off it.

Whether `clicked: { … }` is a handler depends on the type `Button` *declares* for
`clicked` — a lookup in `Definitions`, which register-then-lower has already
populated. It is not an **inferred** type and does not need checking to have run.
By this file's own rule that is the *"needs the definition tables"* row — **this
stage, phase 2 or 3** — not the *"needs a type to choose the target"* row.

**Consequence, and it is the large one:** the UI tree lowers to functions and
calls **before** typechecking, so the checker never sees UI at all. `typeck.rs`'s
~2.8k lines of element/property/handler/children cases evaporate rather than
being ported — a bigger simplification than [S1](#s1--adopt-the-builtin-table-1)
offers for builtins.

**The exception is binders**, and only binders: a UI region that introduces a
variable — `for item in items`, and a `match` arm binding a payload — takes that
variable's type from checking. `hir/lower.rs:1152` already writes
`item_ty: Ty::ERROR, // Will be inferred`, and `thir/typeck.rs:559–575` fills it
via `locals.set_ty`, so a local outliving its unknown type is the existing
mechanism rather than a new one. The desugaring emits the structure; checking
fills the slot.

State the concession plainly in any brief built from this: **a generated region
function does not have a complete signature at construction.** "Everything
becomes functions" invites the opposite assumption, and it is wrong for exactly
`for` and `match` regions.

Full ordering: [directions §9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it).

### D2 · `For` does not carry the item type

**Decided 2026-07-29: remove `item_ty: Ty`.**
[B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)
verbatim — a typeck result stored on an untyped IR node. If 4 needs it keyed by
node, that is a `NodeMap` side table it owns.

### D3 · `For` does not carry the loop-variable name

**Decided 2026-07-29: remove `item_name`; fix the scope structure.**

The frozen field exists with the comment *"stored directly to avoid LocalScope
lookup issues."* A duplicate of information the scope already holds, kept because
the lookup did not work, is the shape this rewrite exists to stop porting. The
obligation that comes with the decision: the `for` loop variable resolves through
the ordinary scope path, and a test resolves it there.

Note this is strictly more than D2. D2 deletes a field that should never have
existed; D3 deletes a field that is currently **load-bearing**, so it is only
free once the scope structure is right ([A9](anti-spec.md#a9--a-ported-construct-is-load-bearing-or-it-is-deleted)).

### D4 · `HirGlobal` has no body — only its functions

**Decided 2026-07-29: no body.** Property defaults stay in `GlobalDef`, and
`HirGlobal` carries the **functions declared on that global** and nothing else.

This is the asymmetry with `HirComponent` made deliberate rather than inherited.
The distinction it encodes: a component's body is a *tree* — it has UI structure
to lower — and a global's is a *bag of declarations*. Giving globals a body for
symmetry's sake would create a node that is always the same degenerate shape,
which is uniformity in the type and not in the thing.

The functions do need a home, because they have real bodies that lower like any
other. They hang off `HirGlobal`; they do not go in `GlobalDef`, which is a
definition-table entry and not an IR node.

### D5 · Globals lower before components

**Decided 2026-07-29: globals, then components — reversing the frozen lowering
order.**

⚠️ **This is the one answer that diverges from the recommendation, and it has a
measured cost. Read before proceeding.**

What the frozen tree actually does — worth stating, because the two loops
disagree on purpose (`yel-core/src/hir/lower.rs:128–160`):

| phase | order |
|---|---|
| registration (1b, 2) | elements → extern components → **globals** → **components** |
| lowering (3) | **components** → **globals** |

So *registration* is already globals-first; this decision makes **lowering** agree
with it.

**The reason is the dependency direction, not tidiness.** Components reference
globals; globals cannot reference components. Lowering in dependency order means
a body is lowered after everything it can refer to, and the file reads in the
direction the language actually depends. "One order instead of two" is a
consequence, not the argument.

The grammar backs the asymmetry: `global_member = { function_decl |
global_callback | global_property | BLOCK_LEVEL_CATCH_ALL }` — **a global has no
UI tree**, and a component is only ever instantiated inside one. There is no
syntactic position in a global where a component can be used.

**Two things this reason does *not* buy, stated so nobody assumes them later:**

1. **It is not a correctness requirement, because register-then-lower already
   decouples resolution from lowering order.** Every name is registered in phases
   1b and 2 before any body lowers, which is precisely what makes forward
   references work in both directions. Globals-first is the right order on the
   merits; it is not load-bearing for resolution, and a test asserting that
   lowering order makes a reference resolve would be asserting something the
   registration phase already guarantees
   ([A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)). Where the
   direction *does* become load-bearing is [6](stage-6-lower.md) —
   initialization order, and `resolve_global_triggers`.
2. **Name resolution does not currently enforce it.** `lower.rs:1169–1174`
   resolves a bare identifier through `Value → Type → Component`, and that path
   is shared by global function bodies — so a bare component name written inside
   a global resolves to `HirExprKind::Def(component_def_id)` today. The direction
   holds because the grammar gives a global no place to *use* a component, not
   because resolution refuses. If 3 wants the rule enforced rather than merely
   unreachable, that is a check to add deliberately, with a diagnostic.

The frozen comment concedes components-first is inherited rather than required —
*"so the type-check order (and therefore diagnostic order) matches the previous
components-then-globals pipeline"* — and shedding compatibility with a pipeline
that no longer exists is what the rewrite is for.

**The cost, measured rather than guessed:**

| | |
|---|---|
| diagnostic fixtures with both a global and a component | **0 / 23** |
| corpus programs with both a global and a component | **815 / 2000** |

The 23 diagnostic fixtures are therefore **unaffected** — none of them can
observe the order. The 815 is the number that matters, and it is not a diagnostic
concern: corpus programs are valid Yel and emit no diagnostics. It is an
**item-order** concern. Lowering order determines the order of `HirItem`s, which
flows to THIR → LIR → codegen and can reorder WIT exports and DOT nodes.

**The obligation this creates, and it is 3's:** run the corpus differential and
show the 815 either byte-identical or diverging *only* in item order, with the
divergence enumerated in [`goldens-changed.md`](goldens-changed.md).

**If WIT bytes move, that is a deliberate golden change, not a reason to
reverse.** The dependency-direction argument settles the order on the merits, so
a reordered export list gets documented and re-blessed **from the frozen
compiler's rules, never from the new compiler's output**
([`oracle-never-rebless`](../../.agents/skills/compiler-rewrite/rules/oracle-never-rebless.md)).
What the differential is for here is *knowing* what moved, not deciding whether
the order was right — measuring it is still mandatory, because "we expected only
item order to change" is a claim, and an unmeasured claim is how a miscompile
gets waved through as an expected reordering.

`Definitions` order is **not** at risk: registration order is unchanged.

### D6 · Doc comments attach to the nearest preceding comment run

**Decided 2026-07-29: attach, using the nearest preceding comment run with no
blank line between it and the item.** *Refined 2026-07-31 at implementation:*
only **`///`** lines are documentation — plain `//` never attaches and ends a
run. WIT's parser was read first (`trim_start_matches('/')` blurs the two);
Rust's hard line was chosen instead, cheap now and expensive after tooling
ships the blurry rule.

Stage 1 deliberately left this open — the green tree holds trivia and decides
nothing about ownership ([`stage-1-syntax.md`](stage-1-syntax.md)), so 3 owns
the rule. The rule is stated positively so it can be tested rather than inferred:

- The run ends at the item's first token, with only whitespace between.
- A blank line breaks attachment — the run belongs to nothing.
- Multiple consecutive comment lines are one run, joined in source order.
- A comment run with no item after it attaches to nothing and is not an error.

The attachment lives in a `NodeMap` side table, not on the node
([B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)) —
this is analysis *about* an item, read off the green tree, and the LSP is its
consumer.

## Numbers · Surprises

**Phase 3 core, measured 2026-07-31** (all uncommitted):

| | |
|---|---|
| workspace | **642 → 675 / 0 failed / 2 ignored** |
| corpus sweep | 2000/2000 lower **without panic**; 996 clean, 1004 reject with **E0071 only** (the package rule — see Surprise 1) |
| positive fixtures | 85/90 exit clean; 5 E0071-only (the five with no `package` clause); **0** unexpected errors |
| diagnostic fixtures | the stage-3-owned codes fire: E0010 `duplicate_definition`, E0040 `duplicate_children`, E0060 `syntax_error`; type-level fixtures correctly silent until stage 4 |
| Definitions differential | **2000/2000 identical**, item-level, contents and order, vs frozen `lower_to_hir` |
| mutation tests | differential: 2/2 killed (registration order flip, kind swap); suite: 3/3 killed (F13 off, UFCS receiver dropped, deps writes-as-reads); all restorations verified by content comparison |
| phase-1 checkpoint | taken 2026-07-31 (`yelc-sema/tests/builtin_checkpoint.rs`, 10 tests) — owed since `9a54ad1` |

### Surprises

1. **The package rule rejects half the frozen corpus.** 1004/2000 corpus
   programs and 5/90 positive fixtures predate `package`-in-every-file
   (2026-07-30) and now fail with E0071 and nothing else. Approved surface
   break, but its scale means every future artifact differential must carve it
   out or the corpus must be regenerated *with* clauses (frozen accepts them) —
   **a decision for the user, not a phase**. Recorded in the differential's
   module doc as carve-out 1.
2. **The phase-1 checkpoint falsified its own premise's membership.** The
   narrowing doc-comment's "9 names in `Namespace::Type`" is right by count and
   wrong by contents: `option`/`result` are allocated but never name-registered
   (reachable only through type syntax), while `Brush` and `event-value` —
   documented nowhere — are registered. The Dom global is deliberately
   unregistered. And frozen `FunctionDef.params` is vestigial (`vec![]` on
   every builtin); real signatures live only on the interned `Func` type — the
   first arity comparison measured the wrong field and reported eleven false
   mismatches.
3. **`count += 1` reads `count`.** The deps pass's first test expected 3 read
   sites and found 4 — the desugared right-hand side is a real read. The test
   was wrong; the pass was right.
4. **Ten UI primitive spellings have no `TyKind`** (`length` … `easing`).
   Measured absent from every checked-in program as an *annotation*; `type_of`
   answers `None`. Decision owed before stage 4 (options in `lower.rs`'s
   module doc).
5. **User builtin-name shadowing diverges silently.** Frozen registers builtin
   callables as defs, so `len: func()…` collides (E0010); the new table keeps
   builtins out of `Definitions`, so the same program registers and shadows.
   No corpus program hits it; recorded rather than reconciled.

### Landed after close-out, same day (2026-07-31, uncommitted)

- **The module produce/consume loop.** `--emit-module` (write side; refuses on
  errors) and `from "geometry" include Geo;` (read side: `FROM_KW`/`INCLUDE_KW`
  contextual, `--include DIR` on the driver, artifact load-and-bind *before*
  lowering because a module binding is registration). `std:` specifiers are
  reserved for compiler-shipped modules and refused rather than searched.
  `SerializedDef` gained member rows (`FORMAT` 2 → 3). Resolution crosses the
  package boundary — `Geo.Geometry.origin-x` lowers to the foreign `DefId` and
  `signalck` records the cross-package read. Four e2e tests over the real
  binary; workspace 675 → 687. Decision text: `plans/modules.md` § *the
  specifier is a plain-name string*.

### Still owed, and why not here

| item | why deferred |
|---|---|
| ~~D6 doc-comment attachment~~ | ✅ **landed 2026-07-31**: `HirModule.docs` (`DefId → Name` side table, write-once), extraction over the S1-guaranteed source text, shown in `--emit-hir` as `///` lines. **Only `///` attaches** — plain `//` is commentary (Rust's rule, adopted after reading WIT's blur; refines D6's "comment run" wording). Seven tests; blank-line rule and the `///`-vs-`//` distinction each mutation-tested. Keyed by `DefId`, not the decision's literal `NodeMap<HirId>` — type declarations have no HIR node under the member-row design; deviation and the attribute-span edge recorded on the field. Top-level items only; members follow with the LSP |
| `ToArtifact`/`FromArtifact` for HIR nodes | serialization of the whole new vocabulary; sized like its own phase; nothing loads HIR artifacts yet. ⚠️ Narrowed 2026-07-31: the **declaration surface** now serializes — `SerializedDef` gained member rows, `Stamp::FORMAT` 2 → 3, wire-byte pin re-blessed *after* the bump per its own protocol, round-tripped through a polluted interner, load side mutation-tested. `yelc2 --emit-module PATH` writes it, refusing on `has_errors()`. What remains deferred is HIR **bodies** |
| provenance **renderer** + "no generated identifier in any diagnostic" test + the three UI type-error fixtures | need stage 4's checker to *produce* those diagnostics; the recording half (map origins via `synthesize`) is in |
| D5 item-order divergence measurement | there is no artifact until stages 5–7; `--emit-hir` shows the item order, but the obligation is about output bytes |
| E0070 `InvalidPackageName` | frozen fires it somewhere later in its pipeline; new tree's site undecided (`package_underscore.yel` currently silent) |
| `--emit-hir` byte-stability test | holds by construction (no hash-map iteration reaches the dump); the two-run comparison test is cheap and should land with the next driver touch |
