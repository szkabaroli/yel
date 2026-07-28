# Stage 2 — `yelc-hir` (2a build+resolve, 2b check)   status: brief written

Replaces (frozen, never edited): `crates/yel-core/src/hir/` (1,995 lines) and
`crates/yel-core/src/thir/` — merged into one crate on 2026-07-28
([`seam-changes.md`](seam-changes.md)). The remaining stages were renumbered to
close the gap: LIR is **3a**/**3b**, codegen is **4**.

Base: — · Started: — · Landed: —

> **Gate.** Stage 1 landed (`33e5c71`, 0 corpus divergences), so invariant 2 is
> satisfied. Still **not briefed**: seam types are not on `main` and D1–D6 are
> unanswered. See [Prerequisites](#prerequisites).

## The shape

**One IR, two phases, types in a side table.** One node vocabulary, one walker.
`types: NodeMap<Ty>` is empty after 2a and total after 2b.

| phase | does | produces |
|---|---|---|
| **2a** build + resolve | AST → HIR; register items; resolve names; collect declared types | HIR + `Definitions` typed |
| **2b** check | bidirectional type checking over the same nodes | `types` map total |

Run in sequence, same crate — mirroring the 3a/3b precedent. 2b is a separate
pass over 2a's output, so a divergence is bisected by dumping after 2a.

**2a's output is a public surface, not an internal intermediate.** Yel will have
lints, and early (syntactic) lints run after 2a while type-aware lints run after
2b. Same nodes, same walker, a lint written once. Lint results are side tables
like every other analysis output ([B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)).

## Brief — 2a (build + resolve)

Port `arkc-hir`'s structure to yel's item vocabulary: name-resolved, bodies
separated from items by id, analysis results in side tables.

- **Three phases, across the whole file set** — invariant [H1](#h1).
- **Bidirectional `HirId ↔ NodeId` map** (ark `hir_map.rs`: `map` + `rev_map`,
  `next_hir_id(node_id)` allocating and recording in one call). What lets a
  diagnostic point at source, and what the LSP needs.
- **Side tables, not fattened nodes** (ark `NodeMap<V>`, `assert!(old.is_none())`
  on insert) — [B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes).
  `CompilerContext::signal_deps` keyed by `DefId` is the existing positive
  precedent and stays.
- **Types are not re-represented** — [see below](#why-there-is-no-parsedtype).
- **Bodies separated from items by id** (ark `Module { node_types, bodies,
  elements }`).
- **One walker**, exhaustive, no `_` arm ([A3](anti-spec.md#a3--no-duplicated-walkers)).
  The frozen tree has a second, hand-rolled one: `collect_children_slots`
  (`lower.rs:52`) re-walks the node tree with its own match over every kind.
- **One uniform item spine** for globals and components
  ([D1](anti-spec.md#d1--the-compilation-unit-is-the-file-not-the-component)).
  `HirItem` is already a real `{Component, Global}` enum; that shape carries over.
- **No `String` survives.** Stage 1 established S4 and the frozen HIR breaks it
  in three places: `HirNodeKind::Element.name`, `HirBinding.name`,
  `HirHandler.name`. Also [keep-list §3](keep-list.md).

## Brief — 2b (check)

Bidirectional type checking, filling `types: NodeMap<Ty>` over 2a's nodes.

- **Identical diagnostic *meaning* on the 23 diagnostic fixtures.** Same
  rejection, same reason, same construct. Wording may improve with the diff read
  and recorded in [`goldens-changed.md`](goldens-changed.md) — never re-blessed.
- **Accumulate and continue.** Recover with `Ty::ERROR` and keep checking; the
  driver bails between phases on `has_errors()` —
  [keep-list §6](keep-list.md#6--accumulate-and-continue-error-policy).
- **Carry the frozen visitor split forward.** `thir/visit.rs` (`ThirVisitor` +
  `walk_expr`/`walk_stmt`, exhaustive, with a `visit_closure` descent hook) is
  **the model**, not debt — the one place the duplicated-walker problem was
  actually solved. In the merged design it becomes *the* walker, shared with 2a.
- **Split `typeck.rs`** (2.8k in the frozen tree) —
  [A2](anti-spec.md#a2--no-god-pass).

### Gaps inherited as decisions, not copies

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

### Directions in play

[§1](directions.md#1--builtins-are-a-table-not-a-field-per-builtin) (typeck asks
a table, not 24 `ctx.known.*` fields) ·
[§3](directions.md#3--generics-are-monomorphization-by-name) (decided here) ·
[§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one)
(model, don't implement) ·
[§5](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger)
(**changes no output**; the sub-decision is *how the trigger is determined* —
§5 records options A and B, and either owes the same dependency-set test) ·
[§6](directions.md#6--modules-are-serializable-artifacts) (the artifact this
stage ends at).

## Inherited from stage 1

S1–S6 hold; assume them, re-verify none. Notably: `green.text() == content`,
every AST node has `NodeId` + `Span`, names interned, ill-formed input yields **a
diagnostic AND an `Error` node**, parsing always returns a `ParsedFile`.

Not assumable: sibling ordering beyond source order, `NodeId` stability across
reparses, any green-tree reuse API. **Trivia attachment is this stage's problem.**

Four of stage 1's eight Surprises change what arrives here:

| # | consequence |
|---|---|
| **1** | The frozen *parser* silently discarded malformed `global`/`record` members; stage 1 reports them. **More programs now reach HIR, carrying `Error` nodes the frozen HIR never saw.** Every `Error` variant and `Recovered::Missing` must lower to something — never a panic, never a skip. |
| **2** | `if {` parses as an **element named `if`** (keywords are not reserved). Element resolution will see `if`/`for`/`else` as names. Reproduced deliberately; do not "fix" it here. |
| **5** | `name: func(…)` is a **property** in a component and a **callback** in a global. Registration must preserve the asymmetry. |
| **6** | `extern component`, legacy `callback name(…);`, and the `bind` modifier are real, are in fixtures, and are absent from `LANGUAGE.md`. All three lower here. |

## What lowerings belong where

**HIR is name-resolved before it is typed, so a desugaring belongs in 2a iff it
is decidable from names alone.**

| needs | belongs in |
|---|---|
| nothing but the syntax tree | 2a, any phase |
| the definition tables | 2a, **phase 2 or 3** — never phase 1 ([H1](#h1)) |
| a *type* to choose the target | 2b |
| the whole module (fan-out, ordering) | 3b — e.g. `resolve_global_triggers` |

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
part's type; `MethodCall` survives into HIR by design) · **UI tree flattening**
(`if`/`for`/`Element` stay structured; flattening is LIR's job) · name errors
(HIR never errors on an unknown name — resolution is *partial* on purpose).

**Every desugaring moves diagnostic spans.** `x += 1` → `x = x + 1` means a later
error reports against a *synthesized* `Binary`. The five carried over are safe by
construction (the 23 fixtures pin them); a **new** one is checked against those
fixtures before it lands.

## Decisions

| # | decision | recommendation |
|---|---|---|
| D1 | Do bindings and handlers stay split? | **No — one uniform prop list.** [below](#d1) |
| D2 | `For.item_ty: Ty` on the node | Remove — [B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes) verbatim. If 2b needs it keyed by node, that is a side table. |
| D3 | `For.item_name` *"stored directly to avoid LocalScope lookup issues"* | Fix the scope structure rather than porting the duplicate. If genuinely needed it is a `NodeMap`, not a field. |
| D4 | Do globals get a body? | `HirGlobal` has none; defaults live in `GlobalDef`. Asymmetric with `HirComponent`. D1's uniform-spine goal argues for symmetry. Decide explicitly. |
| D5 | Item and diagnostic ordering | **Preserve exactly.** The frozen tree lowers all components then all globals *"so the type-check order (and therefore diagnostic order) matches"*. Asserted by the 23 fixtures, and the one place "uniform spine" and "match output" pull apart. |
| D6 | Trivia / doc-comment attachment | Decide the rule, or decide explicitly not to attach yet. Not implicit. |
| D7 | Flatten `else if` into nested `If`? | **Decided: yes** — [log](#d7--flatten-else-if-chains) |

### D1

Recommendation: **one uniform prop list.** Stage 1's AST already unified them
into `NamedProp { modifier, name, value }`; re-deriving two lists is an analysis
result on the node (B3). HIR *cannot* classify correctly anyway — whether
`bumped: { … }` is a handler depends on `bumped`'s declared type, which 2b owns
([F8](findings.md#f8)). And the payload param falls out: the frozen
`HirHandler.param` exists so `drop: (payload) { … }` binds a body-scoped local,
which in the landed AST is just `ClosureExpr { params, body }`.

**Caveat, and it is the real work:** the frozen lowering *uses* the split to
decide scoping. A uniform lowering must produce the same locals in the same
order — `LocalId` ordinals reach the type checker, and `HirHandler`'s doc says
typeck re-defines the param "to produce the THIR `LocalId` with matching arena
parity." Verify local allocation order is unchanged before declaring D1 free.

## Contract

> **Proposed. Lands on `main` as compiling Rust before the agent starts**
> ([`contract-before-fanout`](../../.agents/skills/compiler-rewrite/rules/contract-before-fanout.md)).
> A needed change is a request in [`seam-changes.md`](seam-changes.md).

**Input:** `&[yelc_syntax::ParsedFile]` — the **whole file set** — plus
`&mut CompilerContext` (from `yelc-sema`: interner, `Definitions`, diagnostics).
**Output:** one typed module for the set.

```rust
pub struct HirId(u32);          // distinct from syntax::NodeId and from DefId
pub struct BodyId(u32);

pub struct HirMap {                       // ark hir_map.rs, both directions
    map:     FxHashMap<HirId, NodeId>,
    rev_map: FxHashMap<NodeId, HirId>,
}
impl HirMap {
    pub fn next_hir_id(&mut self, node: NodeId) -> HirId;   // alloc + record
    pub fn node_of(&self, hir: HirId) -> Option<NodeId>;
    pub fn hir_of(&self, node: NodeId) -> Option<HirId>;
}

pub struct HirModule {
    pub source: SourceId,
    pub items:  IndexVec<HirItemId, HirItem>,
    pub bodies: IndexVec<BodyId, HirBody>,
    pub map:    HirMap,
    pub types:  NodeMap<Ty>,    // empty after 2a, total after 2b
}

/// Side table. One value per HirId, write-once.
pub struct NodeMap<V>;
impl<V> NodeMap<V> {
    pub fn insert(&mut self, id: HirId, v: V);   // asserts no prior value
    pub fn get(&self, id: HirId) -> Option<&V>;
}

/// Types are NOT re-represented. A HIR entity refers to the AST `TypeRef` it
/// was written as, by `NodeId`.
pub struct TypeId(NodeId);

/// The one syntax→`Ty` function: resolves `TypeKind::Named` against the
/// definition tables and interns. Memoized in a `NodeMap<Ty>`.
/// **Callable only after H1 phase 1.** Calling it earlier is F3, not a variant of it.
pub fn type_of(&mut self, ty: TypeId) -> Ty;

/// Each of H1's three phases sweeps every file before the next begins.
pub fn lower_files(parsed: &[ParsedFile], ctx: &mut CompilerContext) -> HirModule;
```

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
| **declared types** — fields, property types, params/returns, variant payloads | **2a, phase 2** | `type_of` → `Ty` into the definition tables |
| **expressions** | **2b** | bidirectional inference → `types` map |

The definition tables carry real `Ty` and always have ([F5](findings.md#f5)) —
rustc's `type_of(def_id)`-before-body-check split, not a deviation. **The frozen
bug is phase ordering only** ([F3](findings.md#f3)): same function, same output
type, wrong moment.

Keeping the written syntax around exactly long enough for there to be something
to resolve against is the whole point of not re-representing types, and is what
makes [H4](#h4) achievable rather than aspirational.

### Multiple files

This stage merges them. **There are no includes:** `ItemKind` has no `Import`
variant, `LANGUAGE.md` has no `import`/`use`, and `ExternComponent` declares a
component implemented *elsewhere* — an import **contract**. Multi-file means the
files on the command line, sharing one package.

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

### Invariants this stage ESTABLISHES

<a id="h1"></a>
**H1 · Register names, collect types, lower bodies — three phases, not two.**
Each sweeps **every file** before the next begins.

| phase | does, across all files | may not |
|---|---|---|
| 1 · register | a `DefId` + name for every item | call `type_of` — no name is guaranteed to exist |
| 2 · collect | `type_of` every **declared** type into the definition tables | look at any body |
| 3 · lower | lower bodies; expressions stay untyped until 2b | register new items |

A body may reference any item regardless of source order, **and so may a declared
type, and so may either across file boundaries.** *Asserted by* two fixtures: a
record whose field is typed as a record declared **last** in the same file, and a
component referencing a record declared in a file passed **second**. Both fail
today ([F3](findings.md#f3), [F4](findings.md#f4)).

**H2 · The `HirId ↔ NodeId` map is total and bidirectional.** Every `HirId` maps
to a `NodeId` present in the input, and `hir_of(node_of(h)) == h`. *Asserted by* a
walk over every corpus program.

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

### What stage 3a may NOT assume

- **No `HirId` stability** across reparses, same as `NodeId`.
- **No classification of element props** into bindings vs handlers, if D1 lands
  as recommended.
- **No capture sets** unless [§4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one)
  is adopted into this brief.

## Verification

**Stage 2 has no artifact of its own** ([F14](findings.md#f14)) — and pretending
otherwise is the failure mode this section prevents. What is comparable, strongest
first:

1. **The `Definitions` table** after 2a — contents **and order**, since `DefId`s
   are ordinals that reach output ordering. Shape-independent, so it works across
   two different IR designs. Compare via a **new read-only oracle crate** that
   depends on frozen `yel-core` as a library and calls `hir::lower_file`: this
   reads the frozen tree, does not edit it, and is allowed by
   [`greenfield-never-touch-old-code`](../../.agents/skills/compiler-rewrite/rules/greenfield-never-touch-old-code.md).
2. **Diagnostics** — meaning, span, **and order** (D5) — over the 2000-seed
   corpus, 91 positive and 23 diagnostic fixtures, via frozen `yelc check`.
3. **No panic, total lowering** over the corpus.

After 2b the stage ends at a **serializable typed module**
([§6](directions.md#6--modules-are-serializable-artifacts)), which is
byte-comparable — recovering the artifact-level differential the old stage-2
boundary could not have.

## Prerequisites

1. ~~Stage 1 closed out~~ ✅ `33e5c71`, 2026-07-28.
2. **`global_filter_default.yel` resolved.** Stage 1 found it writes
   `[1,2,3,4].filter(|x| x > 2)` — `|` is not an operator, the catch-all ate the
   line, and *the module-scope filter path it guards has never been exercised*.
   Rewriting to `{ x -> x > 2 }` and re-blessing changes what reaches HIR, so it
   happens **before** numbers are taken, with a line in
   [`goldens-changed.md`](goldens-changed.md).
3. **The two silent `_ => {}` parser arms** filed as a `known_bugs` entry.
4. **Seam types landed on `main`** as compiling Rust.
5. **D1–D6 answered in writing** in the Decision log.

## Reference

- **ark** `~/Documents/Code/ark/compiler/arkc-hir/src/`: `hir_map.rs` ·
  `hir/hir_id.rs` · `hir/hir_node.rs` · `hir/module.rs` · `hir/visit.rs` ·
  `ty.rs`. ⚠️ The stub cited `parsety.rs`; **no such file** — verify every
  reference path before quoting it.
- **Frozen** `yel-core/src/hir/` (`lower.rs` 1,510 lines — an
  [A2](anti-spec.md#a2--no-god-pass) case) and `yel-core/src/thir/`.
- **Landed stage 1** `yelc-syntax/src/ast.rs` — `ItemKind`, `ComponentMember`,
  `GlobalMember`, `UiNode`, `NamedProp`, `Recovered<T>`, `MaybeIdent`. Read the
  doc comments: several record frozen grammar behaviour lowering must respect.

## Definition of done

- [ ] `yelc-hir` compiles; depends on `yelc-base`, `yelc-syntax`, `yelc-sema`
      and no other **workspace** crate (third-party is not what this clause is
      about — see the stacker precedent in [`seam-changes.md`](seam-changes.md)).
- [ ] H1–H5 each asserted by a named test, not by review.
- [ ] One walker, exhaustive, no `_` arm; no `collect_children_slots` counterpart.
- [ ] 2000 corpus programs + 91 positive + 23 diagnostic fixtures lower and
      check without panic.
- [ ] `Definitions` identical to the frozen tree's — contents **and order** —
      over the full corpus, via the read-only oracle crate.
- [ ] Diagnostic set identical in meaning, span, and order over the full corpus.
- [ ] No `String` in any HIR type. No `Ty` on any HIR **node** — the definition
      tables and the `types` side table carry `Ty` by design.
- [ ] `type_of` structurally unreachable from H1 phase 1 (the collector does not
      exist yet), not merely commented.
- [ ] D1–D6 recorded with reasoning.
- [ ] Adversarial review panel, read-only, one lens each.
- [ ] Surprises written — [D3](anti-spec.md#d3--a-stage-documents-what-surprised-it).

## Decision log

### D7 · Flatten `else if` chains

**Decided 2026-07-28, before briefing. Adopted: yes.** `If` drops
`else_if_branches`; an `else if` chain becomes a nested `If` in the `else` branch.

**Free parts, both checked first.** The nested `If` is *not* synthesized — stage
1 gives every `ElseIfBranch` its own `NodeId` and `Span`, so it maps to a real
AST node: **H2** holds, spans point at the actual `else if`, no diagnostic
moves. And visit order is unchanged (`a, b, c` either way), so D5 is unaffected.

**Not output-neutral** — [F10](findings.md#f10). The frozen lowering treats
`else if` as a flat N-way selector at one anchor and nested `if` as two
independent 1-way selectors.

**Obligations.**
1. **Stage 3b must recognise the chain** — a nested `If` whose `else` holds
   exactly one `If` and nothing else lowers as the flat N-way selector. Uniform
   IR, smart lowering. Without it, every `else if` in the corpus diverges.
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

*D1–D6: to be answered before briefing.*

## Numbers · Surprises

*To be written at close-out.*
