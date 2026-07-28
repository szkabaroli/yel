# Stage 2 — `yelc-hir`                    status: brief written · not briefed

Replaces (frozen, never edited): `crates/yel-core/src/hir/`
— `lower.rs` (1,510), `node.rs` (178), `expr.rs` (149), `local_scope.rs` (158).

Base: — · Started: — · Landed: —

> **Gate.** This brief is written; the stage is **not briefed**. Stage 1 is
> `implemented, in review` and invariant 2 says stage N+1 is not handed to an
> agent until stage N is closed out to its definition of done. Writing the brief
> early is the loop working as intended — *"a file filled in at the end is a
> report, not a brief"*. Handing it out early is not.
>
> Three prerequisites, all orchestrator work, all listed under
> [Prerequisites](#prerequisites-before-this-stage-is-briefed).

## Brief

Port `arkc-hir`'s structure to yel's item vocabulary: a name-resolved, still
untyped IR between `yelc_syntax::ParsedFile` and typeck, with bodies separated
from items by id and analysis results in side tables.

### Must honour

- **Register-then-lower ordering.** All items registered before any body lowers,
  so forward references resolve. Invariant H1, and the reason the frozen
  lowering works at all — see
  [`pass-register-then-lower`](../../.agents/skills/compiler-skills/rules/pass-register-then-lower.md).
  The frozen `lower_file` runs it in four phases: type defs → elements/externs/
  globals → component *headers* → component bodies.
- **Bidirectional `HirId ↔ NodeId` map** (ark's `hir_map.rs`: `map` +
  `rev_map`, `next_hir_id(node_id)` allocating and recording in one call). This
  is what lets a HIR diagnostic point at source, and what the LSP needs.
- **Side tables, not fattened nodes** (ark's `NodeMap<V>` keyed by `HirId`, with
  `assert!(old.is_none())` on insert) —
  [B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes).
  `CompilerContext::signal_deps` keyed by `DefId` is the existing positive
  precedent and stays.
- **Unresolved types stay unresolved** — a lazily-filled cell, so name
  resolution runs after construction without a second tree. **Not** interning a
  named type as `Unknown` and hoping something overwrites it —
  [B2](anti-spec.md#b2--no-deferred-name-resolution-encoded-as-a-lie). The frozen
  tree does exactly that:

  ```rust
  AstTyKind::Named(_) => {
      // Named types need resolution - return Unknown for now
      self.intern(InternedTyKind::Unknown)          // types/interner.rs:331
  }
  ```

  and calls it from registration (`intern_ast_ty` on every record field), so a
  record field whose type is a user record is `Unknown` in the definition table.
- **Bodies separated from items by id** (ark's `Module { node_types, bodies,
  elements }`; `FnBodyId` allocated from the same map).
- **One walker**: `hir/visit.rs`, exhaustive, no `_` arm
  ([A3](anti-spec.md#a3--no-duplicated-walkers)). The frozen tree has a second,
  hand-rolled one — `collect_children_slots` (`lower.rs:52`) re-walks the node
  tree to find `@children` markers, with its own match over every node kind.
- **Globals and components lower through one uniform item spine** —
  [D1](anti-spec.md#d1--the-compilation-unit-is-the-file-not-the-component).
  `HirItem` is already a real `{Component, Global}` enum with a symmetric
  accessor set; that shape is correct and carries over.
- **No `String` survives.** Stage 1 established S4 (names are interned) and the
  frozen HIR breaks it immediately in three places: `HirNodeKind::Element.name`,
  `HirBinding.name`, `HirHandler.name` are all `String`. Also
  [keep-list §3](keep-list.md).

### Inherited from stage 1 — assume all of it, re-verify none of it

S1–S6 hold on the input ([stage 1 § Contract](stage-1-syntax.md)). Explicitly:
`green.text() == content`, every AST node has a `NodeId` + `Span`, names are
interned, ill-formed input yields **a diagnostic AND an `Error` node**, and
parsing always returns a `ParsedFile` — there is no failure return.

What stage 1 says stage 2 may **not** assume: sibling item ordering beyond
source order, `NodeId` stability across reparses, any green-tree reuse API.
**Trivia attachment is stage 2's problem** — doc comments are read off the green
tree here, and stage 1 deliberately did not decide it.

### The stage-1 surprises that land on this stage

Four of stage 1's eight change what stage 2 receives:

| # | What it means for stage 2 |
|---|---|
| **1** | The frozen *parser* silently discarded malformed `global`/`record` members (`parse_global`'s `_ => {}`, `parse_record`'s rule filter). Stage 1 reports them. **More programs now reach HIR, carrying `Error` nodes the frozen HIR never saw.** `ItemKind::Error`, `ComponentMember::Error`, `UiNode::Error`, and `Recovered::Missing` must all lower to something — an error item, never a panic and never a skip. |
| **2** | `if {` parses as an **element named `if`** (keywords are not reserved). Element-name resolution will see `if`, `for`, `else` as element names. Reproduced deliberately; do not "fix" it here. |
| **5** | `name: func(…)` is a **property** in a component and a **callback** in a global. The landed AST records this in `PropertyDecl`'s doc comment. Registration must preserve the asymmetry. |
| **6** | `extern component`, the legacy `callback name(…);` form, and the `bind` prop modifier are real, are in fixtures, and are absent from `LANGUAGE.md`. All three lower here. |

## The decisions this stage must make

Each needs a written call **before** the agent starts. Copying the frozen
answer is a valid call; making it by accident is not.

| # | Decision | Frozen behaviour | Recommendation |
|---|---|---|---|
| D1 | **Do bindings and handlers stay split in HIR?** | `HirNodeKind::Element { bindings: Vec<HirBinding>, handlers: Vec<HirHandler> }` — two lists, classified syntactically during lowering | **No — one uniform prop list.** Stage 1's AST already unified them (`NamedProp { modifier, name, value: Expr }`); re-splitting here is stage 2 re-inventing a distinction the layer above deliberately removed, and the classification is *type*-directed ([§5](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger)) while HIR is pre-type. See below. |
| D2 | **`For.item_ty: Ty` in HIR** | `HirNodeKind::For` carries a `Ty` — a typeck result in an untyped IR | Remove. This is [B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes) verbatim. If stage 3 needs it keyed by node, that is a side table stage 3 owns. |
| D3 | **`For.item_name: Name`** — *"stored directly to avoid LocalScope lookup issues"* | a workaround duplicating what `LocalScope` knows | Fix the scope structure rather than porting the duplicate. If it is genuinely needed, it is a `NodeMap`, not a node field. |
| D4 | **Do globals get a body?** | `HirGlobal` carries `def_id`/`name`/`span`/`is_export` and **no body**; property defaults live in the `GlobalDef` side table | Asymmetric with `HirComponent`, which carries `body: Vec<HirNode>`. Either is defensible; D1's uniform-spine goal argues for symmetry. Decide explicitly — the frozen doc comment claims the asymmetry is deliberate. |
| D5 | **Item and diagnostic ordering** | `lower_file` lowers **all components, then all globals**, with the comment *"so the type-check order (and therefore diagnostic order) matches the previous components-then-globals pipeline"* | **Preserve exactly.** This is output-affecting: diagnostic order is asserted by the 23 diagnostic fixtures. It is also a trap — it is the one place where "uniform item spine" and "match the frozen output" pull apart. |
| D6 | **Trivia / doc-comment attachment** | not decided by stage 1; frozen tree has no doc comments | Decide the rule (nearest preceding comment run, no blank line) or decide explicitly not to attach yet. Do not leave it implicit. |

### On D1 — why one prop list

Three independent reasons, and one caveat:

- **Stage 1 already decided it.** `NamedProp` is one node. HIR re-deriving two
  lists from one is an analysis result stored on the node (B3).
- **HIR cannot classify correctly.** Whether `bumped: { count += 1; }` is a
  handler depends on `bumped`'s declared type being `func()` — a fact typeck
  owns. Classifying syntactically ("the value is a closure literal") is the
  heuristic [§5](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger)
  exists to replace.
- **The payload param falls out.** The frozen `HirHandler.param` exists so
  `drop: (payload) { … }` binds a body-scoped local. In the landed AST that is
  just `ClosureExpr { params, body }` — lower closure params uniformly and no
  handler-specific path is needed.

**Caveat, and it is the real work:** the frozen HIR lowering *uses* the split to
decide scoping (it pushes a scope for the handler param and not for a binding).
A uniform lowering must produce the same locals in the same order, because
`LocalId` ordinals reach THIR and the arena-parity comment in `HirHandler`'s
doc says typeck re-defines the param "to produce the THIR `LocalId` with
matching arena parity." Verify local allocation order is unchanged before
declaring D1 free.

## Contract — the stage-2 seam

> **Proposed. Lands on `main` as compiling Rust before the agent starts**
> ([`contract-before-fanout`](../../.agents/skills/compiler-rewrite/rules/contract-before-fanout.md)).
> A needed change is a request in [`seam-changes.md`](seam-changes.md), not an
> agent decision.

**Input:** `yelc_syntax::ParsedFile` + `&mut CompilerContext` (from `yelc-sema`:
interner, `Definitions`, diagnostics).
**Output:** `HirModule`.

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
}

/// Side table. One value per HirId, write-once.
pub struct NodeMap<V> { /* … */ }
impl<V> NodeMap<V> {
    pub fn insert(&mut self, id: HirId, v: V);   // asserts no prior value
    pub fn get(&self, id: HirId) -> Option<&V>;
}

/// A type as written, before resolution. Never `Unknown`-as-placeholder.
pub enum ParsedType {
    Primitive(PrimitiveType),
    Named { name: Name, span: Span, resolved: OnceCell<DefId> },
    List(Box<ParsedType>),
    Func { params: Vec<ParsedType>, ret: Option<Box<ParsedType>> },
    Error,
}

pub fn lower_file(parsed: &ParsedFile, ctx: &mut CompilerContext) -> HirModule;
```

### Invariants stage 2 ESTABLISHES (stage 3 may assume all of them)

- **H1 · Register-then-lower.** Every item is registered in `Definitions`
  before any body lowers. A body may reference any item in the file regardless
  of source order. *Asserted by:* a fixture whose first component references a
  record declared last.
- **H2 · The `HirId ↔ NodeId` map is total and bidirectional.** Every `HirId`
  in the module maps to a `NodeId` present in the input `ParsedFile`, and
  `hir_of(node_of(h)) == h`. *Asserted by:* a walk over every corpus program.
- **H3 · No analysis result on the node it describes.** No HIR node field is
  written by a later pass. No `Ty`, no capture set, no resolution outcome —
  those are `NodeMap`s. *Asserted by:* review, plus the absence of a `Ty` import
  in the HIR node module.
- **H4 · An unresolved name is unresolved.** A named type is
  `ParsedType::Named { resolved: OnceCell }` until resolution fills it; it is
  never interned as `Unknown`. *Asserted by:* a test that a record field of a
  user-record type resolves to that record's `DefId`, which fails against the
  frozen behaviour.
- **H5 · Nothing is silently dropped.** Every AST item, member, node, and
  `Recovered::Missing` produces either a HIR entity or a diagnostic. No `_ => {}`
  arm anywhere in lowering. *Asserted by:* exhaustive matches (no `_` arm) and a
  fixture per `Error` variant. This carries stage 1's S5 through stage 2 —
  and it is the invariant the frozen *parser* broke for the whole of its life
  ([stage 1 § Surprises 1](stage-1-syntax.md)).

### What stage 3 may NOT assume

- **No types.** HIR is untyped. `ParsedType` is a *syntactic* type with a
  resolution cell, not a `Ty`.
- **No `HirId` stability across reparses**, same as `NodeId`.
- **No classification of element props** into bindings vs handlers, if D1 lands
  as recommended. Stage 3 does that with the declared type in hand.
- **No capture sets.** [§4](directions.md#4--closures-are-a-value-and-the-new-irs-are-shaped-for-one)
  is a stage-3 decision; HIR carries closure params and bodies, nothing more.

## How this stage is verified — read this before writing the DoD

**There is no HIR differential available, and pretending otherwise is the
failure mode this section exists to prevent.** The CLI exposes `ast`, `ir`
(LIR), `check`, and `compile` — there is no `hir` dump, the frozen tree may not
be edited to add one, and the two HIRs are *designed* to have different shapes,
so a serialized byte-diff would be meaningless even if it existed.

What is genuinely comparable, in descending order of strength:

1. **The `Definitions` table after lowering.** Names, kinds, spans, and
   **order** — `DefId`s are ordinals that reach output ordering, so this is
   load-bearing and shape-independent. Comparable via a **new, read-only oracle
   crate** that depends on frozen `yel-core` as a library and calls
   `hir::lower_file` — this reads the frozen tree, it does not edit it, and is
   allowed by
   [`greenfield-never-touch-old-code`](../../.agents/skills/compiler-rewrite/rules/greenfield-never-touch-old-code.md).
2. **HIR-stage diagnostics.** Duplicate-name errors and friends are emitted here
   and are observable through frozen `yelc check`. Compare the diagnostic set —
   meaning, span, **and order** (D5) — over the 2000-seed corpus, the 91
   positive fixtures, and the 23 diagnostic fixtures.
3. **No panic, total lowering.** Every corpus program produces an `HirModule`.

None of these is the artifact-level differential that
[`verify-differential-not-review`](../../.agents/skills/compiler-rewrite/rules/verify-differential-not-review.md)
asks for, because stage 2 has no artifact. **Full attribution arrives at stage
3**, when `check` output becomes comparable end-to-end. That is a known,
accepted weakness of this stage and the reason its definition of done leans
harder on invariant tests than stage 1's did — not a reason to weaken either.

## Prerequisites before this stage is briefed

1. **Stage 1 closed out** to its definition of done (invariant 2).
2. **`global_filter_default.yel` resolved.** Stage 1 found the fixture writes
   `[1,2,3,4].filter(|x| x > 2)` — `|` is not an operator, pest's catch-all ate
   the line, and *the module-scope filter path it guards has never been
   exercised*. Rewriting it to `{ x -> x > 2 }` and re-blessing changes what
   reaches HIR, so it happens **before** stage 2's numbers are taken, and gets a
   line in [`goldens-changed.md`](goldens-changed.md).
3. **The two silent `_ => {}` parser arms** filed as a `known_bugs` entry the
   rewrite fixes ([stage 1 § Surprises 1](stage-1-syntax.md) follow-ups).
4. **The seam types above landed on `main`** as compiling Rust.
5. **D1–D6 answered in writing** in this file's Decision log.

## Reference

- **ark** (`~/Documents/Code/ark/compiler/arkc-hir/src/`):
  `hir_map.rs` · `hir/hir_id.rs` (`HirId`, `FnBodyId`) · `hir/hir_node.rs` ·
  `hir/module.rs` · `hir/visit.rs` · `ty.rs`.
  ⚠️ The stub cited `parsety.rs`; **there is no such file** — the type
  representation lives in `ty.rs`. Verify every reference path before quoting it.
- **Frozen** `yel-core/src/hir/` — `lower.rs` (1,510 lines, one struct, also an
  [A2](anti-spec.md#a2--no-god-pass) case), `node.rs`, `expr.rs`,
  `local_scope.rs`.
- **Landed stage 1** `yelc-syntax/src/ast.rs` — `ItemKind`, `ComponentDecl` +
  `ComponentMember`, `GlobalDecl` + `GlobalMember`, `UiNode`, `NamedProp`,
  `Recovered<T>`, `MaybeIdent`. Read the doc comments: several record frozen
  grammar behaviour that lowering must respect.

## Definition of done

- [ ] `yelc-hir` compiles; depends on `yelc-base`, `yelc-syntax`, `yelc-sema`
      and **nothing else** (third-party crates are not what this clause is
      about — see the stacker precedent in [`seam-changes.md`](seam-changes.md)).
- [ ] H1–H5 each asserted by a named test, not by review.
- [ ] `hir/visit.rs` exhaustive, no `_` arm; `collect_children_slots` has no
      counterpart (the one walker finds `@children`).
- [ ] All 2000 corpus programs + 91 positive + 23 diagnostic fixtures lower
      without panic.
- [ ] `Definitions` table identical to the frozen tree's — contents **and
      order** — over the full corpus, via the read-only oracle crate.
- [ ] HIR-stage diagnostic set identical in meaning, span, and order over the
      full corpus.
- [ ] No `String` in any HIR type. No `Ty` in any HIR node.
- [ ] D1–D6 recorded in the Decision log with reasoning.
- [ ] Adversarial review panel, read-only, one lens each.
- [ ] Surprises written. *"A stage documents what surprised it"* —
      [D3](anti-spec.md#d3--a-stage-documents-what-surprised-it).

## Numbers · Decision log · Surprises

*To be written at close-out.*
