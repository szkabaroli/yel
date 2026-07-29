# Stage 2a — `yelc-hir`, build + resolve            status: brief written

Replaces (frozen, never edited): `crates/yel-core/src/hir/` (1,995 lines).
Phase **2a** of the merged HIR stage; phase 2b is
[`stage-2b-hir-check.md`](stage-2b-hir-check.md). Same crate, run in sequence.

Base: — · Started: — · Landed: —

> **Gate.** Stage 1 landed (`33e5c71`, 0 corpus divergences). Still **not
> briefed**: seam types are not on `main` and D1–D6 are unanswered. See
> [Prerequisites](#prerequisites).

## The shape (shared with 2b)

**One IR, two phases, types in a side table.** One node vocabulary, one walker.
`types: NodeMap<Ty>` is empty after 2a and total after 2b —
[`seam-changes.md`](seam-changes.md), 2026-07-28.

| phase | does | produces |
|---|---|---|
| **2a** *(this file)* | AST → HIR; register items; resolve names; collect declared types | HIR + `Definitions` typed |
| **2b** | bidirectional type checking over the same nodes | `types` map total |

**2a's output is a public surface, not an internal intermediate.** Yel will have
lints, and early (syntactic) lints run here while type-aware lints run after 2b.
Same nodes, same walker, a lint written once. Lint results are side tables like
every other analysis output
([B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)).

## Brief

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
  2b shares this walker; it does not get its own.
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

## What lowerings belong here

**HIR is name-resolved before it is typed, so a desugaring belongs in 2a iff it
is decidable from names alone.**

| needs | belongs in |
|---|---|
| nothing but the syntax tree | 2a, any phase |
| the definition tables | 2a, **phase 2 or 3** — never phase 1 ([H1](#h1)) |
| a *type* to choose the target | [2b](stage-2b-hir-check.md) |
| the whole module (fan-out, ordering) | [3b](stage-3b-lower.md) — e.g. `resolve_global_triggers` |

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

### Candidates, and what blocks each

Not proposals — a list so nobody re-derives it. The rustc rule that generates it:
**desugar to a general form early, then lower the general form once.** Six Rust
constructs (`for`, `while`, `while let`, `if let`, `?`, `let else`) collapse to
`loop` + `match` before HIR exists, so MIR building handles two, not six.

| candidate | today | blocked on |
|---|---|---|
| `Ternary` → a general conditional | carried by **all four IRs**; `ExprKind` has `Ternary` and no `If`, so yel has *three* unrelated conditional constructs — expression, statement, UI node ([F18](findings.md#f18)) | the `match` / general-conditional decision in [3b's gap table](stage-2b-hir-check.md#gaps-inherited-as-decisions-not-copies). **The target form does not exist yet.** |
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

## Decisions

**All decided 2026-07-29.** Reasoning in the [Decision log](#decision-log).

| # | decision | answer |
|---|---|---|
| D1 | Do bindings and handlers stay split? | **No — one uniform prop list.** 2b classifies. [log](#d1--bindings-and-handlers-are-one-uniform-prop-list) |
| D2 | `For.item_ty: Ty` on the node | **Remove.** [log](#d2--for-does-not-carry-the-item-type) |
| D3 | `For.item_name` *"stored directly to avoid LocalScope lookup issues"* | **Remove; fix the scope structure.** [log](#d3--for-does-not-carry-the-loop-variable-name) |
| D4 | Do globals get a body? | **No.** `HirGlobal` carries only its functions; defaults stay in `GlobalDef`. [log](#d4--hirglobal-has-no-body--only-its-functions) |
| D5 | Item and diagnostic ordering | ⚠️ **Globals then components** — reverses the frozen lowering order, and carries a measured obligation. [log](#d5--globals-lower-before-components) |
| D6 | Trivia / doc-comment attachment | **Attach** — nearest preceding comment run, no blank line. [log](#d6--doc-comments-attach-to-the-nearest-preceding-comment-run) |
| D7 | Flatten `else if` into nested `If`? | **Decided: yes** — [log](#d7--flatten-else-if-chains) |
| D8 | What identifies a module — one `SourceId`, or the file set? | **Decided: `ModuleId` + `Vec<SourceId>`** — [log](#d8--a-module-is-identified-by-itself-not-by-a-file) |

### D1

Recommendation: **one uniform prop list.** Stage 1's AST already unified them
into `NamedProp { modifier, name, value }`; re-deriving two lists is an analysis
result on the node (B3). HIR *cannot* classify correctly anyway — whether
`bumped: { … }` is a handler depends on `bumped`'s declared type, which
[2b](stage-2b-hir-check.md) owns ([F8](findings.md#f8)). And the payload param
falls out: the frozen `HirHandler.param` exists so `drop: (payload) { … }` binds
a body-scoped local, which in the landed AST is just `ClosureExpr { params, body }`.

**Caveat, and it is the real work:** the frozen lowering *uses* the split to
decide scoping. A uniform lowering must produce the same locals in the same
order — `LocalId` ordinals reach the type checker, and `HirHandler`'s doc says
typeck re-defines the param "to produce the THIR `LocalId` with matching arena
parity." Verify local allocation order is unchanged before declaring D1 free.

## Contract

> **Proposed. Lands on `main` as compiling Rust before the agent starts**
> ([`contract-before-fanout`](../../.agents/skills/compiler-rewrite/rules/contract-before-fanout.md)).
> A needed change is a request in [`seam-changes.md`](seam-changes.md).
>
> **2a owns this contract**; 2b assumes it and adds only the `types` map.

**Input:** `&[yelc_syntax::ParsedFile]` — the **whole file set** — plus
`&mut CompilerContext` (from `yelc-sema`: interner, `Definitions`, diagnostics).
**Output:** `HirModule` with `Definitions` typed and `types` empty.

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
    pub id:      ModuleId,        // identity of the module, not of a file
    pub sources: Vec<SourceId>,   // the file *set* — see "Multiple files"
    pub items:   IndexVec<HirItemId, HirItem>,
    pub bodies:  IndexVec<BodyId, HirBody>,
    pub map:     HirMap,
    pub types:   NodeMap<Ty>,     // empty after 2a, total after 2b
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

### Designed for serialization — what 2a owes §6

[§6](directions.md#6--modules-are-serializable-artifacts) needs the module to be
writable and re-readable. **None of that is implemented here**; what 2a owes is
that it stays *possible*, which costs three decisions made now and nothing later.
Swift's mechanism is the reference — see §6 for the `XREF` citation.

**The rule: two ID classes, and only one of them is ever written.**

| | internal | external |
|---|---|---|
| what | `DefId`, `HirId`, `BodyId`, `Ty` | `DefPath` |
| shape | dense index into this module's table | module id + a path of name pieces |
| resolved by | array index — O(1), used everywhere in-process | **lookup** in the target module |
| on mismatch | undefined behaviour, silently wrong | a **diagnostic**, loudly |
| serialized? | **never** | yes — this is the only thing that crosses |

```rust
/// The only identity that crosses a module boundary. Serialized; resolved by
/// lookup on load. A `DefId` is NEVER serialized — it is an index into a table
/// the reader does not own.
pub struct DefPath {
    pub module: ModuleId,
    pub pieces: Vec<PathPiece>,
}

pub enum PathPiece {
    /// A top-level item: record, enum, variant, element, global, component.
    /// `ns` is the existing `Namespace` — it is the kind discriminator.
    Item { name: Name, ns: Namespace },
    /// A member: field, variant case, property, function.
    /// `overload` is `None` for everything a user can write.
    Member { name: Name, ns: Namespace, overload: Option<OverloadKey> },
}
```

**Three things this pins down.**

1. **`Ty` is structural on the wire, a handle in memory.** Serialization writes
   the `InternedTyKind` shape recursively; loading re-interns into the host
   interner. There is no `Ty` remap table, so there is none to forget.
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

**Deferred, deliberately:** the serializer itself, the lazy-load offset table
(Swift's index block — worth allowing in the format, not worth building for
modules this small), and the format version constant. The format recommendation
(serde + postcard in a hand-written section envelope, and the two traps it walks
into) is in [§6](directions.md#the-format-serde--postcard-in-a-hand-written-envelope).

**One trap is already armed in this crate's dependencies.** `Ty` is
`pub struct Ty(pub u32)` and **already derives `Serialize`/`Deserialize`**, so a
derive on any struct containing a `Ty` silently writes the interner handle. A
serialized position needs a wrapper that writes the type's *structure*. Code that
merely compiles is wrong here, which is why it is a DoD line and not a comment. What is *not* deferred is
the `DefId`/`DefPath` split, because retrofitting it means touching every
downstream consumer that holds a `DefId`.

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
| **expressions** | **[2b](stage-2b-hir-check.md)** | bidirectional inference → `types` map |

The definition tables carry real `Ty` and always have ([F5](findings.md#f5)) —
rustc's `type_of(def_id)`-before-body-check split, not a deviation. **The frozen
bug is phase ordering only** ([F3](findings.md#f3)): same function, same output
type, wrong moment.

Keeping the written syntax around exactly long enough for there to be something
to resolve against is the whole point of not re-representing types, and is what
makes [H4](#h4) achievable rather than aspirational.

### Multiple files

This phase merges them. **There are no includes:** `ItemKind` has no `Import`
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

### Invariants this phase ESTABLISHES

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

<a id="h2"></a>
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

## Verification

**2a has no artifact of its own** ([F14](findings.md#f14)) — and pretending
otherwise is the failure mode this section prevents. What is comparable,
strongest first:

1. **The `Definitions` table** — contents **and order**, since `DefId`s are
   ordinals that reach output ordering. Shape-independent, so it works across two
   different IR designs. Compare via a **new read-only oracle crate** that
   depends on frozen `yel-core` as a library and calls `hir::lower_file`: this
   reads the frozen tree, does not edit it, and is allowed by
   [`greenfield-never-touch-old-code`](../../.agents/skills/compiler-rewrite/rules/greenfield-never-touch-old-code.md).
2. **Diagnostics** — meaning, span, **and order** (D5) — over the 2000-seed
   corpus, 91 positive and 23 diagnostic fixtures, via frozen `yelc check`.
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
| declared types | from H1 phase 2; expressions stay untyped until 2b |
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
[2b](stage-2b-hir-check.md#verification).

## Prerequisites

1. ~~Stage 1 closed out~~ ✅ `33e5c71`, 2026-07-28.
2. **`global_filter_default.yel` resolved.** Stage 1 found it writes
   `[1,2,3,4].filter(|x| x > 2)` — `|` is not an operator, the catch-all ate the
   line, and *the module-scope filter path it guards has never been exercised*.
   Rewriting to `{ x -> x > 2 }` and re-blessing changes what reaches HIR, so it
   happens **before** numbers are taken, with a line in
   [`goldens-changed.md`](goldens-changed.md).
3. **The two silent `_ => {}` parser arms** filed as a `known_bugs` entry.
   *Verified absent 2026-07-28* — `known_bugs/` holds only `README.md` and
   `runtime/s32_to_string_aliasing.yel`.
4. **`yelc-sema` exists** — brief: [`infra-sema.md`](infra-sema.md).
   ⚠️ **The real blocker, and larger than "seam types" sounds.** This phase's
   input is `&mut CompilerContext` *from `yelc-sema`*, a crate that does not yet
   exist while six plan documents depend on it. Frozen equivalent, minus what
   `yelc-base` already carries:

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

   **The precedent is `yelc-base`:** shared infrastructure, no stage number,
   landed before the stage that needs it. `yelc-sema` is the same category — it
   is not a pipeline stage, it transforms no IR. But it carries real design
   decisions ([§1](directions.md#1--builtins-are-a-table-not-a-field-per-builtin)
   lives entirely inside it; so does the
   [`DefId`/`DefPath` split](#designed-for-serialization--what-2a-owes-6) and
   `Ty`'s structural serialization), so it needs a written scope before someone
   starts, not just a `cargo new`.

   Unlike 2a it **does** have a standalone artifact: `lookup_known_definitions`
   registers builtins from no input at all, so the resulting `Definitions` table
   is comparable against the frozen one before any source is parsed.

5. **2a's own seam types landed on `main`** as compiling Rust — `HirId`,
   `BodyId`, `HirMap`, `HirModule`, `NodeMap`, `TypeId`, `type_of`,
   `lower_files`.
6. ~~**D1–D6 answered in writing** in the Decision log~~ ✅ 2026-07-29. D7 and D8
   were already decided. **D5 carries an obligation into the stage** — the
   corpus item-order differential; see its log entry.

## Reference

- **ark** `~/Documents/Code/ark/compiler/arkc-hir/src/`: `hir_map.rs` ·
  `hir/hir_id.rs` · `hir/hir_node.rs` · `hir/module.rs` · `hir/visit.rs` ·
  `ty.rs`. ⚠️ An earlier stub cited `parsety.rs`; **no such file** — verify every
  reference path before quoting it.
- **Frozen** `yel-core/src/hir/` — `lower.rs` 1,510 lines, an
  [A2](anti-spec.md#a2--no-god-pass) case.
- **Landed stage 1** `yelc-syntax/src/ast.rs` — `ItemKind`, `ComponentMember`,
  `GlobalMember`, `UiNode`, `NamedProp`, `Recovered<T>`, `MaybeIdent`. Read the
  doc comments: several record frozen grammar behaviour lowering must respect.

## Definition of done

- [ ] `yelc-hir` compiles; depends on `yelc-base`, `yelc-syntax`, `yelc-sema`
      and no other **workspace** crate (third-party is not what this clause is
      about — see the stacker precedent in [`seam-changes.md`](seam-changes.md)).
- [ ] H1–H5 each asserted by a named test, not by review.
- [ ] One walker, exhaustive, no `_` arm; no `collect_children_slots` counterpart.
- [ ] 2000 corpus programs + 91 positive + 23 diagnostic fixtures build and
      resolve without panic.
- [ ] `Definitions` identical to the frozen tree's — contents **and order** —
      over the full corpus, via the read-only oracle crate.
- [ ] Diagnostic set identical in meaning, span, and order over the full corpus.
- [ ] No `String` in any HIR type. No `Ty` on any HIR **node** — the definition
      tables and the `types` side table carry `Ty` by design.
- [ ] `type_of` structurally unreachable from H1 phase 1 (the collector does not
      exist yet), not merely commented.
- [ ] **`yelc2 --emit-hir` renders this phase's output**, yel-flavoured, showing
      resolved `DefId`s, declared types, and the desugarings. Byte-stable across
      runs. Explicitly **not** round-trippable — see above.
- [ ] **No `DefId` is reachable from a serializable position.** `DefPath` is the
      only identity that crosses a module boundary
      ([§6](directions.md#6--modules-are-serializable-artifacts)); a `DefId` in a
      would-be-serialized struct is the bug this split exists to prevent.
- [ ] `HirModule` carries a `ModuleId` and a *set* of `SourceId`s — not one
      source. A module is built from the file set (H1), so a single-source field
      is a category error.
- [x] D1–D6 recorded with reasoning. ✅ 2026-07-29
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
1. **[Stage 3b](stage-3b-lower.md) must recognise the chain** — a nested `If`
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

### D8 · A module is identified by itself, not by a file

**Decided 2026-07-28, before briefing.** `HirModule` carries `id: ModuleId` and
`sources: Vec<SourceId>`. It does **not** carry a single `SourceId`.

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
[§6](directions.md#6--modules-are-serializable-artifacts) needed a `ModuleId` to
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
name, value }` — and [2b](stage-2b-hir-check.md) classifies using the declared
type.

The rationale is above under [D1](#d1) and does not repeat here. The **caveat is
binding and is 2a's to discharge, not to note**: the frozen lowering uses the
split to decide scoping, `LocalId` ordinals reach the type checker, and
`HirHandler`'s doc says typeck re-defines the param "to produce the THIR
`LocalId` with matching arena parity." A uniform lowering must produce the same
locals in the same order. That is a test — locals of a closure-valued prop
enumerated in allocation order, against the frozen tree's — not a review remark
([A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)).

Couples to [F1](open-decisions.md#f1--how-is-a-bodys-trigger-determined), which
is 2b's and still open.

### D2 · `For` does not carry the item type

**Decided 2026-07-29: remove `item_ty: Ty`.**
[B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)
verbatim — a typeck result stored on an untyped IR node. If 2b needs it keyed by
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
free once the scope structure is right ([A9](anti-spec.md#a9--load-bearing-or-deleted)).

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
   direction *does* become load-bearing is [3b](stage-3b-lower.md) —
   initialization order, and `resolve_global_triggers`.
2. **Name resolution does not currently enforce it.** `lower.rs:1169–1174`
   resolves a bare identifier through `Value → Type → Component`, and that path
   is shared by global function bodies — so a bare component name written inside
   a global resolves to `HirExprKind::Def(component_def_id)` today. The direction
   holds because the grammar gives a global no place to *use* a component, not
   because resolution refuses. If 2a wants the rule enforced rather than merely
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

**The obligation this creates, and it is 2a's:** run the corpus differential and
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
blank line between it and the item.**

Stage 1 deliberately left this open — the green tree holds trivia and decides
nothing about ownership ([`stage-1-syntax.md`](stage-1-syntax.md)), so 2a owns
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

*To be written at close-out.*
