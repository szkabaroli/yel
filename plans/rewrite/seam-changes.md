# Seam changes — contract change log

> **Append-only.** Rule:
> [`orchestrate-integrator-owns-seams`](../../.agents/skills/compiler-rewrite/rules/orchestrate-integrator-owns-seams.md)

Agents implement against seam types; they never edit them. When an agent finds
the contract genuinely wrong, it **stops and files a request here** — it does not
work around the seam locally and it does not change it unilaterally. Two agents
that each "fixed" the IR between them produce two reasonable designs that do not
compose, and the merge is not a merge; it is a third rewrite done by whoever is
holding the branch.

The integrator (rewrite owner) decides. The decision is recorded here whether it
was granted or refused — a refused request is as useful to the next agent as a
granted one, and stops the same request arriving three times.

## Format

```markdown
## YYYY-MM-DD — <seam> — <one-line summary>

**Requested by:** <agent / stage>
**Request:** what the agent hit and what it wants changed.
**Options considered:** at least two, with what each costs.
**Decision:** granted / refused / modified — and the reasoning.
**Blast radius:** which stages must be re-checked; which landed code changes.
```

## Log

## 2026-07-28 — stage numbering — renumbered to close the gap left by the merge

**Requested by:** the integrator. Supersedes the "stage numbering downstream is
unchanged" clause in the entry below.

**Request.** After HIR and THIR merged, the stages read 1, 3/4, *(nothing)*,
5/6, 5. Close the gap, in file names too.

| was | is | crate |
|---|---|---|
| 5 | **3** | `yelc-lir` |
| 6 | **4** | `yelc-lower` |
| 5 | **4** | `yelc-codegen` |

`stage-4-lir.md` → `stage-3-lir.md`; `stage-7-codegen.md` → `stage-7-codegen.md`.

**Why the earlier reasoning did not hold.** The merge entry argued renumbering
"would break every existing cross-reference to buy nothing", citing
[A17](anti-spec.md#a17--test-input-selection-is-stable-under-renames). That rests
on renames being expensive to verify — and they are not: a mechanical
link-and-anchor check over `plans/rewrite/` proves the result in one command.
What the gap actually bought was a permanent numbering hole with no marker in it
(the tombstone that explained it was deleted as unreferenced noise), which is a
standing invitation for someone to renumber it later, wrongly, by hand.

**Two collisions checked before touching anything**, because "stage N" is
overloaded in this repo:

1. **`lir-resource-flatten` has its own stages** — "Stage 4", "Stage 5c", "Stage
   5e-4" appear throughout `crates/yel-core/src/lir/` and
   `crates/yel-wasm-codegen/`. Different scheme, frozen code, **untouched**.
2. **Cutover phases are not stage numbers.** "cutover phase 4" (deletion) in
   `stage-7-codegen.md`, `README.md`, `corpus.md` and `stage-1-syntax.md`,
   including the `#final-deletion--cutover-phase-4` anchor, **untouched**.

**Blast radius.** `plans/rewrite/*.md` only — 8 files plus 2 renames. Two
headings changed anchor (`keep-list.md` §10 "(stage 5)" → "(stage 4)";
`anti-spec.md` § C "(stages 4–5)" → "(stages 3–4)") and every inbound link was
repointed. Verified: **0 broken links or anchors** across all 14 docs. No source
file, test, or fixture references rewrite stage numbers.

## 2026-07-28 — stage boundary — HIR and THIR merge into one IR, two phases

**Requested by:** the integrator (project owner). First move made under the
boundary relaxation logged immediately below; that entry's requirements are
answered here.

**Request.** Stages 2 (`yelc-hir`) and 3 (`yelc-thir`) become **one crate with
two sequential phases** — `yelc-hir`, phase **3** build + resolve, phase **4**
check. The stage list becomes **AST → HIR → LIR → WASM**; THIR ceases to exist
as a stage name.

**Why.** Three independent lines arrived at the same shape:

1. **Serialization** ([directions §6](directions.md#6--modules-are-serializable-artifacts)).
   An artifact that lets a consumer skip typecheck contains types. "Make HIR
   self-contained and typed" and "make HIR into THIR" are the same sentence.
2. **[A3](anti-spec.md#a3--no-duplicated-walkers)**. Two IRs mean two node
   vocabularies and two visitors over one language.
3. **rustc's reasons do not transfer — but not the reason first written here.**
   The first draft of this entry said "yel has no lints". **Yel will have
   lints**, so that justification is withdrawn. It was also confused: rustc's
   lints run on **HIR**, and THIR is not a lint surface at all. "HIR is the lint
   surface" argues for HIR existing — which it does, as the IR before phase 4
   fills the type map — not for a *second* IR after it. What genuinely does not
   transfer is THIR's actual job: pattern-exhaustiveness desugaring for `match`,
   which yel has no `match` for. Swift — the reference §6 is named after —
   type-checks one AST in place.

   **Lints argue for the phase split, and against option 2.** An early
   (syntactic) lint wants the IR with no types; a type-aware lint wants the same
   IR with the map total. Two phases over one vocabulary give both surfaces for
   free and let a lint be written once. Two IRs would force lint authors to pick
   a vocabulary, or write the lint twice.

The shape: **one node vocabulary, `types: NodeMap<Ty>` empty after 3 and total
after 4.** Types live beside nodes, not on them
([B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)).

**Options considered.**
1. **Keep two stages**, artifact at the THIR boundary. Preserves per-stage
   attribution; keeps the duplicated vocabulary and leaves stage 2 with no
   diffable artifact at all.
2. **One undivided stage.** Simplest table, largest single agent task in the
   rewrite — the skill's own guidance says a stage that will not fit one agent
   contains a seam worth contracting.
3. **3 + 4 in one crate, run in sequence.** Chosen.

**Decision: granted as option 3**, mirroring the existing 3/4 precedent
exactly, so the process around it is unchanged.

### What is lost, and what replaces it

The relaxation entry requires this section, so it is answered plainly.

**Lost:** independent attribution between building/resolution and checking. A
divergence that would have been "stage 2 or stage 3" is now "stage 3 or 4".

**Replaced by — and this is a net gain, not a mitigation:** stage 2 previously
had **no artifact whatsoever**. HIR is not dumpable, the frozen tree may not be
edited to add a dump, and the two HIRs were designed to differ in shape, so
nothing could be diffed
([3 § Verification](stage-3-hir-build.md#verification)). The merged stage ends
at a **serializable typed module**, which is byte-comparable. Attribution between
3 and 4 is recovered the cheap way: 4 runs as a separate pass over the output
of 3, so a divergence is bisected by dumping after 3.

### Consequence: 3's output is a public surface, not an internal intermediate

Because lints are coming, the IR after phase 3 is **read by consumers that do
not transform it** — early lints, and the LSP. That is a design requirement on
the merged stage, not a later concern:

- **Both lint classes are phase positions, not IRs.** Syntactic lints run after
  3 (type map empty); type-aware lints run after 4 (type map total). Same
  nodes, same walker, one lint written once.
- **Lint results are side tables**, like every other analysis output —
  [B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes).
  A lint may not annotate the node it fires on.
- **Node identity and spans must be good enough to point a diagnostic at**,
  which H2 and H3 already require for a different reason. Lints and the LSP want
  exactly what the differential wants: stable ids, real spans, one exhaustive
  walker. No new mechanism is owed — but "3's output is inspectable" moves from
  a testing convenience to a contract.

**Blast radius.**
- `scope.md` stage-order row: THIR removed.
- `README.md`: status table, crate layout, dependency graph.
- `stage-2-hir.md`: absorbed stage 3's brief, then split per phase into
  `stage-3-hir-build.md` + `stage-4-hir-check.md`.
- `stage-3-thir.md`: **deleted.** Its content moved into the stage-2 files — none
  discarded, including the inherited-gap table (closure capture, function-type
  inference, generics, `match`, `color`/`brush`) and the §5 trigger
  sub-decision. It briefly existed as a tombstone; once every inbound link was
  repointed there was nothing left for it to redirect, and a redirect with no
  referrers is noise. The file is in git history at `33e5c71` if the original
  wording is ever wanted.
- ~~**Stage numbering downstream is unchanged.**~~ **Superseded the same day —
  see the entry above this one.** The original reasoning ("renumbering breaks
  every cross-reference to buy nothing", citing
  [A17](anti-spec.md#a17--test-input-selection-is-stable-under-renames)) rested
  on renaming being expensive to verify. It is not: a mechanical link-and-anchor
  check over `plans/rewrite/` makes it cheap and provable, and a permanent
  numbering gap with no marker is a standing invitation to "fix" it wrongly.
  Stages were renumbered to 3/4 (LIR) and 4 (codegen).
- `yelc-thir` is never created. The crate layout loses a row rather than gaining
  a transitional one ([A4](anti-spec.md#a4--no-permanent-bridge)).

## 2026-07-28 — `scope.md` stage-order freeze — relaxed, with a reason attached

**Requested by:** the integrator (project owner), while scoping
[directions §6](directions.md#6--modules-are-serializable-artifacts).

**Request.** *"We do not strictly need to keep module boundaries between stages
if they dictate a wrong pattern."* [`scope.md`](scope.md) currently freezes
*"Stage names and their order: AST → HIR → THIR → LIR → WASM"* in the same column
as the surface syntax and the WIT world.

**Background.** The freeze was written to stop a stage agent from redesigning the
pipeline because a local problem looked easier one layer up. That reason is
sound, but it is not the reason the table *gives*, and the two are different:

- A boundary is frozen because **differential attribution depends on it** — one
  stage in flight, one place a divergence can come from. Move a boundary and the
  divergence is no longer attributable to a stage.
- A boundary is *not* sacred because the frozen compiler happened to draw it
  there. `docs/ARCHITECTURE.md` describes the old code and
  [does not constrain the new design](README.md).

**Options considered.**
1. **Leave frozen.** Cheapest, and wrong for the stated reason: it makes the
   freeze an unexamined inheritance rather than a working constraint.
2. **Unfreeze entirely.** Restores exactly the failure mode the rule prevents —
   every stage agent gets to relitigate the pipeline.
3. **Freeze with a stated reason and a named exit.** Boundaries hold by default;
   moving one is an integrator decision recorded here, with the attribution cost
   named and a plan for how the affected stages are verified without it.

**Decision: granted, as option 3.** The stage *order* is unchanged and no
concrete move is authorised by this entry. What changes is the standing of the
rule: an agent that believes a boundary dictates a wrong pattern now has a
channel — a request here — instead of either silently working around it
([A4](anti-spec.md#a4--no-permanent-bridge)) or silently obeying it.

The first case examined under the new standing was **whether the serialized
module artifact forces HIR and THIR to merge**, and the answer was **no**: the
artifact belongs at the THIR boundary, which already exists. Recorded in §6 with
the Swift/rustc evidence. A relaxed rule that produces a well-argued *no* on its
first use is working.

**Blast radius.** `scope.md`'s row is amended to carry the reason. No stage
changes. Any future boundary move must state, in its own entry: which stages lose
independent attribution, and what replaces the differential for them —
[`verify-differential-not-review`](../../.agents/skills/compiler-rewrite/rules/verify-differential-not-review.md)
does not stop applying because a boundary moved.

## 2026-07-25 — stage 1 / `yelc-syntax` dependencies — `stacker` admitted

**Requested by:** stage-1 implementer, after review round 2.

**Request.** `yelc-syntax` takes a third-party dependency on `stacker` (0.1.24)
to guard `ast::visit::walk_expr`. This departs from the stage-1 definition of
done, which says the crate "depends only on `yelc-base`".

**Background.** `parse_binary`/`parse_postfix` are iterative loops, so
`a.b.b.b…` builds an arbitrarily long `Box<Expr>` spine from **valid,
diagnostic-free** input while `MAX_NESTING_DEPTH` reads 2 — anti-spec A11. Three
consumers overflowed: `green.text()`, green `Drop`, and `Expr` `Drop` were made
iterative (now pass at n=500,000). `walk_expr` could not be: the recursion runs
through the overridable `Visitor::visit_expr` hook, so flattening it into a
worklist would stop calling that hook on spine nodes, breaking the single-walker
rule (A3).

**Options considered.**
1. **`stacker`.** Same mechanism and same reason as rustc's own
   `rustc_data_structures::stack::ensure_sufficient_stack`. Cost: one
   third-party dependency on a frontend crate.
2. **Dependency-free restructuring** (`#[inline(never)]` frame splitting).
   Measured, not assumed: reached n=12,986 — still below the frozen parser's
   ~14,544 — so it would ship a ceiling under the oracle.
3. **Bound the chain length.** Rejected: on the parse-only comparison it is a
   narrowing (see the correction below).

**Decision: granted.** Three reasons. The DoD line meant **internal crate-graph
discipline** — no edge to the frozen tree, and no path from `yelc-lir`/
`yelc-codegen` to a frontend crate — not a ban on third-party crates;
`yelc-base` itself carries `serde`, `rustc-hash`, and `parking_lot`, so the
strict reading was never the operative one. The precedent is exact: rustc solves
this identical problem (unbounded recursion over user-controlled structure in a
compiler frontend) with this identical mechanism. And option 2 was measured to be
a known narrowing, which A10/A11 exist to prevent.

The DoD wording is corrected in `stage-1-syntax.md` to say what it meant.

**Blast radius.** `yelc-syntax` only. The rule that matters downstream is
unchanged and is checked per stage: `yelc-lir` and `yelc-codegen` have no
dependency path to any frontend crate.

### Correction to the measurement this decision rests on

The implementer reported "frozen aborts at n≈14,544, so every available bound
would be a narrowing." Verified, with a caveat the integrator found and the
implementer did not report:

| what was measured | threshold (`a.b` chain, release CLI) |
|---|---|
| frozen **parse only** (`yelc ast`) | survives 14,000, aborts by 16,000 |
| frozen **full pipeline** (`yelc check`) | survives 1,600, **aborts by 2,000** |

Both numbers are real; they measure different things. Parse-only is the right
oracle for a parser stage, so the decision stands. But the stronger claim —
"bounding would narrow the language" — does not survive intact: for any chain
between ~1,800 and ~14,544 the frozen *parser* returns an AST while the frozen
*product* crashes, so no such program has ever compiled. A crash is neither
acceptance nor rejection, and rejecting cleanly where the frozen compiler aborts
is an improvement, not a narrowing.

This does not change the outcome — iterative consumers plus `stacker` are
strictly better than any bound, because they handle inputs the frozen compiler
cannot. It changes the *justification*, and the distinction is recorded because
the next stage will face the same question about its own recursion and should not
inherit an argument that is stronger than its evidence.

---

*Below: entries pre-loaded at stage 0, before the questions were asked.*

---

Two entries are pre-loaded as **anticipated** requests, so the answer exists
before the question is asked:

### Anticipated — `TokenSet(u128)` capacity

Ark's `TokenSet` is a `u128` bitset, capping the grammar at 128 token kinds.
Yel's grammar is larger than ark's (elements, attributes, bindings,
interpolation, ranges, unit suffixes). **If the kind count exceeds 128, the seam
becomes `TokenSet([u64; N])` with the same `const fn` API** — granted in advance,
because it is a capacity fact about yel's grammar, not a design preference. The
agent counts the kinds *before* implementing and reports the number either way.

### Anticipated — diagnostics API shape

An agent porting ark's frontend will want ark's flat `ParseError` enum and its
`Vec<ParseErrorWithLocation>` return channel, because that is what the reference
does. **Refused in advance.** Yel's `diagnostic.rs` — builder, `ErrorCode`,
accumulating sink, `render(&SourceMap)` — is frozen infrastructure
([keep-list §1](keep-list.md#1--diagnostics--yel-coresrcdiagnosticrs-285-lines)). Adding a
new `ErrorCode` variant is expected and needs no request. Changing the API shape
does, and the answer is no.

---

## 2026-07-29 — the HIR/THIR merge stands; HIR→THIR is the vocabulary boundary

**Request considered and declined: split HIR and THIR into two IRs.**

**Why it was raised.** The 2026-07-28 merge gave one node vocabulary with types
in a side table. That removed the layer at which a desugaring can *change the
vocabulary* — and that is precisely why `Ternary`, `IfStmt` and `IfNode` survive
all four IRs today ([F18](findings.md#f18)). Splitting looked like the fix.

**Why it was declined.** The missing boundary was never a *crate* boundary. It is
the point at which the UI tree stops existing, and that point is HIR→THIR whether
or not they are separate IRs. Once UI lowers there, the three conditionals
collapse and `Ternary` dies — with the merge intact.

Everything the split was reached for is delivered without it:

| wanted | delivered by |
|---|---|
| signal deps computed while the UI tree exists | HIR phase 1 — `signalck.rs` needs no types (426 lines, reads only `Def`/`Local`) |
| typecheck sees no UI | UI lowers **before** checking; the desugaring needs only *declared* types from `Definitions` |
| a vocabulary-changing layer | HIR→THIR |
| good UI diagnostics | provenance recorded by the desugaring — [stage 3's obligation](stage-3-hir-build.md#the-desugarings-diagnostic-obligation) |

**And a MIR was considered for the same job, also declined.** It would have been a
fifth IR whose only purpose was "UI-free middle language" — which is what THIR
already is once UI lowers before checking. Separately, rustc's own reasons for
MIR (borrow checking, drop elaboration, lifetime analysis) do not exist in yel,
and a CFG would cost a relooper against a target with no arbitrary jumps.

**What the merge keeps**, and why it was worth keeping: one node vocabulary, one
walker, a lint written once against both phases.

**Recorded so it is not re-proposed.** Both the split and the MIR are reasonable
readings of a real problem. The problem is real; the fix is the lowering point,
not another IR.

---

## 2026-07-29 — `FuncSignature` gains `type_params`

**Seam change, requested and applied.** `yelc-syntax`'s `FuncSignature` is frozen
for stage 3, so this is filed rather than edited in.

**What.** `pub type_params: Vec<Recovered<TypeParam>>`, plus a new `TypeParam`
node, `TYPE_PARAM_LIST`/`TYPE_PARAM` kinds, and a `visit_type_param` arm.

**Why.** `<T>` was added to the language
([`scope.md`](scope.md), [`LANGUAGE.md` § Type Parameters](../../LANGUAGE.md#type-parameters))
because §2's stdlib is unwritable without it. Stage 3 lowers signatures, so the
AST has to carry them or stage 3 gets built without generics and retrofits.

**Shape, and why it differs from `params`.** `params` is
`Recovered<Vec<Recovered<_>>>` — the outer layer distinguishes "no `(` at all"
from "`()`", because a missing `(` is malformed. A missing `<` is the ordinary
case, so `type_params` is a plain `Vec` and absent means empty.

**The bug this went through, worth keeping.** `parse_type`'s dispatch looked
ahead for `(` only, so `func<T>(…)` fell through to the **named-type** branch and
`func` parsed as an ordinary type name. The green tree held
`NAMED_TYPE(FUNC_KW)` with `<T>` stranded as sibling `ERROR` nodes. It is a
*silent misparse*: the file still round-trips, so S1 does not catch it, and the
only signal is the node shape. `tests/generics.rs` asserts it.

**A related gap, not fixed here.** `visit.rs`'s exhaustive matches make a new
*variant* a compile error in one file — but a new **field** is not. `type_params`
had to be added to `walk_func_signature` by hand, and nothing would have failed
had it been forgotten: the walker would silently skip every type parameter. The
guarantee is narrower than "adding to the AST is a compile error", and stages 3+
should not assume otherwise.

**Additive.** Stage 1's parity (7) and identity (12) suites pass unchanged, which
is what shows no existing accept/reject verdict moved. Node kinds 76 → 78, with
the budget assertion updated and the reason recorded in `token.rs`.

---

## 2026-07-29 — ten AST declarations gain `attributes`; `AttributeList` / `Attribute` / `AttributeArg` are new

**Seam change, requested and applied.** `yelc-syntax`'s public AST is frozen for
stage 3, so this is filed rather than edited in.

**What.** Three new nodes —

```rust
pub struct AttributeList { id, span, attributes: Vec<Recovered<Attribute>> }
pub struct Attribute     { id, span, name: MaybeIdent, args: Vec<Recovered<AttributeArg>> }
pub struct AttributeArg  { id, span, name: MaybeIdent, value: Expr }
```

— plus `attributes: Option<AttributeList>` on the ten declaration structs that
can carry one (`RecordDecl`, `EnumDecl`, `VariantDecl`, `ElementDecl`,
`ExternComponentDecl`, `GlobalDecl`, `ComponentDecl`, `PropertyDecl`,
`FunctionDecl`, `GlobalProperty`), four node kinds (`ATTRIBUTE_LIST`,
`ATTRIBUTE`, `ATTRIBUTE_ARG_LIST`, `ATTRIBUTE_ARG`), and
`visit_attribute_list` / `visit_attribute` / `visit_attribute_arg` with their
`walk_*` functions.

**Why.** Attributes were added to the language
([`scope.md`](scope.md) § *2026-07-29 — attributes on items, and `unsafe`*).
`@unsafe` gates the primitive/cast machinery the uniform-ref stdlib needs, and
the same mechanism subsumes the `primitive` keyword — one surface break instead
of two. Stage 3 lowers declarations, so the AST has to carry them or stage 3 gets
built without attributes and retrofits.

### The `@children` collision — where the recorded design was underspecified

`scope.md` says the two are *"separable by position: an attribute precedes a
**declaration**, and `@children` appears in a **UI tree body**"*, and instructs
the parser to decide by context rather than by lookahead over the name.

**Position does not separate them, and the brief was written believing it did.**
`@children` is legal as a *direct component member* — `parse_component_member`
routes `AT` through `NODE_FIRST` into `parse_ui_node`, and
`export component App { @children }` is a row in `tests/identity.rs`'s
hand-written table. So an attributed member and a `@children` node occupy the
**same position in the same parse function**, and "which parse function am I in"
returns the same answer for both.

What was implemented instead is one **total** rule, stated once in
`parser/attributes.rs`:

> An `AT` whose next *raw* token is `CHILDREN_KW` is the children marker. Every
> other `AT` in a declaration position opens an attribute list.

That is not the shape the brief warns about, and the difference is worth being
precise about, because it is one token of lookahead either way:

- It reads **one token kind the lexer already assigned**, not a table of
  attribute spellings. Adding an attribute never touches it, so it cannot drift
  out of sync with the attribute set the way `parse_type`'s `(`-only lookahead
  drifted out of sync with `func<T>` (entry above).
- It is **total**: there is no third outcome and nothing falls through to a
  different production. An unknown attribute still parses *as an attribute* and
  is reported; it does not silently become something else.
- The predicate is `Parser::at_children_marker`, the crate's existing and only
  definition of the marker, including its no-trivia rule.

**Consequence, stated so it is a decision:** `@children` cannot be spelled as an
attribute. It is a node wherever it is legal, in every position, unchanged.
`@ children` (with a gap) was never the marker — `children_node` is one atomic
string literal in the frozen grammar — so it is now an *unknown attribute*: still
rejected, which is why the `parity.rs` rows for it do not move.

### Two deviations from the recorded shape

1. **`args` is `Vec<Recovered<AttributeArg>>`, not `Vec<AttributeArg>`.**
   `parse_list` — the generic recovery helper whose `R: Recovery` bound is what
   makes it *impossible* to write a list production that drops its failures —
   requires the element type to implement `Recovery`, and `Recovery::recovery`
   hands out exactly **one** `NodeId`. `AttributeArg` owns two further node
   positions (`name`, `value`), so implementing it would mean either three nodes
   sharing one id (breaking invariant S3) or a bespoke loop outside `parse_list`.
   `Recovered<_>` is also what every other element list in this crate uses
   (`fields`, `params`, `type_params`, and `AttributeList::attributes` itself).

2. **`args` has no outer `Recovered`.** `@unsafe` and `@unsafe()` are the same
   thing; a missing `(` on an attribute is the ordinary case, not a malformed
   signature. Same call as `FuncSignature::type_params`, opposite of
   `FuncSignature::params`.

### `@primitive`'s argument form, as recorded, does not typecheck against its own decision

`scope.md`'s motivating example is

```yel
@primitive("@wasm.ref_array_any_get")
```

— a **positional** string — while the same entry decides three paragraphs later
that arguments are named pairs. Both cannot hold. Named pairs won, because that
is the decision with a reason attached (WIT passthrough), so the spelling is
`@primitive(op = "@wasm.ref_array_any_get")`. The key `op` is this entry's
invention and is the smallest thing here that a later decision may overrule; it
is only ever read by stage 3.

**The `⚠️` in `scope.md` is now discharged.** The WIT gate grammar was checked
against the specification
([`WebAssembly/component-model` `design/mvp/WIT.md` § Feature Gates](https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md)):

```ebnf
unstable-gate ::= '@unstable' '(' feature-field ')'
since-gate    ::= '@since' '(' version-field ')'
feature-field ::= 'feature' '=' id
version-field ::= 'version' '=' <valid semver>
```

It is `key = value`, not `key: value`. The recollection was right and
passthrough stays passthrough.

`@since` / `@unstable` / `@deprecated` are deliberately **not** in the parser's
known-attribute registry: they motivated the argument *form*, but no decision has
landed that yel has them, and a registry entry with no decision behind it is a
shape-only port ([A9](anti-spec.md#a9--a-ported-construct-is-load-bearing-or-it-is-deleted)).
The registry holds `unsafe` and `primitive`.

### The same walker gap as `type_params`, and what was done about it this time

The entry above records it: `visit.rs`'s exhaustive matches make a new *variant*
a compile error, but a new **field** is not. `attributes` had to be wired into
ten `walk_*` functions by hand, and forgetting one would silently skip every
attribute on that declaration with everything still compiling.

The gap is still real and still unclosed by the compiler. What is new is that it
is now **asserted**:
`tests/attributes.rs::the_walker_reaches_the_attributes_on_every_declaration_that_can_carry_them`
parses one program carrying an attribute on all ten owners and counts what the
visitor sees. Deleting any one `walk_attributes` line fails it — verified by
deleting the one in `walk_property_decl`.

**No `TokenSet` changed.** `AT` was deliberately *not* added to `ITEM_FIRST`:
`ITEM_RECOVERY` aliases it, and `MEMBER_RECOVERY`, `PARAM_RECOVERY`,
`EXPR_LIST_RECOVERY` and `TYPE_LIST_RECOVERY` all union it, so a one-line
addition would have moved where five unrelated list productions stop recovering
— and the mutation sweeps insert `@` characters. Attributes are consumed *before*
the gate instead, so every FIRST and recovery set is bit-for-bit what it was.

**Additive, measured.** Workspace **513 → 531 / 0 failed / 2 ignored**
(`cargo test --workspace`); execution **85 / 85**
(`cargo test -p yel-wasm-codegen --test execution`); stage 1's parity (12) and
identity (7) suites pass **unchanged**, which is what shows no existing
accept/reject verdict and no construct identity moved. Node kinds 78 → 82, with
the budget assertion updated and the reason recorded in `token.rs`.

---

## 2026-07-29 — `Block` is extracted and shared; `ClosureExpr::body` changes type; `ForNode` becomes position-shared

**Seam change, requested and applied.** `yelc-syntax`'s public AST is frozen for
stage 3, and this moves three existing fields, so it is filed rather than edited
in.

**What.** One new node, one new enum, one renamed alias, one deleted field:

```rust
pub struct Block { id, span, stmts: Vec<Stmt>, tail: Option<Expr> }
pub enum ForBody { Nodes(Braced<UiNode>), Statements(Recovered<Block>) }
pub type Braced<T> = Recovered<Vec<T>>;   // was `Block<T>`
```

| field | was | is |
|---|---|---|
| `ClosureExpr::body` | `Vec<Stmt>` | `Block` |
| `FunctionDecl::body` | — | `Option<Block>` |
| `IfStmt::{then_branch, else_branch}` | `Block<Stmt>` | `Recovered<Block>` |
| `ForNode::body` | `Block<UiNode>` | `ForBody` |
| `ExprStmt::has_semicolon` | `bool` | **deleted** |
| `Stmt` | — | new variant `For(Box<ForNode>)` |

Plus node kinds `FUNC_BODY` and `FOR_STMT` (82 → 84), `Visitor::visit_block` /
`walk_block`, and `Block` in the driver's AST dump.

**Why.** Function bodies and `for`-as-a-statement were added to the language
([`scope.md`](scope.md), 2026-07-29). Both were found by writing
[`stdlib/`](../../stdlib/README.md): a named function had nowhere to put an
implementation, and yel had no loop statement at all, so `filter` and `map` had
no expressible body.

### The name `Block` was already taken, and the recorded design does not say so

`scope.md` specifies `pub struct Block { id, span, stmts, tail }` outright. There
was already a `pub type Block<T> = Recovered<Vec<T>>` — the braced-body alias
behind `IfNode`, `ElseIfBranch`, `ForNode` and `IfStmt`. One name cannot be both
a bare `Vec` of UI nodes and a statement list with a tail.

The alias was renamed to `Braced<T>`, which is what it always meant: *"the
`{ … }` after a template construct"*. It is now only ever instantiated at
`Braced<UiNode>` and stays generic because the call sites read better that way,
not because a second instantiation exists.

### Statement blocks were unified rather than left at two representations

`scope.md` names only `ClosureExpr` and `FunctionDecl` as `Block`'s owners.
Taken literally that leaves `IfStmt` on `Recovered<Vec<Stmt>>` while a
`for`-statement body — added the same day, in the same position, with the same
rule — would be a `Recovered<Block>`. Two statement-block representations in one
crate is the second `Block`
[directions §9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it)
is trying not to have, so `IfStmt` moved too. No behaviour changed with it: an
`if` branch is still `statement*`, `allow_trailing` is still false there, and
`tail` is `None` for every well-formed one.

### `tail` replaces `ExprStmt::has_semicolon`, and that is the point

The trailing expression used to be the last `Stmt`, flagged with
`has_semicolon: false`. It is now `Block::tail`, and the flag is deleted because
it became unrepresentable-false.

This is the shape §9 needs: `match` arms, `if` branches and ternary arms are all
blocks whose tail *is* their value, so "statement position" versus "expression
position" stops being a node distinction and becomes whether the block has a
tail. A boolean on the last element cannot carry that — a consumer has to know
to look at the last element and know what the flag means.

`tail` is filled in wherever a semicolon-less final expression is *read*,
including the two positions where writing one is an **error** (an `if`-statement
branch, a `for`-statement body). The diagnostic is reported and the expression is
kept: dropping the subtree the user wrote is what invariant S5 forbids.

**Construct identity did not move**, and that was the risk. `identity.rs`
projects the tail as `stmt:expr` at the tail expression's own span, which is what
the frozen parser reads it as — pest's `expr_statement` is `expr ~ ";"?`, so its
semicolon-less form is a `Statement::Expr` starting at the same offset. Verified
over 2093 comparable programs and 5264 mutations, and verified *non-vacuous* by
deleting that projection line, which produces four failures.

### One `for` parser, one `for` node, one walker

`ForNode` is shared between the template and statement positions, and so is
`Parser::parse_for`: the head (`for x in e key(k)?`) is read once and the body
comes in as a closure. Which position is being parsed is decided by **the
caller** — `parse_ui_node` versus `parse_stmt_inner` — never by lookahead inside
the shared function.

Two consequences, both deliberate:

1. **`key(…)` is grammatical in statement position.** One parser accepts the
   whole `for_node` shape wherever a `for` is legal. It is only *meaningful* for
   template reconciliation; rejecting it is a later phase's call, and that phase
   has the node to reject. The alternative — a flag threaded into the shared
   parser — is the position leaking back into the thing that was shared to be
   position-free.
2. **`ForBody` is an enum, not a type parameter on `ForNode`.** A generic
   `ForNode<B>` would make the two positions' types exact, at the cost of a
   generic `visit_for_node` and a hand-written arm in the driver's dump macro,
   which takes a concrete type. One enum keeps *one* `walk_for_node`, which is
   what [A3](anti-spec.md#a3) is actually about.

### The statement guard is tighter than the template guard, and has to be

`parse_ui_node` opens a `for_node` on `FOR_KW` followed by anything that is not
`{`. In statement position that would be wrong, because `for` is not reserved and
**is** a legal expression there: `{ for = 1; }` and `{ for + 1 }` are about a
variable called `for`. `at_for_statement` asks for the whole head —
`FOR_KW ~ name ~ IN_KW` — which is the production, not a maintained list of
spellings, so it cannot drift the way `parse_type`'s `(`-only lookahead drifted
out of sync with `func<T>` (entry above). Every text it claims was a syntax error
on both parsers before.

### No `TokenSet` changed, again

`FOR_KW` was already in `STATEMENT_FIRST`, through
`KEYWORD_FIRST ⊆ NAME_FIRST ⊆ EXPRESSION_FIRST ⊆ STATEMENT_FIRST` — every
keyword is a legal identifier in this grammar. So every FIRST and recovery set is
bit-for-bit what it was, for the third change running.

### What the accept/reject and identity suites cannot see here

Measured, not assumed. Two deliberate dispatch breakages — the statement `for`
guard loosened to the template one, and function bodies switched off entirely —
leave **both `parity.rs` and `identity.rs` completely green**. The frozen parser
rejects every program containing either construct, so there is nothing for either
suite to compare. `tests/blocks.rs` is the cover, and every assertion in it was
confirmed to fail under a deliberate break.

**Additive, measured.** Workspace **531 → 554 / 0 failed / 2 ignored**
(`cargo test --workspace`); execution **85 / 85**
(`cargo test -p yel-wasm-codegen --test execution`); stage 1's parity (12) and
identity (7) suites pass **unchanged**. Node kinds 82 → 84, with the budget
assertion updated and the reason recorded in `token.rs`.
