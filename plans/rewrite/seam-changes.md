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

## 2026-07-30 — stage 3's seam — `HirMap` is keyed by `(SourceId, NodeId)`, and `type_of` is refused

**Requested by:** stage 3, phase 2 (the seam-types landing).

**Request.** Two changes to the contract in
[`stage-3-hir-build.md` § Contract](stage-3-hir-build.md#contract), both found by
writing it as Rust with no body to serve.

**1 · `HirMap`'s key.** The contract specifies

```rust
pub struct HirMap { map: FxHashMap<HirId, NodeId>, rev_map: FxHashMap<NodeId, HirId> }
```

copied from ark's `hir_map.rs`, which the brief names as the model. A
`yelc_syntax::NodeId` is unique **within one `ParsedFile`** and allocated from
zero per file; `lower_files` is handed the whole file set. So `rev_map` merges
file 1's node 7 with file 2's, silently, on every multi-file input. The forward
map is no better: a bare `NodeId` handed back by `node_of` is a number the caller
cannot interpret.

**Why the citation misleads rather than being wrong.** ark's `NodeId` comes from
one process-global `AtomicUsize` (`arkc-parser/src/parser.rs:28`), so it *is*
unique across the compilation and ark's map is correct. Stage 1 rejected that
design deliberately — a process-global counter makes a node's id depend on how
many files were parsed earlier, which is
[A6](anti-spec.md#a6--no-random-seeded-iteration-reaching-output) and would make
any golden containing node ids unstable. Both calls were right; the composition
is not. **A reference is a source of shape, and a shape carries assumptions** —
worth adding to how the ark citations are read, because this is the one construct
in stage 3's brief that names ark as the model and it is the one that had to
change.

**Options considered.**

| | cost |
|---|---|
| **(a) qualify the key** — `SourceNodeId { source, node }`, both directions | one 8-byte key instead of 4; a new public type in the seam |
| (b) make `NodeId` globally unique | reopens a closed stage, reintroduces the A6 hazard stage 1 rejected |
| (c) one `HirMap` per file | `HirModule` spans the set by [D8](stage-3-hir-build.md); N maps is the category error D8 exists to correct |
| (d) key by `Span` | a `Span` is `(SourceId, start, end)` and two nodes can share one — `Recovered::Missing` has a zero-width span at the same offset as its parent |

**Decision: granted, (a).** It is not a design choice so much as the only shape
that types: (b) and (c) each contradict a landed decision, and (d) is not
injective. Landed as `yelc_hir::SourceNodeId`, and `TypeId` wraps it for the same
reason.

**H2 was restated with it, and that is the more important half.** As written —
*"total and bidirectional … `hir_of(node_of(h)) == h`"* — H2 **passes under the
collision it should catch**: the reverse map keeps the last writer, the forward
map still answers, and the round trip holds for the survivor. A real invariant, a
real test, and neither can see a real bug
([A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)). H2 now also
requires **injectivity**, asserted in `next_hir_id` where the invariant is
established, and its test is a two-file input — the round-trip assertion alone is
kept in the suite and is explicitly *not* the one that covers this.

**2 · `type_of` — refused, pending a decision that is not this agent's.**

```rust
pub fn type_of(&mut self, ty: TypeId) -> Ty;   // "Memoized in a NodeMap<Ty>"
```

Three defects, and all three are contract rather than implementation:

1. `&mut self` names no receiver. No type in the brief owns it.
2. The memo cannot be a `NodeMap<Ty>`: `NodeMap::insert` takes a `HirId` and a
   `TypeId` is not one. The two declarations are twenty lines apart in the same
   code block and contradict each other.
3. The definition of done requires it *"structurally unreachable from H1 phase 1
   (the collector does not exist yet)"* — a statement about a type the contract
   never declares.

**Decision: not landed.** Naming the owner closes all three at once (the receiver
is that type; the memo is a field on it keyed by `TypeId`), and naming it is a
contract decision. Landing a guess would have made the guess the contract, which
is the one thing types-before-body exists to prevent. Recorded as the remaining
gate on phase 3.

**3 · `HirModule` — flagged, not renamed.** `fbaa95e` renamed `ModuleId` →
`PackageId` because *"the noun was one level off"*. The identical argument applies
to `HirModule`: it holds a `PackageId`, spans a `Vec<SourceId>`, and D8 says it
*is* the package — while `module` is becoming a surface keyword for a WIT
interface and `ModuleId` now means a symbol-table node. `HirPackage` is what the
reasoning produces. **Refused for this landing**, on the same grounds as
`type_of`: the seam list said `HirModule`. Filed for decision before phase 3's
lowering.

**Blast radius.** Stage 4 assumes stage 3's contract and adds only the `types`
map; it inherits `SourceNodeId` unchanged. Stages 5/6 are unaffected — neither
`HirMap` nor `SourceNodeId` may cross the frontend seam, and the crate graph
already forbids it. One artifact consequence, recorded in the brief: `SourceId`
**does** derive `Serialize` in `yelc-base`, so a `SourceNodeId` in a `Wire` type
compiles and writes a producer-local index. `Ty`'s type-level guard does not cover
it; whether `HirMap` crosses at all is stage 3's to decide.

## 2026-07-29 — `yelc-sema` gains `artifact`; `Namespace` derives `Serialize`

**Requested by:** the package-artifact agent (`plans/modules.md` §6.6).

**Request.** Build the artifact format. `yelc-sema`'s public surface is contract,
and this adds a module to it: `artifact::{Artifact, Stamp, PackageName,
LoadedPackage, LoadError, ToArtifact, FromArtifact, ArtifactWriter, wire::*}`.
One existing type changed: `Namespace` derives `Serialize`/`Deserialize`.

**Decision: granted, additive.** Nothing existing changed shape. `Namespace`
carries no index — it means the same thing in every compilation — so deriving
`Serialize` on it is not the B1 hazard that the same derive on `Ty` is, and the
doc comment now says so at the derive site rather than here.

**Blast radius.** `yelc-sema` only. Stage 3 gains an obligation
(`ToArtifact`/`FromArtifact` for HIR nodes) in place of the paragraph it had.
Workspace 569 → 594 / 0 failed / 2 ignored; execution 85/85.

### The encoding is postcard, and the criteria decided it 3–0

`modules.md` §6.6 left this open and asked for ten minutes on the stability
guarantees. It took longer and the answer is not close.

| criterion | postcard | bincode |
|---|---|---|
| schema stability across crate versions | a **separate published wire-format specification**, stable since 1.0.0; changing it requires a 2.0 *with an updated spec* | no specification document; the format is whatever the implementation does, and it moved across 1.x → 2.0 |
| compactness | every integer > 8 bits is a varint; this format is almost entirely small indices, so one byte each | 1.x writes fixed 8-byte lengths by default; 2.x varint |
| no self-describing overhead | *"As `struct`s have a known number of elements with known names, their length and field names are not encoded on the wire"* | also non-self-describing — a tie |

Criterion 1 is the one the `format` field exists to protect, and it is the one
with a real separation: postcard's bytes move when **our** schema moves and at no
other time, so `format` tracks one thing. It is asserted, not assumed —
`the_encoding_carries_no_field_names` greps the bytes for six schema names, and
swapping the codec for `serde_json` fails it.

**Two statements in the plans are wrong, and one is worse than wrong.**

1. **§6.6: "the frozen tree and `arkc` both use `bincode`".** The frozen tree
   does not use bincode. It has never had it as a dependency — `serde` +
   `serde_json`, and `grep -rn bincode` over the repo returns only the two plan
   sentences. `arkc` does, pinned at `2.0.0-rc.3`: a release candidate, which is
   evidence *against* criterion 1 rather than for familiarity.
2. **bincode is unmaintained.** RUSTSEC-2025-0141, 2025-12-16. Development ceased
   permanently after a harassment incident; 3.0.0 is a tombstone release
   containing a README and a compiler error, and the advisory's own recommended
   alternatives list begins with postcard. §6.6 was written on 2026-07-29 — seven
   months later — and still frames it as a live option. **A "both are defensible"
   framing survives in a plan long after one of them stops being defensible**,
   because nothing re-checks a resolved-looking sentence. Worth generalising: a
   recorded comparison of two dependencies has a shelf life.

### §6.6 and `directions.md` §6 both specify this, and they disagree

`directions.md` §6 says **"Decided: postcard"** and specifies a hand-written
envelope — `magic "YELM" · format_version: u32 · input_hash: [u8; 32] ·
section_count: u32` plus a section table. §6.6, written later, says the encoding
is "not decided here" and specifies `Stamp { compiler, format }` with no magic,
no hash and no sections.

§6.6 was implemented, as briefed. The divergences are live and unreconciled:

- **`input_hash` vs `compiler`.** A content hash catches a stale artifact whose
  *inputs* moved; the stamp only catches one whose *compiler* moved. They are not
  substitutes and §6.6 has no replacement for the hash.
- **Magic bytes.** §6.6 has none, so a non-artifact file reaches the decoder and
  is rejected as a decode error rather than as "not an artifact".
- **The section table.** §6.6's flat struct forecloses the lazy-load door §6
  explicitly left open. It reopens with a `format` bump, so this is a cost
  deferred rather than paid.

### `DefPath` is documented as the serialized form and is not serializable

`ids.rs` says a `DefPath` is *"the serialized form of a definition's identity"*.
It cannot be written:

- `package: Name` and `segments: Vec<Name>` are **interner indices** — the same
  class of value as a `Ty` handle, with the same failure mode, and `Name` *does*
  derive `Serialize` in `yelc-base`. A naive derive compiles and writes indices.
- `overload: OverloadKey` holds `Vec<Ty>`, and `OverloadKey`'s own doc says a
  `DefPath` carrying it must write them structurally — which `DefPath` has no
  way to do, having no type table.

So `DefPath` is the *resolution-independent in-process* form, one step short of
the wire, and the artifact needs a third representation (`wire::SerializedDefPath`,
with `String` segments and `Vec<TypeIndex>` overloads). **Not requested as a seam
change**: `Name` is right in-process and `DefPath` is used nowhere yet. But its
doc comment claims a property it does not have, which is how a future author
derives `Serialize` on it and ships the B1 bug through the one type named after
avoiding it.

### `DefPath` cannot name a definition — the namespace is missing

`Definitions` keys names by `(Name, Namespace)`, and `definitions.rs`'s own test
`namespaces_do_not_collide` asserts that a record and a component may share a
name. `DefPath` has no namespace field, so it cannot distinguish them; the
`OverloadKey` does not help, because that disambiguates *within* a namespace.

`SerializedDefPath` carries `namespace`, and
`a_path_distinguishes_two_namespaces_sharing_a_name` pins it: dropping the field
from the loader's key makes the record and the component resolve to each other.
**If `DefPath` is ever used, it needs the same field.**

### B1's enforcement is real for `Ty` and absent for `Name`

`Ty` not deriving `Serialize` works exactly as designed. Simulating the bug —
writing the type table as raw interner indices — needed **four** edits including
a new `Ty::from_raw_index` constructor. It is not a slip; it is a campaign.

`Name` and `SourceId` have no such protection: both derive `Serialize` in
`yelc-base`, so a wire struct with a `Name` field compiles and writes an index.
The artifact module contains neither, and `wire.rs` is the only place wire types
are declared, but that is a convention. **Not requested**, because removing
`Name: Serialize` is a `yelc-base` seam change with no current beneficiary — but
when stage 3 adds HIR to the artifact the surface grows a great deal, and this is
the moment to decide whether the derive should survive it.

### `Definitions` cannot represent an overload set, so B3 is unreachable

B3 puts one `OverloadKey` in `yelc-sema`, and `stdlib.rs` already registers `len`
twice. But `Definitions::by_name` is keyed `(Name, Namespace)` with no
discriminator, so the second `len` is a `Duplicate`. `SerializedDefPath.overload`
exists, is always empty, and fills in without a format change the day
`Definitions` learns the key. Until then a colliding artifact is **rejected**
(`LoadError::DuplicateDefinition`) rather than silently keeping one.

### The `compiler` half of the stamp currently discriminates nothing

`Stamp::COMPILER` is `env!("CARGO_PKG_VERSION")` — `0.1.0`, workspace-pinned, and
it does not move when the compiler does. Every artifact the rewrite writes
carries the same string, so the field that exists to catch *"a node's meaning
changed without its encoding changing"* catches nothing. §6.6 says `compiler` is
"the build's own version" and does not notice that the build has no version. The
fix is a build identity (`shadow-rs` is already a workspace dependency) and it is
a change to that one constant; it is documented at the constant so it is not
found by an artifact that loads when it should not.

### A4 obligation 2 and the brief's `Infer` test contradict each other

open-decisions A4 says an `Infer` **must never be serialized**. The brief asks
that `TyKind::Infer` survive a round trip and stay distinct from `Param`. Both
are right about different layers, and collapsing them loses one:

- the **encoding** represents `Infer` distinctly, so an `Infer` can never be
  quietly written *as* a `Param` — the confusion A3 and A4 were split to prevent;
- the **producer policy** is `Artifact::inference_holes()`, which reports every
  hole so publishing one is a loud failure.

An encoding that could not represent `Infer` would satisfy A4 by turning a
detectable bug into an undetectable one.

### `stage-2-driver.md`'s CLI signature is already contradicted, in §6.5

Not this change's scope, but it is recorded in `modules.md` §6.5 as *"a
consequence for stage 2 that nobody recorded"* and it is still unrecorded here:
`yelc2 [OPTIONS] <FILE>` cannot name a package, and a package is a directory.
Flagged so it is not discovered during stage 6.


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

## 2026-07-29 — `Stmt` gains `Return(ReturnStmt)`; `return` becomes a token kind

**Seam change, requested and applied.** `yelc-syntax`'s public AST is frozen for
stage 3, so a new `Stmt` variant is filed rather than edited in.

**What.** One new node, one new variant, one new **token** kind and one new node
kind:

```rust
pub struct ReturnStmt { id, span, value: Option<Expr> }
pub enum Stmt { …, Return(ReturnStmt), … }
```

| kind | was | is |
|---|---|---|
| `TokenKind::RETURN_KW` | — | new **token** (`EOF` 73 → 74) |
| `TokenKind::RETURN_STMT` | — | new node kind (84 → 85) |
| `KEYWORD_FIRST` | 24 members | 25 |

Plus `Visitor::visit_return_stmt` / `walk_return_stmt`, `ReturnStmt` in the
driver's AST dump, and a `stmt:return` arm in `identity.rs`'s projection.

**Why.** `return` was added to the language ([`scope.md`](scope.md),
2026-07-29), reversing the "no `return`" decision recorded the same day. Found by
writing [`stdlib/string.yel`](../../stdlib/string.yel): `starts-with` needs to
stop iterating on the first mismatch and there is no construct that means that.

### This is the first surface change that is **not** additive

The four before it — `<T>`, attributes, function bodies, the `for` statement —
could each say "every text this claims was a syntax error on both parsers
before", and that sentence is what made them safe to land without an oracle.
`return` cannot say it, and `scope.md`'s entry does not notice: it presents the
reversal as the same kind of change as the other four.

The reason is structural, not incidental. The other four are guarded by a
production the frozen grammar also rejects — `for` commits only on
`FOR_KW ~ name ~ IN_KW`, a function body only on a `{` where the frozen grammar
demands a `;`. `return` has no such head. The frozen grammar has **no `return`
production at all**, so every `return` it sees is an ordinary *name*, and a
`return` statement's whole syntax overlaps texts it already accepts.

Measured against the frozen parser, in statement position it accepts all nine of:

| text | frozen reads it as |
|---|---|
| `return;` | expression statement about a variable `return` |
| `g(); return` | the block's trailing expression |
| `return - 1;` | binary subtraction |
| `return(x);` | a call whose callee is `return` |
| `return [0];` | an index into `return` |
| `return = 1;` · `return += 1;` | assignment to `return` |
| `return.x = 1;` · `return?.x;` | member access on `return` |

and rejects `return x;`, `return 1;`, `return false;`, `return "s";`,
`return !x;` — the five shapes the feature exists for. **There is no guard that
keeps the first list and adds the second**; they overlap.

### The rule is one token, and the narrowing is bounded to statement position

`parse_stmt_inner` commits on `RETURN_KW` unconditionally — one token kind the
lexer already assigned, no lookahead list, no third outcome. The narrower
alternative was considered and refused: *"commit only when what follows is in
`EXPRESSION_FIRST ∪ {;}`"* would preserve `return = 1;` and `return.x;` while
still taking `return;` and `return -1;`, which buys a smaller narrowing at the
price of a rule nobody can state — a variable you may assign to but never read —
and it is the maintained-lookahead shape that silently misparsed `func<T>`.

`RETURN_KW` is in `KEYWORD_FIRST ⊆ NAME_FIRST`, so `return` is still a legal
*name* everywhere a name is legal: a property, a record field, an element name, a
`let` binder, a member. The narrowing is confined to statement position, which is
`tests/returns.rs::return_is_still_an_ordinary_name_outside_statement_position`.

### A `TokenSet` changed — the first one that did

Three landings running reported "no `TokenSet` changed". This one adds a token
kind, which shifts `EOF` and every kind above it, so **every** set's bit
positions move. That is safe only because all of them are `const`-folded from the
enum in `token.rs` and nothing outside serialises a discriminant. Set
*membership* is unchanged: `RETURN_KW` lands exactly where `return`-as-an-
`IDENTIFIER` already was, in `KEYWORD_FIRST ⊆ NAME_FIRST ⊆ EXPRESSION_FIRST ⊆
STATEMENT_FIRST`.

### What `parity.rs` and `identity.rs` cannot see here — and why that is worse than last time

Last time both suites were blind because the frozen parser rejects every program
containing the new construct. This time they are blind for a different and less
comfortable reason: the word `return` **does not occur outside a comment in any
of the 2118 checked-in `.yel` files**, and neither mutation generator can
introduce a word that is not already in the text (`SOUP_TOKENS` has no
identifiers, and the deterministic sweep only truncates and deletes). So a real
accept/reject change and a real construct-identity change both land with parity
at 12 and identity at 7, unmoved.

The cover is `tests/returns.rs`, which reads the frozen parser directly and
enumerates the boundary in both directions — nine texts narrowed, five widened,
each asserted on *both* parsers so neither the premise nor the consequence can
rot into a claim.

**Additive where it can be, measured.** Workspace **554 → 569 / 0 failed / 2
ignored**; execution **85 / 85**; parity **12** and identity **7**, unchanged.
Node kinds 84 → 85 and token kinds 73 → 74, with both halves of
`token_kind_counts` updated and the reasons recorded there.

## 2026-07-29 — `yelc-sema` — `Namespace` is deleted; `Definitions` becomes a single-namespace symbol table

**Requested by:** stage 3 phase 1 (`yelc-sema`), integrator-approved in advance.

**Request.** `Definitions` keyed names by `(Name, Namespace)`. Replace that with
one scope keyed by `Name`, whose values are a multi-valued `Sym`:

```rust
pub enum Sym { Type(DefId), Value(DefId), Component(DefId), Global(DefId), Module(ModuleId) }
by_name: FxHashMap<Name, SmallVec<[Sym; 1]>>
```

**Blast radius.** `Namespace` was public API of `yelc-sema` and appeared on the
wire. Both change, and nothing outside `yelc-sema` consumed either yet:

| was | is |
|---|---|
| `definitions::Namespace` | **deleted** |
| `Definition.namespace: Namespace` | `Definition.kind: DefKind` |
| `Definitions::lookup(name, ns) -> Option<DefId>` | `lookup(name) -> &[Sym]` · `lookup_def(name, kind) -> Option<DefId>` |
| `Duplicate { existing: DefId }` | `Collision { name, existing: Sym, existing_span, attempted, span }` |
| `Known::namespace()` | `Known::kind()` |
| `SerializedDefPath.namespace: Namespace` | `SerializedDefPath.kind: DefKind` |
| `Stamp::FORMAT = 1` | `= 2` |
| — | `ids::ModuleId`, `definitions::{Sym, Module}`, `Definitions::{register_overload, register_module, bind_in_module, lookup_in_module, module, modules, span_of}` |

**Decision: granted.** The surface consequence — a record and a component may no
longer share a name — is recorded in [`scope.md`](scope.md) as the first
non-additive, non-`return` break. The rest is design, and three parts of it are
worth writing down.

### The tag survives; the key dies

`DefKind` has the same four variants `Namespace` had, and a reviewer is entitled
to ask what actually changed. What changed is that **nothing looks a name up
inside a kind**. `lookup` takes a `Name` and returns whatever is bound to it;
`lookup_def(name, kind)` is a *filter on the one binding*, not a second index, so
a `Color` declared as a global makes `Known::Color` **missing** rather than
found-elsewhere.

The tag itself cannot go, for two reasons that are not the old one:

- a diagnostic must say *"`Point` is already a type"*, and
- **loading an artifact reconstructs a definition**, so `register` has to be told
  what to build. That is the new job of `SerializedDefPath.kind`, and it is why
  the field survived a change that removed its original purpose.

### `Sym` and `DefKind` are two enums, deliberately

They are not the same set. `Sym::Module` has no `DefKind` counterpart, and that
absence is load-bearing rather than tidy: `bind_in_module` takes a `DefKind`, so
**a module inside a module does not compile**. The two-level limit that
[`plans/modules.md` §3](../modules.md) argues for on WIT grounds is carried by a
signature instead of a depth check. `DefKind::sym(DefId) -> Sym` is the one
bridge between them, so they cannot drift.

### `Sym::Module(ModuleId)`, not `Sym::Module(PackageId)`

`PackageId` was the obvious payload and is wrong: a package holds several
modules, and [`modules.md` §4.1](../modules.md) settled that an `include` names a
*module*, one node per `include` — so two `include`s of one package would be
indistinguishable. A `DefId` is wrong too: a module has no declared type, no
export flag and no row in the definition table.

What the resolution path actually needs from `Sym::Module` is **a scope to look
the next segment up in**. `ModuleId` indexes the symbol table's own module arena,
and the node it reaches carries the `PackageId` its definitions belong to — which
is what lets a `DefId` resolved through the module be read out of that package's
own `Definitions` (`artifact/load.rs::LoadedPackage`). Nothing populates a module
scope yet; `include` does not parse. The shape is here so the thing that will
populate it has somewhere correct to go.

### The format bump is invisible in the bytes, so it got a guard

`Namespace` and `DefKind` have the same four variants in the same order, and
postcard writes neither field names nor variant names. **The encoding did not
move by a single byte.** A missing `FORMAT` bump would therefore have let a stale
artifact load into a schema that means something else, with no diagnostic — the
exact failure `Stamp` exists to prevent, arriving in the one form `Stamp` cannot
detect on its own.

`tests/artifact.rs::the_wire_bytes_are_pinned_so_a_schema_change_cannot_be_silent`
pins the bytes of a fixed artifact and asserts byte 6 equals `Stamp::FORMAT`.
Any future `wire.rs` change either moves the bytes or does not; either way the
author is made to look. Confirmed by reverting `FORMAT` to `1`, which fails it.

### B3 is half-unblocked, and the other half has a blocker no plan names

`9a54ad1` recorded *"B3 is unreachable: `Definitions` keys on `(Name, Namespace)`
with no discriminator"*. That blocker is gone — `register_overload` takes an
`OverloadKey`, and a name may carry several `Sym::Value`s with distinct non-empty
keys.

The **loader** still cannot rebuild one, for a reason that is not the map's
shape: `load_into` registers definitions in pass 1 and resolves the type table in
pass 2, because a declared type may name an ADT that only exists once the
definitions do. A `Ty`-valued overload key is therefore unavailable at the exact
moment registration needs it. `SerializedDefPath.overload` stays empty and an
artifact carrying an overload set is rejected with `DuplicateDefinition` — now
reachable from a real registration, not only from a hand-built artifact
(`an_overload_set_is_refused_by_the_loader_not_lost`). Breaking the cycle wants a
key that does not depend on the type table; Swift mangles one into the path.
That is a separate decision and is not made here.

**Measured.** Workspace **594 → 612 / 0 failed / 2 ignored**; execution
**85 / 85**; parity **12** and identity **7**, unchanged — and *unchanged is not
evidence here*, see [`scope.md`](scope.md). freeze-check clean. 17 deliberate
breakages, 17 caught; one no-op control, correctly not caught.

---

## `yelc-hir` gains `check_package_identity` — 2026-07-30

**Requested by** the driver going directory-oriented. **Granted**, because the
alternative homes are all worse for stated reasons rather than taste.

`plans/modules.md` decided that a package is a directory, every file declares
`package ns:name@version;`, and disagreement is an error. Enforcing that needs a
`&[ParsedFile]` and a `Diagnostics` in one place, and until now nothing had both:

- **`yelc-syntax` cannot**, and this is the interesting one — the rule is about a
  *set* of files, so a parser that enforced it would be a parser holding state
  across parses. That is a different kind of object than the one stage 1 built.
- **`yelc-sema` cannot** — it sits below `yelc-syntax` and cannot name
  `ast::PackageDecl`. The same wall that stopped `type_of` from living on
  `CompilerContext`, hit from the other side.
- **The driver must not** — its module doc forbids growing language behaviour,
  and "which files disagree" is a language rule.

So `yelc-hir` is not a convenient home, it is the **first** one, and that is the
argument for it. It runs before `lower_files` rather than inside: it reads each
file's `package` clause and nothing else, needs no symbol table, and reports
without one.

Two error codes added in `yelc-base`: `MissingPackageDecl` (E0071) and
`PackageNameMismatch` (E0072). **Deliberately two.** Absence has no other file to
point at, so it is reported against the one file alone; a mismatch names both
sides, because an error that reports a disagreement while naming one side asks
the reader to go find the other. Go names both files and is right to.

### A pre-existing driver bug this surfaced

`--emit-ast` was declared `num_args = 0..=1` without `require_equals`, so clap
read the **next token** as the optional filter. `yelc2 --emit-ast foo.yel`
filtered for an item named `foo.yel` and then reported the positional missing —
the flag only worked when the path came first. It predates this change and was
invisible because every recorded invocation happened to put the path first.

The driver also kept its own `SourceMap` and `Diagnostics` beside the ones on
`CompilerContext`: two id spaces, nothing checking they agreed, spans minted
against one and rendered against the other (`anti-spec.md` F12). Now one of each,
on the context.

**Measured.** Workspace **612 → 642 / 0 failed / 2 ignored**. freeze-check clean.
The ten new tests were mutation-tested against three breakages — version dropped
from the comparison, mismatch arm silenced, absence arm silenced — and all three
were caught by 4, 4, and 3 tests respectively. The implementation was restored
and the restoration verified by content comparison, not by assumption.

**Not covered by parity or identity, and this is expected**: the corpus has no
multi-file program and no `package` clause, so both suites stay green through the
whole change. That is the oracle's vocabulary blindness (`scope.md`), which is
why the tests above are hand-written rather than inferred from a sweep.
