# Scope — frozen vs. free

> Rule: [`scope-frozen-vs-free`](../../.agents/skills/compiler-rewrite/rules/scope-frozen-vs-free.md)
>
> **This table is the first thing in every agent brief.** An agent that wants to
> move an item from frozen to free *asks*; it does not decide. The decision is
> logged in [`seam-changes.md`](seam-changes.md).

"Rewrite the internals, keep the language and the stages" is clear to someone
holding the project in their head and ambiguous to an agent at exactly the
boundaries that matter. Every ambiguity resolves toward scope growth, because
redesigning is more interesting than transcribing — and a rewrite that also
changes the language **cannot be differentially tested**, because the corpus
stops compiling. The moment that happens, every other rule loses its teeth
simultaneously.

## The table

| Frozen — changing it is a separate, approved decision | Free — expected to change |
|---|---|
| Surface syntax as specified in [`LANGUAGE.md`](../../LANGUAGE.md) — **one planned exception, see below** | Parser implementation, grammar technology (pest → hand-written lexer + recursive descent), AST node shapes |
| Stage names and their order: **AST → HIR → LIR → WASM** — frozen *because differential attribution depends on it*, not because the frozen compiler drew it there. Moving a boundary is an integrator decision logged in [`seam-changes.md`](seam-changes.md) ([2026-07-28](seam-changes.md#log)), and must say which stages lose independent attribution and what replaces the differential for them. THIR was merged into HIR under that rule — one IR, phases 3/4. | Every type, pass, and helper *inside* a stage |
| Exported WIT world and the `yel:ui/dom@0.1.0` host contract | How WIT is constructed and emitted (`wit_ast.rs` internals) |
| Observable DOM-op behaviour asserted by the **85 execution tests** | The lowering and codegen that produce it |
| Diagnostic *meaning* for the 23 diagnostic fixtures, and the `diagnostic.rs` API | Which stage reports what; new `ErrorCode` variants; message wording (with a recorded diff) |
| CLI surface: `yelc compile -o {wasm,wit,dot}`, `ast`, `ir`, `check` | Driver internals, `pipeline.rs` structure, the new `yelc2` stage-selection seam |
| Determinism of all output (byte-stable across runs) | Which data structures produce it (Fx maps, sorted derivations) |
| The keep-list items ([`keep-list.md`](keep-list.md)): diagnostics, `SourceMap`/`Span`, interning, typed ids + `IndexVec`, context threading, accumulate-and-continue, no-silent-fallbacks, the determinism lint | Everything else in `yel-core` and `yel-wasm-codegen` |
| The frozen tree itself — `crates/yel-core`, `crates/yel-wasm-codegen`, `crates/yelc` are **never edited** | New crates beside them |
| The 2000-seed corpus and its provenance ([`corpus.md`](corpus.md)) | — (regenerated from the old compiler only, never from the new one) |

**Anything not in this table defaults to frozen.**

## Clarifications that come up every time

### Diagnostic wording may improve; diagnostic *meaning* may not.

A fixture asserting `cannot infer type` must still reject that program, for that
reason, at a span covering the same construct. Improved wording is a golden
update with the diff **read and justified** in
[`goldens-changed.md`](goldens-changed.md). The diagnostic *infrastructure* is
frozen outright — see [`keep-list.md`](keep-list.md) §1.

### The stage boundary is frozen; the stage's data model is not.

"Keep LIR" means a block-based low IR exists — its arena traits, op set, and
resource model are all free, and stage 3 is expected to change them
substantially.

**THIR is no longer a stage.** It merged into HIR on 2026-07-28
([`seam-changes.md`](seam-changes.md)): one node vocabulary, `types: NodeMap<Ty>`
empty after phase 3 and total after 4. What that decision preserved is the
*obligation* — a typed form still exists before LIR, with a documented contract —
not the second IR that used to carry it.

### Parser technology is free, and expected to change.

Dropping pest for a hand-written lexer + recursive-descent parser over a lossless
green tree is **the plan**, not a scope violation — see
[`frontend-follow-ark-reference`](../../.agents/skills/compiler-rewrite/rules/frontend-follow-ark-reference.md).
What stays frozen is the *grammar it accepts*: every one of the 91 positive
fixtures, every corpus program, and every `LANGUAGE.md` construct parses, and
nothing new parses that did not before. "It simplifies the grammar" is never a
reason to require a trailing comma, reserve a new keyword, or tighten a
whitespace rule.

### The LIR's UI vocabulary is free — in one direction only.

Removing `tree_shape`, `boundary`/`mount`, `$Comp` self-ref, and `yel:ui/dom`
concepts from below the frontend seam is *mandated*, not merely permitted —
anti-spec C1. What is frozen is the **observable result**: the same DOM ops in
the same order, byte-identical WIT.

### The WIT world is frozen at its current *content*, not its current *code*.

`wit_ast.rs` may be rewritten freely. The emitted `.wit` text for the 91 positive
fixtures must be byte-identical, and the `yel:ui/dom@0.1.0` import surface must
match instruction-for-instruction, because the host on the other side is not part
of this rewrite. Note the hard-coded `0.1.0` default for sources that omit a
package version (`TECH_DEBT.md` §4) is **frozen behaviour** for now — fixing it
changes output and is a separate approved decision.

### Known bugs are free to fix, and that must be recorded.

The `known_bugs` fixtures and any latent bug the rewrite happens to fix are wins,
not divergences — but an unexplained corpus divergence is a **failure** even when
the new output looks better. Every divergence is enumerated with a reason. "The
new one is correct" is a reason; it still gets written down.

## Out of scope entirely (not frozen — simply not this project)

- `crates/yel-lsp` — consumes the frontend; it is a *beneficiary* of the green
  tree, not a stage. It is rewired after stage 1 lands, tracked separately.
- `crates/yel-host` — the Wasmtime dev host. Frozen because the execution tests
  run against it; not otherwise touched.
- `crates/yel-flow-*`, `floc` — the experimental flow frontend (gitignored). It
  is the *reason* stage 3 must be frontend-agnostic, but it is not rewritten
  here.
- `yel-viewer`, `yel-flow-editor` — not compiler code.
- Language features that do not exist yet (`match`, closures/capture analysis,
  `color`/`brush` as property types). The rewrite must not *foreclose* them —
  anti-spec B4, C4 — but it does not implement them.


## The surface freeze has planned breaks

**Recorded 2026-07-29.** `match` is being added to the language
([`LANGUAGE.md` § Match](../../LANGUAGE.md#match),
[directions §9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it)).
That is a **surface language change**, which this table otherwise forbids, so it
is written down rather than left as an inconsistency for a reviewer to find.

Why it is allowed: variants are currently **write-only** — `LANGUAGE.md`
documents how to construct `some(v)` and `none` and no way to take either apart.
That is a hole in the language, not a wart in the implementation, and the three
unrelated conditional constructs it sits beside ([F18](findings.md#f18)) are
about to be lowered into a general form. Deciding that general form *after*
stages 5–7 are built on the current arrangement is the expensive order.

What the exception does **not** license:

- **It is not a precedent.** Surface changes remain out of scope; this one is
  named, dated and bounded to `match` and its patterns.
- **It does not move now.** Stage 1 is closed and in the ratchet. The design lands
  now because stages 3/4 need it; the grammar lands as a scoped stage-1 reopening
  after stage 4 closes, or at cutover, with its own ratchet row.
- **It does not get a differential.** The frozen compiler will never parse
  `match`, so it has no oracle — see directions §9. `yel-smith` must learn to
  generate it *before* it lands.


### 2026-07-29 — `<T>`, explicit type parameters

**Decided.** Functions may declare type parameters
([`LANGUAGE.md` § Type Parameters](../../LANGUAGE.md#type-parameters)).
Inference at the call site, no constraints, no generic user-defined types.

**Why now.** [§2](directions.md#2--the-stdlib-is-yel-source-embedded-in-the-binary)
wants the stdlib written in `.yel`, and its valuable half is generic — every
`list`, `option` and `result` operation. `filter` is
`(list<T>, func(T) -> bool) -> list<T>`, and there was **no way to write that
signature**: `function_decl` and `func_type` carry no parameter list, and the `T`
in `list<T>` was prose in a documentation table. Without this the stdlib move is
limited to `min`, `max`, `starts-with` and the `*-to-string` family — the rows
whose bodies are one intrinsic call each, which is the half worth the least.

**Why declared rather than inferred.** Grain gets a generic stdlib with no
`<T>` syntax at all, because ML-family let-generalization turns a `let` binding
into a polymorphic scheme automatically. That path was available and was not
taken, because it reopens
[A2](open-decisions.md#a2--how-much-inference-sits-inside-the-bidirectional-checker) —
decided the same day as option 2, unification **without** generalization.

A2's evidence was sound and is worth re-reading in this light: it established
that yel has no polymorphic bindings *as the language stands*. §2 changes how the
language stands. So the choice was which of the two to move, and declared
parameters move the smaller thing — new grammar, no change to the checking
algorithm — where generalization would move the algorithm and leave `E0002`'s
behaviour to be re-derived.

It also composes with [A1](open-decisions.md#a1--how-are-parameterized-types-represented):
monomorphization is Rust's representation strategy and pairs with declared
parameters directly. (Grain's path would also have composed — MLton monomorphizes
whole-program HM — but via a longer argument.)

**What it retires.** [S7](stage-3-hir-build.md#s7--does-ty-gain-a-non-concrete-variant)
justified `TyKind::Param` by "a generic body is checked once, generically, so
errors land in the stdlib rather than at the user's call site". That needs
template *bodies*, which needed §2, which needed this. `Param` was machinery
ahead of its feature; this is the feature.

**Deliberately excluded, and each is a separate decision if wanted later:**
constraints/bounds · generic user-defined types · explicit type arguments at a
call site (turbofish).

### The freeze now carries three breaks — read them together

`match`, `primitive` ([§2](directions.md#2--the-stdlib-is-yel-source-embedded-in-the-binary),
still unspent with two options), and `<T>`. Three breaks decided one at a time,
each when it became urgent, is how a freeze stops meaning anything.

They are not independent: `<T>` gates the stdlib, the stdlib motivates
`primitive`, and all three are additive — everything that parsed before still
parses. What they share is a cost the differential cannot absorb: **the frozen
compiler parses none of them**, so every program using them is outside the
oracle. `yel-smith` must learn each construct *before* it lands, or it is tested
only by the cases someone thought of
([A13](anti-spec.md#a13--the-generator-ships-not-its-instances)).

None of the three moves now. Stage 1 is closed and in the ratchet; they land as
one scoped reopening after stage 4, with one ratchet row.

### 2026-07-29 — function bodies, sharing `Block` with closures

**Decided.** A function declaration may carry a block body
([`LANGUAGE.md` § Function Bodies](../../LANGUAGE.md#function-bodies)).
Parameters come from the signature; a bodyless declaration still means "someone
else implements this", which is how host callbacks and component-supplied
functions work today.

**Why.** Found by writing [`stdlib/`](../../stdlib/README.md): yel had **no way
to give a named function a body at all**. `function_decl` is
`name: func(…) -> T;`, and bodies existed only as closure literals bound to
func-typed properties. So `filter`'s implementation had nowhere to live, and
[§2](directions.md#2--the-stdlib-is-yel-source-embedded-in-the-binary)'s source
stdlib was blocked on something no analysis had named — four lines of attempted
`.yel` found it.

**The shared construct is a `Block`, and it does not exist yet.**
`ClosureExpr { params, body: Vec<Stmt> }` holds a bare statement list. The change
is to extract

```rust
pub struct Block { id, span, stmts, tail }
```

and give it two owners: `ClosureExpr { params, body: Block }` and
`FunctionDecl { …, body: Option<Block> }`. A function body and a closure body
then differ **only** in where parameters come from, which is the whole content of
the decision.

**This is worth more than the feature.** `Block` with an explicit `tail` is
already the shape [directions §9](directions.md#9--match-is-the-general-conditional-everything-desugars-into-it)
needs for the conditional collapse — `match` arms, `if` branches and ternary
arms are all blocks whose tail is their value, and "statement position" versus
"expression position" stops being a node distinction and becomes *whether the
block has a tail*. Extracting it here means §9 finds it already built rather than
inventing a second one.

**Not implemented.** Design and grammar only. The AST refactor touches
`ClosureExpr`'s field type, so it is a
[seam change](seam-changes.md) and needs its own entry when it lands.

### The freeze now carries four breaks

`match` · `primitive` (unspent, two options) · `<T>` (parses as of `8daa4b9`) ·
function bodies. Still additive, still all outside the differential, still one
scoped reopening rather than four.

The pattern worth naming: **three of the four were found by trying to write yel,
not by reading the plan.** `<T>` and function bodies came out of `stdlib/`,
`match` came out of asking what consumes a variant. Analysis found the fourth
(`primitive`) and has been sitting on it, undecided, longest.

### 2026-07-29 — attributes on items, and `unsafe`

**Decided.** Items may carry attributes, written `@name` or `@name(args)` before
the declaration. `@unsafe` is the first, gating the primitive/cast machinery the
[uniform-ref stdlib](../../stdlib/README.md) needs.

```yel
@unsafe
@primitive("@wasm.ref_array_any_get")
array-any-get: func(a: ref, i: s32) -> ref;
```

**`@` is already taken, and this is the part to get right.** `AT` is a token, it
is a member of the `NODE_START` set, and `@children` is a **UI node** — two of
the 23 diagnostic fixtures are about it (*"component `Panel` does not declare
`@children`"*, *"component already has a `@children` slot"*).

The two are separable by position: an attribute precedes a **declaration**, and
`@children` appears in a **UI tree body**. So no new sigil is needed. But the
parser must not decide by lookahead over the name — `@children` and `@unsafe`
differ only in the identifier, and a lookahead list is exactly what silently
misparsed `func<T>` (see [`seam-changes.md`](seam-changes.md), and the
`parse_type` dispatch comment). Decide by **context**, not by which name follows
the `@`.

**Unknown attributes are an error**, not ignored. An attribute that is silently
dropped is the `_ => {}` shape ([F20](findings.md#f20)) with a friendlier face —
the user writes `@unsfae` and gets working code with no gate.

### This collapses two breaks into one

With attributes, **`primitive` does not need to be a keyword.** A primitive is a
bodyless declaration carrying `@primitive("@op")`, which is one mechanism instead
of two and removes a top-level item form from the grammar. Grain has both — an
`@unsafe` decorator *and* a `primitive` keyword — but that is a historical shape,
not a requirement.

So the surface-break list is not five. It is:

| break | shape |
|---|---|
| `match` | new expression / node / statement form |
| `<T>` | ✅ parses (`8daa4b9`) |
| function bodies | a `Block` shared with closures (`5ac81f3`) |
| attributes + `@unsafe` | one mechanism; **subsumes `primitive`** |
| `ref` opaque type | one type name |

Five items, four mechanisms, one scoped reopening after stage 4. `primitive`'s
"two unspent options" question is answered by not asking it.

#### Shape: `Attribute` / `AttributeList`, and the WIT-passthrough consumer

Following ark's *structure* while rejecting its content
([`arkc-parser`'s `ModifierList`](https://github.com/szkabaroli/ark) is on
`Import`/`Module` only, its `Modifier` carries **no name**, and it is marked
"remove in next step" — nothing to port):

```rust
pub struct AttributeList { id, span, attributes: Vec<Recovered<Attribute>> }
pub struct Attribute { id, span, name: MaybeIdent, args: Vec<AttributeArg> }
```

Nodes with `id`/`span`, not side data — an attribute is source text and has to
round-trip (S1), so it belongs in the tree.

**Why `args` are `key = value`, not positional.** The first real consumer is
**WIT passthrough**. WIT's own feature gates are already spelled
`@since(...)` / `@unstable(...)` / `@deprecated(...)` with **named** arguments,
so a yel attribute on an exported item can emit near-literally into the WIT
rather than being translated. That is the reason to design `args` as named pairs
now instead of discovering it later.

⚠️ **Check the exact WIT gate grammar against the spec before fixing the arg
form** — `key = value` versus `key: value` is from recollection, not verified,
and getting it wrong makes "passthrough" a translation layer.

`wit_ast.rs` has **no notion of gates today** — `to_wit_name` (:1322) is the only
name-level machinery. So this is additive there too.

**The `@children` collision stays unresolved on purpose.** Both spellings live
together: an attribute precedes a *declaration*, `@children` is a *UI node*.
Changing `@children` would be the first **non-additive** break — measured at
**1020 of 2000 corpus programs** — so it waits for cutover, alongside
[§7](directions.md#7--keywords-get-a-word-boundary--at-cutover-by-deletion)'s
keyword reservation, where there is no oracle left to invalidate.

#### Corrections — three statements above are wrong

Found by implementing them (`a68e127`). Left in place with corrections rather than
edited away, because each was wrong for a reason worth keeping.

**1. "Separable by position" is false.** The claim was that an attribute precedes
a *declaration* while `@children` sits in a *UI tree body*, so context
disambiguates. It does not: `export component App { @children }` is a legal
**direct component member** — it is a row in `identity.rs`'s hand-written table
(:730). An attributed member and a `@children` node occupy the same position in
the same parse function, so "which parse function am I in" returns the same
answer for both.

The implemented rule is total instead: **an `AT` whose next raw token is
`CHILDREN_KW` is the slot marker; every other `AT` in a declaration position
opens an attribute list.** It reads one token kind the lexer already assigned
rather than a table of attribute spellings, so it cannot drift as attributes are
added, and it has no third outcome. Consequence, accepted deliberately:
`@children` can never be spelled as an attribute.

**2. `@primitive("@wasm…")` contradicted the named-args decision** three
paragraphs below it. Both could not hold.

**3. And it is moot — `primitive` is a top-level item form after all.**

```yel
primitive array-any-get: func(a: ref, i: s32) -> ref = "@wasm.ref_array_any_get";
```

Keyword, name, **type**, `=`, op string. So `@primitive(op = …)` is not needed:
`@unsafe` stays an attribute, `primitive` is an item, and the two mechanisms do
not overlap. The earlier "attributes subsume `primitive`" collapse is withdrawn.

**Keep the type in the source, unlike Grain.** Every Grain primitive is a
function and carries **no type annotation** — `provide primitive load =
"@wasm.load_int32"` — so its compiler owns the signature, keyed off the op
string. That makes the stdlib opaque: you cannot read `wasmi32.gr` and learn a
single type. Yel's builtin table already stores `params`/`ret`, so the type must
exist somewhere; putting it in the source makes the stdlib self-describing.

**The cost, and the fix already in use.** Type-in-source means the *declared*
type and the *op's actual* signature are two things that must agree — which is
[F12](findings.md)'s shape. The compiler knows each op's true signature, so it
**verifies the declared type against it once, at registration**, failing there
with every mismatch listed. That is exactly [C2](stage-3-hir-build.md)'s
lang-items pattern: one assertion where the invariant is established, not trust
plus re-checking at each use. It validates the op string for free — `op = "@typo"`
fails at registration rather than lowering to nothing.

**Revised surface list:** `match` · `<T>` ✅ · function bodies · attributes +
`@unsafe` ✅ (`a68e127`) · `ref` type · `primitive` item form. Six items, five
mechanisms.
