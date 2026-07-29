# `yelc-sema` — shared semantic infrastructure       status: brief written

> **This is no longer a separate landing.** As of 2026-07-29 it is **phase 1 of
> stage 2a** ([`stage-2a-hir-build.md` § Work in scope](stage-2a-hir-build.md#work-in-scope)),
> not its own row on the board. This file stays as the brief — the scope, the
> S1–S7 decisions and every cross-reference to them are unchanged, and the design
> questions it raises (Clusters A–D of [`open-decisions.md`](open-decisions.md))
> are still open and still gate the work.
>
> What changed is only *who owns it*: 2a's agent, as its first phase, instead of
> a handoff between two briefs. **Keep the standalone checkpoint** —
> `lookup_known_definitions` registers builtins from no input at all, so the
> resulting `Definitions` is comparable against the frozen table before a single
> source file is parsed. That comparison now lands in 2a's Numbers rather than in
> its own ratchet row, which makes it easier to skip and no less required.

Replaces (frozen, never edited): `yel-core/src/{context.rs, definitions.rs,
known.rs, stdlib_lookup.rs, types/}` — **in part**; see [Scope](#scope).

Base: — · Started: — · Landed: —

> **Not a stage.** It transforms no IR, so it gets no stage number — the
> precedent is `yelc-base`, which landed the same way. It **does** get a ratchet
> row: it lands on `main`, and the rule is that landing anything never lowers the
> number.
>
> **It is the open blocker for [2a](stage-2a-hir-build.md).** Six plan documents
> already depend on it. Nothing owned it until this file.

## Scope

> **Read the frozen tree; do not port it.** The old compiler is the
> *specification* — it encodes years of correct behaviour in code that is
> feature-incomplete and structurally wrong. What transfers is the **inventory
> and the behaviour**: which builtins exist, what a definition is, what
> diagnostics fire. The implementation is written fresh.
>
> The exception is the [keep-list](keep-list.md), which is kept by contract and
> is **already landed in `yelc-base`** — nothing on it is in scope here.

| frozen file | lines | what it tells you (read this) | what you write (fresh) |
|---|---|---|---|
| `stdlib_lookup.rs` | 1,029 | **the builtin inventory** — every name, signature and arity, and the order they register in | one table, [S1](#s1--adopt-the-builtin-table-1). The inventory is the spec; the 1,029 lines are not |
| `definitions.rs` | 742 | what a definition is; namespaces; lookup and duplicate semantics | fresh tables, `DefId` module-qualified from day one ([S5](#s5--defid-shape)) |
| `types/interner.rs` | 389 | which types exist and how equality works | a fresh interner — but **not necessarily smaller**; whether it gains a non-concrete variant is [S7](#s7--does-ty-gain-a-non-concrete-variant), and it is open |
| `known.rs` | 413 | which builtins downstream actually reaches for | mostly nothing — [S3](#s3--does-known-survive-at-all) |
| `context.rs` | 963 | which state is *genuinely* shared vs parked | a small context ([S4](#s4--what-stays-on-the-context)); a quarter of it cannot compile here at all |

**Why not port.** This crate's frozen half is where the measured defects
concentrate — not style complaints, findings with repros:

- [F12](findings.md#f12) — one builtin is four things that must agree, checked by
  nothing.
- [F3](findings.md#f3) — `intern_ast_ty` runs before names exist, so a named type
  silently becomes `Unknown`.
- [F1](findings.md#f1) — `option` is registered with `payload: Ty::ERROR` under a
  comment calling it a "generic placeholder".
- [F5](findings.md#f5) — the tables *are* typed, but filled at the wrong moment.

Porting carries all four forward and then asks a reviewer to notice. Writing
fresh against the inventory does not.

**Already in `yelc-base`, out of scope:** `ids.rs`, `index_vec.rs`,
`interner.rs` (string interning), `source.rs`, `diagnostic.rs` —
[keep-list](keep-list.md) §1–§4, landed.

### `context.rs` is a cross-pipeline god object and cannot be ported

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
It belongs to [3a](stage-3a-lir.md) (the ids and their allocation) and
[3b](stage-3b-lower.md) (the lifecycle and fanout tables, which are lowering
bookkeeping). That is a note *for those briefs*, and it is written down here
because this is where someone will first notice.

## Brief

Provide, to `yelc-hir` and above, exactly four things:

1. **The type interner.** `Ty` handles, `InternedTyKind`, structural equality.
2. **The definition tables.** `DefId`, `DefKind`, `Namespace`, registration and
   lookup, spans.
3. **Context threading.** [keep-list §5](keep-list.md#5--context-threading--yel-coresrccontextrs)
   keeps the *pattern* — one context threaded through every phase — **not the
   963-line struct**. What it holds is [S4](#s4--what-stays-on-the-context).
4. **The builtin table** — see [S1](#s1--adopt-the-builtin-table-1).

Plus two things the frozen tree has no equivalent of, both from
[§6](directions.md#6--modules-are-serializable-artifacts):

5. **`ModuleId` and `DefPath`** — the identity that crosses a module boundary.
6. **Structural `Ty` serialization** — see [S2](#s2--ty-must-not-serialize-as-its-handle).

## Decisions this crate must make

Written calls before anyone starts. Numbered `S` so they do not collide with
2a's `D`.

Answers come from [`open-decisions.md`](open-decisions.md); this table is the
record.

| # | decision | status |
|---|---|---|
| S1 | Adopt [§1](directions.md#1--builtins-are-a-table-not-a-field-per-builtin)'s builtin table? | ✅ **yes** — one table, replacing `stdlib_lookup.rs` + `known.rs` (C1, 2026-07-29) |
| S2 | How does `Ty` serialize? | ⬜ **open** (B1) |
| S3 | Does `known.rs` survive? | ⬜ **open** (C2) |
| S4 | What stays on `CompilerContext`? | ⬜ **open** (D0) |
| S5 | `DefId` shape, given `DefPath` | ✅ **module-qualified from day one** — `DefId { module, index }` (B2, 2026-07-29) |
| S6 | Who owns `OverloadKey` — here or 3b? | ✅ **here** — one key, consumed by `DefPath` and §3's mangling (B3, 2026-07-29) |
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

### S3 · Does `known` survive at all?

`KnownDefinitions` is five sub-structs of `Option<DefId>`. S1 removes the
*functions* half outright. What is left is the question of builtin **elements**,
**enums** and **variants** — UI vocabulary with no lowering target, which may
genuinely want a different home than a call table. Decide; do not port 413 lines
because they exist.

Note the `Option` is load-bearing nowhere: every read is an unwrap-or-diagnostic
for a case that cannot occur once registration has run
([A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed)).

### S4 · What stays on the context

Keep-list §5 keeps context *threading*, not the god object. The test for each
field: **is it produced and consumed within sema, or is it a later stage's state
parked here?** The LIR fields fail that test and cannot compile here anyway.

Watch for the same shape re-forming: `signal_deps` keyed by `DefId` is cited as
the *positive* precedent for side tables
([B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)) — but
it is reactivity analysis, which is a frontend concern, not a sema one. Decide
where it lives rather than inheriting its address.

### S5 · `DefId` shape

`DefId` stays a dense module-local index — it is used everywhere in-process and
must stay O(1). What changes is that it is **module-qualified from day one**, so
that `DefPath` (the serialized form) is derivable and nothing downstream has to
be retrofitted later. See
[2a § Designed for serialization](stage-2a-hir-build.md#designed-for-serialization--what-2a-owes-6).

### S6 · `OverloadKey`

Needed twice: by [§6](directions.md#6--modules-are-serializable-artifacts)'s
`DefPath` (Swift's `XREF_VALUE_PATH_PIECE` carries the type, because a name does
not identify a decl under overloading) and by
[§3](directions.md#3--generics-are-monomorphization-by-name)'s mangling
(`len` is both `list<T> -> s32` and `string -> s32`). **Same key, settle once
here.**

### S7 · Does `Ty` gain a non-concrete variant?

**Decided 2026-07-29: yes, both.** `InternedTyKind` gains `Param` (A3) *and*
`Infer` (A4). **This reverses the recommendation previously written here**, which
was "no" on both. The reasoning that recommendation rested on is recorded below,
along with why it did not survive — a recommendation that loses is more useful
kept than deleted.

Two holes, two lifetimes, and they are **not interchangeable**:

| variant | means | legal | must be gone by |
|---|---|---|---|
| `Param(idx)` | the `T` in a declaration | in a template's stored signature | substitution at instantiation |
| `Infer(var)` | unknown, to be solved | during 3b checking | the end of 3b |

#### Why "no" lost

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

#### What both variants now oblige

1. **Neither may ever be serialized.** A module artifact containing a `Param` or
   an `Infer` is a bug, not a state. This tightens
   [S2](#s2--ty-must-not-serialize-as-its-handle): the structural writer must
   *refuse* them, not merely encode them faithfully.
2. **`Infer` must not outlive 3b.** 3b's postcondition strengthens from "`types`
   is total" to "`types` is total **and contains no unresolved variable**" —
   rustc's `has_infer()` check, asserted rather than assumed.
3. **`Param` must not outlive substitution.** A `Param` reaching 4a is the same
   class of error: the instantiation did not happen.
4. **Structural equality must distinguish them.** Two `Param(0)`s from different
   templates are not the same type; two distinct `Infer` variables are never
   equal. Decide whether variables live in the interner at all or in a side
   unification table — interning a value that is *about to change* is the usual
   mistake here.
5. **The interner's uniquing invariant weakens.** Today equal types share a
   handle. With `Infer`, two handles may become equal *later*, which every
   `Ty == Ty` comparison in the checker must be written knowing.

## Contract

> Lands on `main` as compiling Rust before 2a is briefed
> ([`contract-before-fanout`](../../.agents/skills/compiler-rewrite/rules/contract-before-fanout.md)).

```rust
// identity
pub struct ModuleId(u32);
pub struct DefId { module: ModuleId, index: u32 }   // dense, in-process, never serialized
pub struct DefPath { module: ModuleId, pieces: Vec<PathPiece> }  // serialized, resolved by lookup

// types
pub struct Ty(u32);                      // handle; NOT Serialize — see S2
pub enum InternedTyKind { /* … */ }
pub struct TyInterner;                   // intern / lookup / structural write

// definitions
pub struct Definitions;                  // alloc, register_name, lookup, span, as_*
pub enum Namespace { Type, Value, Component, Global }

// builtins
pub struct BuiltinTable;                 // §1: name → { arity, type scheme, lowering target }

// threading
pub struct CompilerContext {             // S4 decides the fields; NOT the frozen 963 lines
    pub interner:   Arc<Interner>,       // from yelc-base
    pub types:      TyInterner,
    pub defs:       Definitions,
    pub builtins:   BuiltinTable,
    pub source_map: SourceMap,           // from yelc-base
    pub diagnostics: Diagnostics,        // from yelc-base
}
```

**Depends on:** `yelc-base` only. **Must not depend on:** `yelc-syntax`,
`yelc-hir`, `yelc-lir` — a `use yelc_lir::BlockId` here is the error this brief
exists to prevent, and cargo will say so.

## Verification

**Unlike [2a](stage-2a-hir-build.md#verification), this crate has a standalone
artifact.** `lookup_known_definitions` registers every builtin from **no input at
all**, so the resulting definition table is comparable against the frozen one
before a single source file is parsed.

1. **The builtin `Definitions` table** — names, kinds, and **order** — identical
   to the frozen tree's after registration. `DefId`s are ordinals that reach
   output ordering, so order is load-bearing, not cosmetic. Compared via the
   read-only oracle crate 2a also needs
   ([`greenfield-never-touch-old-code`](../../.agents/skills/compiler-rewrite/rules/greenfield-never-touch-old-code.md)
   permits depending on the frozen tree as a library).
2. **`Ty` interning is structurally equal** — the same source type produces the
   same `InternedTyKind`, and equal types share a handle.
3. **A determinism run** — register twice in one process, and across two
   processes; the table must be byte-identical
   ([A6](anti-spec.md#a6--no-random-seeded-iteration-reaching-output)).

This is a stronger gate than 2a gets, and it is available *first* — which is a
reason to land this crate before 2a rather than alongside it.

## Definition of done

- [ ] `yelc-sema` compiles; depends on `yelc-base` and no other workspace crate.
- [ ] **No `yelc-lir` vocabulary anywhere** — no `BlockId`, `BlockDebugName`,
      `ComponentLifecycleBlocks`, no fanout table. Enforced by the crate graph,
      confirmed by grep.
- [ ] Builtin `Definitions` table identical to the frozen tree's, contents **and
      order**, via the oracle crate.
- [ ] Registration is deterministic across processes.
- [ ] `Ty` does **not** derive `Serialize`; the structural writer is the only
      path (S2).
- [ ] `DefId` is module-qualified; `DefPath` exists and is the only identity in a
      serializable position (S5).
- [ ] S1–S6 recorded with reasoning.
- [ ] The LIR-state note is filed into [3a](stage-3a-lir.md) and
      [3b](stage-3b-lower.md) — they inherit what this crate refused.
- [ ] Ratchet row landed and met.
- [ ] Adversarial review panel, read-only, one lens each.
- [ ] Surprises written — [D3](anti-spec.md#d3--a-stage-documents-what-surprised-it).

## Reference

- **Frozen:** `yel-core/src/context.rs` (963 — the god object),
  `definitions.rs` (742), `stdlib_lookup.rs` (1,029), `known.rs` (413),
  `types/interner.rs` (389).
- **Directions:** [§1](directions.md#1--builtins-are-a-table-not-a-field-per-builtin)
  (the table), [§3](directions.md#3--generics-are-monomorphization-by-name)
  (`OverloadKey`), [§6](directions.md#6--modules-are-serializable-artifacts)
  (`DefPath`, structural `Ty`).
- **Findings:** [F1](findings.md#f1) (no type variables),
  [F5](findings.md#f5) (the tables are typed),
  [F12](findings.md#f12) (a field per builtin).

## Numbers · Decision log · Surprises

*To be written at close-out.*
