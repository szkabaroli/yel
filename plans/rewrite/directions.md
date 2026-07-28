# Directions — recorded intent, not yet contract

> **A direction is not a mandate.** Nothing in this file constrains a stage that
> is already briefed, and nothing here is verified by the ratchet. A direction
> becomes binding only when it is copied into a stage brief with a definition of
> done. Until then it exists so the idea is not re-derived from scratch three
> stages later, and so the *next* brief has a starting point instead of a blank
> page.

Distinct from the neighbouring documents on purpose:

| | |
|---|---|
| [`scope.md`](scope.md) | what may change |
| [`anti-spec.md`](anti-spec.md) | shapes that may not be reproduced |
| [`keep-list.md`](keep-list.md) | what carries over intact |
| **this file** | shapes we would *like* to reach, with the reference that motivated them |

Append-only, same as the anti-spec. An entry that is adopted gets a line saying
which stage adopted it; an entry that is rejected stays, with the reasoning.

---

## 1 · Builtins are a table, not a field per builtin

**Status: open. Not scheduled. Explicitly out of scope for stage 1.**
Earliest possible consumer: `yelc-sema`, read by stage 3 and stage 4b.

### The shape in the frozen tree

Builtins are registered imperatively and then addressed by name-as-Rust-field:

```
yel-core/src/stdlib_lookup.rs   1,029 lines of register_* calls
yel-core/src/known.rs             413 lines: KnownDefinitions
                                  { KnownElements, KnownEnums, KnownVariants,
                                    KnownFunctions, KnownBuiltinTypes }
                                  every member `Option<DefId>`
```

Call sites: 51 in `stdlib_lookup.rs`, 24 in `thir/typeck.rs`, 5 across
`lower_to_lir/`.

Adding one builtin function touches four places that must agree and are checked
by nothing: a new `Option<DefId>` field, a `register_function` call, a typeck arm
that reads the field, and a lowering arm that matches on the same `DefId`. The
`Option` is load-bearing nowhere — every read is a `known.functions.concat`
followed by an unwrap-or-diagnostic for a case that cannot occur after
registration runs. That is
[A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed) in slow motion: the
registration/consumption agreement is a convention, not an invariant, and the
compiler cannot tell you when it breaks.

### The reference — Grain's `primitive`

Grain's stdlib is written in Grain down to the allocator and the refcounting GC.
The one hole in the floor is a binding form with **no type annotation and no
body**:

```grain
provide primitive (+)           = "@wasm.add_int32"
provide primitive allocateTuple = "@allocate.tuple"
```

The string is not a link symbol. It is a key into `prim_map`, a hashtable in
`compiler/src/typed/translprim.re`:

```ocaml
("@heap.start",     Primitive0(HeapStart)),
("@allocate.tuple", Primitive1(AllocateTuple)),
("@throw",          Primitive1(Throw)),
("@wasm.add_int32", Primitive2(WasmBinaryI32({
   wasm_op: Op_add_int32,
   arg_types: (Wasm_int32, Wasm_int32),
   ret_type: Wasm_int32,
}))),
```

`transl_prim` then does three things, and all three are the interesting part:

1. **Looks the name up.** A miss is a hard compiler error — the set is closed.
2. **Synthesizes the type** from the arity tag (`prim0_type` … `primn_type`), so
   the signature is a property of the table, not of anything the user wrote.
3. **Synthesizes an eta-expanded body**, so the binding is a real first-class
   value you can pass around, while a direct application still inlines to the raw
   instruction downstream.

Two categories share one table: instruction-shaped (`@wasm.*`) and
compiler-generated-stub-shaped (`@allocate.*`, `@throw`). One lookup, one place
that knows the arity, one place that knows the type.

### What is worth carrying

A single table in `yelc-sema`, keyed by a stable name, whose row carries
everything the rest of the pipeline needs:

```
name  →  { arity, type scheme, lowering target }
```

- The type scheme comes from the table, so typeck asks the table rather than
  holding 24 references to named fields.
- The lowering target comes from the same row, so 4b asks the table rather than
  matching on `DefId`s it obtained from a different module.
- Adding a builtin is one row. Removing one is one row. A row that no consumer
  reads is dead and visible as dead —
  [A9](anti-spec.md#a9--a-ported-construct-is-load-bearing-or-it-is-deleted).
- Unknown name is a hard error at registration, not a `None` that surfaces four
  passes later as a diagnostic for an impossible case.

Grain's third move — eta-expansion — matters more than it looks. It is what lets
a builtin be simultaneously a first-class value and a zero-cost inline op without
partial application becoming a special case. Yel already has the equivalent
question for `len`, `min`, `max`, `filter`; the frozen tree answers it per-site.

### What is *not* worth carrying

Grain's runtime model is the wrong half of the reference and should not follow
the table into yel:

- **The untagged `WasmI32` layer and `@unsafe`.** Grain needs it because its
  stdlib implements its own allocator in linear memory. Yel targets WASM-GC; the
  host VM owns allocation. There is no yel equivalent and inventing one would
  contradict the recorded position that yel is AOT with no runtime crate.
- **`foreign` / FFI syntax.** Distinct mechanism from `primitive` in Grain
  (`foreign wasm fd_write: … from "wasi_snapshot_preview1"` emits a real import).
  Yel's import surface is the frozen `yel:ui/dom@0.1.0` WIT world and is not
  user-extensible. `dom_imports.rs` is the existing analogue and stays that way.
Grain's **surface syntax** for `primitive` is a separate question, and the answer
is that we want it — see [§2](#2--the-stdlib-is-yel-source-embedded-in-the-binary),
which this direction is the floor for. §1 is adoptable on its own with the table
populated from Rust; §2 is not adoptable without §1.

### Why this does not touch stage 1

The whole mechanism lives at and below the `yelc-sema` seam. It is a registration
table and a lookup, not a construct the parser sees:

- **No new keyword, no new token kind, no grammar change.** The frozen grammar is
  frozen ([`scope.md`](scope.md) — "Parser technology is free … what stays frozen
  is the *grammar it accepts*"), and this direction does not ask for an inch of
  it.
- **No change to any landed seam type**, so no
  [`seam-changes.md`](seam-changes.md) entry is owed.
- **No change to `scope.md`.** Builtin registration is "everything else in
  `yel-core`" — already free.

The surface-syntax version — a `.yel` prelude declaring builtins the way Grain
declares them — **is** wanted, and it *is* a language change, a scope-table move,
and a `seam-changes.md` decision. It is [§2](#2--the-stdlib-is-yel-source-embedded-in-the-binary),
tracked separately precisely so that adopting the table does not smuggle a
grammar change in with it. §1 stays stage-1-neutral; §2 does not, and says so.

### Open questions for whoever adopts it

- **Where does the row live: `yelc-sema` or split?** Typeck needs the type
  scheme; 4b needs the lowering target; `yelc-lir` must not see either
  ([C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam)). One
  table with two projections, or two tables that a test asserts are key-aligned?
  The second is honest about the seam and pays for it with a consistency check.
- **Do builtin *elements* and *enums* belong in the same table as functions?**
  `KnownElements` is 15 fields of UI vocabulary and does not have a "lowering
  target" in the same sense. Forcing one table over both may be the tidiness trap
  the anti-spec is full of.
- **Does the table survive the frontend-agnostic goal?** The visual flow language
  shares the substrate below the seam but not the builtins. The table is a
  *frontend* artifact; check that adopting it does not create a second path by
  which UI names reach `yelc-lir`.
- **Variadics.** `concat` is registered with an empty parameter list and a comment
  saying it is really variadic — the frozen tree models it nowhere. A table with a
  declared arity has to answer this rather than leave it in a comment.

### Reference

- `stdlib/runtime/unsafe/wasmi32.gr`, `stdlib/runtime/malloc.gr`,
  `stdlib/pervasives.gr` — <https://github.com/grain-lang/grain/tree/main/stdlib>
- `compiler/src/typed/translprim.re{,i}` — the table and `transl_prim`

---

## 2 · The stdlib is yel source, embedded in the binary

**Status: wanted. Not scheduled. Has a stage-1 consequence — see below.**
Depends on [§1](#1--builtins-are-a-table-not-a-field-per-builtin); blocked on
language features stage 3 currently lists as open gaps.

### The intent

`crates/yel-core/src/stdlib_lookup.rs` builds the standard library by calling
`register_function(ctx, "starts-with", &[Ty::STRING, Ty::STRING], Ty::BOOL)`.
The library exists only as Rust that constructs `DefId`s. It has no source form,
no span, and nothing typechecks it — the signature in `LANGUAGE.md` and the
signature in the Rust call are related by nobody.

The direction is that the stdlib is written in **yel**, in `.yel` files, and
those files are **embedded into the `yelc` binary at Rust compile time**
(`include_str!` / `include_dir!`), not shipped alongside it.

```
stdlib/prelude.yel        written in yel, typechecked like any other source
        ↓ include_str!
yelc-sema (or yelc-driver)   embedded &'static str, no filesystem lookup
        ↓ parse → HIR → THIR, same stages as user code
        ↓ bottoms out in
primitive declarations  →  the §1 table  →  LirOps
```

The floor is §1: the handful of things that genuinely cannot be written in yel
are `primitive` declarations whose type and body the compiler supplies. Everything
above the floor is ordinary yel. This is exactly Grain's layering — `wasmi32.gr`
is nothing but `provide primitive` lines; `list.gr` above it is plain Grain.

### Why embedded rather than a shipped directory

Grain ships its stdlib as an npm package whose entire `index.js` is
`module.exports = __dirname` — the library is a *path*, resolved at runtime.
Do not copy that:

- **Determinism.** [keep-list](keep-list.md) requires byte-stable output. A
  filesystem-resolved stdlib makes output depend on what is installed, which is
  a determinism hole that no amount of sorting closes.
- **The differential.** The 2000-seed corpus is only an oracle if both compilers
  see the same library. Embedding makes "which stdlib" a property of the binary.
- **No install story.** `yelc` stays a single hermetic artifact; cross-compiling
  and CI do not acquire a data-file dependency.

The cost is that the stdlib is parsed and checked on every invocation. Note it,
measure it when the stage is briefed, and do not pre-solve it — the caching
answer (a serialized prelude snapshot) is a real design with real staleness bugs
and should not be adopted before the number justifies it.

### Why this has a stage-1 consequence

This is the part that changes relative to §1, and it must not be discovered late.

`primitive` (or whatever it is spelled) is a **new top-level item form**, so the
parser has to accept it. [`scope.md`](scope.md) freezes the grammar with two
clauses, and this direction is fine against the first and violates the second:

| clause | verdict |
|---|---|
| every existing fixture / corpus program / `LANGUAGE.md` construct still parses | **holds** — the addition is purely additive |
| "nothing new parses that did not before" | **violated, deliberately** |

So it needs an approved scope move and a `seam-changes.md` entry. It is not a
thing a stage-N agent may decide to do because the stdlib needed it.

Two ways to spend that, to be decided when it is briefed:

1. **Add the form to the language.** `primitive` becomes real yel that a user
   could write. Simplest parser; largest surface commitment; `LANGUAGE.md` grows
   a section for a feature that exists to serve the compiler's own library.
2. **A stdlib-only dialect bit.** The parser accepts the form only when the unit
   is flagged as stdlib — Grain's `@runtimeMode`/`@noPervasives` are this idea.
   `LANGUAGE.md` stays frozen and users cannot write it. Cost: two grammars in
   one parser, and a mode flag that every parser test has to be aware of.

Option 2 keeps the *language* frozen while unfreezing the *parser*, which is the
narrower move and probably right — but it earns a note in the anti-spec's
neighbourhood, because "a flag that changes what the grammar accepts" is the kind
of thing that spreads. Neither option is chosen here.

**Nothing above requires touching stage 1 now.** Stage 1's contract is a parser
for the frozen grammar; this direction adds a form later, additively. It is
recorded so stage 1 does not accidentally build something that makes an additive
item form expensive — a hand-written recursive-descent item parser over a green
tree is exactly the shape that makes it cheap.

### What can actually move to source, and what is blocked

The honest finding: most of the interesting stdlib **cannot be written in yel
today**, because yel has no generic functions and stage 3 lists closure capture
analysis as stubbed. Sorting the existing builtins by what they need:

| tier | builtins | needs |
|---|---|---|
| **A — writable now** | `min`, `max` (`(s32,s32) -> s32`) | nothing; a ternary |
| **B — declared in source, implemented by the table** | `concat`, `starts-with`, all `*-to-string` | §1 only |
| **C — blocked on parametrization** | `len` (`list<T>`/`string` overload), `some`/`none` (generic constructors), `list.get`, `append` | [§3](#3--generics-are-monomorphization-by-name-not-a-type-system-feature) only |
| **C′ — blocked on parametrization *and* closures** | `filter` (`(list<T>, func(T) -> bool) -> list<T>`) | §3 **and** [§4](#4--closures-are-a-value-and-the-new-irs-are-shaped-for-one) |
| **D — probably never source** | builtin elements (`vstack`, `text`, … with attribute schemas), `yel:ui/dom` imports | a declaration form for element schemas / a `foreign` analogue |

Tier C is the whole reason the stdlib is interesting, and it is gated on the two
stage-3 gaps. That is a **sequencing fact, not an objection**: it says this
direction lands after stage 3 decides the closure and generics questions, and it
gives those decisions a concrete consumer to be judged against.

The generics half of that gate has a cheap answer —
[§3](#3--generics-are-monomorphization-by-name-not-a-type-system-feature) — which
moves everything except `filter` out of "blocked". The closure half does not, and
`filter` stays blocked on it.

Tier D is a genuine question, not a foregone conclusion. Grain keeps the parallel
mechanism (`foreign wasm … from "wasi_snapshot_preview1"`) in source too. Yel's
import surface is the frozen `yel:ui/dom@0.1.0` world, so a source form would be
a *declaration* of a frozen contract, not an extension point.

### There is no module system, and this direction must not invent one

`LANGUAGE.md` § File Structure: a `.yel` file is a package declaration plus
top-level items. There is **no `import`, no `use`, no `from … include`** — and
adding one is a far larger language change than `primitive` is.

So the stdlib is an **implicit prelude**: its items are in scope everywhere with
no import, the way `KnownFunctions` effectively is today. That preserves current
behaviour exactly and needs no new syntax. Grain's `pervasives.gr` is this, and
its `@noPervasives` annotation exists to break the resulting cycle — the stdlib
cannot import itself. Whatever yel's equivalent is, it is a compiler-internal
flag on the stdlib unit, not user-facing.

If a source stdlib turns out to want multiple files that reference each other,
that is a module system, and it is a separate decision — not an implementation
detail of this one.

### The differential will move, and that is the risk

Today a builtin is a `DefId` with a synthesized type and no body. As source it
gets a span, real HIR, real THIR, and — for tiers A and C — real LIR and real
emitted code. Plausible consequences: function ordering in the module, WIT
content, DOT output, diagnostic spans that now point into stdlib source.

Per [`scope.md`](scope.md), every one of those is an enumerated divergence with a
written reason, and this lands as **its own change with its own fixtures**, never
as a side effect of whichever stage happened to be in flight. If adopting it
produces an unexplained corpus divergence, it is a failure even if the new output
looks better.

### Open questions for whoever adopts it

- **Where does the embedded source live and who compiles it?** `yelc-sema` owns
  builtins today, but if the stdlib is source it is compiled by stages 1–3 like
  any unit, which points at `yelc-driver`. `yelc-sema` would keep only the §1
  table. Decide before the crate graph calcifies.
- **Spelling.** `primitive` is Grain's word. Yel's naming conventions are
  kebab-case identifiers and no abbreviations; `intrinsic` is the other obvious
  candidate. Cheap to decide now, expensive to change after fixtures exist.
- **Do stdlib diagnostics ever reach users?** A type error inside the prelude is
  a compiler bug, not a user error. It needs a distinct presentation — or an
  assertion that the prelude typechecks clean, checked once in CI rather than on
  every invocation.
- **Does the prelude participate in the file-is-the-compilation-unit rule?**
  [D1](anti-spec.md#d1--the-compilation-unit-is-the-file-not-the-component) says
  the unit is the file. The prelude is a file the user did not write; confirm
  that does not need a second notion of unit.

### Reference

- `stdlib/pervasives.gr` (`@noPervasives`, implicit prelude),
  `stdlib/runtime/unsafe/wasmi32.gr` (the `primitive` floor),
  `stdlib/index.js` (`module.exports = __dirname` — the part not to copy)

---

## 3 · Generics are monomorphization by name, not a type system feature

**Status: wanted, and the cheapest of the three.** Unblocks all of §2 tier C
except `filter`. Decided by stage 3.

### The proposal

Do not add type variables, unification, or generalization. Let a parameterized
stdlib item be a **template over a name**, instantiated to a concrete monomorphic
item per type it is used at:

```
declared once      list<T>        →  internal names  $list_s32, $list_string, $list_Person
                   len(list<T>)   →                  $len_list_s32, $len_list_Person
```

Each instantiation is a distinct `DefId` with a fully concrete signature — which
is *exactly* what `register_function(ctx, "starts-with", &[Ty::STRING, …])`
produces today. Parametrization does not introduce a new kind of definition; it
generates the kind that already exists.

### Why it fits: there are no type variables to remove

This is the argument that decides it. `InternedTyKind`
(`yel-core/src/types/interner.rs:50`) is:

```rust
List(Ty), Option(Ty), Result { ok: Option<Ty>, err: Option<Ty> },
Tuple(Vec<Ty>), Adt(DefId), Func { params: Vec<Ty>, ret: Option<Ty> },
Error, Unknown, Unit,   // + primitives + UI types
```

There is **no `TyVar` / `Param` variant**. `List(Ty)` is always concrete.
`Mode::Infer` in `typeck.rs` is a bidirectional checking *mode*, not a type
variable — it means "synthesize", not "unknown, to be solved". Yel's type system
is already monomorphic all the way down, and the frozen tree admits it: `option`
is registered as a "template variant" with `payload: Some(Ty::ERROR)` and the
comment *"Generic placeholder — actual types are `option<T>`"*
(`stdlib_lookup.rs:63`). `Ty::ERROR` standing in for a type parameter is
[A5](anti-spec.md#a5--no-silent-fallback) wearing a comment.

So the choice is not "real generics vs. a simplification." It is:

| | cost |
|---|---|
| **Real generics** | add `TyVar` to `InternedTyKind`, a substitution, unification, generalization/instantiation, and a polymorphic-representation decision at the LIR seam — touching stage 3 *and* stage 4 |
| **Monomorphization by name** | a substitution pass over stdlib source + a memo table; `Ty`, THIR, LIR, and codegen unchanged |

The second is not a compromise that pays for itself later. It is the design the
existing `Ty` already implies, and it is the one that matches the target: on
WASM-GC you want a concrete struct type per instantiation anyway. A uniform
polymorphic representation would force boxing at exactly the seam
[C2](anti-spec.md#c2--one-representation-chosen-at-the-seam) says to keep single.

### Internal-only, or it breaks the corpus

The mangled name must be an **internal** name, never surface syntax.
`LANGUAGE.md` documents `list<T>`, `option<T>`, `items.filter(...)`, and the
surface is frozen — if users write `$list_s32`, the corpus stops compiling and
every other rule loses its teeth at once ([`scope.md`](scope.md)).

```
user writes        list<s32>          ← unchanged, frozen, documented
compiler names     $list_s32          ← internal DefId name, never rendered
```

This is what makes §3 cheaper than §2: **it needs no grammar change at all** on
the user-facing side. The stdlib source needs *some* way to spell the parameter,
but that rides along with the stdlib dialect §2 already has to pay for — it does
not add a second scope move.

Two naming notes, cheap now and expensive after fixtures exist:

- **`i32` is not a yel type.** LANGUAGE.md spells the primitives `s32`/`u32`.
  Mangle with yel's own vocabulary (`$list_s32`) or the compiler acquires a
  second name for every integer type.
- **`$` is already taken.** `$Comp` is the component self-reference in the frozen
  tree ([anti-spec C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam)
  mandates removing it from below the seam, but it exists above). A second
  meaning for `$` in the same compiler is a collision; pick a sigil that is not
  in use, or none at all — an internal name never parsed does not need one.

### What triggers instantiation — the one real design question

Everything else here is bookkeeping; this is not.

- **Eager, over a closed set of types.** Pre-instantiate for `s32`, `string`,
  `bool`, `f64`, … Simplest possible implementation, no on-demand machinery.
  **Rejected on a fact, not a preference:** `list<Person>` over a user record is
  a core documented feature (list rendering, `LANGUAGE.md` § List Rendering). A
  closed set cannot cover types the stdlib has never heard of.
- **On-demand, memoized.** When checking a use at a type not yet instantiated,
  substitute and instantiate, keyed by (template, concrete type args). This is
  the answer. It is a monomorphization pass — well-understood, and it must be
  argued to terminate.

**It terminates.** Divergence in monomorphization comes from polymorphic
recursion (`f<T>` calling `f<list<T>>`), which requires writing a type parameter
in a nested position — and the surface has no type-parameter syntax at all, since
§3 is internal-only. The set of reachable instantiations is bounded by the
concrete types appearing in the program. Write that argument down when the pass
is built; do not leave it as folklore.

Note what this still requires: resolving `items.len()` to `$len_list_Person`
means picking an instantiation from the argument types at the call site. That is
type-directed *instantiation*, and it is unavoidable — but it is the easy half of
generics. The hard half (generalization: inferring that an unannotated function
*is* polymorphic) never happens, because only the stdlib is parameterized and it
declares its parameters explicitly.

### Ad-hoc overloading falls out for free

`len` is `list<T> -> s32` **and** `string -> s32` — that is overloading, not
parametrization, and a real generics feature would need a separate mechanism
(traits/typeclasses) or a special case. Here both are just entries in a set of
monomorphic names:

```
$len_string          hand-written in stdlib source
$len_list_s32        generated
$len_list_Person     generated
```

Resolution is one lookup keyed by (name, concrete argument types) — which is what
a monomorphic compiler already does. One mechanism covers both, and
[B4](anti-spec.md#b4--no-special-cased-control-flow-where-a-general-form-exists)
is satisfied rather than dodged.

### Constraints that are easy to get wrong

- **A mangled name must never reach a rendered diagnostic.** An error in
  `$filter_list_Person` reports as `filter`, at the user's span. Diagnostics are
  frozen keep-list infrastructure; leaking the internal name is a regression in
  diagnostic *meaning*, not a cosmetic one. Worth a test, not a convention.
- **Instantiation order must be deterministic.** The memo table is a hash map and
  the generated `DefId`s reach WIT/DOT output. Sort before emission —
  [A6](anti-spec.md#a6--no-random-seeded-iteration-reaching-output),
  [keep-list](keep-list.md) determinism.
- **Code size duplication is a non-issue at this scale** (a stdlib over a handful
  of types) and should not be pre-optimized.

### Open questions

- **Do user-defined records get parameterized too, or only the stdlib?** Only the
  stdlib is the smaller commitment and covers the motivating case. `record Box<T>`
  is a language feature and a separate decision — but check the mangling scheme
  does not foreclose it ([B4](anti-spec.md#b4--no-special-cased-control-flow-where-a-general-form-exists),
  and the rewrite must not foreclose features it does not implement).
- **Where does the pass run — HIR or THIR?** Instantiation is type-directed, so
  it wants THIR; but it generates *definitions*, which HIR owns. Likely a loop
  between them, which is a shape worth designing rather than discovering.
- **Does `option<T>`/`result<T,E>` stop being `InternedTyKind` variants?** They
  are built-in kinds today. If `list`/`option` become parameterized stdlib items,
  `List(Ty)`/`Option(Ty)` may be redundant with `Adt(DefId)` — unifying them is
  attractive and would change output, so it is its own decision with its own
  divergence set.
- **What is `T` spelled as in stdlib source?** The user-facing surface never sees
  it, so this is a stdlib-dialect question only, and it should be settled with §2's
  spelling question (`primitive` vs `intrinsic`) in one pass.

---

## 4 · Closures are a value, and the new IRs are shaped for one

**Status: the *design obligation* is not optional; the *implementation* is a
separate call.** Decided by stage 3 (representation) and stage 4a (whether LIR
gets a function value).

### Read the frozen tree as evidence, not as a constraint

The frozen compiler does not support closures-as-values. That is a fact about
**what output must be matched**, and it is nearly vacuous here — see below. It is
not a fact about what the new IRs may contain. This entry exists because that
distinction is easy to lose: every finding about the old compiler arrives
phrased as a limitation, and a limitation reads like a requirement.

### What the frozen tree actually does

Closures are in the surface — `LANGUAGE.md` § Closures documents
`{ x: s32 -> x + 1 }`, inferred parameter types, and statement bodies. They are
**non-first-class**, supported in exactly one position:

| stage | state |
|---|---|
| typeck | only checks in `Mode::Check` against an expected `Func` type; `Mode::Infer` errors *"cannot infer the type of this closure without an expected type"*. The only producer of that expected type is a hardcoded `filter` arm |
| THIR | `ThirClosure.captures: Vec<LocalId>` — always `vec![]`, `// TODO: capture analysis`, both sites |
| LIR | `LirExprKind::Closure { params, body }` — **no captures field exists**; the empty vec has nowhere to go |
| codegen | `LirExprKind::Closure` returns an error. The sole handler opens `if func_name != "filter" \|\| args.len() != 2 { return; }` — a **builtin matched by string in the back end** |

`FilterCallEntry` is `(comp_idx, elem_ty, elem_size, (LocalId, Ty), LirExpr)`:
one parameter, one predicate expression, **no environment**. The predicate is not
a closure at codegen time; it is an expression inlined into a generated function.

Reads of component state inside a predicate work anyway, by a different route —
`SignalRead` resolves through `$self`'s `$Comp` struct "or a filter-captured WASM
param" (`lir/expr.rs:94`). That is why `for_filter_over_signal.yel` passes while
capture in general does not.

### The measured behaviour: it is a panic, not a semantics

A predicate capturing an enclosing **local** (not a signal):

```
not yet implemented: Local not found in captured locals or local_to_slot: Local(LocalId(2))
  wasm/expr.rs:192, in generate_filter_function (codegen/record_list.rs:463)
```

Three consequences, and they are the whole point of this entry:

1. **There is no differential constraint.** The corpus is 2000/2000 compiling, so
   no corpus program does this. There is no output to match and nothing to
   diverge from.
2. **The precedent is already recorded.** From the stacker decision in
   [`seam-changes.md`](seam-changes.md): *"A crash is neither acceptance nor
   rejection, and rejecting cleanly where the frozen compiler aborts is an
   improvement, not a narrowing."* That applies here verbatim — and so does its
   stronger form: implementing cleanly where the frozen compiler panics is also
   not a divergence.
3. **The `todo!` itself is correct and stays loud** if the case remains
   unimplemented — [A5](anti-spec.md#a5--no-silent-fallback), and stage 4b's
   standing instruction not to soften `todo!()` cliffs into fallbacks.

### The obligation: model it, do not implement it

[`scope.md`](scope.md) already says closures/capture analysis are out of scope
and that the rewrite **must not foreclose** them. Stage 3's gap table already
carries the pattern for `match`: *"model the general form now so lowering has one
path."* Closures get the same treatment. Concretely:

- **THIR.** A closure has a type and a capture set. If `captures` exists it is
  computed and consumed, or it does not exist — a field that is always empty is
  [A9](anti-spec.md#a9--a-ported-construct-is-load-bearing-or-it-is-deleted).
  There is a way to make it load-bearing immediately: let the capture set derive
  the generated filter function's signature instead of hardcoding one parameter
  plus `$Comp`. Same output for the signal-capture case that works today; a
  diagnostic instead of a panic for the case that does not.
- **LIR (4a).** Whether a function value exists is a stage-4a design question and
  it should be answered on **generic** grounds — the flow frontend wants callable
  values too. A closure representation admitted for `filter`'s sake is UI
  vocabulary below the seam ([C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam));
  a general function value that `filter` happens to use is substrate.
- **Codegen (5).** `func_name != "filter"` does not survive under any option.
  A back end recognising a frontend builtin by string is C1 outright. Lowering
  emits a generic op; codegen transcribes it.

None of the above emits a single new instruction, so none of it changes output.

### What is *not* settled here

Whether to implement capture at all. The evidence above lowers the estimated cost
— no output to match, no language change (`LANGUAGE.md` already documents
capturing closures), and a panic rather than a semantics on the other side — but
"cheaper than it looked" is not authorisation. `scope.md` lists this as out of
scope, and moving it is the integrator's call with an entry in
[`seam-changes.md`](seam-changes.md), not a stage agent's.

The distinction to hold: **the design obligation is unconditional; the
implementation is a scope decision.** Shaping the IRs so capture *can* land costs
nothing and is required by scope.md's own no-foreclosure clause. Landing it is a
separate, enumerated change with its own fixtures.

---

## 5 · Handlers and closures are one concept, split by trigger

**Status: wanted, and the only entry here that changes no output.** Pure internal
restructuring — same blocks emitted, so it is differentially verifiable end to
end. Decided by stage 3 (the node) and stage 4b (the single lowering).

### Three mechanisms doing one thing

The frozen tree has three separate implementations of "a body of statements,
deferred, evaluated later in a captured environment":

| | node | deferred as | capture strategy |
|---|---|---|---|
| event handler | `ThirHandler { name, param: Option<LocalId>, body }` | `DeferredHandlerBody` | **six env-snapshot fields** |
| filter predicate | `ThirExprKind::Closure` | inlined by `generate_filter_function` | one hardcoded param + `$Comp` |
| derived signal | — | `DeferredDerivedBody` | an interned `LirExprId` |

`DeferredHandlerBody` snapshots `local_bindings`, `outer_item_field_slots`,
`for_stack`, `for_iter_body_stack`, and `for_item_iter_body` so that "loop-var
locals, outer for-item slots, and enclosing boundary refs resolve identically to
today's inline lowering." **That is capture analysis**, performed at the LIR
layer, because THIR declined to do it — and its result is exactly what
`ThirClosure.captures` is supposed to hold ([§4](#4--closures-are-a-value-and-the-new-irs-are-shaped-for-one)).

This is [B4](anti-spec.md#b4--no-special-cased-control-flow-where-a-general-form-exists)
(a special case where a general form exists), [A3](anti-spec.md#a3--no-duplicated-walkers)
(three walkers over one shape), and part of why `blocks.rs` is 8,500 lines
([A2](anti-spec.md#a2--no-god-pass)).

### The surface already treats them as one thing

Two facts from the frozen compiler, both checked:

- `Child { bumped: { count += 1; } }` **typechecks** against `bumped: func()`.
  A handler-shaped block is already a value of function type — the type system
  does not believe in the split.
- `on-change: func(value: s32) -> string` — a "handler" with a parameter *and* a
  return type. `drop: (payload) { … }` is a handler with a param;
  `{ v -> v > threshold }` is a closure with a param. Two spellings for one
  construct.

Structurally the two nodes differ only in degree: a handler is a closure with
0-or-1 parameters and no trailing expression. `LANGUAGE.md` already defines a
closure body as "statements followed by an optional trailing expression" — drop
the trailing expression and you have a handler.

(Note `ThirHandler.name: String` — a raw `String`, not an interned `Name`.
Whatever happens to the merge, that does not survive
[keep-list §3](keep-list.md).)

### The axis that actually matters is the trigger, not the shape

Do not merge them into "everything is a closure." The real distinction is
**when the body runs and what its reads mean**:

| trigger | reads register as dependencies? | re-runs when? |
|---|---|---|
| **reactive** — filter predicate, derived signal | **yes** | any dependency changes |
| **event** — handler | **no** | dispatched |

The frozen tree states this itself, in `blocks.rs`: *"Closures capture state from
the enclosing component — walk their captured signals to the outer iterable's dep
set."* A predicate's reads join the dependency set; a handler's reads must not,
or every handler that reads `count` re-runs on every write to `count`.

So the unified concept is **one node — parameters, body, capture set, trigger —
with one capture analysis and one lowering to a block.** The trigger decides
whether the capture set feeds dependency registration. That is a field, not a
second node kind.

### Two things that must survive the merge

1. **The capture set is not `Vec<LocalId>`.** `DeferredHandlerBody`'s six fields
   are the honest measure of what a body can close over: plain locals, for-item
   field slots, the enclosing for stack, iter-body refs, boundary refs. A
   unified capture set that only carries `LocalId` is a **narrowing**, and it
   will present as for-loop handlers breaking. Enumerate what the six fields
   cover before designing the replacement —
   [A10](anti-spec.md#a10--an-allow-list-entry-is-characterized-by-evidence-about-the-other-implementation).
2. **Trigger discipline is a correctness property, not a convention.** Reads
   leaking from a handler into a dep set gives spurious re-renders; reads not
   reaching it from a predicate gives stale UI. Neither is caught by "it
   compiles". This wants a test that asserts the dependency set of a body of
   each trigger kind, not a comment.

### The hazard the current split hides by accident

Today a filter predicate *cannot* mutate, because predicates are `ThirClosure`
and only handler bodies carry assignments through a path that reaches signal
writes. Merge them and "a reactive body that writes a signal it depends on"
becomes expressible — which is an infinite loop: the write re-triggers the
filter, which re-runs the predicate, which writes.

Merging therefore needs an explicit answer, not an accident: either the trigger
kind carries a purity requirement (reactive bodies may not write), or there is a
cycle check. **Reject at typecheck with a diagnostic** — silently permitting it
is the kind of thing that shows up as a hang in an execution test
([A5](anti-spec.md#a5--no-silent-fallback)).

### Why this is the safest entry in this file

§1–§4 all change output or require a scope decision. This one does neither: the
same blocks are emitted from the same bodies, so the corpus, the 85 execution
tests, and the WIT/DOT goldens are all unchanged. It is verifiable by exactly
the differential the rewrite already runs.

### Open questions

- **Does the derived-signal body become the same node too?** It is the third
  mechanism, and it is reactive-triggered, so it should — but `DeferredDerivedBody`
  is "structurally trivial" and deferred for a different reason (trigger dispatch
  ordering, not environment). Confirm that reason survives the merge rather than
  assuming it folds in.
- **Where does the merged node live?** Handlers hang off `ThirNode` (UI
  vocabulary); closures are `ThirExprKind` (expressions). A merged node that is
  an expression is cleaner and matches the type system's existing belief — but
  check what that does to the node/expression split in the new THIR.
- **Does the trigger belong on the node or at the use site?** The same body text
  is reactive in `filtered: list<P> = people.filter({…})` and event-triggered in
  `clicked: {…}`. If the trigger is contextual rather than intrinsic, it is a
  property of the binding, not the body.

### How the trigger gets decided — two options, neither adopted

The open question just above ("on the node or at the use site?") has two concrete
answers. Recorded together because stage 3 has to pick one, and picking by
default is how a surface change gets made without anyone deciding to make one.

#### Option A — a keyword on the closure

Mark the trigger in the source: some spelling that says "this body's reads do not
join a dependency set."

**What it buys.** The trigger becomes a *syntactic* fact. No inference, no
classification pass, and a reader can see at the literal whether it is reactive.
It also covers the one case a type cannot: deliberately wanting a non-reactive
body in a reactive position, or the reverse.

**What it costs, and this is the blocking part.** The surface syntax is frozen
([`scope.md`](scope.md)). The two sub-cases are both unattractive:

- **Required keyword** — every program that writes `clicked: { count += 1; }`
  stops parsing. That is all 91 positive fixtures, the examples, and any corpus
  program with a handler. The differential dies, and with it the only correctness
  gate the rewrite has.
- **Optional keyword** — existing programs keep working, but the unmarked case
  still needs the positional inference. The mechanism is not removed, only
  supplemented, so the cost is paid without the simplification being collected.

Adopting A therefore means an explicit, approved language change with its own
fixtures and a `goldens-changed.md` entry — not something a stage decides while
implementing something else.

#### Option B — the trigger lives in the slot's type

The trigger is not a property of the closure. The same literal `{ x -> x > 2 }`
is reactive flowing into `filter` and event-shaped bound to `clicked`. What
differs is **where it flows**, and the destination already has a declared type:
the frozen tree checks `Child { bumped: { count += 1; } }` against `bumped:
func()`.

So put the trigger on the function type — `filter` takes a reactive function, an
event slot takes an event function — and let bidirectional checking push it into
the literal. `typeck.rs` already dispatches on `Mode::{Infer, Check(Ty)}`; this
is the `Check` direction carrying one more bit.

**What it buys.** Zero surface change: every existing program, fixture and corpus
entry is untouched, so the differential stays alive. The trigger is still a
*declared* fact rather than a positional guess — it is declared on the slot
instead of the literal. One node with a trigger field, filled by checking rather
than by parsing. And it composes: a closure bound to a local and passed on
carries its trigger with its type, where a keyword would be lost at the binding.

**What it costs.** A function type gains a component, so type equality,
inference, and any function-typed WIT surface must all account for it. Whether it
reaches the WIT boundary needs checking — global callbacks are `func(...)`-typed
and cross it. If the trigger is internal-only, that must be *arranged*, not
assumed.

#### The test that either option owes

Unchanged by the choice, and stated here so it is not lost with the decision:
reads leaking from an event body into a dependency set gives spurious
re-renders; reads failing to reach one from a predicate gives stale UI. **Neither
is caught by "it compiles."** Whichever option is adopted asserts the dependency
set of a body of each trigger kind, on a fixture, in an execution test.
