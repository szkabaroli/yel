# Directions — recorded intent, not yet contract

> **A direction is not a mandate.** Nothing here constrains a briefed stage, and
> nothing here is verified by the ratchet. A direction becomes binding only when
> copied into a stage brief with a definition of done.
>
> **Where things live.** `scope.md` = what may change · `anti-spec.md` = shapes
> that may not be reproduced · `keep-list.md` = what carries over ·
> `findings.md` = measured facts · **this file** = shapes we want, and why.
>
> **Rules.** Append-only. Evidence is **cited** from `findings.md`, never
> restated. An adopted entry records which stage adopted it; a rejected one stays
> with the reasoning.

| § | Direction | Status | Decided by | Changes output? |
|---|---|---|---|---|
| [1](#1--builtins-are-a-table-not-a-field-per-builtin) | Builtins are a table, not a field per builtin | ✅ **adopted 2026-07-29** | `yelc-sema` (S1) | no |
| [2](#2--the-stdlib-is-yel-source-embedded-in-the-binary) | The stdlib is yel source, embedded in the binary | wanted | stage 2b+ | yes, enumerated |
| [3](#3--generics-are-monomorphization-by-name) | Generics are monomorphization by name | ✅ **adopted 2026-07-29** — by *type*, with a `Param` variant | 3b (A1, A3) | yes, enumerated |
| [4](#4--closures-are-a-value-and-the-irs-are-shaped-for-one) | Closures are a value; the IRs are shaped for one | **design obligation** | 2b + 3a | no (modelling only) |
| [5](#5--handlers-and-closures-are-one-concept-split-by-trigger) | Handlers and closures are one concept, split by trigger | wanted | 2b + 3b | **no** |
| [6](#6--modules-are-serializable-artifacts) | Modules are serializable artifacts | wanted | 2a/2b seam | no |
| [7](#7--keywords-get-a-word-boundary--at-cutover-by-deletion) | Keywords get a word boundary | **adopted 2026-07-28** | stage 1 | no — 8000/8000 corpus artifacts byte-identical |
| [8](#8--the-reactive-plan-is-an-artifact-and-its-shape-is-open) | The reactive plan is an artifact — and its shape is open | wanted; shape **undecided** | 2b emits, 3b consumes | depends on the shape |

**Dependency order:** §1 → §2. §3 unblocks most of §2. §4 blocks the rest of §2.
§5 and §6 are independent. §6 is why HIR and THIR merged into one stage
([`seam-changes.md`](seam-changes.md)).

---

## 1 · Builtins are a table, not a field per builtin

| | |
|---|---|
| **Status** | ✅ **adopted 2026-07-29** ([C1](open-decisions.md#c1--how-are-builtins-registered)) — recorded as [S1](infra-sema.md) |
| **Home** | `yelc-sema`; read by 3b and 4b |
| **Changes output** | no |

**Decision.** One table keyed by a stable name, whose row carries everything
downstream needs: `name → { arity, type scheme, lowering target }`. Typeck asks
the table for the type scheme; 3b asks the same row for the lowering target.
Adding a builtin is one row; an unread row is visibly dead
([A9](anti-spec.md#a9--a-ported-construct-is-load-bearing-or-it-is-deleted)).
An unknown name is a hard error at registration.

**Why.**
- The frozen shape spreads one builtin across four places that must agree and are
  checked by nothing — [F12](findings.md#f12), anti-spec A8.
- **Reference: Grain's `primitive`.** `provide primitive (+) = "@wasm.add_int32"`
  — no type annotation, no body. The string keys `prim_map` in
  `compiler/src/typed/translprim.re`, and `transl_prim` (a) looks it up, a miss
  being a hard error, (b) *synthesizes the type* from an arity tag, (c)
  eta-expands a body so the binding is a first-class value while direct
  application still inlines. Two categories share one table: instruction-shaped
  (`@wasm.*`) and compiler-stub-shaped (`@allocate.tuple`, `@throw`).

**Not carried.** Grain's untagged `WasmI32`/`@unsafe` layer and its `foreign` FFI
— both exist because Grain implements its own allocator in linear memory. Yel is
WASM-GC and AOT with no runtime crate; its import surface is the frozen
`yel:ui/dom@0.1.0` world (`dom_imports.rs` stays as-is).

**Stage-1-neutral.** No keyword, no token kind, no grammar change, no seam
change. Builtin registration is already in scope.md's *free* column. The
surface-syntax version is [§2](#2--the-stdlib-is-yel-source-embedded-in-the-binary),
tracked separately so adopting the table cannot smuggle a grammar change with it.

**Open.**
- One table with two projections, or two tables with a key-alignment test? The
  second is honest about the seam and pays for it with a check.
- Do builtin *elements* belong in it? `KnownElements` is 15 fields of UI
  vocabulary with no "lowering target" in the same sense — forcing one table over
  both may be a tidiness trap.
- **Variadics.** `concat` is registered with an empty param list and a comment
  saying it is really variadic. A table with declared arity must answer this.

---

## 2 · The stdlib is yel source, embedded in the binary

| | |
|---|---|
| **Status** | wanted; blocked on §3 and §4 |
| **Depends on** | §1 (the floor) |
| **Changes output** | yes — builtins gain spans, real IR, real code |

**Decision.** The stdlib is written in `.yel` and **embedded into the binary at
Rust compile time** (`include_str!`/`include_dir!`). It bottoms out in §1's
table: the few things not expressible in yel are `primitive` declarations whose
type and body the compiler supplies. Grain's layering exactly — `wasmi32.gr` is
nothing but `primitive` lines; `list.gr` above it is plain Grain.

**Why embedded, not a shipped directory.** Grain ships its stdlib as an npm
package whose entire `index.js` is `module.exports = __dirname` — the library is
a *path*. Do not copy that: a filesystem-resolved stdlib makes output depend on
what is installed (a determinism hole no sorting closes), and the corpus is only
an oracle if both compilers see the same library.

**The cost** is a parse + resolve + check per invocation. [§6](#6--modules-are-serializable-artifacts)
is the answer, and it is a general mechanism rather than a stdlib cache.

**Grammar consequence.** `primitive` is a new top-level item form, so this
**violates scope.md's "nothing new parses that did not before"** — deliberately.
It holds the other clause (purely additive: everything that parsed still parses).
Needs an approved scope move and a `seam-changes.md` entry. Two ways to spend it,
neither chosen: add the form to the language (simplest parser, largest surface
commitment), or a stdlib-only dialect bit à la Grain's `@runtimeMode` (language
stays frozen, cost is two grammars in one parser).

**What can actually move to source.**

| tier | builtins | needs |
|---|---|---|
| A — writable now | `min`, `max` | nothing |
| B — declared in source, implemented by the table | `concat`, `starts-with`, all `*-to-string` | §1 |
| C — blocked on parametrization | `len`, `some`/`none`, `list.get`, `append` | [§3](#3--generics-are-monomorphization-by-name) |
| C′ — blocked on parametrization **and** closures | `filter` | §3 **and** [§4](#4--closures-are-a-value-and-the-irs-are-shaped-for-one) |
| D — probably never source | builtin elements with attribute schemas, `yel:ui/dom` imports | a declaration form / a `foreign` analogue |

### What the stdlib must provide, not just what can move into it

The tier table above asks *"which existing builtins can be written in yel?"* That
is only half the question. The other half: **which types must the stdlib provide
because the compiler wants to desugar into them?**

| the compiler wants to desugar | into | exists today? |
|---|---|---|
| `0..10`, `0..=10` | a `Range { start, end, inclusive }` value | **no** — `Range` is carried as a dedicated node by all four IRs ([F18](findings.md#f18)) |
| `#ff0000` | `Color.rgba((r,g,b,a))` | yes — the mechanism already works |
| `"a {x} b"` | `concat(…)` | yes |

`Range` is the live one, and it reverses the dependency: an earlier draft of
[3a's candidate list](stage-2a-hir-build.md#candidates-and-what-blocks-each)
called the range desugaring *"blocked on §2"*, which treats the stdlib as an
obstacle. It is planned work. **So the desugaring is a requirement on §2's
contents, and `Range` belongs on the list of what the stdlib ships.**

Two consequences:

- **§1's table must cover types the compiler names**, not only functions the user
  calls. The frozen tree already does this for `Color` — the literal desugaring
  needs the `Color` def — so the mechanism exists; the question is whether the
  new table keeps it ([C2](open-decisions.md#c2--what-happens-to-builtin-elements-enums-and-variants)).
- **`Range` is probably not generic.** `LANGUAGE.md` uses ranges over integers
  with `for` and list operations, so it is tier A/B, not the tier C that waits on
  [§3](#3--generics-are-monomorphization-by-name). It does not inherit that
  blocker.

Generalised, because it will recur: **when the compiler desugars, it desugars
*into* something, and that something is a stdlib design requirement.** Collect
those requirements as desugarings are decided rather than discovering them when
the stdlib is written.

**No module system, and this must not invent one.** There is no `import`/`use`
and no `Import` item kind. The stdlib is an **implicit prelude** — in scope
everywhere, no import syntax, preserving current behaviour exactly. Grain's
`pervasives.gr` is this, and its `@noPervasives` exists to break the resulting
cycle. If a source stdlib later wants multiple files referencing each other,
that is a module system and a separate decision.

**Open.** Who compiles the embedded source — `yelc-sema` or `yelc-driver`? ·
spelling (`primitive` vs `intrinsic`, settle with §3's parameter spelling in one
pass) · do stdlib diagnostics ever reach users (a prelude type error is a
compiler bug and needs distinct presentation, or a CI assertion instead of a
per-invocation check)?

---

## 3 · Generics are monomorphization by name

| | |
|---|---|
| **Status** | ✅ **adopted 2026-07-29** — monomorphization **by type**, and with a `Param` variant ([A1](open-decisions.md#a1--how-are-parameterized-types-represented), [A3](open-decisions.md#a3--does-ty-get-a-param-variant)) |
| **Decided by** | 3b |
| **Changes output** | yes — unblocks tier C of §2 |

> **One thing this entry argued that the decision reversed.** §3 was written
> assuming *no* type variables — templates carried as syntax, interned only once
> concrete. [A3](open-decisions.md#a3--does-ty-get-a-param-variant) chose a
> `Param` variant instead, which means a generic body is **checked once,
> generically** rather than at each instantiation. That is Rust's arrangement,
> not C++'s, and it removes the error-message cost this entry accepted. The
> monomorphization half is unchanged; see
> [S7](infra-sema.md#s7--does-ty-gain-a-non-concrete-variant).

**Decision.** No type variables, no unification, no generalization. A
parameterized item is a template instantiated per concrete type it is used at:
`list<T>` → internal `$list_s32`, `$list_Person`. Each instantiation is an
ordinary monomorphic definition — exactly what `register_function` already
produces. **Internal only**: `list<s32>` stays the surface, the mangled name is
never parsed and never rendered.

**Scope of the claim.** This removes the need for a **type parameter** variant
(`Param`) — the `T` in `list<T>` — *if* templates are carried as syntax and
interned only once concrete. It says nothing about **inference variables**
(`Infer`), which are a separate question owned by 2b's function-type-inference
gap. See [`infra-sema.md` S7](infra-sema.md#s7--does-ty-gain-a-non-concrete-variant);
conflating the two is a mistake this file made once already.

**Why it fits.** There are no type variables to remove — [F1](findings.md#f1).
The choice is not "real generics vs. a compromise" but:

| | cost |
|---|---|
| real generics | add `TyVar`, substitution, unification, generalization, **plus a polymorphic-representation decision at the LIR seam** — stages 2b *and* 4 |
| monomorphization by name | a substitution pass + a memo table; `Ty`, the IR, LIR and codegen unchanged |

It also matches the target: on WASM-GC you want a concrete struct type per
instantiation. A uniform polymorphic representation would force boxing at exactly
the seam [C2](anti-spec.md#c2--one-representation-chosen-at-the-seam) keeps
single.

### Code size — the argument against, and the middle path

For a web target this is a first-class constraint, not a footnote. An earlier
draft of this entry dismissed duplication as "a non-issue at this scale"; that
was too glib. Three things bound the cost, and one option remains open.

1. **The generic surface is small** — about five stdlib functions (`len`,
   `filter`, `some`/`none`, `list.get`, `append`) with tiny bodies, over perhaps
   5–15 concrete types in a real application.
2. **The baseline is already worse than what §3 proposes.**
   [F15](findings.md#f15): the frozen compiler monomorphizes `filter` per **call
   site** — two identical filters over one type emit four symbols. Per-*type*
   instantiation is a reduction, not an increase.
3. **Monomorphization feeds the optimizer that already runs.** Release mode is
   `--gufa --type-merging -O3 --converge -Oz --closed-world`. A specialized
   instantiation under a closed world inlines and constant-folds; a *generic*
   body that must handle any type stays general because it has to. **Erasure
   ships one copy that handles cases the program never uses — it fights
   dead-code elimination rather than feeding it.**

**The middle path, if measurement says the cost is real: GC-shape stenciling**
(Go 1.18's approach). Instantiate once per *memory shape* rather than per type.
On WASM-GC the shape partition is tiny — roughly `{i32, i64, f32, f64, ref}` — so
`list<Person>` and `list<Address>` share one `$len_list_ref`, and every generic
function is bounded at ~5 copies **regardless of how many user types exist**.

> **This is not a fork in the road.** Stenciling *is* monomorphization with a
> coarser instantiation key: `(template, concrete args)` becomes
> `(template, shapes(args))`. Same memo table, same pass, different key function.
> Adopting §3 does not foreclose it, and coarsening later is a local change —
> which is the main reason §3 is safe to adopt before the measurement exists.

Cost if adopted: a second concept (shape ≠ type), and the mangling key becomes
the shape, which interacts with [`infra-sema.md` S6](infra-sema.md#s6--overloadkey).

**The measurement that settles it:** build one generic at two instantiations with
the same GC shape and check whether `--gufa --type-merging` already merges them.
If it does, stenciling is redundant and the plain key is correct.

**The two closed alternatives**, for completeness: *erasure/boxing* reintroduces
a second value representation ([C2](anti-spec.md#c2--one-representation-chosen-at-the-seam))
and distributes cast code across every use site; *witness tables* (Swift's
answer) need function values, which
[§4](#4--closures-are-a-value-and-the-irs-are-shaped-for-one) ruled out and the
canonical ABI has no type for.

**Instantiation is on-demand and memoized** on `(template, concrete args)`.
The eager closed-set alternative is rejected on a fact, not taste: `list<Person>`
over a user record is a documented feature, and a fixed type list cannot cover
types the stdlib never heard of. **It terminates** — divergence needs polymorphic
recursion, which needs a type parameter in nested position, which the surface
cannot express since this is internal-only. *Write that argument down in the
pass; do not leave it as folklore.*

Type-directed *instantiation* at the call site remains, and is unavoidable. The
hard half — generalization, inferring that an unannotated function *is*
polymorphic — never happens, because only the stdlib is parameterized and it
declares its parameters explicitly.

**Ad-hoc overloading falls out free.** `len` is `list<T> -> s32` **and**
`string -> s32`. Real generics would need traits or a special case; here
`$len_string` (hand-written) and `$len_list_s32` (generated) are entries in one
set of monomorphic names, resolved by one lookup on (name, argument types).

**Constraints.**
- A mangled name must **never reach a rendered diagnostic** — that is a
  regression in diagnostic *meaning*, not cosmetics. Worth a test.
- Instantiation order must be deterministic; generated `DefId`s reach WIT/DOT
  ([A6](anti-spec.md#a6--no-random-seeded-iteration-reaching-output)).
- Naming: yel spells its primitives `s32`, not `i32` — mangle in yel's own
  vocabulary. `$` is already `$Comp`; an internal name never parsed needs no
  sigil.

**Open.** The reachable set is **fixed by the grammar** — [F2](findings.md#f2)
means §3 can only parameterize spellings the grammar already has. Extending to
`record Box<T>` needs type application in the surface: a scope move, not an
implementation detail. · Where does the pass run (instantiation is type-directed
so it wants 2b; it generates *definitions*, which 2a owns — likely a loop worth
designing rather than discovering)? · Do `option`/`result` stop being
`InternedTyKind` variants and become `Adt(DefId)`? Attractive, changes output,
its own decision.

---

## 4 · Closures are a value, and the IRs are shaped for one

| | |
|---|---|
| **Status** | the **design obligation** is not optional; the *implementation* is a separate scope call |
| **Decided by** | 2b (representation), 3a (whether LIR has a function value) |
| **Changes output** | no — modelling only |

**Read the frozen tree as evidence, not as a constraint.** Every finding about
the old compiler arrives phrased as a limitation, and a limitation reads like a
requirement. It is not one.

**What is there.** Closures are in the surface (`LANGUAGE.md` § Closures) and
**non-first-class**, supported in one position. Typeck only accepts a closure in
`Mode::Check` against an expected `Func` type, and the only producer of that type
is a hardcoded `filter` arm. `ThirClosure.captures` is always `vec![]`. LIR's
`Closure { params, body }` has **no captures field**. Codegen matches `filter` by
string — [F7](findings.md#f7).

**The behaviour is a panic, not a semantics** — [F6](findings.md#f6). Three
consequences:

1. **No differential constraint.** No corpus program does this; nothing to match.
2. **The precedent is already recorded.** From the stacker decision in
   [`seam-changes.md`](seam-changes.md): *"A crash is neither acceptance nor
   rejection, and rejecting cleanly where the frozen compiler aborts is an
   improvement, not a narrowing."* Its stronger form applies too — implementing
   cleanly where it panics is not a divergence either.
3. **The `todo!` stays loud** if the case remains unimplemented
   ([A5](anti-spec.md#a5--no-silent-fallback)).

**The obligation — model it, do not implement it.** `scope.md` already says
closures must not be *foreclosed*, and stage 2b carries the same pattern for
`match` (*"model the general form now so lowering has one path"*). None of the
below emits an instruction:

- **IR.** Either `captures` is computed and consumed, or it does not exist — an
  always-empty field is [A9](anti-spec.md#a9--a-ported-construct-is-load-bearing-or-it-is-deleted).
  There is a way to make it load-bearing immediately: let the capture set derive
  the generated filter function's signature instead of hardcoding one parameter
  plus `$Comp`. Same output where it works today; a **diagnostic instead of a
  panic** where it does not.
- **LIR (3a).** Whether a function value exists is answered on **generic**
  grounds — the flow frontend wants callable values too. A closure representation
  admitted for `filter`'s sake is UI vocabulary below the seam
  ([C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam)); a
  general function value that `filter` happens to use is substrate.
- **Codegen.** `func_name != "filter"` does not survive under any option.

**No funcrefs.** Every function-valued position in yel resolves statically:
`filter` predicates are direct arguments; host callbacks are WIT import indices;
parent→child wiring happens at a known instantiation site
([F8](findings.md#f8)). And the **component-model canonical ABI has no
function-reference type**, so funcrefs could only ever be internal — where
nothing needs them. Defunctionalize instead: the generated function's parameters
are `(element, ...captures)`, which is the same work as fixing [F6](findings.md#f6).

*What would flip it:* a closure stored in a field or list, returned, selected at
runtime then called, a higher-order stdlib function with an unknown callee, or
the flow frontend needing callable node references.

*Anti-foreclosure requirement:* the frozen `Call { func: DefId, args }` bakes
"callee is a known definition" into the op and every consumer reads the `DefId`
directly. Resolve the callee through **one** place in codegen so an indirect case
is one edit — not by adding a dead `Indirect` variant, which is A9 in a costume.

**Not settled: whether to implement capture at all.** The evidence lowers the
cost — no output to match, no language change (`LANGUAGE.md` documents capturing
closures), a panic rather than a semantics on the other side. "Cheaper than it
looked" is not authorisation; `scope.md` lists this out of scope and moving it is
an integrator decision.

---

## 5 · Handlers and closures are one concept, split by trigger

| | |
|---|---|
| **Status** | wanted; **the only entry that changes no output** |
| **Decided by** | 2b (the node), 3b (the single lowering) |
| **Changes output** | **no** — same blocks from the same bodies |

**Decision.** One node — parameters, body, capture set, **trigger** — with one
capture analysis and one lowering to a block. The trigger is a *field*, not a
second node kind.

**Why.** Three mechanisms implement one concept, and one of them is doing capture
analysis by hand at the wrong layer — [F9](findings.md#f9). The type system
already disbelieves the split: a handler-shaped block typechecks and compiles
against a `func()` prop — [F8](findings.md#f8). Structurally a handler is a
closure with 0-or-1 parameters and no trailing expression, which `LANGUAGE.md`'s
own closure definition already permits.

**The axis is the trigger, not the shape.** Do not merge into "everything is a
closure":

| trigger | reads register as dependencies? | runs when |
|---|---|---|
| **reactive** — predicate, derived signal | **yes** | a dependency changes |
| **event** — handler | **no** | dispatched |

`blocks.rs` states this itself: *"Closures capture state from the enclosing
component — walk their captured signals to the outer iterable's dep set."*

**Two things that must survive.**
1. **The capture set is not `Vec<LocalId>`.** [F9](findings.md#f9)'s six fields
   are the honest measure of what a body closes over. A set carrying only locals
   is a **narrowing** that presents as for-loop handlers breaking
   ([A10](anti-spec.md#a10--an-allow-list-entry-is-characterized-by-evidence-about-the-other-implementation)).
2. **Trigger discipline is a correctness property.** Reads leaking from an event
   body into a dep set gives spurious re-renders; reads failing to reach one from
   a predicate gives stale UI. **Neither is caught by "it compiles."** Assert the
   dependency set of a body of each trigger kind, on a fixture, in an execution
   test. *This test is owed regardless of how the trigger is determined.*

**The hazard the current split hides by accident.** Today a predicate cannot
mutate, because only handler bodies reach signal writes. Merged, "a reactive body
that writes a signal it depends on" becomes expressible — an infinite loop.
Needs an explicit answer: a purity requirement on the reactive trigger, or a
cycle check, **rejected at typecheck**. Silently permitting it surfaces as a hung
execution test ([A5](anti-spec.md#a5--no-silent-fallback)).

### How the trigger is determined — two options, neither adopted

Stage 2b must pick one. Picking by default is how a surface change gets made
without anyone deciding to make one.

**Option A — a keyword on the closure.** The trigger becomes a syntactic fact: no
inference, visible at the literal, and it covers the case a type cannot
(deliberately wanting a non-reactive body in a reactive position). **Blocking
cost:** the surface is frozen. *Required* → every `clicked: { … }` stops parsing,
taking all 91 fixtures and the differential with it. *Optional* → existing
programs work but the unmarked case still needs positional inference, so the
mechanism is supplemented rather than removed and the cost is paid without the
simplification being collected. Adopting A is an approved language change with
its own fixtures and a `goldens-changed.md` entry.

**Option B — the trigger lives in the slot's function type.** The same literal is
reactive flowing into `filter` and event-shaped bound to `clicked`; what differs
is *where it flows*, and the destination has a declared type. Put the trigger on
the function type and let bidirectional checking push it in — `Mode::Check`
carrying one more bit. **Buys:** zero surface change, so the differential stays
alive; the trigger stays *declared* (on the slot, not guessed positionally); and
it composes — a closure bound to a local and passed on carries its trigger with
its type, where a keyword would be lost at the binding. **Costs:** a function type
gains a component, so type equality, inference, and any function-typed WIT
surface must account for it. Global callbacks are `func(...)`-typed and cross the
boundary — if the trigger is internal-only that must be *arranged*, not assumed.

---

## 6 · Modules are serializable artifacts

| | |
|---|---|
| **Status** | wanted; **caused the 2+3 merge** ([`seam-changes.md`](seam-changes.md)) |
| **Decided by** | the 2a/2b seam |
| **Changes output** | no |

**Decision.** A compiled module is written to disk with its declarations resolved
and typed. A consumer **reads** it instead of re-parsing and re-checking —
skipping name resolution and typecheck for everything it imports. Precompiling
the stdlib is then an ordinary case rather than a special mechanism.

**The artifact is post-typecheck.** "Skip resolution *and* typecheck" means the
serialized form already contains types. Neither reference serializes its untyped
IR: Swift's `.swiftmodule` is type-checked AST + SIL; rustc's `.rmeta` is typed
definition metadata + MIR, and HIR is not in it.

**Why the IRs merged.** "Make HIR self-contained and typed" and "make HIR into
THIR" are the same sentence — so the artifact question is an IR-count question.
rustc's split is easy to misread as an argument for two IRs: its **lints run on
HIR**, and THIR is not a lint surface. That argues for HIR existing — which it
does, as the IR before the type map is filled — not for a second IR after it.
What yel lacks is THIR's actual job, `match` exhaustiveness. Yel **will** have
lints, and that cuts the same way: a syntactic lint wants the nodes with no
types, a type-aware lint wants the same nodes with types, and two phases over one
vocabulary give both while a lint gets written once.

> **The shape: one IR, two phases, types in a side table.** `types: NodeMap<Ty>`
> is empty after 2a and total after 2b. Serialization writes nodes plus that
> table. Satisfies [A3](anti-spec.md#a3--no-duplicated-walkers) (one walker) and
> [B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes)
> (types beside the node) at once.

**Design it in; the remaps are symptoms, not constraints.** An earlier draft
listed `DefId` and `Ty` remapping as constraints. They are what bolting
serialization onto a one-compilation design produces. The rule that removes them:

> **The serialized form is structural. It contains no index into a table the
> reader does not own.**

| | retrofit (a remap to forget) | designed in (nothing to forget) |
|---|---|---|
| `DefId` | ordinal into one arena; remap on load | carries its module; a cross-module reference serializes as a **name path** and resolves by lookup on load — *a lookup that fails loudly*, not an offset that is silently wrong |
| `Ty` | interner index; re-intern and remap | serialize the **structure**. Loading re-interns as a matter of course, because no handle was ever written. Two modules cannot disagree about `Ty(7)` because neither wrote `7`. |

**Swift does exactly this split**, and states it plainly
(`docs/Serialization.md`): *"Decl nodes may be **cross-references to other
modules**, while types are **always serialized with enough info to regenerate
them at load time**. Nodes are accessed by a file-unique 'DeclIDs' … and
'TypeIDs'; the two sets of IDs use separate numbering schemes."*

Inside a `.swiftmodule`, `DeclID`/`TypeID`/`IdentifierID` are **module-local
indices** into an offset table. Across modules, an `XREF` record stores a base
module ID plus a **path of name pieces** (`XREF_TYPE_PATH_PIECE`,
`XREF_VALUE_PATH_PIECE`, `XREF_EXTENSION_PATH_PIECE`,
`XREF_OPERATOR_OR_ACCESSOR_PATH_PIECE`, …) which the reader re-resolves by
lookup. A miss is a diagnosed cross-reference error.

**Two details worth stealing, both non-obvious:**

1. **`XREF_VALUE_PATH_PIECE` carries the *type*, not just the name** — because a
   name does not identify a decl under overloading. Yel has this problem
   already: [§3](#3--generics-are-monomorphization-by-name) keeps `len` as both
   `list<T> -> s32` and `string -> s32`, resolved by (name, argument types). A
   bare `(module, symbol)` reference **cannot name one of two `len`s**. The
   cross-module reference must carry the same discriminator the resolver uses.
2. **Path pieces carry a "private discriminator"** so a non-exported decl is
   still referenceable without colliding with a same-named one elsewhere. Yel's
   `export` keyword makes the same distinction and will need the same handling.

The one genuine constraint: **staleness is a hash** — of inputs *and* compiler
version. A stale module that loads successfully is worse than one that fails to.

**Swift's answer to cross-version modules is not to have one.** `.swiftmodule`
is version-locked — `SWIFTMODULE_VERSION_MINOR` is currently **1013**, under the
rule *"when the format changes IN ANY WAY, this number should be incremented"* —
and a mismatch refuses to load. Stability across compiler versions is a
*separate, textual* artifact (`.swiftinterface`: source-like, **re-typechecked**
by the consumer). Two artifacts, two jobs: the binary one is a cache and is
allowed to be brittle; the textual one is the contract.

That is a real answer to the versioning question below, and a cheap one for yel:
version-lock the binary module, refuse on mismatch, and recompile from source —
which is always available, because [§2](#2--the-stdlib-is-yel-source-embedded-in-the-binary)
embeds the stdlib source in the binary. Yel gets `.swiftinterface`'s guarantee
without a second format.

*General form, because it will recur:* **a property that is hard to add later is
usually cheap to assume from the start, and the frozen compiler's difficulties
are evidence about retrofitting, not about the property.**

### The format: serde + postcard, in a hand-written envelope

**Decided: postcard.** `serde` is already a workspace dependency and `yelc-base`
already uses it; `postcard` adds the codec.

**Why not something heavier.** Version-locking (above) removes schema evolution —
the main reason to reach for protobuf/capnproto/flatbuffers. Cross-language
readers are not a requirement; only `yelc` reads these. And zero-copy (`rkyv`)
should not be chosen before deserialization is *measured* to be the bottleneck:
its cost is **design** cost, because archived types shape the data structures
that use them. Adopting it early means the serializer dictates the IR — the same
inversion the [boundary relaxation](seam-changes.md) exists to catch. Revisit if
prelude load time shows up in a profile.

**Why postcard over its near neighbours.** `bincode` and `borsh` are both
defensible and both deterministic *given deterministic data* — which is our
problem, not the codec's. The separator is **wire-format stability independent of
dependency bumps**: postcard commits to a documented wire spec, so the bytes move
only when *our* schema moves. That matters because
[2b byte-compares the serialized module](stage-2b-hir-check.md#verification); with
a codec whose encoding can shift under a version bump, every artifact diff lights
up for a reason that has nothing to do with the compiler. (`borsh` is canonical
by design and would serve equally; the tiebreak was ecosystem familiarity, not
engineering.)

**Keep the codec behind one boundary** — `encode(&Module) -> Vec<u8>` /
`decode(&[u8]) -> Result<Module>`, imported by nothing else. Then this decision
is reversible in an afternoon and `rkyv` stays live if a profile ever demands it.

**One derive set, two encoders.** The same serde impls give a **text dump**
(RON/JSON) alongside the binary artifact, for one extra call. That is not a
nicety: [F14](findings.md#f14) leaves stage 2 without an artifact and pushes its
differential onto `Definitions`-table comparison — a readable dump makes that
comparison reviewable instead of a hexdump.

**The envelope is hand-written and tiny**, so Swift's index-block door stays open
without building lazy loading now:

```
magic "YELM" · format_version: u32 · input_hash: [u8; 32] · section_count: u32
section table:  (kind: u8, offset: u32, len: u32) × N
sections:       postcard payloads
```

Lazy loading later = read the table, decode one section. Version mismatch or hash
mismatch = refuse, recompile from source.

**Two traps that are format-adjacent and already armed.**

1. **`Ty` derives `Serialize` and is a `u32` index.** `pub struct Ty(pub u32)`
   with `#[derive(…, Serialize, Deserialize)]` (`types/interner.rs:13`) means a
   naive derive on anything containing a `Ty` writes **the handle** — precisely
   the bug this direction exists to prevent. The existing derive is a loaded gun:
   serialized positions need a wrapper or `serialize_with` that writes the
   *structure*. Anything that merely compiles here is wrong.
2. **Iteration order reaching bytes.** `clippy.toml` already states the rule —
   `FxHashMap` is seedless and therefore fine for *iteration*, but "where the
   iteration order itself reaches output, additionally sort by a stable key or
   use a `BTreeMap`." **Serialized bytes are output.** So a serialized map is
   sorted on write or is a `BTreeMap`; `HirMap`'s `FxHashMap` fields qualify.
   Cheaper still: `HirMap` is *derivable* from the nodes, so the honest question
   is whether it is serialized at all.

**What it buys beyond speed.** §2 becomes affordable · separate compilation stops
being impossible ([F4](findings.md#f4): "multi-file" currently means concatenating
files in an order that decides whether the program compiles) · **the merged stage
gets a diffable artifact where stage 2 had none** ([F14](findings.md#f14)).

**Open.** Do bodies ship, or only interfaces? Interface-only suffices to skip
resolution and typecheck, but a consumer still needs the implementation to emit
code — so either bodies ship or the module carries compiled LIR and the back end
links. Yel is AOT into a single component, which argues for shipping bodies. ·
Module identity: there is no `import` syntax, so a file cannot name a dependency;
an implicit prelude works, anything more is a language change. · What
discriminator does a cross-module reference carry so it can name one of two
overloads (see the `XREF_VALUE_PATH_PIECE` note above)? That is the same question
as §3's mangling scheme and should be settled once, for both.

~~Versioning and ABI across compiler versions.~~ **Answered:** version-lock the
binary module and recompile from source on mismatch, per Swift's split above.

---

## 7 · Keywords get a word boundary — at cutover, by deletion

> **Status: ADOPTED, 2026-07-28, during stage 1 — not at cutover.** Everything
> below the fold is the pre-adoption reasoning, kept because two of its three
> predictions were wrong and the record of *how* is worth more than a tidy entry.
>
> **What landed.** A keyword ends only where an identifier could not continue,
> in **both** compilers in one change, for the construct keywords `record enum
> variant element extern component global package export func callback if else
> for in key let set bind in-out out`. Not for `unit_suffix` (an ordered prefix
> match by design) and not for `primitive_type` (`s32x` did not move).
>
> **What it deleted**, all in `yelc-syntax`: `Parser::at_keyword_prefix`,
> `eat_keyword`, `assert_keyword`, the `Follow` enum, `starts_identifier`,
> `items.rs::keyword_prefix_of`, `item_keyword_prefix` and its `ITEM_KEYWORDS`
> table, `nodes.rs::next_starts_with_in`, `at_glued_else_if`,
> `condition_here_is_followed_by_a_block`, and the text-prefix halves of
> `after_export`, `global_property_direction` and `at_named_prop` — with all ~56
> call sites. In `yel-core`, `grammar.pest` gained 21 `GLUED_*` rules and 22
> predicates; **`syntax/parser.rs` was not touched**.
>
> **Where this entry was wrong, twice.**
>
> 1. **"The frozen half needs the pair-walking work first, and that is the
>    expensive part."** It does not. Both approaches this entry measured used the
>    boundary rule *positionally*. Used under a **negative predicate** —
>    `GLUED_RECORD = @{ "record" ~ IDENT_CONT }`, then
>    `record_decl = { !GLUED_RECORD ~ "record" ~ … }` — the atomic rule still
>    kills the implicit whitespace, but a predicate consumes nothing and **emits
>    no pair**, so the pair tree is unchanged and the 3.3k-line hand-written
>    parser needed no edit at all. The expensive part did not exist.
> 2. **"That also retires the `if`/element speculation."** It does not.
>    `try_parse`, `Speculation`, `failed_attempts` and `Checkpoint` all stay:
>    `if` followed directly by `{` still has two live readings that gluing had
>    nothing to do with — `if { a: 1 } { div {} }` is an if-node over a
>    record-literal condition, `if { span { "x" } }` is an element called `if`.
>    The ~150–190× amplification on nested glued `if`s does go, because the input
>    that produced it is now unambiguous. The three speculation probes were
>    re-pointed, not deleted.
>
>    (`split_token` and `partial_offset` staying, which this entry got right and
>    an earlier draft of it got wrong, is below.)
>
> **Why now rather than at cutover.** The blocking argument was that the
> differential goes blind. It was testable, and it was tested rather than
> reasoned about: the corpus was regenerated and **all 8000 artifacts came back
> byte-identical**, so the differential lost nothing. No allow-list entry was
> added. Full evidence and every moved assertion:
> [`goldens-changed.md`](goldens-changed.md).

**Original status: wanted, blocked on cutover phase 4 rather than on design.**
The mechanism is understood, the win is large, and the *only* reason not to do it
now is that it would blind the differential while the differential is still the
correctness gate.

### The shape

`grammar.pest` matches keywords as **bare string literals with no word
boundary**, so `"if"` matches the first two characters of `ifa` and `ifa { … }`
parses as `if a { … }`. Likewise `recordFoo` is a record named `Foo`,
`exportcomponent A` is an exported component, `forx in xs` is a `for` over `x`,
and `iflex { color: red }` is an element only because a `named_prop` is not a
`node`.

A tokenizing lexer does not naturally behave this way. `yelc-syntax`'s
`keyword_kind(word)` is called on a **complete** lexed word, so `ifa` is one
`IDENTIFIER` and only an exact `if` is `IF_KW`. Word boundaries are the default.

So stage 1 had to build machinery to *undo* its own lexer:
`at_keyword_prefix` / `eat_keyword` / `assert_keyword`, plus the `if`/element
speculation (`Parser::try_parse`), which exists because `ife {` has two live
readings and which carries a measured **~150–190× parse-time amplification** on
nested glued `if`s (3.19 s vs 17 ms at 16,000 levels).

### What survives, and why — do not over-claim this

`split_token` and `partial_offset` **stay.** `split_token` has two callers, and
only one is about keywords:

| caller | purpose | after a word boundary |
|---|---|---|
| `eat_keyword` (`parser.rs:545`) | keyword prefix — `recordFoo` → `record` + `Foo` | goes |
| `expect_type_close` (`parser.rs:627`) | takes the `>` out of a `>=`, so `list<s32>=1` closes the generic | **stays** |

The `>=` split is a *separate* consequence of pest being scannerless, and a
keyword boundary does nothing for it. So the cursor keeps `partial_offset`, and
`Checkpoint` keeps its field.

An earlier draft of this entry claimed the boundary deletes "all of it,
including `partial_offset` and its checkpoint field". That was wrong, and it is
exactly the kind of over-claim a deletion PR would act on before discovering
`list<s32>=1` had stopped parsing.

### Why not now, in both compilers

Two pest-specific obstacles, both hit and measured:

1. **A shared boundary rule does not work.** `"record" ~ WB ~ identifier`
   becomes `"record" ~ WHITESPACE* ~ WB ~ …`, because pest inserts implicit
   whitespace between every `~` in a non-atomic rule. The space is skipped
   *before* the boundary is tested, so it sees `F` and fails. `record Foo`
   stops parsing.
2. **Atomic keyword rules work, but change the pair tree.**
   `kw_record = @{ "record" ~ !(ALNUM | "_" | "-") }` is correct as a grammar —
   atomic suppresses the whitespace insertion. But an atomic rule **emits a
   pair**, so `record_decl`'s children become `[kw_record, identifier, …]` and
   the hand-written parser walking `into_inner()` finds a keyword where it
   expects a name. Everything rejects.

pest has no single modifier for *silent and atomic*: `_` suppresses the pair and
reintroduces the whitespace, `@` fixes the whitespace and emits the pair. Doing
it in the frozen tree therefore also means updating ~20 productions' pair-walking
in a 3.3k-line file, where a mistake is a silent misparse rather than a compile
error.

### Why not now, in the new parser only

Technically trivial — it is deletion. But it breaks the premise the whole rewrite
rests on: **the differential is only meaningful while both compilers accept the
same language.** Change one and every keyword-prefix input becomes a reported
divergence, and the harness can no longer tell an intended change from a
regression introduced beside it. The allow-list needed to cover the class
(`recordFoo`, `component8A`, `exportcomponent`, `forx`, `iflex`, `letx`, `keyx`,
…) would be large enough to stop being a ratchet — the escape-hatch shape
[A10](anti-spec.md#a10--an-allow-list-entry-is-characterized-by-evidence-about-the-other-implementation)
names, at scale.

There is also a product answer: the frozen compiler is what ships. If the two
disagree, the new one rejects source the shipping compiler accepts.

### Why cutover is the right moment

At phase 4 the frozen tree is deleted and there is **no oracle left to
preserve**. The change stops being a surface change fought against a differential
and becomes a deletion PR:

- drop the 56 `at_keyword_prefix`/`eat_keyword`/`split_token` call sites
- drop `partial_offset` from the cursor and its checkpoint field
- drop the `if`/element speculation, and with it the ~150–190× amplification
- the lexer's natural behaviour simply stands

Nothing is blocked by waiting: the machinery is built, tested, and correct
against the frozen grammar today. Waiting costs nothing and removes all the risk.

**Cross-reference:** [`stage-4-codegen.md` § Final deletion](stage-4-codegen.md#final-deletion--cutover-phase-4)
carries this as a checklist item so it is picked up rather than rediscovered.

### If it is ever wanted *before* cutover

It is a surface change and gets the same evidence the kebab lookahead got
([`goldens-changed.md`](goldens-changed.md)): apply it to **both** compilers,
regenerate the corpus, and require all 8000 artifacts byte-identical. Real yel
writes `if a {`, not `ifa {`, so that is a plausible outcome — but the frozen
half needs the pair-walking work above first, and that is the expensive part.

---

## 8 · The reactive plan is an artifact, and its shape is open

| | |
|---|---|
| **Status** | the *split* is wanted; the *granularity* is an open design question |
| **Decided by** | **3b** (`yelc-hir` check) emits the plan · **4b** (`yelc-lower`) chooses granularity |
| **Not touched by** | **4a** (`yelc-lir`) — it must never learn what a signal is ([C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam)) |
| **Changes output** | the split: no · the granularity choice: **yes** |

### Which stage does which

The two halves land in different stages, and the line is the same
analysis-vs-representation test that decides everything else here:

| | stage | why |
|---|---|---|
| **the plan** — what work runs when which signal changes | **3b**, as a declared output | it is derived from dep sets, triggers and captures, all of which 3b already has. Nothing about it needs the target. |
| **granularity** — how that work is packaged into functions | **4b** | how many WASM functions to emit, and how coarse, is a *representation* choice. 4b also knows the lowered body sizes, which is the input the decision actually needs. |

**Consequence for the plan's shape, and it is a correction to the first draft:**
the plan must carry **reactive units** — a body, its trigger, its dependency set —
**not function identities**. If 3b names functions, it has already made 4b's
decision. A unit is "this work runs when these signals change"; packaging units
into one function per site, one per component, or inlined at the write site is
then entirely 4b's call, and can even differ per component.

This also keeps 3b's output stable if the granularity choice is revisited later:
re-packaging is a 4b change with no frontend churn.

### The part that is settled: plan, then transcribe

After 2b the compiler knows the UI tree, every dependency set
(`thir/signalck.rs` already computes these), every body and its trigger
([§5](#5--handlers-and-closures-are-one-concept-split-by-trigger)), and every
capture set ([§4](#4--closures-are-a-value-and-the-irs-are-shaped-for-one)). From
those, **which functions exist and who calls whom is fully determined without
knowing anything about the target.**

So 2b emits a **plan** — function identities, their trigger, their dependencies,
which body each runs — and 3b emits the *bodies*. This is the pattern already
used once in this codebase (`410d874`: *"plan the module-start globals-init in
LIR; codegen only transcribes it"*), one layer up.

**Why it is worth doing.** `blocks.rs` is 8,500 lines with 50+ mutating fields
largely because it **discovers structure while emitting it**.
`pending_block_id_override` exists so a deferred body can reference a block
before it is emitted; [F9](findings.md#f9)'s six env-snapshot fields exist because
the walk that finds captures is the walk that emits. Both are symptoms of one
pass doing two jobs. Separate them and each half is small.

**Two constraints:**

- **The plan is part of the seam, not the context.** `signal_deps` lives on
  `CompilerContext` today (`context.rs:77`); a plan stashed there is
  [A1](anti-spec.md#a1--no-side-channel-ir) side-channel IR. As a declared output
  of 2b consumed by 3b it is just the contract. Feeds
  [S4](infra-sema.md#s4--what-stays-on-the-context).
- **Frontend ids only.** The plan cannot name `BlockId` — a `yelc-lir` type the
  frontend crates cannot reach. It carries its own identity and 3b maps it, the
  same shape as the `HirId ↔ NodeId` map.

### The part that is NOT settled: what decomposition the plan describes

**An earlier draft of this entry listed "effects, update functions, mount/unmount
functions, derived recomputes, handlers, predicates" as though that were a fact
about compiling reactivity. It is not — it is the frozen compiler's
decomposition, read off its own DOT output.** The rewrite is greenfield and does
not owe it.

**A second correction, larger.** That draft also had a table ranking three
strategies and claimed the frozen compiler used the worst of them (a Solid-style
runtime effect registry). **It does not** — [F16](findings.md#f16): dispatch is
already **fully static**, direct `CallBlock`s resolved at compile time, with no
runtime registry and no dirty mask. The DOT graph that suggested otherwise is a
*compile-time call graph*. The strategy that draft was building toward
recommending is the one already in use.

What that leaves is a real question, but a narrower and better-posed one, because
**dispatch and granularity are orthogonal** and the draft conflated them:

| axis | options | frozen |
|---|---|---|
| **dispatch** — how a write reaches its dependents | direct static calls · runtime registry · dirty-mask scan | **direct static calls** — and this is the best of the three for a closed-world AOT compiler. Not in question. |
| **granularity** — how coarse the called functions are | one function per reactive site · one per component · inlined at the write site | **one per site**, plus separate mount/unmount and an update-function layer |

**The open question is granularity, and it is a code-size question.** With direct
dispatch and per-site functions, a component with N reactive sites emits N
functions and each writer emits a call per dependent. The alternatives:

- **Coarser functions** — one `update(mask)` per component, called directly (still
  static dispatch). Fewer functions, branch tests inside. This is Svelte-3's
  *granularity* without its *dispatch*; the two are separable and the draft
  treated them as one choice.
- **Inline at the write site** — no function at all for a small body; duplicate it
  per writer. Smallest for one writer, worst for many.

Component sizes bear on this directly: measured across 83 fixture
components/globals, **max 14 reactive properties, median 2, p90 4**. At those
sizes there is very little for a mask to amortise over, which argues the frozen
granularity is closer to right than the draft assumed.

### Open

- **What granularity?** Genuinely undecided — but note dispatch is *not* in
  question ([F16](findings.md#f16)). Measurable: the 85 execution tests pin
  behaviour, so the comparison is module bytes and update cost, not correctness.
- **One granularity, or chosen per component?** Two reactive sites and fifty want
  different answers. A hybrid is possible, and it is how a plan-based design earns
  its keep — the plan is the natural place to make that choice per component.
- **Does mount/unmount need to be two functions**, or one parameterized by
  direction? Same question for the effect indirection: it may simply be
  removable.
- **This changes output**, so whichever strategy is chosen lands as its own
  enumerated divergence set. It is not a refactor.
