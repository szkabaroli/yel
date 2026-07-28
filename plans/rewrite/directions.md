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
| [1](#1--builtins-are-a-table-not-a-field-per-builtin) | Builtins are a table, not a field per builtin | open | stage 2b, 3b | no |
| [2](#2--the-stdlib-is-yel-source-embedded-in-the-binary) | The stdlib is yel source, embedded in the binary | wanted | stage 2b+ | yes, enumerated |
| [3](#3--generics-are-monomorphization-by-name) | Generics are monomorphization by name | wanted | stage 2b | yes, enumerated |
| [4](#4--closures-are-a-value-and-the-irs-are-shaped-for-one) | Closures are a value; the IRs are shaped for one | **design obligation** | 2b + 3a | no (modelling only) |
| [5](#5--handlers-and-closures-are-one-concept-split-by-trigger) | Handlers and closures are one concept, split by trigger | wanted | 2b + 3b | **no** |
| [6](#6--modules-are-serializable-artifacts) | Modules are serializable artifacts | wanted | 2a/2b seam | no |

**Dependency order:** §1 → §2. §3 unblocks most of §2. §4 blocks the rest of §2.
§5 and §6 are independent. §6 is why HIR and THIR merged into one stage
([`seam-changes.md`](seam-changes.md)).

---

## 1 · Builtins are a table, not a field per builtin

| | |
|---|---|
| **Status** | open, not scheduled |
| **Home** | `yelc-sema`; read by 2b and 3b |
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
| **Status** | wanted; the cheapest of the six |
| **Decided by** | stage 2b |
| **Changes output** | yes — unblocks tier C of §2 |

**Decision.** No type variables, no unification, no generalization. A
parameterized item is a template instantiated per concrete type it is used at:
`list<T>` → internal `$list_s32`, `$list_Person`. Each instantiation is an
ordinary monomorphic definition — exactly what `register_function` already
produces. **Internal only**: `list<s32>` stays the surface, the mangled name is
never parsed and never rendered.

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
| `DefId` | ordinal into one arena; remap on load | carries its module; a cross-module reference serializes as `(module, symbol)` and resolves on load — *a lookup that fails loudly*, not an offset that is silently wrong |
| `Ty` | interner index; re-intern and remap | serialize the **structure**. Loading re-interns as a matter of course, because no handle was ever written. Two modules cannot disagree about `Ty(7)` because neither wrote `7`. |

The one genuine constraint: **staleness is a hash** — of inputs *and* compiler
version. A stale module that loads successfully is worse than one that fails to.

*General form, because it will recur:* **a property that is hard to add later is
usually cheap to assume from the start, and the frozen compiler's difficulties
are evidence about retrofitting, not about the property.**

**What it buys beyond speed.** §2 becomes affordable · separate compilation stops
being impossible ([F4](findings.md#f4): "multi-file" currently means concatenating
files in an order that decides whether the program compiles) · **the merged stage
gets a diffable artifact where stage 2 had none** ([F14](findings.md#f14)).

**Open.** Do bodies ship, or only interfaces? Interface-only suffices to skip
resolution and typecheck, but a consumer still needs the implementation to emit
code — so either bodies ship or the module carries compiled LIR and the back end
links. Yel is AOT into a single component, which argues for shipping bodies. ·
Module identity: there is no `import` syntax, so a file cannot name a dependency;
an implicit prelude works, anything more is a language change. · Versioning and
ABI across compiler versions — the hash detects it, does not solve it.
