# Modules, imports, and the WIT boundary

**Status: design, nothing implemented.** Settled over a long design session on
2026-07-29; written down so the reasoning survives and so the several reversals
along the way are not re-argued.

Related: [`stdlib/DESIGN.md`](../stdlib/DESIGN.md) (the compiler↔stdlib
interface), [`plans/rewrite/scope.md`](rewrite/scope.md) (the surface-freeze
ledger), [`directions.md` §6](rewrite/directions.md) (serializable modules).

---

## 1 · The constructs, and what each is *for*

The whole design is one question asked repeatedly: **what is this construct's
one job?** Every reversal below came from a construct doing two.

| construct | holds | state | emits WIT |
|---|---|---|---|
| `component C { }` | UI tree + properties | **signals** | a `resource` |
| `global G { }` | properties + functions | **signals** | an `interface`, if `export` |
| `impl T { }` | a type's methods | none | **never** |
| `module M { }` | types + functions | none | an `interface`, if `export` |

**`global` is reactive state that happens to have a name** — not a namespace that
happens to hold state. That is why it survives: it is the only construct that
holds a signal outside a component. `export` decides whether the host sees it;
`global` decides that it is reactive. Two orthogonal things, correctly factored.

**`impl` never emits WIT**, because it cannot: WIT primitives carry no methods
and WIT records carry no methods. `string.len()` is internal by construction.
Consequently `export` on an `impl` member must be an **error** — there is nothing
to export it as, and silently accepting it is the `_ => {}` shape
([F20](rewrite/findings.md)).

**`module` is stateless**, which is exactly what a WIT interface is. That 1:1
correspondence is its entire justification — see §3.

---

## 2 · `impl`, not `extend`, and not `@impl`

`impl T { … }` declares a type's method set: one per type, in the module that
owns the type.

**Closed, not open.** Swift's `extension` is *open* — any module may add methods
to any type. That earns its keep in Swift through protocol extensions and
conditional conformance (`extension Array where Element: Comparable`), and **yel
has no traits or constraints**, so that entire class of use is unavailable. What
remains is "add a method to someone else's type", which is pleasant and not
load-bearing.

**Closed → open is additive; open → closed is a break.** So start closed. The
test is *does anything need it today* — the stdlib does not, since every type's
methods are declared once in the file that owns the type. (This is the same test
that was right about `while` and wrong about `return`; the difference is that
`return`'s evidence arrived, and extension's has not.)

**Name it for what it is.** Calling it `extend` while implementing closed
semantics promises openness the language does not have.

**Why not `@impl(type)` on a `global`** — considered and dropped. `global` means
*stateful singleton* in yel, so using it as a method namespace made it mean two
things, and it forced a rule for a question that should not exist (*"is `@impl` on
a global that also holds state legal?"*). As its own item form the question
disappears rather than needing an answer.

**Method calls need no new mechanism.** `x.f(a)` → `f(x, a)` is a desugar the
frozen tree already performs (`MethodCall → Call`, a recorded keeper). `impl`
supplies the *scope* the callee is looked up in; the desugar supplies the *call
shape*. Both, not either.

> **Reversal recorded.** An earlier step argued the desugar alone sufficed and
> `@impl` was unnecessary. That was wrong: the desugar resolves in a **flat**
> namespace, and the stdlib is not flat — `len` lives inside `impl string`.
> Flattening it would need a per-function registry agreeing with the source with
> nothing checking it, which is [F12](rewrite/findings.md)'s shape, which the
> builtin table exists to retire. Scoped lookup also disposes of the `len`
> overload: `String.len` and `List.len` stop being one overload set.
>
> Cost, stated: scoped lookup needs the receiver's type, so it is **stage 4**.
> The desugar alone would have been stage 3. Worth paying.

---

## 3 · `module` → WIT `interface`, 1:1

```yel
export module Math {
    record Point { x: f64, y: f64 }
    distance: func(a: Point, b: Point) -> f64 { … }
}
```
```wit
interface math {
    record point { x: f64, y: f64 }
    distance: func(a: point, b: point) -> f64;
}
```

No adapter, no synthesized accessors, no state to reconcile.

**Better than `global` → interface, which is what happens today.** A WIT
interface is stateless; a `global` is not. Mapping a stateful global onto a
stateless interface is an impedance mismatch handled by synthesizing getters and
setters. A module has no state to mismatch.

**It buys three things yel currently lacks:**

1. **The import direction.** `yel:ui/dom@0.1.0` is hardcoded in
   `dom_imports.rs`. Nothing lets a user declare a host interface. §4 fixes this
   and deletes the special case.
2. **A namespace for types in WIT.** Records, enums and variants are top-level in
   yel and land flat; WIT expects them inside an interface.
3. **The split of `global`'s two jobs.** `global` keeps reactive state; `module`
   takes the host boundary.

> **Reversal recorded.** An earlier step concluded `module` had no job — correct
> when evaluated as a *namespace*, since `impl` had taken type methods. Wrong
> frame: as a **WIT interface** it does something no other construct does, and
> `extern`/imported interfaces are something nothing does at all.

### No nesting, no globs

Grain allows `module Helpers { … }` nested, `provide { module Helpers }`, and
`use List.*`.

**Nesting is refused because WIT interfaces do not nest.** A WIT package contains
interfaces; an interface contains types and functions, not interfaces. Allowing
nesting means either mangling on export (`a-b`) — at which point the yel
structure and the WIT structure stop corresponding, losing the only argument for
`module` — or nested modules that cannot be exported, which is a second class of
module. Flat matches the target exactly, and relaxing later is additive.

**Globs are refused** because they make *"where did this name come from"*
unanswerable without whole-program knowledge, and yel has no traits to make them
necessary.

---

## 4 · Two levels: package and module

| | is | maps to | precompilable |
|---|---|---|---|
| **package** | a directory of files; **the compilation unit** | WIT package `ns:name@ver` | **yes** — [§6](rewrite/directions.md) |
| **module** | a namespace *within* a package | WIT `interface` | — |

`module M { … }` at the top level of any file declares an interface. Several
files, several modules, one package — and files in a package see each other with
no import ([D8](rewrite/stage-3-hir-build.md)).

**"Submodule" means a module within a package, not a module within a module.**
Nesting stays refused (§3).

This mirrors WIT and Go exactly. A WIT package is a **directory**: every `.wit`
file in it repeats the same `package ns:name@version;`, they merge into one
namespace, and dependencies live in `deps/`. Go's rule is the same — a package is
a directory, files in it see each other, `import` reaches only *other* packages.
Nested directories are **independent** packages in both: nesting is naming, not
scoping.

So yel did not merely pick a model compatible with WIT — [D8](rewrite/stage-3-hir-build.md)
independently arrived at WIT's own file rule. A yel package directory can *be* a
WIT package directory.

## 4.1 · Imports: the string is a **locator**, not a kind

**An `include` names a MODULE, not a package** — because that is what WIT's
import path names (`ns:pkg/iface@version`). One `include` per module actually
used, so the yel source and the world's import list correspond one-to-one.

```yel
from "yel:ui/dom@0.1.0"    include Dom;      // package yel:ui, interface dom
from "std:hash/sha256@1.0" include Sha256;   // package std:hash, module sha256
from "./ui.yel"            include MyUi;     // a yel package, by path
from "yelmodule"           include Module;   // a pre-compiled yel package

use Dom.{ create-element, set-attribute };
```

**Every specifier therefore carries the `/module` segment**, which removes an
ambiguity an earlier draft had to legislate against: there is no bare `ns:name`
form for a `ns:pkg/iface@ver` form to collide with. One uniform shape, and the
only thing distinguishing a WIT interface from a yel package is what the resolver
finds — which is the whole point of *locator, not kind*.

Locators, **two kinds of result**:

| result | effect | WIT |
|---|---|---|
| WIT interface | declarations only; the host implements | **adds an import to the world** |
| yel package | code, compiled in | none |

The distinction that matters falls out of *what was resolved*, not from saying it
twice in the syntax.

### `include` and `use` are separate because they differ at the boundary

| | effect | WIT |
|---|---|---|
| `from "…" include X` | adds a dependency | **an entry in the world's import list** |
| `use X.{ a, b }` | local scope sugar | **nothing** |

One is boundary-affecting and one is not. Keeping them distinct means a reader
can tell which lines change the emitted component. Collapsing them hides that.
This is a stronger reason than Grain's, which is organisational.

### The path locates; the `package` declaration identifies

**D8 says a yel module spans files** — two files in one package see each other
with no import. So a file-path import is only meaningful for a *different*
package, and `./ui.yel` must be read as *where to look*, with
`package my:ui@1.0` inside saying *what it is*. Then `from "my:ui@1.0" include
MyUi` is the same import through a different locator.

**State this explicitly**, or `from "./a.yel"` will be read as "file = unit" and
`./b.yel` in the same package already being visible will be a surprise.

Consequence, worth knowing: **imports become rare.** Cross-file within a package
needs none, so a typical app has perhaps one — for the DOM. That is a better fit
than Grain's model, where every file split costs an import line, and it is why
"one component per file" stays free.

### The specifier shapes are disjoint by construction

An earlier draft required the interface segment on WIT ids so `ns:name` and
`ns:pkg/iface@ver` could not be confused — distinguishing them by *presence of
`/`* would have been a lookahead rule of exactly the kind that has misparsed this
grammar twice ([`func<T>`](rewrite/seam-changes.md), `@children`).

**That rule is no longer needed.** Since an `include` names a module, every
specifier has the segment; there is no bare form. The ambiguity is designed out
rather than legislated against.

### What this retires

`extern module` is unnecessary: `from "./dom.wit" include Dom` says it without
transcribing the interface into yel — and hand-transcription is two artifacts
that must agree with nothing checking them, permanently, since the host's `.wit`
is the source of truth.

### Do not write a WIT parser

`wit-parser` from `wasm-tools` is the reference implementation, and `wasm-tools`
is already a dependency of the test flow. Writing our own is how the yel reading
of WIT drifts from the spec.

---

## 5 · Kept from Grain, and not

| | |
|---|---|
| ✅ `from "…" include X` | the locator form |
| ✅ `use X.{ a, b }` | scope sugar, no boundary effect |
| ❌ `provide` | keep `export` — it already means *appears in the emitted WIT*, which is sharper than Grain's, and it is what the fixtures and emitter use |
| ❌ `module Name` header | keep `package ns:name@version;` — it already does file identity and carries the version WIT needs |
| ❌ nested modules | WIT interfaces do not nest |
| ❌ `use List.*` | unanswerable provenance |

**Grain's compilation unit is the file** — one `.gr`, one module, one `.wasm`,
linked. Yel chose otherwise in D8, and the trade is real: Grain gets separate
compilation per file; yel's recompilation unit is the package. That is right
while an app is single-package, and §6 reintroduces granularity at package
boundaries rather than file boundaries.

---

## 6 · Naming collision to fix first

`ModuleId` in `yelc-sema` means the **serialization unit**. If `module` becomes a
surface keyword meaning *WIT interface*, three things want the word: the surface
construct, the serialization unit, and the WIT interface.

The mapping is *surface module → WIT interface* (1:1) and *package →
serialization unit* (holds several modules). So ~~rename `ModuleId` → `PackageId`~~ — **done 2026-07-29.**

**The compiler's current name describes the wrong level.** §6's decisions — B1
(`Ty` written structurally), B2 (module-qualified `DefId`), B3 (`OverloadKey` in
the `DefPath`) — were all made for *"a module is serializable"*. Under this
hierarchy the thing being serialized is a **package**: the compilation unit, and
the only level with a version to be compatible against. The decisions are right;
the noun is off by one level.

---

## 6.5 · Resolution: `deps/`, no manifest

**Decided: take WIT's model, not Go's.** A package is a directory; its
dependencies are *vendored* into `deps/`; there is **no manifest file and no
version resolver**.

```
my-app/
  app.yel            ← all declare `package my:app@1.0;`
  widgets.yel
  deps/
    yel-ui/*.wit     ← a WIT interface package
    std-hash/*.yel   ← a yel package, as source
    std-json/*.yelp  ← a yel package, precompiled (§6)
```

| | Go | WIT | **yel** |
|---|---|---|---|
| unit | directory | directory | directory |
| identity | `go.mod` declares it | the `package …@ver;` **in the source** | **in the source** |
| dependencies | `require` + module cache | **`deps/`, vendored** | **`deps/`, vendored** |
| version resolution | minimal version selection | **none** | **none** |

### Why WIT's and not Go's

**Yel already has the identity half.** Every file carries
`package ns:name@version;`. A directory knows what it is without a manifest —
which is the exact job `go.mod` exists to do, and yel does not need it done.

**Closed-world compilation does not benefit from a resolver.** The optimisation
story rests on `--gufa --closed-world`. A build-time version solver is machinery
in service of a flexibility that a single closed-world artifact cannot use.

**Zero new file formats.** `deps/` is a convention, not a schema: nothing to
design, nothing to version, nothing to parse. And it keeps the property that
motivated the whole model — **a yel package directory stays literally usable as a
WIT package directory**.

So dependency resolution needs **no new mechanism in yel at all**: a directory
convention and a walker. [§6](rewrite/directions.md) then reduces to *"a `deps/`
entry may be an artifact instead of source"*, which is a far smaller change than
introducing a package manager.

**The cost, stated:** no automatic upgrades, no dedup across transitive
dependencies, vendoring by hand. That is what the component-model ecosystem does
today, and it is the right trade while yel's output is one artifact.

### The CLI must become directory-oriented, and currently is not

Go's CLI selects a **package** from a path defaulting to cwd (`go build`,
`go build ./widgets`, `go build ./...`) and finds the module root by walking up.
WIT's tooling takes the **directory** (`wasm-tools component wit ./wit`).

**Yel's CLI is file-oriented** — `yelc compile -o wasm path.yel`, and
[`stage-2-driver.md`](rewrite/stage-2-driver.md) specifies `yelc2 [OPTIONS]
<FILE>`. That disagrees with the model: a file-oriented CLI cannot express
*"compile this package"*, so it cannot name the unit the design is built around,
and would have to infer the package from a file's siblings — the same information
arrived at backwards.

This is a **consequence for stage 2 that nobody recorded**. The current signature
is right for a parser-only driver and wrong at the design level, so it changes
when packages become real:

```
yelc2 build            # the package in cwd
yelc2 build ./widgets  # a specific package
```

Not urgent — nothing consumes packages yet — but it should not be discovered
during stage 6.

### Unverified against the spec

Four WIT details are load-bearing here and are recollection rather than checked.
All are cheap to confirm in one pass against `wit-parser`, and all are expensive
to get wrong in a loader:

1. whether every `.wit` file in a package must repeat the `package` declaration,
   or only one may
2. whether `deps/` resolves **transitively** (a dep's own `deps/`)
3. the exact feature-gate grammar — `key = value` vs `key: value` (§4)
4. whether an `interface` may hold a `resource` alongside plain functions (§3)

---

## 7 · What is not decided

- **Yel package import: compiled-in or composed?** One WASM artifact (like the
  stdlib) or a separate component linked via WIT. Compiled-in is almost certainly
  right for a browser target — composition would break the closed-world
  assumption `--gufa --closed-world` depends on. Decide it before the syntax,
  because the syntax is trivial beside it.
- **The serialized-module format** — §6. The decisions exist (B1 structural `Ty`,
  B2 module-qualified `DefId`, B3 `OverloadKey`); the artifact does not.
- **Whether `impl` may appear outside the stdlib.** Closed-world makes coherence
  decidable — error on a duplicate method — so user `impl` on *user* types is
  nearly free. Retroactive `impl` on stdlib types is the line worth drawing
  first.
- **Whether a WIT interface may hold a `resource` alongside functions.** If so,
  `module` may eventually be where an exported component-with-methods belongs.
  Unverified against the spec.

---

## 8 · Sequencing

Nothing here is implemented, and none of it is on the surface-break list in
[`scope.md`](rewrite/scope.md) yet.

1. ~~**Rename `ModuleId` → `PackageId`**~~ — ✅ done 2026-07-29.
2. **`impl T { }`** — unblocks the stdlib; needed before stdlib source lands.
3. **WIT import** (`from "…" include`, WIT locator only) — bounded, deletes
   `extern module` and the `dom_imports.rs` special case, uses `wit-parser`.
4. **`module M { }` + `export`** — the emit direction.
5. **Yel package import** — with §6, after the compiled-in/composed decision.

Steps 2–4 are surface changes and belong in `scope.md`'s ledger with the other
nine, landing in the same scoped reopening after stage 4 — additive, and outside
the differential, so **`yel-smith` must learn each before it lands**.
