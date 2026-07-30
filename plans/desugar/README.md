# `plans/desugar/` — the desugaring illustration, and what checking it found

Two files, plus this one:

| file | is |
|---|---|
| [`counter.yel`](counter.yel) | the source. A real file; it compiles. |
| [`counter.yelir`](counter.yelir) | what it becomes after lowering, written as an **artifact with no commentary** |

Deliberately not under `examples/` or `tests/fixtures/` — those directories are
swept by `yelc-syntax`'s identity and parity suites against pinned counts, and
these are documentation.

**The dump is not any one stage's output.** It spans four, and reading it as
stage 3's would conflate them:

| in the dump | stage |
|---|---|
| the component as a record; UI gone; `mount`/`unmount` as functions; resolved names; UI `if` → `match` | **3** |
| types on every node; interpolation → `concat`; which `*-to-string` | **4** |
| the `Component`→`TreeRoot`→`If0` layout; the registry; effects collapsed to one `update` per signal | **6** |
| `concat` → `$concat2`; the mangled `@export` names; `cabi_realloc`; the return area; the packed handler id | **codegen** |

[`plans/rewrite/stage-3-hir-build.md`](../rewrite/stage-3-hir-build.md) carries
that boundary as a brief section and cites this file for the detail.

`counter.yelir` used to carry ~600 lines of comment. It carried them because it
was an *argument*; a dump is not an argument, and a dump full of prose drifts
from the thing it claims to describe in a second way, on top of the notation
drift the "must parse as yel" rule already addresses. So the reasoning is here
and the file is clean.

---

## 1 · What `.yelir` is, and the invariant

**`.yelir` is a subset of yel.** Not a second syntax — the same one, with the
surface constructs already lowered away. That buys a property worth asserting
once stage 3 exists:

> the HIR dump must parse as yel

which is the same class of invariant as S1's byte-exact round-trip, and it is
what makes the desugaring inspectable rather than merely printable. A dump in a
bespoke notation can drift from the language it claims to describe; one that
must re-parse cannot.

**Today it does not hold.** Each gap was checked with `yelc2 --emit-ast`, not
assumed:

| construct | status |
|---|---|
| `record R { x: s32, }` at top level | parses ✓ |
| `@primitive(op = "…")` | accepted ✓ |
| `@export(…)` / `@import(…)` | **parse**, then `E0060` — see below |
| `module M { … }` | does **not** parse — `E0060: expected 'component' or 'global' after 'export'` |
| `foo: func(x: s32) -> s32` at top level | does **not** parse — `E0060` |
| module-level mutable state | does **not** parse |

The `@export`/`@import` row is different in kind from the others and the
difference matters. Both forms produce a complete, correct
`AttributeList` / `Attribute` / `AttributeArg` tree — the *grammar* landed in
`a68e127` and needs nothing. What rejects them is `KNOWN_ATTRIBUTES`
(`crates/yelc-syntax/src/parser/attributes.rs:76`), which holds exactly
`["unsafe", "primitive"]` and **reports** anything else rather than dropping it
silently. So this is a two-entry registry change, not a syntax question. The
registry is deliberately conservative — its own doc comment refuses entries with
no decision behind them (anti-spec A9). This file is that decision.

Every name the dump uses is now declared. `node` and `any` are the two
exceptions: `node` is `u32` (the host assigns DOM handles; there is no resource,
so nothing is dropped or refcounted) and `any` is the WASM-GC top type that
`Handle.inst` holds before `registry-get` downcasts it. Neither is a yel type
today. §4a records which declarations are read from the emitter and which are
derived; §5 records what declaring the rest proved.

---

## 2 · `export` vs `@export`: two levels, not a duplicate

```yel
export component Counter { … }     // the DECLARATION — "put this in the
                                   // interface". Written by a user.

@export(name = "yel:desugar-demo/counter-component@0.1.0#[method]counter.mount")
                                   // the SYMBOL it lowered to. Generated.
```

The mangled name is real — core export section of `wasm-tools print
counter.wasm` — and until now it lived only inside the emitter as a `format!`
string. A name that exists only in a formatter is a name nothing can check.

Three things the attribute form buys:

- the mangled export name becomes **visible in the IR** rather than a side
  effect of string concatenation in codegen
- it is **uniform with `@primitive(op = …)`** — one mechanism binding a yel
  declaration to a target-level name, now with three uses: primitive, export,
  import. Not three mechanisms that happen to rhyme.
- `@import` is the first construction site for `LoweringTarget::HostImport`
  (`crates/yelc-sema/src/builtins.rs:67`), which an anti-spec review flagged as
  A9: a variant with **zero** constructions anywhere in the workspace. Its only
  other mention is a match arm in a debug dump
  (`crates/yelc-driver/src/driver/emit.rs:33`) that can never fire. It exists
  for exactly this, and nothing had claimed it.

The same level distinction runs the other way. A user writes `from
"yel:ui/dom@0.1.0" include Dom;` plus `use Dom.{ … }` (`modules.md` §4.1). By
the time the dump exists that has been **resolved** into explicit `@import`
declarations — the locator turned into symbols, exactly as `export component`
turned into a mangled name.

One consequence worth stating because it is only expressible now: when a
setter's params spill (§4), the wide function **loses its `@export`** and a
trampoline takes the name. "The export name moved to a different function" has
no way to be said when the name lives in a `format!`.

---

## 3 · Evidence: what is read, and what is constructed

This distinction is the difference between evidence and plausible invention, so
it is recorded per name.

### Read from `wasm-tools print` on a compiled artifact

Everything in `counter.yelir` except as noted below. Specifically: all eighteen
`yel:ui/dom@0.1.0` import names and signatures; the five
`yel:desugar-demo/counter-component@0.1.0#…` export names; the
`yel:ui/dispatch@0.1.0#dispatch` export name; `cabi_realloc`; the `event-value`
case list and order; the `(rep << 16) | ordinal` handler-id encoding; the
registry array/free-list shape; `mount: func(root: u32)`.

Reproduce:

```bash
cargo build --release -p yelc
./target/release/yelc compile -o wit  plans/desugar/counter.yel
./target/release/yelc compile -o wasm plans/desugar/counter.yel > counter.wasm
wasm-tools print counter.wasm
wasm-tools component wit counter.wasm
```

### Constructed by rule, never read

- **`cabi_post_…`, the return area, `lower-string`/`lift-string`.** `counter.yel`
  has no `string` property, so none of that appears in `counter.wasm` and none
  of it is in the dump. The shapes in §4 come from a *second* component compiled
  for the purpose (`label: string`, `maybe: option<s32>`, `items: list<s32>`),
  whose export names are `yel:strdemo/demo-component@…`. The
  `yel:desugar-demo/…#[method]counter.get-label` name that would appear here is
  therefore **derived from the naming rule, not observed**.
- **`@export(name = "counter-registry")` on a global.** The core module does
  export `counter-registry`, but globals do not parse, so it is not in the dump.
- **`use Dom.{ … }` and `use CounterComponent.{ … }`.** The names brought into
  scope are real; the `use` *syntax* is `modules.md` §4.1's, which nothing
  implements. There are **no aliases** in either — see §4a.
- **`registry-get` / `-insert` / `-free`, and `@primitive(op = "@wasm.memory_copy")`.**
  See §4a — the registry helpers are a factoring the dump introduces (the
  backend inlines all three at every call site), and the `memory_copy` op key is
  not registered anywhere.
- **The five function names the emitter does not assign** — `new`, `unmount`,
  and the three export wrappers. §4a lists what a WAT dump shows instead.

### Not determined

- Whether the registry-slot leak and the never-freed inbound setter buffer (§4)
  are known-and-accepted or unnoticed. No issue, TODO, or `TECH_DEBT.md` entry
  found for either; git history not searched.
- A possible inverted `option` discriminant in `list<option<T>>` materialization
  (`accessors.rs:2291-2293` stores `ref.is_null` without the `I32Eqz` every
  other path applies). Self-consistent on round-trip, wrong for a host. Not
  executed against a host, so not asserted.

---

## 4 · The boundary, as the frozen backend actually builds it

The first draft of `counter.yelir` reasoned the boundary out rather than reading
it. Twelve of its claims were wrong (§6). This is the corrected picture.

### The full export surface

| scope | exports |
|---|---|
| once per module | `memory` (17 pages), `cabi_realloc`, `yel:ui/dispatch@0.1.0#dispatch`, and a `start` function `globals-init` |
| once per exported component | `[constructor]`, `[method].mount`, `[method].unmount`, `<name>-registry` (a *global*, for `yel-host gc-dump`; wit-component hides it from the WIT surface) |
| per declared property | `[method].get-<p>`, `[method].set-<p>`, and `cabi_post_<full getter export name>` when the getter allocates |

Nothing else crosses.

### The resource rep is a registry index, not the record

The guest **never calls `resource-rep`**: for a `borrow<counter>` receiver the
canonical ABI hands the core function the rep as a bare `i32` param. That is why
every wrapper in the dump takes `rep: u32` and none lifts a handle.

And the rep is not a pointer to the record. It is an index into a per-component
registry — a GC array of `Handle` cells with an intrusive free list, four
globals (`<c>-registry`, `-registry-len`, `-registry-free-head`,
`-current-handle`). `resource-new` takes an `i32`, and a GC struct reference is
not an `i32`; the registry is the bridge between the linear-memory world the
component model speaks and the GC world the state lives in. It means **every**
exported method starts with an array read and a downcast — generated identically
five times over (four wrappers plus `dispatch`), which is why the dump factors
it into `self-of`.

### What each non-scalar flattens to

| type | flat form |
|---|---|
| `string`, `list<T>` | `(ptr: u32, len: u32)`. Element stride is per-`T`: 1/2/4/8 for scalars, the record layout for records, **8/align 4** for a nested string or list (the element is itself a fat pointer). |
| `option<T>` | `(disc, ..payload)`, `none = 0` / `some = 1`. In memory the discriminant is **one byte** at offset 0, payload at `align_to(1, align_of(T))`. |
| `record` | field by field, declaration order, transitively — a nested record flattens inline, it does not become a pointer. |
| `variant` | `(disc, ..join(payloads))`. The join is slot-wise: equal types pass, otherwise anything 64-bit or a reference promotes that slot to `i64`, else `i32`. Shorter cases zero-pad. |

Two rules follow that the first draft got wrong:

- **A getter returns at most one flat value** (`MAX_FLAT_RESULTS = 1`). Anything
  wider goes through a **return area** the guest allocates with `cabi_realloc`.
  So `get-label` is `(rep: u32) -> u32`, not `-> (ptr, len)`.
- **Setter params spill past 16.** If `1 + flat_len(T) > 16`, the wide setter
  stops being exported and a `func(ptr: u32)` trampoline is exported in its
  place, loading `self` from `ptr + 0` and each flat slot from
  `align_to(4, align_of(T)) + offset`. The limit is on *params*, not on the
  record, and the indirection is a separate function that inherits the symbol.

### String lifecycle

**Out** — registry lookup → `cabi_realloc(0, 0, 4, 8)` for the return area → copy
the GC `(array i8)` into a *second* `cabi_realloc` buffer → store `(ptr, len)` at
`+0`/`+4` → return the area pointer. Freed by the host calling the getter's
`cabi_post_…`, whose body is `if len { free(ptr, len) } free(area, 8)`.

A `cabi_post` is emitted only when three things hold: the result flattens to more
than one slot, the signal lives in the GC struct, and the type is not `func`. A
memory-resident composite getter returns a pointer *into* live storage and is
deliberately skipped — freeing it would be use-after-free. `option<s32>` still
gets one, and its whole body is `free(area, 8)`.

In the yel-shaped form the dump would carry if `Counter` had a `label`:

```yel
@export(name = "…#[method]counter.get-label")
export-get-label: func(rep: u32) -> u32 {
    let self = self-of(rep);
    let area = cabi-realloc(0, 0, 4, 8);
    let s = lower-string(self.label);
    store-u32(area + 0, s.ptr);
    store-u32(area + 4, s.len);
    area
}

@export(name = "cabi_post_…#[method]counter.get-label")
export-cabi-post-get-label: func(area: u32) {
    let ptr = load-u32(area + 0);
    let len = load-u32(area + 4);
    match len > 0 {
        true -> free(ptr, len)
        false -> unit
    }
    free(area, 8);
}
```

**In** — the setter takes the flat slots as params, copies into a fresh GC array,
`struct.set`s it, and calls the signal's update function. **The incoming buffer
is never freed.** The canonical ABI transfers ownership of `(ptr, len)` to the
guest — the host allocated it through the guest's own `cabi_realloc` — and the
emitted setter drops it. Grep is unambiguous: `free` has exactly two call sites
in a compiled module, both inside `cabi_realloc` itself.

`cabi_post_…` is also the one place a symbol is **derived** rather than chosen:
the literal prefix `cabi_post_` on the getter's full export name, which is how
the encoder wires it. Getting the prefix wrong silently produces a component with
no post-return rather than an error.

### The allocator

`cabi_realloc` is the **host's** allocator inside the guest: it is how a string
the host wants to pass in gets a home. It is not a wrapper over a system
allocator — the guest ships its own, a bump-plus-free-list over linear memory
with a minimum alignment of 8, three globals (`heap_base`, `heap_ptr`,
`free_list`), and string literals interned into the data segment below
`heap_base`.

Its `@export` name is unqualified — no package, no interface, no `#` — which is
what a core export looks like, and is the visible difference between the two
levels.

One consequence that is invisible otherwise: a string literal reaching a DOM call
is *already* linear-memory `(ptr, len)` in the data segment, so it costs nothing.
A string that has been through the GC world costs a `cabi_realloc` per crossing —
and interpolation currently round-trips (`literal → GC → linear → concat → GC →
linear`), so `"Count: {count}"` allocates four buffers per update and frees none.

### Dispatch

`add-event-listener` is `func(node: u32, event: string, handler-id: u32)`. No
function reference crosses. `handler-id = (rep << 16) | ordinal`, where `rep` is
the registry index and `ordinal` is a per-component `AddEventListener` counter
capped at 65536 sites. The mount wrapper writes `<c>-current-handle` first
because the registration site knows the ordinal statically but learns the
instance only at mount time — so the encoding is a **runtime** value, and a
listener cannot be registered outside a mount.

`event-value` flattens to three core params past the handler-id: a discriminant
plus a joined `(i64, i32)`, because `string`'s `(i32,i32)` joined against `f64`'s
single slot promotes slot 0 to `i64`. Core signature `(i32,i32,i64,i32)`, and
`dispatch` is the only export in `counter.wasm` lifted with `(realloc)`.

The generated `match` has **no default arm**: an unknown handler-id falls off the
end and returns, silently. For a *binding* handler (`value: <-> field`) the
backend runs a preamble before the user body — parse the arm, write the signal,
fire its effects — so `dispatch` is not purely a router.

### Reactivity is compiled away entirely

`notify` is not a table lookup. Per signal the backend emits **one** update
function that calls its dependents in a fixed order, and every writer — the
exported setter and the click handler alike — calls that one function directly.
The "graph must be inverted somewhere" obligation is discharged at compile time;
the runtime has no graph, no table, and no indirect call.

---

## 4a · Names

`crates/yel-wasm-codegen/src/wasm/codegen/name_section.rs` is the scheme. Two
paths produce names, and **only one of them is trustworthy** (§9).

### The live path: block functions

`build_block_func_name` (`:19–38`) produces

```
{comp}-{kind}[-b{boundary}]*[-s{signal}]#{block_id}
```

keyed off `block_func_indices`, the same map the code section uses, so the names
land on the right functions. For `counter.yel`:

| emitted | is |
|---|---|
| `counter-constructor#0` | the user's constructor body — empty here |
| `counter-mount#1` | `mount` |
| `counter-handle-clicked#2` | `handle-clicked` |
| `counter-if-branch-mount-b1-b2#3` | `if-branch-mount` |
| `counter-if-branch-unmount-b2#4` | `if-branch-unmount` |
| `counter-if-update-b0#5` | `if-update` |
| `counter-update-b0-s485#6` | `update` — `s485` is the signal's `DefId` index |
| `counter-block#7` | `new` |
| `counter-block#8` | `unmount` |
| `counter-block#9` | `export-new` |
| `counter-block#10` | `export-mount` |
| `counter-block#11` | `export-unmount` |

**Five functions have no registered `BlockDebugName`** and fall back to
`kind("block")` (`:180`, `:554`), distinguished only by block id. They are
`new`, `unmount`, and all three export wrappers — precisely the boundary
functions, the ones a WAT dump most needs named. The dump uses readable names
for them because "unnamed" is the truth and adopting the emitter's `block#N`
fallback would be adopting a placeholder, not a name.

The module prefix does the rest of the work: with the component's functions
inside `module CounterComponent` carrying
`@interface(name = "yel:desugar-demo/counter-component@0.1.0")`, the dump's
`mount` **is** `counter-mount` — the `{comp}-` half comes from the module, so no
declaration in the dump needs to repeat it.

### Types and globals

| dump | emitted | source |
|---|---|---|
| `Component` | `counter-component` | `:145` |
| `TreeRoot` | `counter-tree_root` | `:119` |
| `If0` | `counter-if_0` | `:119` |
| `If0Then` | `counter-if_0_then` | `:119` |
| `Handle` | `handle` (module-shared, one per module) | `:78` |
| `registry` | `counter-registry` | `:218` |
| `registry-len` | `counter-registry-len` | `:221` |
| `registry-free-head` | `counter-registry-free-head` | `:224` |
| `current-handle` | `counter-current-handle` | `:227` |

The registry is **four globals**, not one opaque thing. An earlier draft of the
dump hid all four behind `registry-get`/`-insert`/`-free`; they are now
declared. Note also that `Handle` is **module-shared, not per-component**
(`:77–82`, "one pair per module instead of per-component pre-unification") — its
`inst` field is therefore untyped (`any`), and `registry-get` is where the
downcast to `Component` happens. That is why every exported method's prologue
ends in a `ref.cast`.

Two spelling inconsistencies in the emitter, noted because they are the kind of
thing a dump makes visible: the tree-struct names are kebab prefix plus **snake**
suffix (`counter-tree_root`, `counter-if_0`), and the component struct is
`{comp}-component` — a suffix — while every other type is `{comp}-{decl-name}`.

### Derived, not read

`registry-get`, `registry-insert`, `registry-free` are a **factoring the dump
introduces**. No such functions exist: the free-list pop, the array grow-by-
doubling, and the `array.get` + `struct.get $inst` + `ref.cast` are inlined at
every one of the five call sites. Declaring them bodyless is the honest form —
they are named for readability, not observed.

### Where each name comes from — import vs. compiled in

`module Dom`'s members carry `@import` because they are genuine WASM imports —
the host supplies them, and each call is a boundary crossing. **Nothing else in
the dump is.** `modules.md` §4.1's rule that a specifier is a *locator, not a
kind* is what makes the distinction expressible: a yel-package locator resolves
to code that is compiled in, so it produces **no world import and no `@import`
attribute**, and a reader can tell from the declaration alone which calls cost a
crossing.

Sorting the rest by that rule turned up an asymmetry and a category that does not
fit.

**`min` is the only genuine `include`.** `yelc-sema/src/stdlib.rs:102–108`
registers it `Visibility::UserFacing`, and `stdlib/num.yel:16` implements it in
ordinary yel (`a < b ? a : b`) — directions.md §2 tier A, *"writable now, nothing
needed"*. So it arrives exactly as `modules.md` §4.1 specifies:

```yel
from "yel:std/num@0.1.0" include Num;
use Num.{ min };
```

No `@import`, no world entry. `min` is the only name in the file that
demonstrates the contrast.

**`concat` and `s32-to-string` cannot arrive that way at all.** Both are
`Visibility::Internal` (`stdlib.rs:143–171`), which the table's own doc comment
defines as a desugaring target: *"Not name-resolvable from source."* A `use`
naming them would resolve something the checker refuses to resolve. They are
declared at root with `@primitive` carrying the **real op strings from the
table** — `"concat"` (`:150`) and `"s32_to_string"` (`:157`). That is the same
mechanism `stdlib/array.yel` uses and the honest one here: `LoweringTarget::Op`
is precisely what `@primitive(op = …)` denotes.

**The two are not the same shape, and an earlier revision had it backwards:**

| | the table declares | the backend emits | the dump calls |
|---|---|---|---|
| `concat` | **one** row, `Arity::Variadic { min: 0, element: STRING }` (`:143–151`) | `$concat2` — one function per distinct arity used | `concat(…)`, the stdlib name |
| `*-to-string` | **eight** rows, `Arity::Fixed(1)`, one per source type (`:154–161`) | `$s32_to_string` etc., 1:1 with the rows | `s32-to-string(…)`, also the stdlib name |

So `concat` is the **only** monomorphization in the file: one variadic stdlib
function, N emitted symbols. `to-string` is *not* monomorphized — the family is
declared in the table, and `s32-to-string` is the real stdlib name rather than an
instantiation of some generic `to-string`. Calling both "families" was true of
the emitted symbols and false of the declarations.

The dump therefore calls `concat` and lets §4a carry the `$concat2` mapping,
while `s32-to-string` needs no note because there is nothing to map. This is the
first place monomorphization is visible in a yel artifact, and it is visible as a
*difference between two adjacent lines*, which is the useful form.

**Variadic has no yel syntax.** `concat: func(parts: string...)` uses a spelling
the grammar does not have — and has never needed, because the only variadic
builtin is `Internal`, so no user has written a call the parser had to accept.
This is the one gap the dump *creates* rather than inherits.

### No aliases

`use Dom.{ … }` brings the import names in **unrenamed**, and every call site
spells them the way the import section does. An earlier revision aliased four
(`set-text-content as set-text`, `remove as el-remove`,
`add-event-listener as on`, `remove-event-listener as off`); they are gone.

The dump's value is that a reader can hold it next to `wasm-tools print` and see
the same names. `on(...)` forces a trip back to the `use` block to learn what it
stands for; `add-event-listener(...)` is the name in the import section, and
matching it is the whole point. Aliasing is a fine feature and the wrong choice
for an artifact whose job is correspondence.

No collision required an alias, so there is no exception to note.

The same argument retired `el` and `set`, which were not aliases but invented
short names for the two compiler-supplied helpers:

| was | is | why |
|---|---|---|
| `el` | `create-element-into` | two imports — `create-element` then `append-child` |
| `set` | `set-attribute-str` | `set-attribute` plus a lift into `attribute-value.str` |

Both are given **bodies** rather than declarations, because they have bodies —
two lines each over `Dom`. That is what makes them category 2 in §5 rather than
category 1, and a two-letter name hid exactly that.

`set-attribute-str` carries one more thing its old name did not: it is **one of a
family**, one per `attribute-value` case, because the case is statically known at
each call site. A single polymorphic `set` does not exist.

### `alloc`, `free` and `copy` fit no category — this part is intent, not observation

The dump's allocator section is the one place a **design intention** is written
down, and it is marked here the same way the `label` accessors are, so it does
not read as an observation.

**Today**, `alloc` / `free` / `cabi_realloc` are emitted by
`crates/yel-wasm-codegen/src/wasm/runtime/memory.rs` — a bump-plus-free-list
generated per module, with `memory.copy` inlined into `cabi_realloc` rather than
called. They are:

- **not host imports** — nothing in the WIT world mentions them
- **not stdlib** — no row in `yelc-sema/src/stdlib.rs`, no declaration in
  `stdlib/*.yel`
- **not `@primitive`** — no op string keys them, because nothing looks them up

That is a **fourth category**: backend-generated, module-local, with no
declaration anywhere in the language or its tables. `cabi_realloc` is the only
member with a name outside the emitter, and only because the canonical ABI
requires it to be exported. Calling them "stdlib" would have been a claim about
where they *should* live.

**Intended**, per directions.md §2, the allocator is ordinary yel over memory
primitives from `stdlib/DESIGN.md`'s `#` namespace. The dump shows that shape:
`copy` as `@unsafe @primitive(op = "@wasm.memory_copy")`, `alloc` and `free` as
bodyless declarations with no attribute — declared and unresolved, which is
exactly their status. The `@unsafe` pairing follows `stdlib/array.yel`: a
primitive naming a WASM instruction is what the marker is for.

Two things about that intent are unlanded and should not be read as settled:

- `@wasm.memory_copy` is **not a registered op key.** `stdlib/array.yel` has
  `@wasm.array_copy_i8`; there is no `memory_copy` anywhere. The mechanism is
  right — it genuinely lowers to one instruction — the key is derived.
- `alloc` and `free` have no primitive floor written for them at all. They need
  memory load/store/grow primitives that `stdlib/DESIGN.md`'s `#` namespace
  anticipates and nothing declares. `#array.len.i8` has an answer; `#alloc` does
  not, and the intended shape does not supply one — it just moves the question
  down one level.

### The first stdlib call in the dump does not typecheck

Following the `include` through produced finding #13, and it is small and real:
`min` is registered `vec![Ty::S32, Ty::S32] -> S32` (`stdlib.rs:104–106`) and
`stdlib/num.yel:10` says why — *"Monomorphic on s32 today because there are no
numeric constraints"*. But `cabi_realloc`'s sizes are **unsigned**: its canonical
signature is `(u32, u32, u32, u32) -> u32`, so `min(old-size, new-size)` is a
`u32` call against an `s32`-only function.

There is no `u32` `min`, no numeric constraint to write one generically
(directions.md §3 is what unblocks that), and no coercion. So the very first
stdlib call the dump makes is one the checker would reject — which is a fair
summary of how far tier A actually reaches.

---

## 5 · The four-way split under the primitives

The first draft asked where `el`, `set`, `on`, `region`, `region-fill`,
`set-text`, `effect` are defined and answered "nowhere". Reading the backend
splits that one question into four with different shapes of answer. (The names
in the first two rows are the draft's; §4a records what they became and why.)

| calls | what they are | mechanism today |
|---|---|---|
| `set-text`, `el-remove`, `on`, `off` | plain host imports the draft had renamed — `set-text-content`, `remove`, `add-event-listener`, `remove-event-listener` | **yes** — `use Dom.{ … }`, `modules.md` §4.1 |
| `el`, `set` | **not** renames. `el(parent, tag)` is *two* imports (`create-element` then `append-child`); `set` is `set-attribute` plus a lift into the `attribute-value` variant, and is one of a family — one per case | none |
| `region`, `region-fill`, `region-clear` | no host counterpart. They lower to `create-comment` for an anchor plus generated struct state, `insert-after`, `remove`. A region is a codegen concept. | none |
| `effect`, `effect-drop`, `notify` | no host counterpart and **no runtime existence**. They vanish at codegen into direct calls. | none |

Only the first row has a mechanism. The *array* floor has an answer
(`#array.len.i8`, the manifest, the `#` namespace); the **UI floor has never been
asked** — and it is at least four questions, not one. The second row in
particular has no name in the provides/requires split in `stdlib/DESIGN.md`:
a compiler-supplied helper *over* a host interface is neither.

Tracing where the *non*-UI names come from added two more rows, both in §4a:

| calls | what they are | mechanism today |
|---|---|---|
| `min`, `max` | genuine stdlib, `UserFacing`, written in yel | **yes** — `from … include`, no world import |
| `concat`, `*-to-string` | builtin-table rows marked `Internal` — desugaring targets, **not name-resolvable from source** | `@primitive(op = …)` only; `use` cannot reach them |
| `alloc`, `free`, `copy` | backend-generated, module-local, declared nowhere at all | none — and not stdlib, whatever it looks like |

Six categories now, from one question. The last row is the one with no home in
any existing split.

### Declaring rows 3 and 4 deleted them

Rows 1 and 2 got declarations and the dump uses them. Rows 3 and 4 got as far as
being written down and then had **no call site left**, which is finding #11:

- **`region`, `region-fill`, `region-clear` do not exist even as a shape.** The
  backend has no generic region. It emits a *per-boundary* struct — `If0` with
  `anchor` / `parent` / `active` / `branch-then`, plus a separate `If0Then` — and
  open-codes the mount/unmount/update against those fields. Declaring
  `region-fill(r, build)` would have invented a uniformity across `if` and `for`
  that nothing implements. The dump now carries the real structs.
- **`effect` and `effect-drop` have no call site.** There is no registration step
  at all: the dependency set is resolved at compile time into one `update`
  function per signal, and `mount` simply calls it. So the earlier dump's
  `effect(self, [count], upd-text)` described a runtime that does not exist.
- **`notify` has no call site either.** Writers call the signal's `update`
  function directly — `update(self, self.tree)` — which is what
  `export-set-count` and `handle-clicked` now do.
- **`upd-text` is not a function.** The text update is inlined into `update`,
  which then calls `if-update`. One update function per signal, not one per
  dependent region.

Two of the four categories turned out to be vocabulary the illustration had
invented for itself. That is what declaring them proved, and it is the reason the
dump is shorter than the draft it replaced rather than longer.

---

## 6 · Claims that were wrong

`counter.yelir` was written from reasoning, not from the backend. Seventeen
claims have been disproved by checking them. Eight were in the file; #9 and #10
are compiler bugs; #11–#14 are things the file had invented; #15 is a gap between
the stdlib and the ABI; #16 and #17 are in
[`stage-3-hir-build.md`](../rewrite/stage-3-hir-build.md), found by connecting it
to the artifact.

1. **"only `export`ed properties get accessor methods; `export` is the filter"** —
   there is no per-property `export`. `export` is a modifier on the *component*
   (`LANGUAGE.md:232`); `counter.yel` marks nothing and still emits
   `get-count`/`set-count`. The real filter is declared-vs-generated, and a
   `func`-typed property inverts the *direction*: it becomes an imported
   `<comp>-callbacks` interface (`incremented: func(self: borrow<demo>)`), not an
   accessor.
2. **`let self = resource-rep(h)`** — wrong twice. The guest never calls it, and
   the rep is a registry index rather than the record.
3. **`export-get-label: func(h) -> (ptr, len)`** — a getter returns one `i32`
   pointing at a return area. Core result arity is capped at 1.
4. **"effects [become] a dispatch table"** (in the deliberately-absent list) —
   they become one direct call per signal. No table, no indirect call.
5. **the `known_bugs/` citation** — all three ABI fixtures ("setter for
   `option<string>` fails wit-component encoding", "record-payload setter
   signature is wrong", "variant setter with mixed payload shapes") are **fixed**,
   by the spill trampoline in §4. The fixtures moved to `positive/` with
   committed `.wit` snapshots; only `known_bugs/README.md` still lists them.
6. **"Today it is a method"** (on `unmount` vs a destructor) — implies a
   destructor is an implemented alternative. No destructor is emitted at all, so
   neither option exists.
7. **the unmount ordering claim** — correct as design, but the backend implements
   *neither* side. There is no effect deregistration, and `remove-event-listener`
   is imported into every module and **never called from anywhere** in the
   back-end. The registry free list is likewise **pop-only**:
   `<c>-registry-free-head` is read in the constructor and written in exactly one
   place, the pop. Nothing pushes a slot back, and there is no destructor, so a
   construct/drop loop grows the array without bound. The dump's `off(…)` and
   `registry-free(…)` are therefore **prescriptive, not descriptive** — two
   missing calls, made visible by writing the teardown out.
8. **`modules.md` §3's `module` ↔ `interface` 1:1** — false in reverse. See §7.
9. **handler ordinals collide across components** — a live miscompilation. See §8.
10. **`{comp}-constructor-internal` / `-mount-internal` / `-unmount-internal`
    name functions that no longer exist** — and their emission is what
    misaligns the whole name section. See §9.
11. **`region*`, `effect*` and `notify` had no call site once the dump matched
    the backend**, and `upd-text` is not a function. See §5.
12. **`event-value` is not declared in `yel:ui/dom@0.1.0`.** An intermediate
    revision of the dump put it there. `wasm-tools component wit` shows the
    `yel:ui` package holding two interfaces: `dom` owns `color` and
    `attribute-value`, `dispatch` owns `event-value`. The dump now matches.
    Worth recording because it is the *only* type the two interfaces could have
    plausibly shared, and it does not.
13. **`concat` and `to-string` are not the same shape**, and neither can arrive
    through `use`. `concat` is one variadic row the backend monomorphizes to
    `$concat{N}`; `*-to-string` is eight declared rows with no monomorphization
    at all. Both are `Visibility::Internal` — desugaring targets the checker
    will not resolve from source. An earlier revision called both "families"
    and implied both were stdlib-importable. See §4a.
14. **`alloc`, `free` and `copy` are not stdlib.** They are backend-generated,
    module-local, and declared nowhere — not the WIT world, not
    `yelc-sema/src/stdlib.rs`, not `stdlib/*.yel`. A fourth category with no
    home in any existing split. The dump shows directions.md §2's *intended*
    shape and §4a marks it as intent. See §4a.
15. **`min` does not typecheck at the one call site the dump has for it.**
    Registered `s32 -> s32` and monomorphic by decision; `cabi_realloc`'s sizes
    are `u32`. The first stdlib call in the dump is one the checker would
    reject. See §4a. Now also in the stage-3 brief's risk list.
16. **`stage-3-hir-build.md` decides the HIR dump must *not* round-trip**, and
    this file asserts the opposite. The brief's three supporting examples —
    `x = x + 1`, `Color.rgba(…)`, a flattened `else if` — are all valid yel, so
    the reason does not support the conclusion. The real obstacles are §1's five
    measured parser gaps, which is a different argument. Recorded in the brief,
    not reconciled.
17. **The brief's cheapest provenance deliverable cannot be written as
    specified.** It says to grep rendered diagnostics for "the generated-name
    prefix". There is no prefix: `__mount_*` / `__ui_*` appear nowhere in the
    frozen tree — they are invented in the brief's own example — and the real
    scheme is `{comp}-{kind}` (`counter-mount`), indistinguishable in shape from
    a user-written name.

---

## 7 · Structural gaps the restructuring exposed

### `module` ↔ `interface` is not 1:1

`modules.md` §3 fixes `module M { … }` ↔ WIT `interface`, and §4 fixes the
package root as the `world`. The mapping holds in the direction it was designed
for — a user writes `module`, an interface comes out. The reverse fails three
ways, all read off emitted artifacts:

1. **`counter-component` is an interface no yel construct declared.**
   `modules.md` §1's table says `component C` emits "a `resource`". It does not:
   a resource cannot be a world export on its own, so the backend synthesizes an
   interface to hold it, named `<comp>-component`. Under §3 that interface needs
   a `module`, and `counter.yel` has a `component`. So `component` is a *second*
   construct that emits an interface, which §1's table does not say.
2. **One component can produce three interface appearances.** Give it a
   `func`-typed property and the world gains `import demo-callbacks` **and**
   `import demo-component` — the latter purely so `borrow<demo>` has a name in
   the callback interface — alongside `export demo-component`. One declaration,
   three entries, two directions.
3. **`yel:ui/dispatch@0.1.0` has no source construct at all.** Emitted once per
   compilation regardless of program content.

The accurate statement: **every `module` becomes an interface; not every
interface comes from a `module`.** The generated ones need an origin rule, and
none is written down.

### A fourth level, with no row in `modules.md`

`memory`, `cabi_realloc` and `<c>-registry` are **core-module** exports, beneath
both world and interface, hidden from the WIT surface by wit-component. They
belong to no module because there is nowhere to put them. `modules.md` §4's table
has package and module; this is a third row below both.

### Module → root, and module → module, visibility is unspecified

Grouping the component's implementation inside `module CounterComponent` — per
the test *"would this exist if `Counter` were deleted?"* — makes
`module Dispatch` reach into it: `dispatch` calls `self-of` and `h-clicked`,
neither of which is part of the `counter-component` interface. The dump writes
that as `use CounterComponent.{ self-of, h-clicked };`.

`modules.md` §4.1 specifies `use` for *interface members* and says files in a
package see each other with no import. It does not say whether a module may
`use` another module's **non-exported** members. Every generated boundary module
needs exactly that, so it is the normal shape rather than a corner case.

This is structural, not incidental: `dispatch` is emitted **once per
compilation** while handlers are **per component**, so the one function that
routes events must reach into every component's internals. With N components it
is N `use` lines into N modules. There is no arrangement of flat modules in which
`dispatch` lives somewhere that already sees what it needs.

### What stayed at root, and why

The test is *would this exist if `Counter` were deleted?*

- **In `module CounterComponent`:** `Counter`, `Handle`, the five `@export`
  wrappers, `self-of`, `new`, `mount`, `unmount`, `upd-text`, `upd-if`,
  `build-if-body`, `h-clicked`. All of them vanish with the component. `Handle`
  in particular is per-component, because `Handle.inst` is typed `Counter`.
- **At root:** `cabi-realloc` and the allocator — emitted once per module
  regardless of component count.
- **`module Dom`** survives too: the eighteen DOM imports are imported into every
  compiled module whether or not the program calls them.
- **`module Dispatch`** is the awkward one. The *interface* survives Counter's
  deletion; the arms of its `match` do not. It is emitted once per compilation
  and its body is per-component.

---

## 8 · Finding #9: handler ordinals collide across components

Not a design gap — a miscompilation, found by asking what `module Dispatch` looks
like with two components in it.

`next_handler_local_id` is keyed by `comp_idx`
(`crates/yel-wasm-codegen/src/wasm/codegen/op_emit.rs:461`), so **each
component's ordinals restart at 0**. But `dispatch` discriminates on the ordinal
alone and never checks which component the id belongs to, and the handler-id
packs `(rep << 16) | ordinal` with no component field — `rep` is a per-component
registry index, so it cannot disambiguate either.

Compile two components that each register one listener and the emitted
`dispatch` contains two arms **both testing `id & 0xFFFF == 0`**:

```wat
(func $dispatch (param i32 i32 i64 i32)
  local.get 0  i32.const 16  i32.shr_u  local.set 4
  local.get 0  i32.const 65535  i32.and  i32.const 0  i32.eq
  if  ;; A
    global.get $a-registry … ref.cast (ref $a-component)
    call $a-handle-clicked#2
    return
  end
  local.get 0  i32.const 65535  i32.and  i32.const 0  i32.eq
  if  ;; B — unreachable
    global.get $b-registry … ref.cast (ref $b-component)
    call $b-handle-clicked#10
    return
  end)
```

The first arm always wins and `return`s. Component B's handler is unreachable,
and a click on B carries `(b_rep << 16) | 0`, which matches arm A and indexes
`$a-registry` with B's rep — then `ref.cast (ref $a-component)`, which traps or
silently resolves to the wrong instance.

The module validates. `wasm-tools validate` cannot see it, and no fixture is both
multi-component and event-driven, so the suite stays green.

How it survived: `WasmPackageBuilder::handler_counter`
(`crates/yel-wasm-codegen/src/wasm/mod.rs:916`) is documented as a *"Global
handler counter for event handler registration/dispatch"* and is reset per
component (`build.rs:2066`, `:2083`). The doc comment says global, the code is
per-component, and the decode was written against the comment.

Repro:

```yel
package yel:two@0.1.0;

export component A {
    a: s32 = 0;
    VStack { Button { label: "a" clicked: { a = a + 1; } } }
}

export component B {
    b: s32 = 0;
    VStack { Button { label: "b" clicked: { b = b + 1; } } }
}
```

```bash
./target/release/yelc compile -o wasm two.yel > two.wasm
wasm-tools print two.wasm | sed -n '/(func \$dispatch /,/^    (func \$globals_init/p'
```

Not fixed here — `crates/` is frozen for this work.

---

## 9 · Finding #10: the name section names three deleted functions

Found while looking up the real internal names for §4a. Debug output only — the
module is valid without a name section — but it makes every WAT dump of a Yel
module mislead, which is how a reader ends up believing in functions that are not
there.

Two independent walks compute the same function-index layout and **disagree on
the stride**:

| file | stride per component |
|---|---|
| `wasm/codegen/build.rs:1772` | `export_slots + 2N`, where `export_slots` is 3 for an exported component and **0** otherwise |
| `wasm/codegen/name_section.rs:514` | `6 + 2N`, unconditionally |

`build.rs` carries the comment explaining why (`:1766`): *"Phase 0.3l: prefix
shrinks from 6+2N to 3+2N — the 3 internal-tier lifecycle entries now emit
through the per-block loop."* The internal lifecycle tier was deleted.
`name_section.rs:509–512` still appends names for it:

```rust
let internal_base = func_idx + 3 + (component.signals.len() as u32 * 2);
func_names.append(internal_base,     &format!("{}-constructor-internal", prefix));
func_names.append(internal_base + 1, &format!("{}-mount-internal",       prefix));
func_names.append(internal_base + 2, &format!("{}-unmount-internal",     prefix));
```

`internal_base` now points at whatever follows the accessors. In `counter.wasm`
that is the accessors themselves: `get-count` is named
`$counter-constructor-internal` and `set-count` is named
`$counter-mount-internal`. The `[constructor]`/`[method]` names from `:485–487`
are displaced too, because `first_component_func` is recomputed at `:472–480`
rather than shared — and it filters the GC list-helper count by
`is_scalar_list_ty` where `build.rs:1754` does not. So they land on the string
materializer and the color lowerer, which is why `wasm-tools print counter.wasm`
shows

```wat
(func $"[constructor]counter" (param i32 i32) (result (ref null $str_bytes)))
```

a "constructor" that takes two `i32`s and returns a string array.

**Consequence for this work:** `{comp}-constructor-internal`,
`{comp}-mount-internal` and `{comp}-unmount-internal` are names for functions
that no longer exist. They are not the internal names of `new` / `mount` /
`unmount` and are not used in the dump. The trustworthy names are the block-func
names in §4a, because that loop (`:539–558`) iterates `block_func_indices` — the
live map the code section itself uses — instead of re-deriving a stride.

Which is the general lesson, and it is the same one as §8: **every place two
walks recompute one layout independently, they have drifted.** `build.rs` /
`name_section.rs` here; `op_emit.rs` / `dispatch.rs` there.

Not fixed here — `crates/` is frozen for this work.
