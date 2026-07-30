# The yel standard library — specification artifact

**Nothing here compiles yet.** These files are signatures, they parse cleanly
against `yelc2` today, and they exist to pin what
[`plans/rewrite/directions.md` §2](../plans/rewrite/directions.md) must support.

The point of writing them before they work: gaps surface from *trying to write
the thing*, not from reasoning about it. The largest gap below was found in four
lines, after an afternoon of analysis had not mentioned it.

## Why signatures only

**Yel has no syntax for a function body.** `function_decl` is
`name: func(…) -> T;` — a declaration, terminated by `;`. Try to give it one:

```yel
double: func(n: s32) -> s32 = { n -> n * 2 };
```
```
error[E0060]: expected `;`, found `=`
```

Bodies exist today only as closure literals bound to func-typed properties
(handlers), and `func`-typed members of a `global` are **callbacks the host
implements**. So `filter`'s implementation has nowhere to live.

This is the real §2 blocker, and it is larger than `primitive`. `primitive`
covers the floor — the few things not expressible in yel. Everything *above* the
floor is supposed to be ordinary yel, and ordinary yel cannot currently define a
named function with a body.

## What each file records

| file | contents |
|---|---|
| `list.yel` | `len`, `filter` (documented) · `get`, `append` (registered by the frozen compiler, undocumented) · `first`, `map`, `contains` (neither — proposed) |
| `string.yel` | `len`, `starts-with` |
| `num.yel` | `min`, `max` |

Three tiers are deliberately mixed and labelled in the source, because the
source stdlib is where the difference stops being invisible: a function the
compiler registers but `LANGUAGE.md` never mentions is either undocumented or
unwanted, and writing it down forces the choice.

## Known gaps these files make concrete

**No function bodies** — above. A surface change, and the fourth now pending
alongside `match`, `primitive` and `<T>`
([`scope.md`](../plans/rewrite/scope.md)).

**No constraints, so `min`/`max` are monomorphic.** `func<T>(a: T, b: T) -> T`
cannot compare `a` and `b` — there is no way to require that `T` is ordered
([`LANGUAGE.md` § Type Parameters](../LANGUAGE.md#type-parameters)). They are
written on `s32` rather than pretending otherwise. Generalising them needs
constraints, which was explicitly excluded.

**`len` appears twice** — `List.len` and `Str.len`. The builtin table models this
as two overloads of one name, and the module split makes them two *qualified*
names instead. Which the language actually offers is undecided; the source
stdlib is where it gets decided.

**Nothing verifies these signatures against the compiler's own table.**
`yelc-sema`'s `stdlib.rs` registers the same functions from Rust, and there is no
check that the two agree. When §2 lands, the Rust registration is deleted and
this becomes the single source — until then they are two things that must agree,
checked by nothing, which is precisely the shape
[F12](../plans/rewrite/findings.md) describes. Treat that as a known cost of the
intermediate state, not as an oversight.

## What has to land before these compile

1. **Function body syntax** — no proposal yet
2. **`primitive` declarations** — [§2](../plans/rewrite/directions.md), scope
   break unspent, two options
3. **HIR lowering for generic signatures** — stage 3
4. **Instantiation in the checker** — stage 4
5. **Monomorphization** — stages 5–6
6. **Closures as values** — [§4](../plans/rewrite/directions.md); `filter` takes
   one

They parse, and that is the whole of what is true today.

## `@impl(type)` — a global is a type's method set

```yel
@impl(string)
export global String {
    len: func(text: string) -> s32 { bytes-len(text) }
    starts-with: func(text: string, prefix: string) -> bool { … }
}
```

`"hello".len()` resolves `len` **in the global registered for the receiver's
type**. Rust's `impl`, Go's method set, and the same shape the stdlib was already
being written in.

**Why not the pure desugar alone.** `x.f(a)` → `f(x, a)` is syntactic and the
frozen tree already performs it (`MethodCall → Call`, listed as a keeper in
[stage 3](../plans/rewrite/stage-3-hir-build.md)). But it resolves `f` in a
**flat** namespace, and the stdlib is not flat — `len` lives inside
`global String`. Something would have to lift every stdlib function into global
scope, and that something is a per-function registry that must agree with the
source with nothing checking it. That is
[F12](../plans/rewrite/findings.md)'s shape, which the builtin table exists to
retire, so reintroducing it here would be circular.

Scoped lookup also disposes of the `len` overload: `String.len` and `List.len`
stop being one overload set disambiguated by argument type, and adding `Map.len`
later costs nothing.

**The two compose; it is not either/or.** The desugar decides the *call shape*
(`x.f(a)` is a call with `x` first); `@impl` decides *where the name is found*.
Keep both.

**Cost, stated:** the lookup needs the receiver's type, so it is stage 4, not
stage 3. The desugar alone would have been stage 3. That is the price of scoped
resolution and it is worth paying — the alternative is a registry nobody checks.

**Open:** whether a type may have more than one `@impl` global (Rust allows many
`impl` blocks; one-per-type is simpler and can be relaxed later), and whether
`@impl` on a global that also has state is legal. Neither blocks writing the
stdlib.

## `Color` is a stdlib variant, not a builtin

**Decided.** `Color` moves out of the compiler and into `stdlib/color.yel` as an
ordinary `variant`. `#ff0000` keeps desugaring to `Color.rgba(…)`.

### This justifies `Known` rather than removing it

The compiler still has to *find* `Color` to emit that desugaring. What changes is
the direction:

| | today | after |
|---|---|---|
| who declares `Color` | the compiler, in `stdlib.rs` | `stdlib/color.yel` |
| what `Known::Color` means | a builtin the compiler **provides** | a definition the compiler **requires** |

That second row is the whole point. A *provides* entry with one item looks like
machinery ahead of its need — which is how the panel found `Known` with zero live
registration sites (A9). A *requires* entry is rustc's `#[lang = "…"]`: the
stdlib declares it, the compiler names it, and resolution fails loudly if the
stdlib does not supply it.

So `KnownItems::resolve` keeps its shape — resolve once, report **every** missing
entry, `DefId` not `Option<DefId>` — and finally has a real reason to exist.

### It also shrinks the Group B problem

The review panel measured ~240 narrowed program shapes if all ~60 frozen builtins
were registered into `Definitions`. Every builtin that becomes ordinary stdlib
source **stops being a compiler-registered name** and stops contributing to that
count — it collides the way any other stdlib declaration would, under whatever
rule packages already follow.

`Color` is one of the 9 Type-namespace builtins. The same move is available for
the other 8 and for most of the 51 Component ones, which is a far better answer
to Group B than choosing how much narrowing to accept.

### Sequencing

**It cannot move yet.** `stdlib/*.yel` does not compile — `impl`, `primitive`,
`ref` and `module` are designed and unparsed. So the current one-entry
registration in `stdlib.rs` is a **placeholder with a scheduled removal**, not a
design, and should say so at the registration site.

The order: stdlib source compiles → `Color` moves to `stdlib/color.yel` →
`stdlib.rs`'s registration deletes → `Known::Color` resolves against the loaded
stdlib. Nothing about `Known`'s API changes on that path, which is the sign the
shape was right even though its only entry was not.
