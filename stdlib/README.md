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
