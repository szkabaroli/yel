# Stdlib ↔ compiler interface — a concrete sketch

Not a decision. A worked example of the three ideas that survived the general
discussion, so they can be judged against real code rather than in the abstract:
**one manifest generating both sides**, **a distinct lexical namespace for
primitives**, and **provides and requires kept apart**.

## The manifest — one file, both sides generated

The agreement problem ("the declared type and the op's real signature must
match") is not solved by checking. It is solved by there being one thing.

```toml
# compiler/primitives.toml — the ONLY place an op exists.

[op."array.len.any"]
sig    = "(ref) -> s32"
encode = "ArrayLen { shape = Any }"

[op."array.len.i8"]
sig    = "(ref) -> s32"
encode = "ArrayLen { shape = I8 }"

[op."array.get.any"]
sig    = "(ref, s32) -> ref"
encode = "ArrayGet { shape = Any }"

[op."array.get.i8"]
sig    = "(ref, s32) -> s32"
encode = "ArrayGetU { shape = I8 }"

[op."array.new.any"]
sig    = "(s32, ref) -> ref"
encode = "ArrayNewDefault { shape = Any }"

[op."i32.add"]
sig    = "(s32, s32) -> s32"
encode = "I32Add"

[op."ref.erase"]
sig    = "<T>(T) -> ref"
encode = "Nop"          # representation-identical; erasure is type-level only

[op."ref.restore"]
sig    = "<T>(ref) -> T"
encode = "RefCast"
```

The compiler **generates** its op enum and encoder dispatch from this. The stdlib
**reads** it as declarations. Neither hand-writes the other's half, so they
cannot disagree — no registration-time verification pass, because there is
nothing to verify.

The three shapes (`any`, `i8`, `i64`) are not a design choice here. WASM-GC has
no polymorphic array access: `(array i8)` and `(array (ref any))` are different
types with different instructions. Grain's `WasmArrayRef` lands on the same
partition. It is the target's, not ours.

## The namespace — `#op` is not an identifier

```yel
#array.len.i8(bytes)
```

No `@unsafe`, no `primitive` keyword, no attribute. A `#` form is not an
identifier, so it cannot collide, cannot be shadowed, and greps exactly. Whether
a module may *write* one is a property of how it was loaded (embedded stdlib vs
user file), not of anything written in it — so the gate needs no syntax either.

Zig's entire builtin surface works this way and scales past a hundred entries
without ambiguity.

---

## Example 1 · strings — unwrapped, so the shape shows

`string` **is** `(array (mut i8))`. There is no wrapper to unwrap.

```yel
extend string {
    len: func(self: string) -> s32 {
        #array.len.i8(self)
    }

    starts-with: func(self: string, prefix: string) -> bool {
        let n = #array.len.i8(prefix);
        if n > #array.len.i8(self) { return false; }

        for i in 0..n {
            if #array.get.i8(self, i) != #array.get.i8(prefix, i) { return false; }
        }
        true
    }
}
```

Every byte op is `.i8`. **`array.yel`'s `ref` floor cannot reach a string** — a
`(array i8)` is not a `(array (ref any))`, and no accessor covers both. Strings
need their own ops, and that is not a wart; it is the same partition the manifest
already encodes.

## Example 2 · lists — generic, so erasure shows

```yel
extend list<T> {
    len: func(self: list<T>) -> s32 {
        #array.len.any(#ref.erase(self))
    }

    filter: func(self: list<T>, keep: func(item: T) -> bool) -> list<T> {
        let matched = 0;
        for item in self {
            if keep(item) { matched = matched + 1; }
        }

        let source = #ref.erase(self);
        let out = #array.new.any(matched, #array.get.any(source, 0));
        //                                ^ still wrong when matched == 0 and
        //                                  when self is empty. See GAP 3.

        let next = 0;
        for item in self {
            if keep(item) {
                #array.set.any(out, next, #ref.erase(item));
                next = next + 1;
            }
        }
        #ref.restore(out)
    }
}
```

`#ref.erase` encodes to nothing — `list<T>` and `ref` are the same bits. The cast
is type-level only, which is why erasure costs nothing here and why the
`WasmRef.fromGrain`/`toGrain` round-trip in Grain is also free.

**GAP 3 is still unsolved and this makes it concrete.** `array.new` needs an
initial element; `filter` has none before its loop runs. The honest fixes are a
nullable element type, or an `array.new.uninit` op paired with a proof every
slot is written. Neither is chosen.

## Example 3 · numbers, and operators as builtins

```yel
extend s32 {
    @inline
    add: func(self: s32, other: s32) -> s32 { #i32.add(self, other) }
}
```

with `a + b` desugaring to `add(a, b)`.

**This is the case where the design has a real cost, and it is worth stating
before adopting it.**

Swift does exactly this — `Int.+` is a stdlib function over `Builtin.add_Int64`
— and it needs `@_transparent` to survive: a *mandatory* inlining pass that runs
**before optimisation and at `-Onone`**. Without it every integer addition in a
debug build is a real call: unwrap, call, rewrap.

**Yel has no such pass.** Optimisation is delegated wholesale to Binaryen, which
inlines at `-O` and not otherwise. So operators-as-stdlib-functions means:

| build | today (compiler-known op) | operators as stdlib |
|---|---|---|
| release | one instruction | one instruction, after Binaryen inlines |
| **debug** | one instruction | **a function call per arithmetic operation** |

Three ways out, and the choice is the actual decision:

1. **Keep operators compiler-known.** `+` never becomes a call. Costs: operators
   are special forever, and a user type can never define one.
2. **Add yel's own `@inline` and honour it at LIR lowering** — splice the body
   unconditionally, before Binaryen sees anything. This is `@_transparent`,
   scoped to one attribute and one pass. It is a small pass, and it is the only
   option that makes operators ordinary *and* keeps debug builds honest.
3. **Adopt it without an inliner.** Debug builds get slower by a constant factor
   on all arithmetic. Cheapest now, and the kind of thing that is very hard to
   undo once the stdlib is written against it.

(2) is the one that generalises: the same pass makes every thin stdlib wrapper
free, not just operators, which is the whole reason Swift built it. It also
converts axis C — wrap or not — from a permanent representation commitment into
an ordinary trade, because a wrapper you can always inline away costs nothing.

## What this sketch does *not* solve

- **GAP 3** — `array.new` with no initial element.
- **Requires-direction items.** The manifest covers *provides*. `#ff0000 →
  Color.rgba` and `match`'s variant-discriminant accessor are the compiler
  *requiring* a definition; that stays a separate table with a different failure
  mode (unfilled requirement = hard error; unused provided op = warning).
- **Whether `extend` or `@impl(type)` spells the method set.** Orthogonal to all
  of the above; see `stdlib/README.md`.
