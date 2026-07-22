# fuzz-narrow-by-probe

> From the minimized declaration, hand-write a matrix of type/construct variants to isolate the exact trigger

## Why It Matters

Delta-minimization hands you a *sufficient* repro (`list<tuple<f64, s32>>`) but
not the *precise* axis. Is it the tuple? the float? the float being first? the
list wrapper? Guessing wastes fixes. A tiny matrix of controlled variants —
each changing one axis — pins it in one build cycle and often reveals a
surprising boundary that reframes the bug. A representative example:
`list<tuple<f64, s32>>` failed but `list<tuple<s32, f64>>` passed — the
*order*-dependence immediately ruled out "floats in tuples" and pointed instead
at code that only handled the element's first field one way (a dead-but-
validated helper). One well-chosen probe changed the whole diagnosis.

## Good

```bash
i=0
while IFS= read -r decl; do
  [ -z "$decl" ] && continue; i=$((i+1))
  cat > /tmp/c.yel <<EOF
package yel:p$i@0.1.0;
component App { v: $decl "x" }
EOF
  cause=$(./target/debug/yelc compile -o wasm /tmp/c.yel 2>&1 >/dev/null \
          | grep -oiE "caused by: [^(]*|invalid IR: [a-z_: ]*" | head -1)
  echo "[$decl] ${cause:-OK}"
done <<'DECLS'
list<tuple<f64, s32>> = [(1.5, 2)];
list<tuple<s32, f64>> = [(2, 1.5)];
tuple<f64, s32> = (1.5, 2);
list<tuple<f64, s32>> = [];
DECLS
# [list<tuple<f64, s32>>...] FAIL        <- fails with a value
# [list<tuple<s32, f64>>...] OK          <- order matters!
# [tuple<f64, s32>...]       OK          <- not the tuple itself
# [list<...>> = [];]         OK          <- not the getter; it's constructing the literal
```

Vary **one axis at a time**: element order, scalar width (i32 vs f64 vs i64),
nesting depth, wrapper (`option`/`result`/`list`/`tuple`/record), empty vs
non-empty literal, default-present vs absent. The `= []` / no-default probe is
especially diagnostic: if the empty case passes, the bug is in *constructing*
the value, not in the getter/materializer emitted regardless of contents.

## See Also

- [fuzz-dump-core-module](fuzz-dump-core-module.md) - Once narrowed, read the exact failing instruction
- [symptom-to-suspect](symptom-to-suspect.md) - Order-/depth-/width-dependence each point at a different bug shape
