# fuzz-delta-minimize

> Shrink a failing seed by deleting lines while a *specific* error signature persists

## Why It Matters

A yel-smith seed is 200–600 lines of noise around one triggering construct.
Reasoning about the whole thing is hopeless; a mechanical delta-minimizer gets
you from 500 lines to ~5 in seconds. The key discipline: minimize against a
**fixed error signature**, not just "still fails" — otherwise the minimizer
happily walks from your target bug into a *different* failure and hands you a
repro for the wrong thing. Line-deletion is crude (it can break brace nesting),
but for yel-smith output — which is mostly independent top-level declarations —
it converges to the culprit signal/type declaration reliably.

## Bad

```bash
# "still errors" — drifts to whatever bug survives deletion
sig() { yelc compile -o wasm "$1" 2>&1 >/dev/null | grep -q Error && echo fail; }
```

## Good

```bash
# Pin the exact signature you're hunting.
TARGET="values remaining on stack"
sig() { yelc compile -o wasm "$1" 2>&1 >/dev/null | grep -oE "$TARGET" | head -1; }

cp seed.yel min.yel
changed=1
while [ $changed -eq 1 ]; do
  changed=0
  for ln in $(seq $(wc -l < min.yel) -1 1); do
    sed "${ln}d" min.yel > try.yel
    [ "$(sig try.yel)" = "$TARGET" ] && { cp try.yel min.yel; changed=1; }
  done
done
cat min.yel   # the surviving declaration IS the trigger
```

The output is the minimal set of declarations that still reproduces *that*
signature. The surviving signal's type (`option<result<result<…>>>`,
`list<tuple<f64, s32>>`, `func(a: s32)`) names the bug. Then hand it to
`fuzz-narrow-by-probe` to isolate the exact axis.

## Minimization can diverge — verify the repro is faithful

You minimize against an error *signature*, but **more than one bug can share a
signature.** The delta-minimizer greedily keeps any deletion that preserves the
signature, so it can walk *out* of the original bug and *into* a neighboring one
that happens to fail the same way — handing you a tiny repro for a **different**
bug than the seed actually hit. This is not hypothetical: minimizing a
`found i64` seed once produced a clean repro whose real cause was an unrelated
construct that also printed `found i64`; fixing it left the original seed still
broken.

Guard against it — before you invest in a fix, confirm the minimal repro is the
*same* bug as the seed:

- **Re-check the seed after the fix.** The real test of "did I fix this cluster"
  is that the *original seeds* now pass — not that your minimized repro does. If
  the repro is green but the seeds are still red, you fixed a different bug
  (worth having, but say so, and keep going).
- **Watch for a mid-minimization signature flip.** If the error's *sub-text*
  (the function name, the offset region, the `expected/found` types) changes
  partway through, the minimizer likely crossed into another bug. Target a
  tighter signature (include the failing function or the exact types), not just
  the top-line category.
- **Sanity-check the culprit is present in the seed.** If your minimized repro
  centers on construct X, grep the original seed for X. If it's absent, you
  diverged.

## See Also

- [fuzz-narrow-by-probe](fuzz-narrow-by-probe.md) - Turn the minimized decl into a clean, controlled repro matrix
- [fuzz-categorize-signatures](fuzz-categorize-signatures.md) - Where the target signature comes from
