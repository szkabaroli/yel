---
name: fuzzer-debugging
description: >
  The method for hunting compiler bugs in yel with the yel-smith fuzzer:
  measure honestly, triage a wall of failures into the highest-leverage cluster,
  delta-minimize a random program to a one-line repro, read the failing WASM
  function/offset out of the encoder, fix the root cause, and prove it with an
  execution round-trip (never shipping validate-but-wrong code). A workflow, not
  a bug list — the durable techniques and the symptom→suspect heuristics, not a
  snapshot of any function. Use when a fuzzer seed, a `wasm-tools validate`
  error, or a hung test points at a codegen or lowering bug. Invoke with
  /fuzzer-debugging.
license: Apache-2.0
metadata:
  author: yel
  version: "1.0.0"
  sources:
    - crates/yel-smith (random valid-Yel generator)
    - "wasm-tools (validate / print)"
    - A fuzzer-driven bug-hunting session (pass rate 19/200 → ~110/200)
---

# Fuzzer-Driven Compiler Debugging

The house method for finding and killing miscompilation bugs in yel. yel-smith
emits random *valid* Yel; each program is a probe into the compiler
(`AST → HIR → THIR → LIR → WASM`). A failing probe is a **compiler bug**, not a
bad program — never work around the fuzzer or weaken it to stop emitting the
construct. This skill is the *workflow* for turning a pile of failing seeds into
a short list of root-cause fixes; it deliberately does not catalog specific bugs
or name internal functions, because those move and the method does not.

## The three failure modes (know which one you have)

Every failing seed is exactly one of these, and they need different tools:

1. **Fails validation** — the encoder's built-in validation rejects the module;
   `yelc compile` errors and writes no bytes. Visible as
   `caused by: type mismatch: expected X, found Y (at offset 0x…)`.
2. **Validates but round-trips wrong** — valid WASM that computes the wrong
   value (a list comes back `[0,0,0]`). Only an execution round-trip catches it;
   validation is blind.
3. **Runtime hang** — valid WASM that never terminates (a bad stride corrupts
   the allocator; a garbage length drives a near-infinite copy). The test just
   stalls. Fuel turns this into a fast, located failure.

## When to Apply

- A fuzzer run has failing seeds and you need to know what to fix first.
- `wasm-tools validate` (or the encoder) rejects a module with a type mismatch.
- A round-trip test returns a wrong value from a `set` → `get`.
- A test hangs and you suspect a non-terminating loop in generated code.

## The loop

**measure → categorize → minimize → narrow → locate → fix → verify.**

| Step | Rule | One line |
|------|------|----------|
| measure | [`fuzz-measure-clean-build`](rules/fuzz-measure-clean-build.md) | Force a clean release build first; a stale binary reports last run's numbers |
| categorize | [`fuzz-categorize-signatures`](rules/fuzz-categorize-signatures.md) | Histogram failures by error signature; fix the tallest bar, not seed #1 |
| minimize | [`fuzz-delta-minimize`](rules/fuzz-delta-minimize.md) | Delete lines while a *specific* signature persists, down to a few lines |
| narrow | [`fuzz-narrow-by-probe`](rules/fuzz-narrow-by-probe.md) | Hand-write a variant matrix; vary one axis to isolate the exact trigger |
| locate | [`fuzz-dump-core-module`](rules/fuzz-dump-core-module.md) | Dump the pre-validation artifact; disassemble to find the failing function/offset |
| verify | [`verify-roundtrip-not-validate`](rules/verify-roundtrip-not-validate.md) | Validates-but-wrong needs an execution round-trip, not a compile check |
| verify | [`verify-fuel-in-tests`](rules/verify-fuel-in-tests.md) | Meter execution with fuel so a hang traps fast instead of stalling the suite |
| verify | [`verify-loud-over-silent`](rules/verify-loud-over-silent.md) | Prefer a loud error to validate-but-wrong; a hang is worse than both |

Plus one heuristic index for the fix step:

- [`symptom-to-suspect`](rules/symptom-to-suspect.md) - Map a symptom (wrong stride, dropped arg, zeroed value, depth-limited nesting, cleanup hang) to the general bug shape to check — no internal function names, because those rot.

---

## Commands

```bash
# Measure (clean build!)
touch crates/yelc/src/main.rs && cargo build --release -p yelc -p yel-smith
pass=0; fail=0
for s in $(seq 1 200); do
  ./target/release/yel-smith --seed $s > /tmp/f.yel 2>/dev/null
  if ./target/release/yelc compile -o wasm /tmp/f.yel > /tmp/f.wasm 2>/dev/null \
     && wasm-tools validate /tmp/f.wasm 2>/dev/null; then pass=$((pass+1)); else fail=$((fail+1)); fi
done
echo "PASS=$pass FAIL=$fail"

# Categorize a failing set by normalized signature (strip offsets / type ids)
for s in $FAILING; do
  ./target/release/yel-smith --seed $s > /tmp/f.yel 2>/dev/null
  err=$(./target/release/yelc compile -o wasm /tmp/f.yel 2>&1 >/tmp/f.wasm)
  [ -z "$err" ] && err=$(wasm-tools validate /tmp/f.wasm 2>&1 | head -1)
  echo "$err" | grep -oiE "found \(?ref[^)]*|found (f32|f64|i64|i32)|values remaining|not yet supported|invalid IR: [a-z_]*"
done | sed 's/[0-9]//g' | sort | uniq -c | sort -rn
```

Use the fast `./target/debug/yelc` for compile checks during minimization; the
freshly-built release binary only for the 200-seed measurement.

## The mindset that makes this work

- **A valid random program that miscompiles is always a compiler bug.** Fix the
  compiler, never the fuzzer.
- **Bisect before you theorize.** A 500-line seed hides one bug in one
  construct; minimize first.
- **Read the generated code.** The disassembly at the failing offset tells you
  exactly which push/store/stride is wrong. Reason from instructions, not
  guesses.
- **Trust the symmetry.** A composite value crosses the boundary through mirror
  paths (produce / consume / clean up); a bug in one usually has a twin.
- **Loud beats silent; a located crash beats a hex dump; a fast trap beats a
  hang.** Every choice should push the next failure toward the good end of that
  ordering.
