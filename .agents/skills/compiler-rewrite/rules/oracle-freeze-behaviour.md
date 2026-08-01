# oracle-freeze-behaviour

> Capture what the current compiler *does* as an executable corpus before a single line of the new one exists

## Why It Matters

The old compiler is unreadable, but it is correct in thousands of small ways
nobody has written down: which programs are rejected and with what wording, how
a nested `for` inside an `if` orders its DOM ops, what the WIT looks like for an
option-of-record. That knowledge lives only in the binary's behaviour. The
moment you start rewriting, you begin losing it — and you lose it *silently*,
because a rewrite that drops an unwritten behaviour still compiles and still
passes the tests you happened to keep.

Freezing means converting behaviour into artifacts on disk that a future
implementation can be diffed against. Do it while the old compiler is still the
only compiler, so there is no temptation to "fix" a captured output.

The capture is not just the existing test suite. The existing suite covers what
someone thought to test; the fuzzer covers what nobody did. Both go into the
freeze, and the fuzzer corpus is the larger half.

## Bad

```bash
# "We have tests, that's the baseline." Start rewriting.
cargo test --workspace   # 396 tests pass. Begin stage 1.
```

The suite pins 91 WIT files and 85 execution behaviours. It does not pin the
diagnostic wording for a program no fixture covers, the DOT output for a shape
no fixture has, or any of the thousands of constructs `yel-smith` can emit. Six
weeks in, a user reports that a nested option-of-list stopped working — and
there is nothing to diff against, because the old compiler is gone.

## Good

```bash
# 1. Record the numbers, commit them into plans/rewrite.md as the baseline.
cargo test --workspace 2>&1 | tail -30
cargo test -p yel-wasm-codegen --test execution 2>&1 | tail -5

# 2. Freeze a corpus far wider than the fixtures — seeds are free.
mkdir -p corpus/{src,wit,dot,wasm}
touch crates/yelc/src/main.rs && cargo build --release -p yelc -p yel-smith
for s in $(seq 1 2000); do
  ./target/release/yel-smith --seed $s > corpus/src/$s.yel 2>/dev/null || continue
  ./target/release/yelc compile -o wit corpus/src/$s.yel > corpus/wit/$s.wit 2>/dev/null
  ./target/release/yelc compile -o dot corpus/src/$s.yel > corpus/dot/$s.dot 2>/dev/null
  ./target/release/yelc compile -o wasm corpus/src/$s.yel > corpus/wasm/$s.wasm 2>/dev/null
done

# 3. Record which seeds FAIL today and how. A rewrite that fixes them is a win;
#    one that fails *different* seeds is a regression wearing the same number.
for s in $(seq 1 2000); do
  err=$(./target/release/yelc compile -o wasm corpus/src/$s.yel 2>&1 >/dev/null)
  [ -n "$err" ] && echo "$s|$(echo "$err" | head -1)"
done > corpus/known-failures.txt
```

Now "did stage N change behaviour?" is a `cmp` over 2000 programs, answerable in
seconds, for the whole life of the rewrite.

## See Also

- [oracle-never-rebless](oracle-never-rebless.md) - The corpus is worthless the moment you regenerate it from the new compiler
- [verify-differential-not-review](verify-differential-not-review.md) - What the frozen corpus is for
- [`/fuzzer-debugging`](../../fuzzer-debugging/SKILL.md) - Generating and triaging the seed sweep
