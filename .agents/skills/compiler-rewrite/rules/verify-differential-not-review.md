# verify-differential-not-review

> Code review cannot clear a rewritten stage — diff old against new artifacts over thousands of inputs

## Why It Matters

Reviewing a rewritten compiler stage means reading several thousand lines of new
code and asking "does this do the same thing as several thousand lines of old
code I also have to hold in my head?" Nobody can do that. Review catches style,
structure, and anti-spec violations — all worth catching, none of them
correctness. A stage that reviews beautifully can silently drop the trailing
element of a list, mis-order two DOM ops, or widen an integer.

The only tool that scales here is **differential testing**: run both
implementations over the same input and compare their output artifacts. yel is
unusually well set up for this, and the reason is `yel-smith` — a generator of
random *valid* Yel that already exists and is already trusted. It turns
verification from "convince yourself by reading" into "here are 5000 programs
where old and new agree, and 3 where they don't, and here are the 3."

Compare **artifacts**, not internals. The new stage's IR is supposed to look
nothing like the old one, so IR-level comparison is meaningless. WIT text, DOT
text, WASM bytes, diagnostic output, and DOM-op traces are the shared language
of the two implementations.

## Bad

```markdown
## PR: rewrite THIR→LIR lowering

Reviewed by two agents. Structure is clean, passes the anti-spec, no god pass,
walker is shared. `cargo test` green. LGTM.
```

Green `cargo test` here means 91 fixtures and 85 execution tests agreed — a
corpus that covers maybe a few hundred distinct constructs, hand-picked over
time to cover what someone thought of. The fuzzer reaches combinations nobody
wrote a fixture for, and that is exactly where a rewritten lowering breaks.

## Good

```bash
# Both implementations live simultaneously behind a switch (see cutover rule).
touch crates/yelc/src/main.rs && cargo build --release -p yelc -p yel-smith

diverge=0; newfail=0; newpass=0
for s in $(seq 1 5000); do
  ./target/release/yel-smith --seed $s > /tmp/f.yel 2>/dev/null || continue

  YEL_STAGE_NEW=0 ./target/release/yelc compile -o wit /tmp/f.yel >/tmp/old.wit 2>/tmp/old.err; o=$?
  YEL_STAGE_NEW=1 ./target/release/yelc compile -o wit /tmp/f.yel >/tmp/new.wit 2>/tmp/new.err; n=$?

  if   [ $o -eq 0 ] && [ $n -ne 0 ]; then newfail=$((newfail+1)); echo "REGRESS  seed=$s"
  elif [ $o -ne 0 ] && [ $n -eq 0 ]; then newpass=$((newpass+1)); echo "IMPROVE  seed=$s"
  elif [ $o -eq 0 ] && ! cmp -s /tmp/old.wit /tmp/new.wit; then
       diverge=$((diverge+1)); echo "DIVERGE  seed=$s"
  fi
done
echo "diverge=$diverge new-failures=$newfail new-passes=$newpass"
```

How to read the three buckets:

| Bucket | Meaning | Action |
|---|---|---|
| `REGRESS` | Old compiled it, new doesn't | **Blocking.** Minimize and fix. |
| `DIVERGE` | Both compiled, different artifact | **Blocking** until each is explained; a rewrite should be output-identical. |
| `IMPROVE` | New compiles what old couldn't | Good — but verify by round-trip that the output is *right*, not merely valid. |

Then the parts a WIT diff cannot see:

- **Semantics need execution.** WIT and DOT are shapes; only the Wasmtime
  execution harness catches "valid WASM that computes the wrong value." Extend
  the execution suite for anything the stage newly touches.
- **Minimize divergences, don't stare at them.** A 400-line seed hides a
  one-construct bug — delta-minimize with the divergence as the predicate,
  exactly as in [`/fuzzer-debugging`](../../fuzzer-debugging/SKILL.md).
- **Raise the seed count as the stage lands.** A few hundred while iterating,
  several thousand before merge. Seeds cost nothing.
- **Keep the failing seeds.** Every divergence found becomes a fixture, so the
  next stage inherits the coverage this one bought.

Review still happens — for the anti-spec, the contract, and the structure. It
just never substitutes for the number.

## See Also

- [oracle-freeze-behaviour](oracle-freeze-behaviour.md) - The corpus this sweep runs against
- [verify-ratchet-never-down](verify-ratchet-never-down.md) - Turning these numbers into a merge gate
- [`/fuzzer-debugging`](../../fuzzer-debugging/SKILL.md) - Minimizing and triaging a divergence once you have one
