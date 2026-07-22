# fuzz-measure-clean-build

> Force a clean release build before every fuzzer measurement — a stale binary reports last run's numbers

## Why It Matters

The fuzzer harness runs `./target/release/yelc`. If you edit the compiler but
`cargo build` decides nothing it tracks changed (or you forget to rebuild), the
loop happily runs the **old** binary and you "measure" a fix that isn't in the
tested code. This wastes a whole cycle and, worse, produces a false PASS number
that sends you chasing the wrong thing. It bit this session repeatedly until the
`touch` became reflexive. Touch a file the release build definitely recompiles
(`crates/yelc/src/main.rs`) so the binary is guaranteed current.

## Bad

```bash
# edited crates/yel-core/... but only rebuilt the debug crate under test
cargo build -p yelc            # may no-op if the release profile looks up-to-date
./target/release/yelc ...      # runs a stale binary → lies about the fix
```

## Good

```bash
touch crates/yelc/src/main.rs
cargo build --release -p yelc -p yel-smith
# now the fuzzer loop runs the code you actually edited
```

Two builds are in play: the fast **debug** `./target/debug/yelc` for quick
compile checks during minimization, and the **release** binary for the 200-seed
measurement. Keep them straight — verify a fix on debug, *measure* it on a
freshly-built release.

## See Also

- [fuzz-categorize-signatures](fuzz-categorize-signatures.md) - What to do with the measured failures
- [verify-roundtrip-not-validate](verify-roundtrip-not-validate.md) - Validation PASS ≠ correct
