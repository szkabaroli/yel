# Known-bug fixtures

These `.yel` files describe **correct Yel programs that currently fail to
compile**. Each one documents a real bug in the compiler via the test
harness rather than being deleted or silently ignored.

## How the harness works

The `known_bugs_fixtures` integration test in `tests/integration.rs`:

1. Compiles each `<name>.yel` through the full pipeline.
2. Reads the matching `<name>.failure` file — one substring per line.
3. Asserts that compilation fails AND every substring appears in the
   diagnostics/error output.
4. If compilation unexpectedly succeeds, the test **fails loudly** with a
   message pointing you at this directory — that's the signal that the bug
   is fixed and the fixture should be moved to `tests/fixtures/positive/`.

This keeps known bugs visible without blocking CI, and flips to a
red test the moment anyone accidentally (or intentionally) fixes them.

## Current inventory

| Fixture | Bug being documented |
|---|---|
| `option_signal.yel` | Setter for an `option<string>` signal fails wit-component encoding (`failed to classify export [method]app.set-maybe-title`). Root cause: component ABI for setters with option/variant payloads isn't emitted in a shape wit-component accepts. |
| `nested_records.yel` | Setter for a signal of a record-containing-record type fails the same wit-component classification. Indicates the record-payload setter signature is wrong (likely missing pointer indirection for large record flattening). |
| `variant_payload.yel` | Setter for a variant signal with mixed payload shapes (unit / string / u32) fails the same wit-component classification. |
| `global_filter_default.yel` | A closure in a **global property default** panics the compiler at `hir/local_scope.rs:73` — an empty scope stack is indexed while binding the closure parameter in a module-scope initializer. The same closure is fine in a component property default and in a global *function* body, which is what narrows it to the module-scope initializer path. Arrived here from `positive/` on 2026-07-29: it had been written `filter(\|x\| x > 2)`, which `BLOCK_LEVEL_CATCH_ALL` silently swallowed, so the regression it documented was never actually compiled. |

The first three share the same error family: the *write* path for complex
signal types produces a core-module export whose signature wit-component
cannot line up with the declared WIT. Reading these signals works; only
the setter is broken. `global_filter_default.yel` is unrelated — a
front-end scoping bug, and the only entry here that *panics* rather than
returning a diagnostic.

**A panicking fixture's signature is coarse.** The harness renders the
panic message but not its location, so `global_filter_default.failure`
matches on `index out of bounds: the len is 0 but the index is 0` — which
any similar panic would also match. It is the strongest signature the
harness can currently express; a fixture whose bug is a panic is pinned
less precisely than one whose bug is a diagnostic.

## Runtime bugs

The `runtime/` subdirectory holds fixtures for **bugs that compile
cleanly but produce wrong runtime behavior**. The compile-failure
harness above doesn't see them (it doesn't recurse), so they're paired
with assertion tests in `tests/runtime.rs` that compile + execute the
fixture and pin the *currently-observed wrong output*. When the bug is
fixed, the runtime test starts failing — that's the signal to flip the
fixture into `tests/fixtures/positive/` and either delete or invert the
runtime test.

| Fixture | Bug being documented |
|---|---|
| `runtime/s32_to_string_aliasing.yel` | Two consecutive `s32_to_string` calls in one string interpolation both return `(ptr, len)` into a shared static buffer; the second call overwrites the first's contents while the first's `len` lingers, so `concat` reads a truncated prefix of the wrong value. The fixture interleaves two integer reads with `Alpha.x=7` / `Beta.y=11` and observes `"alpha=1 beta=11"` instead of `"alpha=7 beta=11"`. |

## Adding a new known bug

1. Drop `<name>.yel` into this directory.
2. Run the test: `cargo test -p yel-wasm-codegen --test integration known_bugs_fixtures`.
3. Copy the relevant substrings from the failure output into `<name>.failure`
   (one per line; blanks skipped).
4. Commit both files together.

## When a bug is fixed

The test will fail with a message like:

    [nested_records] expected compile to fail (known bug), but it succeeded.
    The bug appears to be fixed — move this fixture to tests/fixtures/positive/
    and delete the .failure file.

Follow the instructions: `mv` the `.yel` into `positive/`, delete the
`.failure` file, re-run the positive suite to capture the new `.wit`
snapshot.
