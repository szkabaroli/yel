# fuzz-dump-core-module

> The encoder validates before writing, so dump the pre-validation artifact to read the failing function and offset

## Why It Matters

The component encoder runs validation and, on failure, returns an error
**without writing any bytes** — so `yelc compile -o wasm` gives you `expected
i32, found f64 (at offset 0x…)` but no file to disassemble. The offset alone is
nearly useless; you need the *function* it lives in and the surrounding
instructions. The fix is a temporary, env-gated dump of the raw core-module
bytes *before* the component encoder runs, then `wasm-tools validate` +
`wasm-tools print` on that. `validate` names the failing **function index**;
`print` gives readable WAT to reason from. This is what turns a bare offset into
"function N pushes two values before a `call` that takes one" — an exact root
cause.

## Good

```rust
// In the back-end, right before the module bytes are handed to the component
// encoder, add a TEMPORARY env-gated dump (remove before committing):
if let Ok(path) = std::env::var("YEL_DUMP_CORE") {
    std::fs::write(&path, &module_bytes).ok();
}
```

```bash
YEL_DUMP_CORE=/tmp/core.wasm ./target/debug/yelc compile -o wasm bug.yel >/dev/null 2>&1
wasm-tools validate /tmp/core.wasm            # -> "func N failed to validate ... expected i32, found f64"
wasm-tools print    /tmp/core.wasm > core.wat # readable WAT
# jump to the named function body:
awk '/\(func .*;N;\)/{p=1} p{print NR": "$0} p&&/\(func .*;M;\)/{exit}' core.wat   # M = N+1
```

Reading the WAT is the whole game: count the values pushed before a `call` vs the
callee's params; check the `i32.const <stride>` in a copy loop against the
element's real width; see whether a struct field is `f64` or `i32`; spot a loop
whose index never advances. **Reason from the emitted instructions, not from a
mental model of what the emitter "should" do.**

Locate the dump point by searching the back-end for where the finished
core-module bytes meet the component encoder — the exact file/variable name will
have moved since this was written, so grep, don't assume. And remove the hook
before committing: it is a debugging scaffold, not a feature.

## See Also

- [fuzz-narrow-by-probe](fuzz-narrow-by-probe.md) - Narrow first so the dumped module is small
- [symptom-to-suspect](symptom-to-suspect.md) - Match the WAT you read to a general bug shape
