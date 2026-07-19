# test-execution-e2e

> Execute the emitted artifact end-to-end to catch "valid output, wrong behaviour" bugs

## Why It Matters

Structural tests that inspect bytecode shape prove the compiler emitted *something plausible*, not that it emitted something *correct*: a reactive effect can compute the wrong string and a dispatch can route to the wrong handler while the IR still looks valid. Only running the code catches those. In yel, `crates/yel-wasm-codegen/tests/execution.rs` compiles a source, instantiates the WASM component under Wasmtime, wires the `yel:ui/dom` host imports to recording closures, drives the exports, and asserts on the observed ordered sequence of DOM ops.

## Bad

```rust
// "It compiled and contains a call" — says nothing about behaviour
let wasm = compile(SRC);
assert!(wasm_contains_call(&wasm, "set_text")); // wrong text would still pass
```

## Good

```rust
// Run it; assert on what the program actually did, as an ordered subsequence
let recorded = run_under_wasmtime(compile(SRC), record_dom_imports());
assert_subsequence(&recorded, &[
    DomOp::CreateElement("button"),
    DomOp::SetText("count: 0"),
]); // incidental reordering won't false-fail; wrong text will
```

## See Also

- [test-known-bugs-ignore](test-known-bugs-ignore.md) - What to do when execution reveals a bug you can't fix yet
- [test-deterministic-output](test-deterministic-output.md) - Ordered assertions need stable ordering
