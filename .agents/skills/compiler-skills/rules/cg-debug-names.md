# cg-debug-names

> Emit a name/debug section so generated artifacts stay inspectable

## Why It Matters

Generated machine code is opaque: a validator error or a wrong result shows up as `func[42]` / `local 7`, and you debug from a hex dump. Carrying human-readable names through lowering and emitting them into the target's debug/name section makes disassembly, profiler output, and validator errors legible — for near-zero cost, and strippable in release. yel keeps a `BlockDebugName` per block (in `ctx.block_names`, a `RefCell<HashMap<(DefId, BlockId), BlockDebugName>>`) and emits a WASM **name section** (`wasm/codegen/name_section.rs`), so a disassembled module shows `$Counter::update_count` instead of an index.

## Bad

```rust
// emit functions/blocks anonymously; every tool shows numeric indices
module.function(ty, &body); // who is func[42]? good luck
```

## Good

```rust
// thread a debug name alongside each emitted entity…
let name = ctx.block_name(def_id, block_id); // "Counter::update(count)"
// …and write a name section so disassembly/validators print it
name_section.functions.append(func_index, &name);
```

## See Also

- [ctx-interior-mutability](ctx-interior-mutability.md) - Where the `block_names` side table lives
- [diag-no-silent-fallback](diag-no-silent-fallback.md) - Both fight "impossible to trace from a hex dump"
- [test-snapshot-golden](test-snapshot-golden.md) - Named output makes snapshots readable
