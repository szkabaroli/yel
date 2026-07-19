# cg-lower-to-primitives

> Lower high-level constructs to generic target ops during lowering, not via a runtime library

## Why It Matters

yel is AOT-compiled to a self-contained WASM component, so high-level and reactive constructs are expanded to generic `LirOp`s inline at the THIR→LIR stage (crates/yel-core/src/lower_to_lir/signals_inline.rs, lifecycle_inline.rs): a signal write or component mount becomes a neutral sequence of `MemConst` / `StoreF32Addr` / `CallBlock` / `RegistryAlloc` ops. The compiler deliberately introduces no `yel-runtime` crate and no new WIT host imports — everything resolves to ops the backend already emits. This keeps the output dependency-free and the IR honest about what actually executes.

## Bad

```rust
// Emit a call to a runtime helper that must be linked/imported separately
lir.push(LirOp::CallImport("yel_runtime::signal_write")); // needs a runtime crate / WIT import
```

## Good

```rust
// Expand inline into ops the backend already knows how to emit
fn lower_signal_write(&mut self, sig: SignalId, val: ExprId) {
    self.push(LirOp::MemConst(addr));
    self.push(LirOp::StoreF32Addr { addr, value: val });
    self.push(LirOp::CallBlock(notify_block)); // no runtime, no new imports
}
```

## See Also

- [ir-layered-lowering](ir-layered-lowering.md) - Each layer lowers toward primitives the next understands
- [pass-deferred-emission](pass-deferred-emission.md) - Defer concrete emission until the backend stage
