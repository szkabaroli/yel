# pass-deferred-emission

> Pre-allocate ids and defer emitting bodies until their dependencies are known

## Why It Matters

Sometimes a body needs a block id that hasn't been computed yet, or its contents depend on analysis that only finishes after the structural walk — a chicken-and-egg. yel's `crates/yel-core/src/lower_to_lir/blocks.rs` resolves this by recording handler bodies and derived-signal update bodies during the structural walk (`deferred_handler_bodies`, `deferred_derived_bodies`) but emitting them LATER, after signal-dependency resolution finalizes `effects_by_signal`. So the deferred body still targets the right block, the `BlockId` is pre-allocated and stashed (`pending_block_id_override`) and referenced before the body is emitted. Deferring keeps emission correct without forcing a premature ordering.

## Bad

```rust
fn lower_handler(&mut self, h: &Handler) {
    // emits the body now, but effects_by_signal isn't resolved yet,
    // so it wires up the wrong (or missing) dependent blocks
    let block = self.emit_block(h.body);
    self.bind_effects(block, &self.effects_by_signal); // incomplete!
}
```

## Good

```rust
fn lower_handler(&mut self, h: &Handler) {
    let id = self.alloc_block_id();            // pre-allocate the id
    self.deferred_handler_bodies.push((id, h)); // record, emit later
}

fn finish(&mut self) {
    self.resolve_signal_deps();                // fills effects_by_signal
    for (id, h) in std::mem::take(&mut self.deferred_handler_bodies) {
        self.pending_block_id_override = Some(id);
        self.emit_block(h.body);               // now deps are known
    }
}
```

## See Also

- [id-stable-across-passes](id-stable-across-passes.md) - Pre-allocated ids stay valid
- [pass-lowering-struct](pass-lowering-struct.md) - The struct holds the deferred buffers
