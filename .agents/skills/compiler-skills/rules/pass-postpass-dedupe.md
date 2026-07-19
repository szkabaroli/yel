# pass-postpass-dedupe

> Deduplicate structurally-identical output in a separate post-pass via normalized hashing

## Why It Matters

Code generators emit many blocks that are identical modulo allocation order; collapsing them shrinks code size, but doing it inline complicates the emitter. yel runs a dedicated post-pass: `dedupe_update_blocks` in `crates/yel-core/src/lir/dedupe.rs` executes after all blocks are emitted. It selects candidate per-(boundary, signal) update blocks, structurally hashes each (`hash_block`/`hash_ops`) with a `SlotNormalizer` that renumbers slot ids so blocks identical modulo allocation order hash equal, iterates to a fixed point, then builds a remap and rewrites references. Keeping it a separate pass leaves the emitter simple and the dedupe logic self-contained.

## Bad

```rust
fn emit_update_block(&mut self, block: Block) -> BlockId {
    // dedupe inlined into emission: emitter now owns hashing, normalization,
    // and a global table -> two concerns tangled, neither testable alone
    let key = self.hash_with_normalized_slots(&block);
    if let Some(id) = self.seen.get(&key) { return *id; }
    /* ... */
}
```

## Good

```rust
// separate post-pass, run after emission is complete
fn dedupe_update_blocks(lir: &mut Lir) {
    let mut remap = HashMap::new();
    loop {
        let mut by_hash: HashMap<u64, BlockId> = HashMap::new();
        for id in candidate_update_blocks(lir) {
            let mut norm = SlotNormalizer::default(); // slot ids renumbered
            let h = hash_block(&lir[id], &mut norm);
            match by_hash.entry(h) {
                Entry::Occupied(e) => { remap.insert(id, *e.get()); }
                Entry::Vacant(e) => { e.insert(id); }
            }
        }
        if remap_is_fixed_point(&remap) { break; } // iterate to fixed point
    }
    rewrite_block_refs(lir, &remap);
}
```

## See Also

- [intern-dedupe-tables](intern-dedupe-tables.md) - Interning dedupes values during construction
- [test-deterministic-output](test-deterministic-output.md) - Normalized hashing keeps output stable
