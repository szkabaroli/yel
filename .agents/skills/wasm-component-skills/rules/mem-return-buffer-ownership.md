# mem-return-buffer-ownership

> Free returned linear-memory buffers in the guest's exported `post-return`, after the host has lifted them — never inline at return time

## Why It Matters

When a guest returns a `string` or `list`, the bytes live in the guest's own linear memory and travel to the host as a `(ptr, len)` fat pointer (yel builds these via `emit_store_fat_ptr` / `emit_pack_fat_ptr_to_i64` in `wasm/runtime/memory.rs`, backed by the `emit_alloc` / `emit_free` allocator over `AllocatorGlobals`). The canonical ABI defines a precise hand-off: the host *lifts* (copies out) those bytes after the call returns, and only then is the guest free to reclaim them. The mechanism for that reclamation is the guest-exported **`post-return`** function — a live, current part of the canonical ABI, not a retired one. The reference ABI calls it right after results are lifted (`call_and_trap_on_throw(opts.post_return, flat_results)`), passing the flattened result values so the guest can free exactly the buffers it returned. So the *callee* owns and frees its return buffers, in `post-return`, *after* the host has read them — not inline at return time. Free too eagerly (e.g. `emit_free` on the buffer before returning it) and the host lifts reclaimed memory; never free and you leak. (Note the easy trap: some host embedders — e.g. wasmtime's typed call API — invoke `post-return` for you, so as a *host* you may never call it by hand. That's a host-library convenience, **not** the spec dropping `post-return`; the guest still must export it to clean up.)

## Bad

```rust
// build the return (ptr, len), then immediately free the buffer
func.instruction(&Instruction::Call(store_fat_ptr)); // (addr, ptr, len)
func.instruction(&Instruction::LocalGet(ptr));
func.instruction(&Instruction::LocalGet(len));
func.instruction(&Instruction::Call(free));          // host hasn't lifted it yet!
// the host now lifts a buffer the guest already reclaimed
```

## Good

```rust
// at return time: hand the (ptr, len) to the host, free nothing
func.instruction(&Instruction::LocalGet(ptr));
func.instruction(&Instruction::LocalGet(len));
func.instruction(&Instruction::End); // return (ptr, len)

// the guest's exported `post-return` does the cleanup, called by the
// host *after* it has lifted the results — frees exactly those buffers
fn emit_post_return(&mut self) -> Function {
    // receives the flat results (ptr, len); free(ptr) here, not inline above
}
```

## See Also

- [mem-cabi-realloc](mem-cabi-realloc.md) - The allocation side of the same hand-off
- [mem-fat-pointer-strings](mem-fat-pointer-strings.md) - The `(ptr,len)` shape being handed over
- [abi-respect-flattening-limit](abi-respect-flattening-limit.md) - Return-area pointers obey the same ownership rules
