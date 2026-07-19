# mem-fat-pointer-strings

> Represent strings/lists as a `(ptr, len)` fat pointer into linear memory; funnel reads/writes through store/load helpers, not ad-hoc per-site memory ops

## Why It Matters

The canonical ABI represents a `string` (and a non-typed-array `list`) as a `(ptr, len)` pair pointing at bytes in linear memory. yel models this as `InternalRepr::FatPointer` (two `i32` slots) in `wasm/repr.rs`, with the bytes laid out as UTF-8 by `StringData` in `wasm/runtime/strings.rs`. Crucially, every site that reads or writes a fat pointer goes through shared helpers — `emit_store_fat_ptr` ((addr, ptr, len) -> ()) and `emit_load_fat_ptr` ((addr) -> (ptr, len)) in `wasm/runtime/memory.rs` (and `emit_pack_fat_ptr_to_i64` ((ptr, len) -> (i64, i32)) for variant payload slots). Centralizing the layout means a nested construct (e.g. a list of records with string fields) can't disagree with its container about where `ptr` and `len` live; the helper's doc comment calls out exactly that aliasing hazard.

## Bad

```rust
// hand-store ptr/len inline at every call site, guessing the offsets
func.instruction(&Instruction::LocalGet(addr));
func.instruction(&Instruction::LocalGet(ptr));
func.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));
// forgot len? wrote it at the wrong offset? a nested ctor reusing the
// same locals clobbers it — and every bug is a different stack shape
```

## Good

```rust
// one helper owns the (ptr @ +0, len @ +4) layout for all sites
let store = self.runtime.store_fat_ptr
    .ok_or_else(|| CodegenError::InvalidIR("store_fat_ptr not generated".into()))?;
func.instruction(&Instruction::LocalGet(addr));
func.instruction(&Instruction::LocalGet(ptr));
func.instruction(&Instruction::LocalGet(len));
func.instruction(&Instruction::Call(store)); // (addr, ptr, len) -> ()
```

## See Also

- [mem-cabi-realloc](mem-cabi-realloc.md) - How the host allocates the bytes a fat pointer points at
- [mem-canonical-alignment](mem-canonical-alignment.md) - The byte offsets the fat-pointer layout assumes
- [comp-string-encoding-explicit](comp-string-encoding-explicit.md) - Why those bytes are UTF-8
