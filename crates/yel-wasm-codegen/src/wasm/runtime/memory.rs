//! Memory operations for WASM runtime.
//!
//! Provides:
//! - `memcpy` - Copy bytes between memory regions
//! - `alloc` - Allocate memory from free list or bump allocator (with block splitting)
//! - `free` - Return memory to free list (with coalescing)
//! - `cabi_realloc` - Component Model ABI realloc (allocate, grow, or free)

use wasm_encoder::{BlockType, ConstExpr, Function, GlobalType, Instruction, MemArg, ValType};

// ============================================================================
// Allocator Globals
// ============================================================================

/// Indices for allocator globals.
///
/// These must be added to the module's global section.
#[derive(Debug, Clone, Copy)]
pub struct AllocatorGlobals {
    /// Global index for heap base constant (immutable)
    pub heap_base: u32,
    /// Global index for heap pointer (mutable, bump allocator)
    pub heap_ptr: u32,
    /// Global index for free list head (mutable, 0 = empty)
    pub free_list: u32,
}

impl AllocatorGlobals {
    /// Create allocator global indices starting at `base_index`.
    pub fn new(base_index: u32) -> Self {
        Self {
            heap_base: base_index,
            heap_ptr: base_index + 1,
            free_list: base_index + 2,
        }
    }

    /// Number of globals required by the allocator.
    pub const fn count() -> u32 {
        3
    }
}

/// Emit the allocator globals.
///
/// Returns [(type, init_value)] for: heap_base, heap_ptr, free_list
pub fn emit_allocator_globals(heap_start: u32) -> [(GlobalType, ConstExpr); 3] {
    [
        // heap_base: immutable, set to heap_start
        (
            GlobalType {
                val_type: ValType::I32,
                mutable: false,
                shared: false,
            },
            ConstExpr::i32_const(heap_start as i32),
        ),
        // heap_ptr: mutable, initially heap_start
        (
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            ConstExpr::i32_const(heap_start as i32),
        ),
        // free_list: mutable, initially 0 (empty)
        (
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            ConstExpr::i32_const(0),
        ),
    ]
}

// ============================================================================
// Allocator Functions
// ============================================================================

/// Generate alloc function with block splitting.
///
/// Signature: (size: i32, align: i32) -> ptr: i32
///
/// Algorithm:
/// 1. Round size up to alignment (minimum 8 for free list header)
/// 2. Search free list for first-fit block
/// 3. If found and block is ≥16 bytes larger: split block, keep remainder in free list
/// 4. If found but no split: remove entire block from free list
/// 5. If not found: bump allocate from heap_ptr
pub fn emit_alloc(globals: &AllocatorGlobals) -> Function {
    // params: size(0), align(1)
    // locals: aligned_size(2), prev(3), curr(4), block_size(5), result(6), remainder(7)
    let mut func = Function::new([(6, ValType::I32)]);

    // --- Ensure minimum alignment of 8 ---
    func.instruction(&Instruction::LocalGet(1)); // align
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::LocalSet(1));
    func.instruction(&Instruction::End);

    // --- Calculate aligned_size = (size + align - 1) & ~(align - 1), minimum 8 ---
    func.instruction(&Instruction::LocalGet(0)); // size
    func.instruction(&Instruction::LocalGet(1)); // align
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalGet(1)); // align
    func.instruction(&Instruction::I32Sub); // -align
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::LocalSet(2)); // aligned_size

    // Minimum block size is 8 (for next ptr + size when freed)
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::End);

    // --- Search free list (first-fit) ---
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(3)); // prev = 0

    func.instruction(&Instruction::GlobalGet(globals.free_list));
    func.instruction(&Instruction::LocalSet(4)); // curr = free_list

    // block $found
    func.instruction(&Instruction::Block(BlockType::Empty));
    // block $not_found
    func.instruction(&Instruction::Block(BlockType::Empty));
    // loop $search
    func.instruction(&Instruction::Loop(BlockType::Empty));

    // br_if $not_found if curr == 0
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::BrIf(1)); // br to $not_found

    // block_size = curr[4]
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Load(MemArg { offset: 4, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::LocalSet(5));

    // br_if $found if block_size >= aligned_size
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(2)); // br to $found

    // prev = curr
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalSet(3));

    // curr = curr.next
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Load(MemArg { offset: 0, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::LocalSet(4));

    // br $search
    func.instruction(&Instruction::Br(0));

    func.instruction(&Instruction::End); // end loop $search
    func.instruction(&Instruction::End); // end block $not_found

    // --- Not found: bump allocate ---
    func.instruction(&Instruction::GlobalGet(globals.heap_ptr));
    func.instruction(&Instruction::LocalSet(6)); // result = heap_ptr

    func.instruction(&Instruction::GlobalGet(globals.heap_ptr));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(globals.heap_ptr)); // heap_ptr += aligned_size

    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::Return);

    func.instruction(&Instruction::End); // end block $found

    // --- Found a block - check if we should split ---
    // remainder = block_size - aligned_size
    func.instruction(&Instruction::LocalGet(5)); // block_size
    func.instruction(&Instruction::LocalGet(2)); // aligned_size
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(7)); // remainder

    // if remainder >= 16 (minimum split size: 8 header + 8 payload)
    func.instruction(&Instruction::LocalGet(7));
    func.instruction(&Instruction::I32Const(16));
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::If(BlockType::Empty)); // $do_split

    // --- Split: keep remainder in free list ---
    // split_block = curr + aligned_size (reuse $result temporarily)
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::LocalGet(2)); // aligned_size
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(6)); // result = split_block

    // split_block.next = curr.next
    func.instruction(&Instruction::LocalGet(6)); // split_block
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::I32Load(MemArg { offset: 0, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));

    // split_block.size = remainder
    func.instruction(&Instruction::LocalGet(6)); // split_block
    func.instruction(&Instruction::LocalGet(7)); // remainder
    func.instruction(&Instruction::I32Store(MemArg { offset: 4, align: 2, memory_index: 0 }));

    // Update links: if prev == 0 then free_list = split_block else prev.next = split_block
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(6)); // split_block
    func.instruction(&Instruction::GlobalSet(globals.free_list));
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::LocalGet(6)); // split_block
    func.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::End);

    // return curr
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::Return);

    func.instruction(&Instruction::Else); // no split

    // --- No split: unlink entire block ---
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    // prev == 0: free_list = curr.next
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::I32Load(MemArg { offset: 0, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::GlobalSet(globals.free_list));
    func.instruction(&Instruction::Else);
    // prev != 0: prev.next = curr.next
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::I32Load(MemArg { offset: 0, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::End);

    // return curr
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::Return);

    func.instruction(&Instruction::End); // end if $do_split

    func.instruction(&Instruction::Unreachable);
    func.instruction(&Instruction::End); // end func

    func
}

/// Generate free function with coalescing.
///
/// Signature: (ptr: i32, size: i32) -> ()
///
/// Algorithm:
/// 1. Find sorted insertion point (prev < ptr < curr)
/// 2. Try to coalesce with previous block if adjacent
/// 3. Try to coalesce with next block if adjacent
/// 4. Handles all edge cases (no prev, no next, both, neither)
pub fn emit_free(globals: &AllocatorGlobals) -> Function {
    // params: ptr(0), size(1)
    // locals: aligned_size(2), prev(3), curr(4), prev_end(5), block_end(6)
    let mut func = Function::new([(5, ValType::I32)]);

    // --- Null check ---
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // --- Align size ---
    func.instruction(&Instruction::LocalGet(1)); // size
    func.instruction(&Instruction::I32Const(7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8i32 as i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::LocalSet(2)); // aligned_size

    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::End);

    // --- Find insertion point (sorted by address) ---
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(3)); // prev = 0

    func.instruction(&Instruction::GlobalGet(globals.free_list));
    func.instruction(&Instruction::LocalSet(4)); // curr = free_list

    // block $insert_point
    func.instruction(&Instruction::Block(BlockType::Empty));
    // loop $find_pos
    func.instruction(&Instruction::Loop(BlockType::Empty));

    // br_if $insert_point if curr == 0
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::BrIf(1));

    // br_if $insert_point if curr > ptr
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::I32GtU);
    func.instruction(&Instruction::BrIf(1));

    // prev = curr
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalSet(3));

    // curr = curr.next
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Load(MemArg { offset: 0, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::LocalSet(4));

    // br $find_pos
    func.instruction(&Instruction::Br(0));

    func.instruction(&Instruction::End); // end loop $find_pos
    func.instruction(&Instruction::End); // end block $insert_point

    // --- Now: prev < ptr < curr (either can be null) ---

    // --- Try to coalesce with previous block ---
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty)); // $no_prev

    // --- No previous block - just set up our block at head ---
    // ptr.size = aligned_size
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::LocalGet(2)); // aligned_size
    func.instruction(&Instruction::I32Store(MemArg { offset: 4, align: 2, memory_index: 0 }));

    // ptr.next = curr
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));

    // free_list = ptr
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::GlobalSet(globals.free_list));

    func.instruction(&Instruction::Else); // has prev

    // --- Check if prev is adjacent to ptr ---
    // prev_end = prev + prev.size
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::I32Load(MemArg { offset: 4, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(5)); // prev_end

    // if prev_end == ptr: merge with prev
    func.instruction(&Instruction::LocalGet(5)); // prev_end
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(BlockType::Empty)); // $merge_prev

    // Merge with prev: extend prev's size
    // prev.size = prev.size + aligned_size
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::I32Load(MemArg { offset: 4, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::LocalGet(2)); // aligned_size
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Store(MemArg { offset: 4, align: 2, memory_index: 0 }));

    // ptr is now absorbed into prev
    // ptr = prev (for next coalesce check)
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::LocalSet(0)); // ptr = prev

    // aligned_size = prev.size (updated)
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::I32Load(MemArg { offset: 4, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::LocalSet(2)); // aligned_size = prev.size

    func.instruction(&Instruction::Else); // not adjacent to prev

    // --- Not adjacent: insert between prev and curr ---
    // ptr.size = aligned_size
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::LocalGet(2)); // aligned_size
    func.instruction(&Instruction::I32Store(MemArg { offset: 4, align: 2, memory_index: 0 }));

    // ptr.next = curr
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));

    // prev.next = ptr
    func.instruction(&Instruction::LocalGet(3)); // prev
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));

    func.instruction(&Instruction::End); // end if $merge_prev
    func.instruction(&Instruction::End); // end if $no_prev

    // --- Try to coalesce with next block ---
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty)); // $no_next
    // No next block, we're done
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // --- Check if ptr+size reaches curr ---
    // block_end = ptr + ptr.size
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::I32Load(MemArg { offset: 4, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(6)); // block_end

    // if block_end == curr: merge with next
    func.instruction(&Instruction::LocalGet(6)); // block_end
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(BlockType::Empty)); // $merge_next

    // Merge with next: extend size and skip curr
    // ptr.size = ptr.size + curr.size
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::I32Load(MemArg { offset: 4, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::I32Load(MemArg { offset: 4, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Store(MemArg { offset: 4, align: 2, memory_index: 0 }));

    // ptr.next = curr.next
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::LocalGet(4)); // curr
    func.instruction(&Instruction::I32Load(MemArg { offset: 0, align: 2, memory_index: 0 }));
    func.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));

    func.instruction(&Instruction::End); // end if $merge_next

    func.instruction(&Instruction::End); // end func

    func
}

/// Generate cabi_realloc function (Component Model ABI).
///
/// Signature: (old_ptr: i32, old_size: i32, align: i32, new_size: i32) -> ptr: i32
///
/// Special cases:
/// - old_ptr=0: allocate new (return alloc(new_size, align))
/// - new_size=0: free old_ptr and return 0
/// - Otherwise: allocate new, copy, free old (uses inline memory.copy)
pub fn emit_cabi_realloc(alloc_func: u32, free_func: u32) -> Function {
    // params: old_ptr(0), old_size(1), align(2), new_size(3)
    // locals: new_ptr(4), copy_size(5)
    let mut func = Function::new([(2, ValType::I32)]);

    // if old_ptr == 0 { return alloc(new_size, align) }
    func.instruction(&Instruction::LocalGet(0)); // old_ptr
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(3)); // new_size
    func.instruction(&Instruction::LocalGet(2)); // align
    func.instruction(&Instruction::Call(alloc_func));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // if new_size == 0 { free(old_ptr, old_size); return 0 }
    func.instruction(&Instruction::LocalGet(3)); // new_size
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(0)); // old_ptr
    func.instruction(&Instruction::LocalGet(1)); // old_size
    func.instruction(&Instruction::Call(free_func));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // new_ptr = alloc(new_size, align)
    func.instruction(&Instruction::LocalGet(3)); // new_size
    func.instruction(&Instruction::LocalGet(2)); // align
    func.instruction(&Instruction::Call(alloc_func));
    func.instruction(&Instruction::LocalSet(4));

    // copy_size = min(old_size, new_size)
    func.instruction(&Instruction::LocalGet(1)); // old_size
    func.instruction(&Instruction::LocalGet(3)); // new_size
    func.instruction(&Instruction::LocalGet(1)); // old_size
    func.instruction(&Instruction::LocalGet(3)); // new_size
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::Select);
    func.instruction(&Instruction::LocalSet(5)); // copy_size

    // memory.copy(new_ptr, old_ptr, copy_size) - inlined
    func.instruction(&Instruction::LocalGet(4)); // new_ptr (dst)
    func.instruction(&Instruction::LocalGet(0)); // old_ptr (src)
    func.instruction(&Instruction::LocalGet(5)); // copy_size
    func.instruction(&Instruction::MemoryCopy { dst_mem: 0, src_mem: 0 });

    // free(old_ptr, old_size)
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::Call(free_func));

    // return new_ptr
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::End); // end func

    func
}

// ============================================================================
// Fat Pointer Helpers
// ============================================================================

/// Generate store_fat_ptr function.
///
/// Signature: (addr: i32, ptr: i32, len: i32) -> ()
///
/// Stores a fat pointer (ptr, len) at the given memory address.
/// - ptr is stored at addr (offset 0)
/// - len is stored at addr+4 (offset 4)
///
/// This helper eliminates local variable conflicts when nested constructs
/// (e.g., ListConstruct containing RecordConstruct with string fields)
/// both need to store fat pointers.
pub fn emit_store_fat_ptr() -> Function {
    // params: addr(0), ptr(1), len(2)
    let mut func = Function::new([]);

    // Store ptr at addr
    func.instruction(&Instruction::LocalGet(0)); // addr
    func.instruction(&Instruction::LocalGet(1)); // ptr
    func.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));

    // Store len at addr+4
    func.instruction(&Instruction::LocalGet(0)); // addr
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(2)); // len
    func.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));

    func.instruction(&Instruction::End);
    func
}

/// Generate load_fat_ptr function.
///
/// Signature: (addr: i32) -> (ptr: i32, len: i32)
///
/// Loads a fat pointer from the given memory address.
/// Returns (ptr, len) as multi-value result.
pub fn emit_load_fat_ptr() -> Function {
    // params: addr(0)
    let mut func = Function::new([]);

    // Load ptr from addr
    func.instruction(&Instruction::LocalGet(0)); // addr
    func.instruction(&Instruction::I32Load(MemArg { offset: 0, align: 2, memory_index: 0 }));

    // Load len from addr+4
    func.instruction(&Instruction::LocalGet(0)); // addr
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load(MemArg { offset: 0, align: 2, memory_index: 0 }));

    func.instruction(&Instruction::End);
    func
}

/// Generate promote_ptr_for_variant function.
///
/// Signature: (ptr: i32, len: i32) -> (i64, i32)
///
/// Promotes a fat pointer (ptr, len) to canonical ABI variant format.
/// Returns (ptr as i64, len as i32) for the variant payload slots.
///
/// This is used by emit_expr_as_attr_value for string variant payloads.
pub fn emit_pack_fat_ptr_to_i64() -> Function {
    // params: ptr(0), len(1)
    // no locals needed - just extend ptr and pass len through
    let mut func = Function::new([]);

    // Extend ptr to i64
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::I64ExtendI32U);

    // Pass len through as i32
    func.instruction(&Instruction::LocalGet(1)); // len

    func.instruction(&Instruction::End);
    func
}

// ============================================================================
// Memory Copy
// ============================================================================

/// Generate memcpy function using bulk memory operations.
///
/// Signature: (dst: i32, src: i32, len: i32) -> ()
///
/// Copies `len` bytes from `src` to `dst` using `memory.copy`.
pub fn emit_memcpy() -> Function {
    // params: dst(0), src(1), len(2)
    // no locals needed with bulk memory ops
    let mut func = Function::new([]);

    // memory.copy dst src len
    func.instruction(&Instruction::LocalGet(0)); // dst
    func.instruction(&Instruction::LocalGet(1)); // src
    func.instruction(&Instruction::LocalGet(2)); // len
    func.instruction(&Instruction::MemoryCopy { dst_mem: 0, src_mem: 0 });

    func.instruction(&Instruction::End);

    func
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emit_memcpy_compiles() {
        let func = emit_memcpy();
        // Just verify it compiles without panicking
        assert!(func.byte_len() > 0);
    }

    #[test]
    fn test_allocator_globals() {
        let globals = AllocatorGlobals::new(0);
        assert_eq!(globals.heap_base, 0);
        assert_eq!(globals.heap_ptr, 1);
        assert_eq!(globals.free_list, 2);

        let globals = AllocatorGlobals::new(10);
        assert_eq!(globals.heap_base, 10);
        assert_eq!(globals.heap_ptr, 11);
        assert_eq!(globals.free_list, 12);
    }

    #[test]
    fn test_emit_allocator_globals() {
        let heap_start = 1024;
        let globals = emit_allocator_globals(heap_start);

        // heap_base: immutable
        assert!(!globals[0].0.mutable);
        // heap_ptr: mutable
        assert!(globals[1].0.mutable);
        // free_list: mutable
        assert!(globals[2].0.mutable);
    }

    #[test]
    fn test_emit_alloc_compiles() {
        let globals = AllocatorGlobals::new(0);
        let func = emit_alloc(&globals);
        assert!(func.byte_len() > 0);
    }

    #[test]
    fn test_emit_free_compiles() {
        let globals = AllocatorGlobals::new(0);
        let func = emit_free(&globals);
        assert!(func.byte_len() > 0);
    }

    #[test]
    fn test_emit_cabi_realloc_compiles() {
        // Function indices for alloc, free
        let func = emit_cabi_realloc(0, 1);
        assert!(func.byte_len() > 0);
    }
}
