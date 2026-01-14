//! Runtime functions for list operations.
//!
//! Provides runtime support for:
//! - `list_get` - Direct index access, traps on out-of-bounds, returns element pointer
//! - `list_get_opt` - Safe index access, returns (is_some, elem_ptr)
//!
//! Caller is responsible for loading the value with the appropriate instruction
//! based on element type (i32.load, i64.load, f32.load, f64.load, call $load_fat_ptr).

use wasm_encoder::{Function, Instruction};

/// Emit `list_get`: (ptr: i32, len: i32, idx: i32, elem_size: i32) -> elem_ptr: i32
///
/// Traps with `unreachable` if index is out of bounds.
/// Returns pointer to the element - caller loads with appropriate instruction.
///
/// ```wat
/// (func $list_get (param $ptr i32) (param $len i32) (param $idx i32) (param $elem_size i32) (result i32)
///   ;; Bounds check: if idx >= len, trap
///   local.get $idx
///   local.get $len
///   i32.ge_u
///   if
///     unreachable
///   end
///   ;; Return element pointer: ptr + idx * elem_size
///   local.get $ptr
///   local.get $idx
///   local.get $elem_size
///   i32.mul
///   i32.add
/// )
/// ```
pub fn emit_list_get() -> Function {
    let mut func = Function::new([]);

    // Bounds check: if idx >= len, trap
    func.instruction(&Instruction::LocalGet(2)); // idx
    func.instruction(&Instruction::LocalGet(1)); // len
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    func.instruction(&Instruction::Unreachable);
    func.instruction(&Instruction::End);

    // Return element pointer: ptr + idx * elem_size
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::LocalGet(2)); // idx
    func.instruction(&Instruction::LocalGet(3)); // elem_size
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);

    func.instruction(&Instruction::End);
    func
}

/// Emit `list_get_opt`: (ptr: i32, len: i32, idx: i32, elem_size: i32) -> (is_some: i32, elem_ptr: i32)
///
/// Returns (1, elem_ptr) if in bounds, (0, 0) if out of bounds.
/// Caller loads the value from elem_ptr if is_some == 1.
///
/// Note: This requires a multi-value return type to be registered in the type section.
/// The type index must be passed when generating the function.
///
/// ```wat
/// (func $list_get_opt (param $ptr i32) (param $len i32) (param $idx i32) (param $elem_size i32) (result i32 i32)
///   ;; Bounds check
///   local.get $idx
///   local.get $len
///   i32.ge_u
///   if (result i32 i32)
///     i32.const 0  ;; is_some = false
///     i32.const 0  ;; elem_ptr = 0 (invalid)
///   else
///     i32.const 1  ;; is_some = true
///     ;; elem_ptr = ptr + idx * elem_size
///     local.get $ptr
///     local.get $idx
///     local.get $elem_size
///     i32.mul
///     i32.add
///   end
/// )
/// ```
pub fn emit_list_get_opt(if_block_type_idx: u32) -> Function {
    use wasm_encoder::BlockType;

    let mut func = Function::new([]);

    // Bounds check
    func.instruction(&Instruction::LocalGet(2)); // idx
    func.instruction(&Instruction::LocalGet(1)); // len
    func.instruction(&Instruction::I32GeU);

    // if/else with multi-value result (i32, i32)
    func.instruction(&Instruction::If(BlockType::FunctionType(if_block_type_idx)));

    // Out of bounds: return (0, 0)
    func.instruction(&Instruction::I32Const(0)); // is_some = false
    func.instruction(&Instruction::I32Const(0)); // elem_ptr = 0

    func.instruction(&Instruction::Else);

    // In bounds: return (1, ptr + idx * elem_size)
    func.instruction(&Instruction::I32Const(1)); // is_some = true
    func.instruction(&Instruction::LocalGet(0)); // ptr
    func.instruction(&Instruction::LocalGet(2)); // idx
    func.instruction(&Instruction::LocalGet(3)); // elem_size
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);

    func.instruction(&Instruction::End);

    func.instruction(&Instruction::End);
    func
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emit_list_get() {
        let func = emit_list_get();
        assert!(func.byte_len() > 0);
    }

    #[test]
    fn test_emit_list_get_opt() {
        // Type index 0 is placeholder - in real usage this would be a registered type
        let func = emit_list_get_opt(0);
        assert!(func.byte_len() > 0);
    }
}
