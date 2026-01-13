//! String operations for WASM runtime.

use std::collections::HashMap;
use wasm_encoder::{BlockType, Function, Instruction, MemArg, ValType};

/// String data for the WASM data section.
///
/// Manages string literal interning and provides offset/length info
/// for the data section.
#[derive(Debug, Clone)]
pub struct StringData {
    /// Raw bytes of all literals
    bytes: Vec<u8>,
    /// String -> (offset_relative, len)
    cache: HashMap<String, (u32, u32)>,
    /// Base offset in linear memory where strings start
    base: u32,
}

impl StringData {
    /// Create a new StringData with the given base offset.
    pub fn new(base: u32) -> Self {
        Self {
            bytes: Vec::new(),
            cache: HashMap::new(),
            base,
        }
    }

    /// Intern a string, returning (absolute_offset, len).
    ///
    /// If the string was already interned, returns the cached location.
    pub fn intern(&mut self, s: &str) -> (u32, u32) {
        if let Some(&(rel_offset, len)) = self.cache.get(s) {
            return (self.base + rel_offset, len);
        }

        let rel_offset = self.bytes.len() as u32;
        let len = s.len() as u32;

        self.bytes.extend_from_slice(s.as_bytes());
        self.cache.insert(s.to_string(), (rel_offset, len));

        (self.base + rel_offset, len)
    }

    /// Check if a string is already interned.
    pub fn get(&self, s: &str) -> Option<(u32, u32)> {
        self.cache.get(s).map(|&(rel, len)| (self.base + rel, len))
    }

    /// Total size of string data in bytes.
    pub fn size(&self) -> u32 {
        self.bytes.len() as u32
    }

    /// Get the raw bytes for the data section.
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Get the base offset.
    pub fn base(&self) -> u32 {
        self.base
    }

    /// Emit to wasm data section.
    pub fn emit(&self, data: &mut wasm_encoder::DataSection) {
        if !self.bytes.is_empty() {
            data.active(
                0,
                &wasm_encoder::ConstExpr::i32_const(self.base as i32),
                self.bytes.iter().copied(),
            );
        }
    }
}

/// Generate concat function for N strings.
///
/// Signature: (s1_ptr, s1_len, s2_ptr, s2_len, ...) -> (ptr, len)
///
/// Uses realloc to allocate the result buffer.
/// Uses bulk memory `memory.copy` for copying.
pub fn emit_concat_n(n: usize, fn_realloc: u32) -> Function {
    assert!(n >= 2, "concat requires at least 2 arguments");

    // params: (ptr, len) pairs for each string = 2*n params
    // locals: total_len, result_ptr, write_ptr
    let num_params = n * 2;
    let local_total_len = num_params as u32;
    let local_result_ptr = num_params as u32 + 1;
    let local_write_ptr = num_params as u32 + 2;

    let mut func = Function::new([(3, ValType::I32)]);

    // Calculate total length: sum of all len params
    // total_len = s1_len + s2_len + ...
    func.instruction(&Instruction::I32Const(0));
    for i in 0..n {
        let len_param = (i * 2 + 1) as u32; // len params are at odd indices
        func.instruction(&Instruction::LocalGet(len_param));
        func.instruction(&Instruction::I32Add);
    }
    func.instruction(&Instruction::LocalSet(local_total_len));

    // Allocate buffer: result_ptr = realloc(0, 0, 1, total_len)
    // realloc signature: (old_ptr, old_size, align, new_size) -> new_ptr
    func.instruction(&Instruction::I32Const(0)); // old_ptr
    func.instruction(&Instruction::I32Const(0)); // old_size
    func.instruction(&Instruction::I32Const(1)); // align
    func.instruction(&Instruction::LocalGet(local_total_len)); // new_size
    func.instruction(&Instruction::Call(fn_realloc));
    func.instruction(&Instruction::LocalSet(local_result_ptr));

    // write_ptr = result_ptr
    func.instruction(&Instruction::LocalGet(local_result_ptr));
    func.instruction(&Instruction::LocalSet(local_write_ptr));

    // Copy each string
    for i in 0..n {
        let ptr_param = (i * 2) as u32;
        let len_param = (i * 2 + 1) as u32;

        // memory.copy(dst, src, len)
        func.instruction(&Instruction::LocalGet(local_write_ptr)); // dst
        func.instruction(&Instruction::LocalGet(ptr_param)); // src
        func.instruction(&Instruction::LocalGet(len_param)); // len
        func.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });

        // write_ptr += s_len
        func.instruction(&Instruction::LocalGet(local_write_ptr));
        func.instruction(&Instruction::LocalGet(len_param));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(local_write_ptr));
    }

    // Return (result_ptr, total_len)
    func.instruction(&Instruction::LocalGet(local_result_ptr));
    func.instruction(&Instruction::LocalGet(local_total_len));
    func.instruction(&Instruction::End);

    func
}

/// Helper to create a MemArg
fn mem_arg(offset: u64, align: u32) -> MemArg {
    MemArg {
        offset,
        align,
        memory_index: 0,
    }
}

/// Generate s32_to_string function.
///
/// Signature: (value: i32) -> (ptr: i32, len: i32)
///
/// Converts a signed 32-bit integer to a string.
/// Uses a fixed buffer at address 0 (first 16 bytes reserved).
pub fn emit_s32_to_string() -> Function {
    // Locals: is_negative, abs_value, digit_count, write_ptr
    let mut func = Function::new([(4, ValType::I32)]);

    const BUFFER_PTR: i32 = 0; // Fixed buffer location

    // Check if negative: is_negative = value < 0
    func.instruction(&Instruction::LocalGet(0)); // value
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32LtS);
    func.instruction(&Instruction::LocalSet(1)); // is_negative

    // abs_value = is_negative ? -value : value
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::LocalSet(2)); // abs_value

    // digit_count = 0
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(3)); // digit_count

    // Handle zero case specially
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    // Store '0' at buffer[0]
    func.instruction(&Instruction::I32Const(BUFFER_PTR));
    func.instruction(&Instruction::I32Const(48)); // '0'
    func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
    // Return (ptr=0, len=1)
    func.instruction(&Instruction::I32Const(BUFFER_PTR));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // Count digits
    func.instruction(&Instruction::LocalGet(2)); // abs_value
    func.instruction(&Instruction::LocalSet(4)); // temp for counting
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    // digit_count++
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(3));
    // temp /= 10
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Const(10));
    func.instruction(&Instruction::I32DivU);
    func.instruction(&Instruction::LocalTee(4));
    // Continue if temp > 0
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32GtU);
    func.instruction(&Instruction::BrIf(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    // If negative, write '-' at buffer[0]
    func.instruction(&Instruction::LocalGet(1)); // is_negative
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I32Const(BUFFER_PTR));
    func.instruction(&Instruction::I32Const(45)); // '-'
    func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
    func.instruction(&Instruction::End);

    // write_ptr = buffer_ptr + is_negative + digit_count - 1
    func.instruction(&Instruction::I32Const(BUFFER_PTR));
    func.instruction(&Instruction::LocalGet(1)); // is_negative (0 or 1)
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(3)); // digit_count
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(4)); // write_ptr

    // Reset abs_value for writing
    func.instruction(&Instruction::LocalGet(1)); // is_negative
    func.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::LocalSet(2)); // abs_value

    // Write digits in reverse
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    // *write_ptr = '0' + (abs_value % 10)
    func.instruction(&Instruction::LocalGet(4)); // write_ptr
    func.instruction(&Instruction::LocalGet(2)); // abs_value
    func.instruction(&Instruction::I32Const(10));
    func.instruction(&Instruction::I32RemU);
    func.instruction(&Instruction::I32Const(48)); // '0'
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
    // abs_value /= 10
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(10));
    func.instruction(&Instruction::I32DivU);
    func.instruction(&Instruction::LocalSet(2));
    // write_ptr--
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(4));
    // Continue if abs_value > 0
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32GtU);
    func.instruction(&Instruction::BrIf(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    // Return (ptr, len) where len = is_negative + digit_count
    func.instruction(&Instruction::I32Const(BUFFER_PTR)); // ptr
    func.instruction(&Instruction::LocalGet(1)); // is_negative
    func.instruction(&Instruction::LocalGet(3)); // digit_count
    func.instruction(&Instruction::I32Add); // len = is_negative + digit_count

    func.instruction(&Instruction::End);
    func
}

/// Generate bool_to_string function.
///
/// Signature: (b: i32) -> (ptr: i32, len: i32)
///
/// Returns pointer to static "true" or "false" string.
pub fn emit_bool_to_string(true_ptr: u32, false_ptr: u32) -> Function {
    // param: b(0)
    // locals: result_ptr(1), result_len(2)
    let mut func = Function::new([(2, ValType::I32)]);

    // if b then set (true_ptr, 4) else set (false_ptr, 5)
    func.instruction(&Instruction::LocalGet(0)); // b
    func.instruction(&Instruction::If(BlockType::Empty));

    // true case: result_ptr = true_ptr, result_len = 4
    func.instruction(&Instruction::I32Const(true_ptr as i32));
    func.instruction(&Instruction::LocalSet(1));
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::LocalSet(2));

    func.instruction(&Instruction::Else);

    // false case: result_ptr = false_ptr, result_len = 5
    func.instruction(&Instruction::I32Const(false_ptr as i32));
    func.instruction(&Instruction::LocalSet(1));
    func.instruction(&Instruction::I32Const(5));
    func.instruction(&Instruction::LocalSet(2));

    func.instruction(&Instruction::End); // end if

    // Return (result_ptr, result_len)
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::End); // end func

    func
}

/// Generate f32_to_string function.
///
/// Signature: (value: f32) -> (ptr: i32, len: i32)
///
/// Converts a 32-bit float to a string with up to 2 decimal places.
/// Uses a fixed buffer at address 0 (first 16 bytes reserved).
pub fn emit_f32_to_string() -> Function {
    // param: value(0) - f32
    // locals:
    //   1: is_negative (i32)
    //   2: abs_value (f32)
    //   3: int_part (i32)
    //   4: frac_part (i32) - scaled by 100 for 2 decimal places
    //   5: digit_count (i32)
    //   6: write_ptr (i32)
    //   7: temp (i32)
    let mut func = Function::new([
        (1, ValType::I32),  // is_negative
        (1, ValType::F32),  // abs_value
        (5, ValType::I32),  // int_part, frac_part, digit_count, write_ptr, temp
    ]);

    const BUFFER_PTR: i32 = 0; // Fixed buffer location

    // Check if negative: is_negative = value < 0
    func.instruction(&Instruction::LocalGet(0)); // value
    func.instruction(&Instruction::F32Const(0.0));
    func.instruction(&Instruction::F32Lt);
    func.instruction(&Instruction::LocalSet(1)); // is_negative

    // abs_value = is_negative ? -value : value
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::If(BlockType::Result(ValType::F32)));
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::F32Neg);
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::LocalSet(2)); // abs_value

    // int_part = trunc(abs_value) as i32
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32TruncF32S);
    func.instruction(&Instruction::LocalSet(3)); // int_part

    // frac_part = round((abs_value - int_part) * 100) as i32
    func.instruction(&Instruction::LocalGet(2)); // abs_value
    func.instruction(&Instruction::LocalGet(3)); // int_part
    func.instruction(&Instruction::F32ConvertI32S);
    func.instruction(&Instruction::F32Sub); // abs_value - int_part
    func.instruction(&Instruction::F32Const(100.0));
    func.instruction(&Instruction::F32Mul);
    func.instruction(&Instruction::F32Nearest); // round
    func.instruction(&Instruction::I32TruncF32S);
    func.instruction(&Instruction::LocalSet(4)); // frac_part

    // digit_count = 0
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(5));

    // Handle zero integer part specially
    func.instruction(&Instruction::LocalGet(3)); // int_part
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I32Const(1)); // count "0" as 1 digit
    func.instruction(&Instruction::LocalSet(5));
    func.instruction(&Instruction::Else);
    // Count digits in int_part
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::LocalSet(7)); // temp = int_part
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    // digit_count++
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(5));
    // temp /= 10
    func.instruction(&Instruction::LocalGet(7));
    func.instruction(&Instruction::I32Const(10));
    func.instruction(&Instruction::I32DivU);
    func.instruction(&Instruction::LocalTee(7));
    // Continue if temp > 0
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32GtU);
    func.instruction(&Instruction::BrIf(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    // If negative, write '-' at buffer[0]
    func.instruction(&Instruction::LocalGet(1)); // is_negative
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I32Const(BUFFER_PTR));
    func.instruction(&Instruction::I32Const(45)); // '-'
    func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
    func.instruction(&Instruction::End);

    // write_ptr = buffer_ptr + is_negative + digit_count - 1
    func.instruction(&Instruction::I32Const(BUFFER_PTR));
    func.instruction(&Instruction::LocalGet(1)); // is_negative (0 or 1)
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(5)); // digit_count
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(6)); // write_ptr

    // Write integer digits in reverse
    func.instruction(&Instruction::LocalGet(3)); // int_part
    func.instruction(&Instruction::LocalSet(7)); // temp = int_part
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    // *write_ptr = '0' + (temp % 10)
    func.instruction(&Instruction::LocalGet(6)); // write_ptr
    func.instruction(&Instruction::LocalGet(7)); // temp
    func.instruction(&Instruction::I32Const(10));
    func.instruction(&Instruction::I32RemU);
    func.instruction(&Instruction::I32Const(48)); // '0'
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
    // temp /= 10
    func.instruction(&Instruction::LocalGet(7));
    func.instruction(&Instruction::I32Const(10));
    func.instruction(&Instruction::I32DivU);
    func.instruction(&Instruction::LocalSet(7));
    // write_ptr--
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(6));
    // Continue if temp > 0
    func.instruction(&Instruction::LocalGet(7));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32GtU);
    func.instruction(&Instruction::BrIf(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    // write_ptr = buffer_ptr + is_negative + digit_count (after integer part)
    func.instruction(&Instruction::I32Const(BUFFER_PTR));
    func.instruction(&Instruction::LocalGet(1)); // is_negative
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(5)); // digit_count
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(6)); // write_ptr

    // Write '.' at write_ptr
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::I32Const(46)); // '.'
    func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

    // write_ptr++
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(6));

    // Write 2 decimal digits
    // First decimal digit: frac_part / 10
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::LocalGet(4)); // frac_part
    func.instruction(&Instruction::I32Const(10));
    func.instruction(&Instruction::I32DivU);
    func.instruction(&Instruction::I32Const(48)); // '0'
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

    // write_ptr++
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(6));

    // Second decimal digit: frac_part % 10
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::LocalGet(4)); // frac_part
    func.instruction(&Instruction::I32Const(10));
    func.instruction(&Instruction::I32RemU);
    func.instruction(&Instruction::I32Const(48)); // '0'
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

    // Return (ptr, len)
    // len = is_negative + digit_count + 1 (.) + 2 (decimals)
    func.instruction(&Instruction::I32Const(BUFFER_PTR)); // ptr
    func.instruction(&Instruction::LocalGet(1)); // is_negative
    func.instruction(&Instruction::LocalGet(5)); // digit_count
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(3)); // '.' + 2 decimals
    func.instruction(&Instruction::I32Add); // total len

    func.instruction(&Instruction::End);
    func
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_data_intern() {
        let mut sd = StringData::new(1000);

        let (ptr1, len1) = sd.intern("hello");
        assert_eq!(ptr1, 1000);
        assert_eq!(len1, 5);

        let (ptr2, len2) = sd.intern("world");
        assert_eq!(ptr2, 1005);
        assert_eq!(len2, 5);

        // Re-interning should return cached value
        let (ptr3, len3) = sd.intern("hello");
        assert_eq!(ptr3, 1000);
        assert_eq!(len3, 5);

        assert_eq!(sd.size(), 10);
    }

    #[test]
    fn test_emit_concat_n() {
        let func = emit_concat_n(2, 100);
        assert!(func.byte_len() > 0);

        let func3 = emit_concat_n(3, 100);
        assert!(func3.byte_len() > 0);
    }

    #[test]
    fn test_emit_s32_to_string() {
        let func = emit_s32_to_string();
        assert!(func.byte_len() > 0);
    }

    #[test]
    fn test_emit_bool_to_string() {
        let func = emit_bool_to_string(100, 104);
        assert!(func.byte_len() > 0);
    }
}
