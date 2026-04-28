# CLAUDE.md - yel-wasm-codegen

## No Silent Fallbacks

Never emit dummy/placeholder WASM instructions (e.g. `i32.const 0`) as a fallback for unimplemented expression kinds or unknown function calls. These produce type-incorrect stack shapes that fail WASM validation or cause wrong runtime behavior — and the root cause is extremely hard to trace from hex dumps.

```rust
// ❌ BAD — wrong stack shape, impossible to debug
} else {
    func.instruction(&Instruction::I32Const(0));  // string needs (ptr, len) not one i32
}

// ✅ GOOD — returns a typed error
return Err(CodegenError::InvalidIR(
    format!("Unimplemented: emit_expr for {:?}", expr.kind)
));
```

Use `todo!()` with a descriptive message, or return `Err(CodegenError::...)`. Both crash the compiler immediately with a clear location and context, instead of producing subtly broken WASM.
