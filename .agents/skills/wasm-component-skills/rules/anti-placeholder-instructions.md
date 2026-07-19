# anti-placeholder-instructions

> Don't emit dummy core instructions for unimplemented paths — wrong stack shapes fail component validation and are untraceable from a hex dump

## Why It Matters

Emitting a placeholder like `i32.const 0` for an expression you haven't implemented yet produces a core stack shape that is type-incorrect: a `string` needs `(ptr, len)` (two `i32`), not one, so the surrounding function no longer validates — or worse, it validates and traps at runtime, far from the emitter. Either way the only clue is a byte offset in a hex dump, which is nearly impossible to trace back to the missing feature. The crate's "No Silent Fallbacks" policy (`yel-wasm-codegen/CLAUDE.md`) bans this outright: crash loudly with `todo!("descriptive msg")` or return a typed `CodegenError`, so the failure names the location and the unimplemented path instead of silently corrupting the output.

## Bad

```rust
} else {
    // string needs (ptr, len); one i32 is the wrong stack shape ->
    // the component fails to validate, traceable only from a hex dump
    func.instruction(&Instruction::I32Const(0));
}
```

## Good

```rust
// fail at the emitter with location + context
return Err(CodegenError::InvalidIR(format!(
    "Unimplemented: emit_expr for {:?}",
    expr.kind
)));
// or: todo!("emit_expr: string interpolation not yet lowered ({:?})", expr.kind)
```

## See Also

- [val-validate-component](val-validate-component.md) - The validation a bad stack shape fails
- [comp-validate-on-encode](comp-validate-on-encode.md) - Catching the failure at encode time
