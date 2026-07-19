# abi-trap-on-invalid-lift

> Lifting must validate and trap on invalid inputs — bad chars, malformed UTF, out-of-bounds or misaligned lists, unknown discriminants — never silently accept

## Why It Matters

Lifting turns *untrusted* core values (raw `i32`s, memory the other side wrote) into typed component values, so the canonical ABI requires it to trap on anything ill-formed rather than fabricate a value. The reference implementation traps in exactly these places: a `char` must be a Unicode scalar value (`trap_if(i >= 0x110000)`, `trap_if(0xD800 <= i <= 0xDFFF)` for surrogates); a `string` must decode cleanly under the negotiated encoding (`memory[...].decode(encoding)` — `except UnicodeError: trap()`); a `list` load checks the pointer is aligned and the whole range is in bounds (`trap_if(ptr != align_to(ptr, alignment(elem)))`, `trap_if(ptr + length*elem_size > len(memory))`); a variant discriminant must name a real case. A generator that skips these accepts corrupted input and turns it into wrong values or a memory-safety bug instead of a clean, debuggable trap. This is the canonical-ABI face of the crate-wide "no silent fallbacks" policy.

## Bad

```rust
// lift a char with no validation — accepts surrogates and out-of-range scalars
func.instruction(&Instruction::LocalGet(raw_i32));
// reinterpreted as char downstream; 0xD800 or 0x110000 sails through
```

## Good

```rust
// trap unless the i32 is a valid Unicode scalar value
func.instruction(&Instruction::LocalGet(raw_i32));
func.instruction(&Instruction::I32Const(0x110000));
func.instruction(&Instruction::I32GeU);
// ... br_if to a trap; also reject the surrogate range 0xD800..=0xDFFF
// strings: decode-or-trap; lists: assert aligned + in-bounds before reading
```

## See Also

- [anti-placeholder-instructions](anti-placeholder-instructions.md) - The same no-silent-fallback principle
- [mem-list-elem-count](mem-list-elem-count.md) - The list bounds/alignment check this enforces
- [mem-string-tagged-units](mem-string-tagged-units.md) - The string decode that must validate
