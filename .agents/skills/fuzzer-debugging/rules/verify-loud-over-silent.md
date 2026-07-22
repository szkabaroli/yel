# verify-loud-over-silent

> Prefer a loud `CodegenError` to validate-but-wrong; but a runtime hang is worse than both

## Why It Matters

The failure modes form a strict preference ordering, and every design choice
should move a not-yet-handled case toward the better end:

```
best   loud compile error (CodegenError / todo!)   — named, located, no bad artifact
  ^    validation failure (encoder rejects)         — caught, but cryptic offset
  |    validates-but-wrong (round-trips wrong)       — ships a lie; needs a round-trip to notice
worst  runtime hang (non-terminating loop)          — no output, stalls the suite
```

A loud error at the emit site tells you the function and the type instantly; a
validation failure at least stops the build; a validate-but-wrong bug silently
ships incorrect behavior; a hang gives you nothing and blocks everything. So an
unimplemented path should `return Err(CodegenError::…)` or `todo!("descriptive
msg")` — never emit a plausible default (a stub fall-through) and never a
placeholder instruction. This is the house no-silent-fallback rule, and the
fuzzer rewards it: a loud `invalid IR: <function>: <case> not yet supported`
names itself and points at the exact gap — a *cheap* fix — whereas the same gap
as a silent default is a multi-hour round-trip hunt.

## Bad

```rust
_ => {
    // don't know how to lift this element yet — just store an i32 and hope
    func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
}
```

## Good

```rust
_ => return Err(CodegenError::InvalidIR(format!(
    "<this routine>: element {elem_ty:?} not yet supported \
     (say what's missing and why, so the message is self-diagnosing)",
))),
```

When you *can't* fully fix a case in scope, upgrade it from silent-wrong to
loud: convert a stub default into an `Err`, so the fuzzer surfaces it as a
named gap instead of a mystery value. And always reach for fuel/watchdogs
(`verify-fuel-in-tests`) to keep the worst case — the hang — off the table
entirely.

## See Also

- [symptom-to-suspect](symptom-to-suspect.md) - The stub-fall-through shape this rule replaces with a loud error
- [verify-fuel-in-tests](verify-fuel-in-tests.md) - Keep the worst failure mode (hang) from ever happening
- [verify-roundtrip-not-validate](verify-roundtrip-not-validate.md) - How you notice a validate-but-wrong that slipped through
