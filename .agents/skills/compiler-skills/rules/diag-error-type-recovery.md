# diag-error-type-recovery

> Poison failed nodes with an `Error` type/value and continue, suppressing cascade errors

## Why It Matters

Once you decide to keep checking after an error, you must stop a single root failure from spawning a cascade of derived complaints. yel reserves `Ty::ERROR` (in `types/interner.rs`): when checking fails — a type mismatch, an unresolved name — the checker emits exactly one diagnostic, assigns `Ty::ERROR` to that expression, and continues. Operations that see `Ty::ERROR` treat it as already-errored and stay silent, so the user fixes one real problem instead of wading through ten phantom ones.

## Bad

```rust
let elem = self.resolve(name).unwrap_or_else(|| {
    self.diags.error(span, "unknown name");
    Ty::I32 // lying: downstream now reports bogus mismatches against i32
});
```

## Good

```rust
let elem = match self.resolve(name) {
    Some(ty) => ty,
    None => {
        self.diags.error(span, "unknown name");
        Ty::ERROR // poison: nothing further is reported off this node
    }
};

// elsewhere: an operand that is already poisoned never re-reports
if lhs == Ty::ERROR || rhs == Ty::ERROR {
    return Ty::ERROR; // no second, derived diagnostic
}
```

## See Also

- [intern-preintern-constants](intern-preintern-constants.md) - `Ty::ERROR` is a pre-interned sentinel constant
- [diag-accumulate-continue](diag-accumulate-continue.md) - Recovery is what makes continuing safe
- [id-invalid-sentinel](id-invalid-sentinel.md) - The same poison idea for id spaces
