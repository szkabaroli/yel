# CLAUDE.md - yel-smith

This file provides guidance to Claude Code when working with the yel-smith crate.

## Overview

**yel-smith** is a random Yel source code generator for fuzzing, inspired by [wasm-smith](https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasm-smith). It generates semantically valid Yel source files that parse, type-check, and compile to valid WebAssembly.

## Key Design Principle

Generated code must be **semantically valid**, not just syntactically correct. This means:

1. All type references must refer to defined types
2. All variable references must refer to defined properties
3. Expressions must type-check against their expected types
4. Element attributes must match their schema definitions

## Module Structure

| File | Purpose |
|------|---------|
| `src/lib.rs` | Core generator with `YelModule`, types, and `GenerationContext` |
| `src/main.rs` | CLI for generating from seeds or stdin |

## Key Types

```rust
YelModule          // Top-level generated module
Config              // Generator configuration (max_types, max_depth, etc.)
GenerationContext   // Stateful context tracking defined types/properties
TypeRef             // Type references (Bool, S32, List<T>, Named, etc.)
ComponentDef        // Component with properties, callbacks, body
Node                // UI nodes (Element, Text, If, For)
Expr                // Expressions (literals, vars, operations)
```

## Generation Flow

1. `YelModule::arbitrary_with_config()` creates a `GenerationContext`
2. Generate type definitions (records, enums, variants) first
3. Register types in context so components can reference them
4. Generate components with properties tracked in context
5. Generate UI nodes and expressions using tracked state

## Validity Invariants

The `GenerationContext` maintains these invariants:

- `records: HashMap<String, Vec<(String, TypeRef)>>` - defined records and their fields
- `enums: HashMap<String, Vec<String>>` - defined enums and their cases
- `variants: HashMap<String, Vec<VariantCase>>` - defined variants
- `properties: HashMap<String, TypeRef>` - current component's properties
- `callbacks: Vec<String>` - current component's callbacks

## Element Schema System

Elements have schema-defined valid attributes:

```rust
fn get_element_schema(element: &str) -> Option<ElementSchema> {
    match element {
        "VStack" | "HStack" => STACK_ATTRS,  // includes gap
        "ZStack" => ZSTACK_ATTRS,            // no gap
        "Text" => TEXT_ATTRS,                // color, font-size
        // ...
    }
}
```

When adding new elements to `yel-dsl/src/stdlib.rs`, update the schema here.

## Common Tasks

### Adding a New Type

1. Add variant to `TypeRef` enum
2. Update `TypeRef::to_source()` for source generation
3. Update `arbitrary_type()` and `arbitrary_simple_type()` if it should be randomly generated
4. Update `arbitrary_expr_of_type()` to generate valid expressions of the type
5. Update `arbitrary_literal_of_type()` for literal generation

### Adding a New Element

1. Define attribute schema in `get_element_schema()`
2. Add to element list in `arbitrary_element()`

### Adding a New Expression Kind

1. Add variant to `Expr` enum
2. Update `Expr::to_source()` for source generation
3. Update `arbitrary_expr_of_type()` to generate it for appropriate types

## Testing

```bash
# Run unit tests
cargo test -p yel-smith

# Generate and validate single seed
./target/release/yel-smith --seed 42 > /tmp/test.yel
./target/release/yelc compile -o wasm /tmp/test.yel > /tmp/test.wasm
wasm-tools validate /tmp/test.wasm

# Batch test (should be 100% pass rate)
for seed in $(seq 1 1000); do
  ./target/release/yel-smith --seed $seed > /tmp/test.yel
  ./target/release/yelc compile -o wasm /tmp/test.yel > /tmp/test.wasm 2>&1
  wasm-tools validate /tmp/test.wasm 2>/dev/null || echo "Seed $seed failed"
done
```

## Debugging Failures

When a seed fails to compile or validate:

1. Generate the failing source: `yel-smith --seed N > /tmp/fail.yel`
2. Check parse errors: `yelc check /tmp/fail.yel`
3. Check WASM validation: `wasm-tools validate /tmp/fail.wasm 2>&1`
4. Look for patterns (e.g., specific types, element combinations)

Common issues:
- Type mismatch: Check `arbitrary_expr_of_type()` generates correct types
- Invalid attribute: Check element schema matches `stdlib.rs`
- Stack imbalance: Check codegen handles all expression types

## Dependencies

- `arbitrary` - Random structured generation
- `anyhow` - Error handling in CLI
- `yel-core` - For parse validation in tests
