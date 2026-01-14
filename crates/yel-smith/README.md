# yel-smith

Random Yel source code generator for fuzzing, inspired by [wasm-smith](https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasm-smith).

## Overview

yel-smith generates random but **semantically valid** Yel source files using the `arbitrary` crate. Unlike naive fuzzers that generate random bytes, yel-smith produces code that:

- Parses correctly (valid syntax)
- Type-checks successfully (valid semantics)
- Compiles to valid WebAssembly

This allows fuzzers to bypass early compiler stages and exercise deeper parts of the compiler (type checking, IR lowering, code generation).

## Usage

### CLI

```bash
# Generate from a seed (deterministic)
yel-smith --seed 12345 > test.yel

# Generate from random stdin bytes
head -c 4096 /dev/urandom | yel-smith > test.yel

# Compile and validate
yelc compile -o wasm test.yel > test.wasm
wasm-tools validate test.wasm
```

### Library

```rust
use arbitrary::Unstructured;
use yel_smith::{Config, YelModule};

let data: &[u8] = /* from fuzzer */;
let mut u = Unstructured::new(data);

let config = Config::default();
let module = YelModule::arbitrary_with_config(&mut u, &config)?;
let source = module.to_source();
```

## Configuration

The generator can be configured via `Config`:

```rust
Config {
    max_types: 5,        // Max record/enum/variant definitions
    max_components: 3,   // Max component definitions
    max_properties: 10,  // Max properties per component
    max_callbacks: 5,    // Max callbacks per component
    max_node_depth: 4,   // Max UI node nesting depth
    max_children: 5,     // Max children per element
    max_expr_depth: 3,   // Max expression nesting depth
}
```

## Architecture

The generator maintains state to ensure validity:

1. **Type Tracking**: Defined records, enums, and variants are tracked so expressions can reference them correctly
2. **Property Tracking**: Component properties and their types are tracked for valid variable references
3. **Schema-Aware Generation**: Element attributes are generated according to their schema (e.g., `gap` only on VStack/HStack, not ZStack)
4. **Type-Directed Expressions**: Expressions are generated to match expected types

### Generated Constructs

- Package declarations
- Type definitions (records, enums, variants)
- Components with properties and callbacks
- UI nodes (elements, text, if/else, for loops)
- Expressions (literals, variables, operations, constructors)
- Event handlers with statements

## Testing

Run the test suite:

```bash
cargo test -p yel-smith
```

Batch validation with the compiler:

```bash
# Test 1000 random seeds
for seed in $(seq 1 1000); do
  yel-smith --seed $seed > /tmp/test.yel
  yelc compile -o wasm /tmp/test.yel > /tmp/test.wasm
  wasm-tools validate /tmp/test.wasm || echo "Seed $seed failed"
done
```

## Integration with Fuzzers

### cargo-fuzz

```rust
#![no_main]
use libfuzzer_sys::fuzz_target;
use yel_smith::YelModule;
use arbitrary::Unstructured;

fuzz_target!(|data: &[u8]| {
    let mut u = Unstructured::new(data);
    if let Ok(module) = YelModule::arbitrary(&mut u) {
        let source = module.to_source();
        // Feed to compiler
        let _ = yel_core::Compiler::new()
            .add_source("fuzz.yel", &source)
            .compile();
    }
});
```

## Limitations

- Callback calls in event handlers are not yet generated (pending codegen support)
- Some complex type combinations may not be fully exercised
- Generated code prioritizes validity over coverage diversity
