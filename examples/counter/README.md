# Counter Example

This example demonstrates how to use Yel with a Rust project that automatically compiles Yel components during the build process, and how to combine the UI component with Rust business logic using `wac`.

## Quick Start

```bash
# Build everything and output final WASM
make all
```

This will:
1. Build the Yel component (generates `counter-ui.wasm` and `counter.wit`)
2. Generate Rust bindings for the callback interface
3. Build the Rust business logic component
4. Compose both components with wac
5. Output the final `counter-app.wasm`

Or step by step:
```bash
# 1. Build the Yel component (generates counter-ui.wasm and counter.wit)
cargo build

# 2. Generate Rust bindings for the callback interface
make bindings

# 3. Build the Rust business logic component
make logic

# 4. Compose both components with wac (outputs counter-app.wasm)
make compose
```

## Structure

- `counter.yel` - The Yel component source code with exported callback `increment: func()`
- `build.rs` - Build script that runs `yelc` to compile the component
- `src/lib.rs` - Rust library implementing the callback interface
- `src/main.rs` - Main Rust application

## How It Works

The Yel component exports `increment: func()` which becomes an **import** in the generated WASM component. This means the component expects the host (or another component) to provide this function.

1. **Yel Component** (`counter.yel`) - Defines the UI and exports `increment` as a callback
2. **WIT Generation** - `yelc` generates `counter.wit` describing the component interface
3. **Rust Bindings** - Generate Rust bindings from the WIT file using `wit-bindgen`
4. **Business Logic** (`src/lib.rs`) - Implement the `increment` function in Rust
5. **Component Composition** - Use `wac` to combine the UI component with the business logic component

## Building

Make sure `yelc` is installed and available in your PATH:

```bash
cargo install --path ../crates/yelc
```

Then build the example:

```bash
cargo build
```

The build script will:
1. Compile `counter.yel` to WebAssembly (`counter.wasm`)
2. Generate the WIT interface (`counter.wit`)
3. Copy `counter.wit` to the project root for easy access

## Generated Files

After building, you can find the generated files in:
- `counter-ui.wasm` - UI component (copied to project root for easy access)
- `counter.wit` - WIT interface (copied to project root)
- `target/debug/build/counter-example-<hash>/out/counter.wasm` - Original UI component location
- `target/debug/build/counter-example-<hash>/out/counter.wit` - Original WIT location

After running `make compose` or `make all`:
- `counter-app.wasm` - **Final composed component** (UI + business logic)

## Generating Rust Bindings

The generated `counter.wit` file shows that the UI component imports `counter-callbacks` interface. You need to create a Rust component that **exports** this interface.

A separate `counter-callbacks.wit` file is provided that defines just the callback interface. Use `wit-bindgen` to generate Rust bindings:

```bash
# Install wit-bindgen if needed
cargo install wit-bindgen-cli

# Generate Rust bindings for exporting the callback interface
wit-bindgen rust --export counter-callbacks.wit --out-dir src/bindings
```

This will generate code that exports the `counter-callbacks` interface. Then implement the `increment` function in `src/lib.rs` using the generated bindings.

## Composing Components with wac

Once you have:
1. `counter-ui.wasm` - The UI component (from Yel, copied to project root)
2. `counter_logic.wasm` - The business logic component (from Rust)

Use `wac` to compose them:

```bash
# Install wac if needed
cargo install wac

# Compose the components (or use 'make compose')
wac compose \
  --output counter-app.wasm \
  counter-ui.wasm \
  target/wasm32-wasip2/debug/counter_logic.wasm
```

Or simply run:
```bash
make all
```

This will build everything and output the final composed component.

## Output Files

The final output is:
- **`counter-app.wasm`** - The complete composed component ready to use

This file contains both the UI (from Yel) and business logic (from Rust), with the `increment` callback properly wired up.
