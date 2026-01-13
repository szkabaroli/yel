//! Test temperature converter with bidirectional data binding

use yel_core::codegen::generate_wasm;
use yel_core::Compiler;

fn main() {
    let source = r#"
        package yel:temperature@1.0.0;

        export component TempConverter {
            celsius: f32 = 0.0;
            fahrenheit: f32 = 32.0;

            HStack {
                Input {
                    value: celsius
                    changed: {
                        fahrenheit = 32.0 + (9.0 / 5.0) * celsius;
                    }
                }
                Text { "°C = " }
                Input {
                    value: fahrenheit
                    changed: {
                        celsius = (5.0 / 9.0) * (fahrenheit - 32.0);
                    }
                }
                Text { "°F" }
            }
        }
    "#;

    let mut compiler = Compiler::new();

    // Parse
    let file = compiler.parse(source).expect("Parse failed");
    println!("Parsed successfully");

    // Lower to HIR
    let hir = compiler.lower_to_hir(&file);
    assert!(!hir.is_empty(), "No components found");
    println!("HIR lowering complete: {} components", hir.len());

    // Type check
    let thir = compiler.type_check(&hir[0]);
    println!("Type check complete");

    // Lower to LIR
    let lir = compiler.lower_to_lir(&thir);
    println!("LIR lowering complete");

    // Check for errors
    if compiler.has_errors() {
        eprintln!("Compilation errors:\n{}", compiler.render_diagnostics());
        std::process::exit(1);
    }
    println!("No compilation errors");

    // Generate WASM
    match generate_wasm(&[lir], compiler.context()) {
        Ok(wasm_bytes) => {
            std::fs::write("/tmp/test_temp_converter.wasm", &wasm_bytes).expect("Failed to write WASM");
            println!("Generated {} bytes of WASM to /tmp/test_temp_converter.wasm", wasm_bytes.len());
            println!("SUCCESS: Temperature converter WASM generation complete!");
        }
        Err(e) => {
            eprintln!("WASM generation failed: {:?}", e);
            std::process::exit(1);
        }
    }
}
