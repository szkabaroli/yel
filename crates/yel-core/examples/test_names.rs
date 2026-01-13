//! Quick test to verify slot debug names in WASM name section

use yel_core::codegen::generate_wasm;
use yel_core::Compiler;

fn main() {
    let source = r#"
        export component Counter {
            count: s32 = 0;

            VStack {
                Text { "Count: {count}" }
                Button {
                    "+"
                    clicked: { count += 1; }
                }
                if count > 0 {
                    Text { "Positive!" }
                }
            }
        }
    "#;

    let mut compiler = Compiler::new();

    // Parse
    let file = compiler.parse(source).expect("Parse failed");

    // Lower to HIR
    let hir = compiler.lower_to_hir(&file);
    assert!(!hir.is_empty(), "No components found");

    // Type check
    let thir = compiler.type_check(&hir[0]);

    // Lower to LIR
    let lir = compiler.lower_to_lir(&thir);

    // Check for errors
    if compiler.has_errors() {
        eprintln!("Compilation errors:\n{}", compiler.render_diagnostics());
        std::process::exit(1);
    }

    // Generate WASM
    let wasm_bytes = generate_wasm(&[lir], compiler.context()).expect("WASM generation failed");

    // Write to temp file
    std::fs::write("/tmp/test_names.wasm", &wasm_bytes).expect("Failed to write WASM");
    println!("Generated {} bytes of WASM to /tmp/test_names.wasm", wasm_bytes.len());
}
