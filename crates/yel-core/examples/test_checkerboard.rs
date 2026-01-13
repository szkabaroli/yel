//! Test checkerboard with nested for-loops and if-condition using both loop items

use yel_core::codegen::generate_wasm;
use yel_core::Compiler;

fn main() {
    let source = r#"
        package yel:counter@1.0.0;

        export component Counter {
            rows: list<u32> = [0, 1, 2, 3, 4, 5, 6, 7];
            cols: list<u32> = [0, 1, 2, 3, 4, 5, 6, 7];

            VStack {
                for row in rows key(row) {
                    HStack {
                        for col in cols key(col) {
                            if (row + col) % 2 == 0 {
                                Box {
                                    style: "background-color: white; width: 20px; height: 20px;"
                                }
                            } else {
                                Box {
                                    style: "background-color: dimgray; width: 20px; height: 20px;"
                                }
                            }
                        }
                    }
                }
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
            std::fs::write("/tmp/test_checkerboard.wasm", &wasm_bytes).expect("Failed to write WASM");
            println!("Generated {} bytes of WASM to /tmp/test_checkerboard.wasm", wasm_bytes.len());
            println!("SUCCESS: WASM generation complete!");
        }
        Err(e) => {
            eprintln!("WASM generation failed: {:?}", e);
            std::process::exit(1);
        }
    }
}
