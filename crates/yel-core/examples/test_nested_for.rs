//! Test nested for-loops with parent's items field

use yel_core::codegen::generate_wasm;
use yel_core::Compiler;

fn main() {
    let source = r#"
        package yel:counter@1.0.0;

        record Item {
            name: string,
            subitems: list<string>,
        }

        export component Counter {
            count: s32 = 0;
            label: string = "Count";
            items: list<Item> = [
                { name: "Alice", subitems: ["a1", "a2"] },
                { name: "Bob", subitems: ["b1", "b2", "b3"] },
            ];

            VStack {
                Text { "{label}: {count}" }

                for item in items key(item.name) {
                    VStack {
                        Text { "{item.name}" }
                        for sub in item.subitems key(sub) {
                            Text { "- {sub}" }
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
            std::fs::write("/tmp/test_nested_for.wasm", &wasm_bytes).expect("Failed to write WASM");
            println!("Generated {} bytes of WASM to /tmp/test_nested_for.wasm", wasm_bytes.len());
            println!("SUCCESS: WASM generation complete!");
        }
        Err(e) => {
            eprintln!("WASM generation failed: {:?}", e);
            std::process::exit(1);
        }
    }
}
