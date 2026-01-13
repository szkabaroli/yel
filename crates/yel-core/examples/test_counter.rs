//! Test counter component compilation

use yel_core::codegen::generate_wasm;
use yel_core::Compiler;

fn main() {
    let source = r#"
        package yel:counter@1.0.0;

        record Person {
            name: string,
            age: u32,
        }

        enum status { pending, active, completed }

        export component Counter {
            count: s32 = 0;
            label: string = "Count";
            items: list<Person> = [{ name: "Alice", age: 30 }];

            incremented: func();

            VStack {
                Text { "{label}: {count}" }

                HStack {
                    Button {
                        "-"
                        clicked: { count -= 1; }
                    }
                    Button {
                        "+"
                        clicked: { count += 1; incremented(); }
                    }
                }

                if count > 10 {
                    Text { "High count!" }
                } else if count < 0 {
                    Text { "Negative!" }
                }

                Text { "Items: {items.len()} {items[0].name}" }

                for item in items key(item.name) {
                    Text { "{item.name}" }
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
            std::fs::write("/tmp/test_counter.wasm", &wasm_bytes).expect("Failed to write WASM");
            println!("Generated {} bytes of WASM to /tmp/test_counter.wasm", wasm_bytes.len());
            println!("SUCCESS: WASM generation complete!");
        }
        Err(e) => {
            eprintln!("WASM generation failed: {:?}", e);
            std::process::exit(1);
        }
    }
}
