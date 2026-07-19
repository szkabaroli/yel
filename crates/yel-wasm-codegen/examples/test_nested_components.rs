//! Test nested component compilation

use yel_core::Compiler;
use yel_wasm_codegen::generate_wasm;

fn main() {
    let source = r#"
        package yel:nested-components@1.0.0;

        // A simple nested component
        component Nested {
            VStack {
                Text { "...and these styles won't!" }
            }
        }

        // Parent component that uses the Nested component
        export component App {
            VStack {
                Text { "These styles..." }
                Nested { }

                // Test for-loop with literal list
                for i in [0, 1, 2] key(i) {
                    Text { "Item {i}" }
                }
            }
        }
    "#;

    let mut compiler = Compiler::new();

    // Parse
    let file = compiler.parse(source).expect("Parse failed");
    println!("Parsed successfully");

    // Lower to HIR
    let items = compiler.lower_to_hir(&file);
    assert!(!items.is_empty(), "No items found");
    println!("HIR lowering complete: {} items", items.len());
    for hir in &items {
        if let Some(c) = hir.as_component() {
            println!("  - {}", compiler.context().str(c.name));
        }
    }

    // Type check all items, keeping the component THIR for lowering.
    let mut thir_items = Vec::new();
    for hir in &items {
        if let yel_core::thir::ThirItem::Component(thir) = compiler.type_check(hir) {
            println!("Type checked: {}", compiler.context().str(thir.name));
            thir_items.push(thir);
        }
    }

    // Lower all to LIR
    let mut lir_components = Vec::new();
    for thir in &thir_items {
        let lir = compiler.lower_to_lir(thir);
        println!("LIR lowered: {} (export={})", compiler.context().str(lir.name), lir.is_export);
        println!("  Mount block: {:?}", lir.mount_block);
        println!("  Blocks count: {}", lir.blocks.len());
        let mount_block = lir.get_block(lir.mount_block);
        println!("  Mount block ops: {} operations", mount_block.ops.len());
        for (i, op) in mount_block.ops.iter().enumerate() {
            println!("    [{i}] {:?}", op);
        }
        lir_components.push(lir);
    }

    // Check for errors
    if compiler.has_errors() {
        eprintln!("Compilation errors:\n{}", compiler.render_diagnostics());
        std::process::exit(1);
    }
    println!("No compilation errors");

    // Generate WASM with all components
    match generate_wasm(&lir_components, compiler.context()) {
        Ok(wasm_bytes) => {
            std::fs::write("/tmp/test_nested_components.wasm", &wasm_bytes).expect("Failed to write WASM");
            println!("Generated {} bytes of WASM to /tmp/test_nested_components.wasm", wasm_bytes.len());

            // Disassemble to WAT for inspection
            let wat = wasmprinter::print_bytes(&wasm_bytes).unwrap_or_else(|e| format!("Failed to print: {}", e));
            std::fs::write("/tmp/test_nested_components.wat", &wat).expect("Failed to write WAT");
            println!("Wrote WAT to /tmp/test_nested_components.wat");

            // Show mount function excerpt
            for line in wat.lines() {
                if line.contains("[method]app.mount") || line.contains("[constructor]nested") {
                    println!("{}", line);
                }
            }

            println!("SUCCESS: WASM generation complete!");
        }
        Err(e) => {
            eprintln!("WASM generation failed: {:?}", e);
            std::process::exit(1);
        }
    }
}
