// Temporary test file to dump WAT
#[test]
fn dump_fuzz_wasm() {
    use yel_core::Compiler;
    use yel_wasm_codegen as codegen;
    
    let source = include_str!("fixtures/positive/fuzz_stack_underflow_nested_iter.yel");
    let mut compiler = Compiler::new();
    let file = compiler.parse(source).unwrap();
    let hir = compiler.lower_to_hir(&file);
    assert!(!compiler.has_errors());
    
    let mut lir_components = Vec::new();
    for h in &hir {
        let thir = compiler.type_check(h);
        assert!(!compiler.has_errors());
        lir_components.push(compiler.lower_to_lir(&thir));
    }
    let thir_globals = compiler.type_check_globals();
    assert!(!compiler.has_errors());
    let lir_globals = compiler.lower_globals_to_lir(&thir_globals);
    
    let ctx = compiler.context();
    let module = yel_core::lir::LirModule {
        components: lir_components.clone(),
        global_defaults: lir_globals.clone(),
        package: file.package.clone(),
    };
    let wasm_options = codegen::WasmWithWitOptions {
        namespace: "ns70".into(),
        name: "pkg42".into(),
        version: "10.2.3".into(),
        global_defaults: lir_globals,
    };
    let wasm = codegen::generate_wasm_module(&module, ctx, &wasm_options).unwrap();
    std::fs::write("/tmp/fuzz_underflow.wasm", &wasm).unwrap();
    println!("Wrote {} bytes to /tmp/fuzz_underflow.wasm", wasm.len());
}
