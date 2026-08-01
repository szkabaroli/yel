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

    let module = compiler.lower_items_to_module(&hir, file.package.clone());
    assert!(!compiler.has_errors());
    let ctx = compiler.context();
    let wasm_options = codegen::WasmWithWitOptions {
        namespace: "ns70".into(),
        name: "pkg42".into(),
        version: "10.2.3".into(),
        wasm_opt_args: None,
    };
    let wasm = codegen::generate_wasm_module(&module, ctx, &wasm_options).unwrap();
    std::fs::write("/tmp/fuzz_underflow.wasm", &wasm).unwrap();
    println!("Wrote {} bytes to /tmp/fuzz_underflow.wasm", wasm.len());
}
