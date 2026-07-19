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
    let mut global_thir_defaults: std::collections::HashMap<
        yel_core::DefId,
        yel_core::thir::ThirExpr,
    > = std::collections::HashMap::new();
    for item in &hir {
        match compiler.type_check(item) {
            yel_core::thir::ThirItem::Component(thir) => {
                assert!(!compiler.has_errors());
                lir_components.push(compiler.lower_to_lir(&thir));
            }
            yel_core::thir::ThirItem::Global(global) => {
                assert!(!compiler.has_errors());
                global_thir_defaults.extend(global.signal_defaults);
            }
        }
    }
    let (lir_globals, lir_global_default_exprs) =
        compiler.lower_globals_to_lir(&global_thir_defaults);

    let interfaces = compiler.build_import_interfaces();
    let ctx = compiler.context();
    let module = yel_core::lir::LirModule {
        resources: lir_components.clone(),
        global_defaults: lir_globals.clone(),
        global_default_exprs: lir_global_default_exprs.clone(),
        interfaces,
        package: file.package.clone(),
    };
    let wasm_options = codegen::WasmWithWitOptions {
        namespace: "ns70".into(),
        name: "pkg42".into(),
        version: "10.2.3".into(),
        global_defaults: lir_globals,
        global_default_exprs: lir_global_default_exprs,
        wasm_opt_args: None,
    };
    let wasm = codegen::generate_wasm_module(&module, ctx, &wasm_options).unwrap();
    std::fs::write("/tmp/fuzz_underflow.wasm", &wasm).unwrap();
    println!("Wrote {} bytes to /tmp/fuzz_underflow.wasm", wasm.len());
}

