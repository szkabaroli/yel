//! Yel DSL Compiler - WASI Component Library
//!
//! This library exposes the Yel compiler as a WASI component that can be
//! compiled to JavaScript/TypeScript bindings using jco.
//!
//! ## Building
//!
//! ```bash
//! # Build the WASI P2 component (outputs component model directly)
//! cargo build --lib -p yelc --target wasm32-wasip2 --release
//!
//! # Generate JS/TS bindings with jco
//! jco transpile target/wasm32-wasip2/release/yelc.wasm -o yel-compiler-js
//! ```
//!
//! ## Usage in JavaScript/TypeScript
//!
//! ```typescript
//! import { compiler } from './yel-compiler-js/yel-compiler-js.js';
//!
//! const source = `
//!   package example;
//!   component Counter {
//!     property count: s32 = 0;
//!     VStack { Text { "{count}" } }
//!   }
//! `;
//!
//! // Check for errors
//! const diagnostics = compiler.check('counter.yel', source);
//! if (diagnostics.length > 0) {
//!   for (const d of diagnostics) {
//!     console.error(`${d.line}:${d.column}: ${d.message}`);
//!   }
//! }
//!
//! // Compile to WASM
//! const result = compiler.compile('counter.yel', source, 'wasm');
//! if (result.tag === 'success') {
//!   console.log(`Generated ${result.val.wasmBytes.length} bytes of WASM`);
//! }
//! ```

// Build info from shadow-rs
shadow_rs::shadow!(build);

/// Shared compiler front-end driver used by every output path.
pub mod pipeline;

#[cfg(target_arch = "wasm32")]
mod wasi_impl {
    use yel_core::Compiler;
    use yel_wasm_codegen as codegen;

    // Generate bindings from WIT
    wit_bindgen::generate!({
        world: "yel-compiler",
        path: "wit",
    });

    use exports::yel::compiler::compiler::{
        CompileOutcome, CompileResult, Diagnostic, Guest, OutputFormat, VersionInfo,
    };

    use super::{build, pipeline};

    /// Convert the compiler's diagnostics into WIT diagnostics.
    fn convert_diagnostics(compiler: &Compiler) -> Vec<Diagnostic> {
        pipeline::diagnostics(compiler)
            .into_iter()
            .map(|d| Diagnostic {
                message: d.message,
                rendered: d.rendered,
                line: d.line,
                column: d.column,
                length: d.length,
                severity: d.severity.to_string(),
            })
            .collect()
    }

    struct YelCompiler;

    impl Guest for YelCompiler {
        fn version() -> VersionInfo {
            VersionInfo {
                version: build::PKG_VERSION.to_string(),
                commit: build::SHORT_COMMIT.to_string(),
                commit_date: build::COMMIT_DATE.to_string(),
                build_time: build::BUILD_TIME.to_string(),
                rust_version: build::RUST_VERSION.to_string(),
            }
        }

        fn compile(filename: String, source: String, format: OutputFormat) -> CompileOutcome {
            compile_impl(vec![(filename, source)], format)
        }

        fn compile_multi(files: Vec<(String, String)>, format: OutputFormat) -> CompileOutcome {
            compile_impl(files, format)
        }

        fn parse_to_json(source: String) -> Result<String, String> {
            match yel_core::syntax::parser::parse_file(&source) {
                Ok(result) => serde_json::to_string_pretty(&result.file)
                    .map_err(|e| format!("JSON serialization error: {}", e)),
                Err(e) => Err(format!("{}", e)),
            }
        }

        fn check(filename: String, source: String) -> Vec<Diagnostic> {
            let _ = filename; // Used for error reporting context
            let mut compiler = Compiler::new();

            // Parse - errors are automatically added to diagnostics
            let parsed = match compiler.parse(&source) {
                Ok(p) => p,
                Err(_) => return convert_diagnostics(&compiler),
            };

            let hir_components = compiler.lower_to_hir(&parsed);

            if compiler.has_errors() {
                return convert_diagnostics(&compiler);
            }

            for hir in &hir_components {
                let _thir = compiler.type_check(hir);
            }

            convert_diagnostics(&compiler)
        }
    }

    /// An all-empty result. Each format arm fills in only its own field via
    /// struct-update syntax (`CompileResult { wit_code, ..blank_result() }`),
    /// so the seven unused fields aren't repeated in every arm.
    fn blank_result() -> CompileResult {
        CompileResult {
            rust_code: String::new(),
            wit_code: String::new(),
            wasm_bytes: Vec::new(),
            wast_code: String::new(),
            hir_code: String::new(),
            thir_code: String::new(),
            lir_code: String::new(),
            dot_code: String::new(),
        }
    }

    fn compile_impl(files: Vec<(String, String)>, format: OutputFormat) -> CompileOutcome {
        let mut compiler = Compiler::new();

        let lowered = match pipeline::lower_all(
            &mut compiler,
            files.iter().map(|(_, source)| source.as_str()),
        ) {
            Ok(lowered) => lowered,
            Err(_) => return CompileOutcome::Failure(convert_diagnostics(&compiler)),
        };

        let ctx = compiler.context();
        let wit_options = pipeline::wit_options(lowered.package());

        // Generate output based on format
        let compile_result = match format {
            OutputFormat::Rust => {
                // Rust codegen temporarily disabled - needs update for block-based LIR
                CompileResult {
                    rust_code: "// Rust codegen not available - use WASM output instead\n"
                        .to_string(),
                    ..blank_result()
                }
            }
            OutputFormat::Wit => {
                // Single WIT document per compilation: library files produce
                // a valid package + library world; files with exports get
                // their full world.
                let wit_code = match codegen::generate_wit(lowered.components(), lowered.interfaces(), ctx, &wit_options) {
                    Ok(code) => code,
                    Err(e) => {
                        let msg = format!("WIT generation error: {}", e);
                        return CompileOutcome::Failure(vec![Diagnostic {
                            message: msg.clone(),
                            rendered: format!("error: {}", msg),
                            line: 0,
                            column: 0,
                            length: 1,
                            severity: "error".to_string(),
                        }]);
                    }
                };
                CompileResult {
                    wit_code,
                    ..blank_result()
                }
            }
            OutputFormat::Wasm => {
                let wasm_options = codegen::WasmWithWitOptions {
                    namespace: wit_options.namespace.clone(),
                    name: wit_options.name.clone(),
                    version: wit_options.version.clone(),
                    global_defaults: lowered.global_defaults().clone(),
                    global_default_exprs: lowered.global_default_exprs().to_vec(),
                    wasm_opt_args: None,
                };
                let wasm_bytes = if !lowered.components().is_empty() {
                    match codegen::generate_wasm_module(&lowered.module, ctx, &wasm_options) {
                        Ok(bytes) => bytes,
                        Err(e) => {
                            let msg = format!("WASM generation error: {}", e);
                            return CompileOutcome::Failure(vec![Diagnostic {
                                message: msg.clone(),
                                rendered: format!("error: {}", msg),
                                line: 0,
                                column: 0,
                                length: 1,
                                severity: "error".to_string(),
                            }]);
                        }
                    }
                } else {
                    Vec::new()
                };
                CompileResult {
                    wasm_bytes,
                    ..blank_result()
                }
            }
            OutputFormat::Wast => {
                // Generate WASM bytes first, then convert to text format
                let wasm_options = codegen::WasmWithWitOptions {
                    namespace: wit_options.namespace.clone(),
                    name: wit_options.name.clone(),
                    version: wit_options.version.clone(),
                    global_defaults: lowered.global_defaults().clone(),
                    global_default_exprs: lowered.global_default_exprs().to_vec(),
                    wasm_opt_args: None,
                };
                let wast_code = if !lowered.components().is_empty() {
                    match codegen::generate_wasm_module(&lowered.module, ctx, &wasm_options) {
                        Ok(bytes) => wasmprinter::print_bytes(&bytes)
                            .unwrap_or_else(|e| format!(";; WAST conversion error: {}", e)),
                        Err(e) => format!(";; WASM generation error: {}", e),
                    }
                } else {
                    ";; No components to compile".to_string()
                };
                CompileResult {
                    wast_code,
                    ..blank_result()
                }
            }
            OutputFormat::Hir => {
                let hir_code = match serde_json::to_string_pretty(&lowered.hir) {
                    Ok(json) => json,
                    Err(e) => format!("// HIR serialization error: {}", e),
                };
                CompileResult {
                    hir_code,
                    ..blank_result()
                }
            }
            OutputFormat::Thir => {
                // THIR serialization would require implementing Serialize on THIR types
                CompileResult {
                    thir_code: "{ \"thir_code\": \"not yet implemented\" }".to_string(),
                    ..blank_result()
                }
            }
            OutputFormat::Lir => {
                let lir_code = match serde_json::to_string_pretty(lowered.components()) {
                    Ok(json) => json,
                    Err(e) => format!("// LIR serialization error: {}", e),
                };
                CompileResult {
                    lir_code,
                    ..blank_result()
                }
            }
            OutputFormat::Dot => {
                let dot_code = if !lowered.components().is_empty() {
                    match codegen::generate_dot(lowered.components(), ctx, &codegen::DotOptions::new()) {
                        Ok(code) => code,
                        Err(e) => format!("// DOT generation error: {}", e),
                    }
                } else {
                    "// No components to render".to_string()
                };
                CompileResult {
                    dot_code,
                    ..blank_result()
                }
            }
        };

        CompileOutcome::Success(compile_result)
    }

    export!(YelCompiler);
}

// Re-export types for non-WASM targets (for testing)
#[cfg(not(target_arch = "wasm32"))]
pub mod native {
    use super::pipeline;
    use yel_core::Compiler;
    use yel_wasm_codegen as codegen;

    /// Output format for compilation.
    #[derive(Debug, Clone, Copy)]
    pub enum OutputFormat {
        Rust,
        Wit,
        Wasm,
    }

    /// Result of a successful compilation.
    #[derive(Debug)]
    pub struct CompileResult {
        pub rust_code: String,
        pub wit_code: String,
        pub wasm_bytes: Vec<u8>,
    }

    /// A diagnostic message from the compiler.
    #[derive(Debug)]
    pub struct Diagnostic {
        /// Plain error message for UIs and LSPs
        pub message: String,
        /// Rendered error with source context (for terminal display)
        pub rendered: String,
        pub line: u32,
        pub column: u32,
        /// Length of the span in characters
        pub length: u32,
        pub severity: String,
    }

    /// Result of compilation.
    #[derive(Debug)]
    pub enum CompileOutcome {
        Success(CompileResult),
        Failure(Vec<Diagnostic>),
    }

    /// Convert the compiler's diagnostics into native diagnostics.
    fn convert_diagnostics(compiler: &Compiler) -> Vec<Diagnostic> {
        pipeline::diagnostics(compiler)
            .into_iter()
            .map(|d| Diagnostic {
                message: d.message,
                rendered: d.rendered,
                line: d.line,
                column: d.column,
                length: d.length,
                severity: d.severity.to_string(),
            })
            .collect()
    }

    /// Compile a single yel source file.
    pub fn compile(filename: &str, source: &str, format: OutputFormat) -> CompileOutcome {
        compile_multi(&[(filename.to_string(), source.to_string())], format)
    }

    /// Compile multiple yel source files.
    pub fn compile_multi(files: &[(String, String)], format: OutputFormat) -> CompileOutcome {
        let mut compiler = Compiler::new();

        let lowered = match pipeline::lower_all(
            &mut compiler,
            files.iter().map(|(_, source)| source.as_str()),
        ) {
            Ok(lowered) => lowered,
            Err(_) => return CompileOutcome::Failure(convert_diagnostics(&compiler)),
        };

        let ctx = compiler.context();
        let wit_options = pipeline::wit_options(lowered.package());

        let compile_result = match format {
            OutputFormat::Rust => {
                // Rust codegen temporarily disabled - needs update for block-based LIR
                CompileResult {
                    rust_code: "// Rust codegen not available - use WASM output instead\n"
                        .to_string(),
                    wit_code: String::new(),
                    wasm_bytes: Vec::new(),
                }
            }
            OutputFormat::Wit => {
                // Single WIT document per compilation: the builder handles
                // any number of components plus globals, and still emits
                // valid output (library world) when there are no exports.
                let wit_code = match codegen::generate_wit(lowered.components(), lowered.interfaces(), ctx, &wit_options) {
                    Ok(code) => code,
                    Err(e) => {
                        let msg = format!("WIT generation error: {}", e);
                        return CompileOutcome::Failure(vec![Diagnostic {
                            message: msg.clone(),
                            rendered: format!("error: {}", msg),
                            line: 0,
                            column: 0,
                            length: 1,
                            severity: "error".to_string(),
                        }]);
                    }
                };
                CompileResult {
                    rust_code: String::new(),
                    wit_code,
                    wasm_bytes: Vec::new(),
                }
            }
            OutputFormat::Wasm => {
                let wasm_options = codegen::WasmWithWitOptions {
                    namespace: wit_options.namespace.clone(),
                    name: wit_options.name.clone(),
                    version: wit_options.version.clone(),
                    global_defaults: lowered.global_defaults().clone(),
                    global_default_exprs: lowered.global_default_exprs().to_vec(),
                    wasm_opt_args: None,
                };
                let wasm_bytes = if !lowered.components().is_empty() {
                    // Pass the assembled module to codegen with package options
                    match codegen::generate_wasm_module(&lowered.module, ctx, &wasm_options) {
                        Ok(bytes) => bytes,
                        Err(e) => {
                            let msg = format!("WASM generation error: {}", e);
                            return CompileOutcome::Failure(vec![Diagnostic {
                                message: msg.clone(),
                                rendered: format!("error: {}", msg),
                                line: 0,
                                column: 0,
                                length: 1,
                                severity: "error".to_string(),
                            }]);
                        }
                    }
                } else {
                    Vec::new()
                };
                CompileResult {
                    rust_code: String::new(),
                    wit_code: String::new(),
                    wasm_bytes,
                }
            }
        };

        CompileOutcome::Success(compile_result)
    }
}
