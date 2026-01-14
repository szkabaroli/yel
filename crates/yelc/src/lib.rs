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

#[cfg(target_arch = "wasm32")]
mod wasi_impl {
    use yel_core::{Compiler, codegen};
    use yel_core::diagnostic::Severity;

    // Generate bindings from WIT
    wit_bindgen::generate!({
        world: "yel-compiler",
        path: "wit",
    });

    use exports::yel::compiler::compiler::{
        CompileOutcome, CompileResult, Diagnostic, Guest, OutputFormat, VersionInfo,
    };

    use super::build;

    /// Convert internal diagnostics to WIT diagnostics.
    fn convert_diagnostics(compiler: &Compiler) -> Vec<Diagnostic> {
        let ctx = compiler.context();
        ctx.diagnostics
            .iter()
            .map(|d| {
                let (line, column, length) = if let Some(span) = d.span {
                    if let Some(source) = ctx.source_map.get(span.source) {
                        let (l, c) = source.line_col(span.start);
                        let len = (span.end - span.start) as u32;
                        (l as u32, c as u32, len.max(1))
                    } else {
                        (0, 0, 1)
                    }
                } else {
                    (0, 0, 1)
                };

                let severity = match d.severity {
                    Severity::Error => "error",
                    Severity::Warning => "warning",
                    Severity::Note => "info",
                };

                Diagnostic {
                    message: d.message.clone(),
                    rendered: d.render(&ctx.source_map),
                    line,
                    column,
                    length,
                    severity: severity.to_string(),
                }
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

        fn compile(
            filename: String,
            source: String,
            format: OutputFormat,
        ) -> CompileOutcome {
            compile_impl(vec![(filename, source)], format)
        }

        fn compile_multi(
            files: Vec<(String, String)>,
            format: OutputFormat,
        ) -> CompileOutcome {
            compile_impl(files, format)
        }

        fn parse_to_json(source: String) -> Result<String, String> {
            match yel_core::syntax::parser::parse_file(&source) {
                Ok(result) => {
                    serde_json::to_string_pretty(&result.file)
                        .map_err(|e| format!("JSON serialization error: {}", e))
                }
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

    fn compile_impl(files: Vec<(String, String)>, format: OutputFormat) -> CompileOutcome {
        let mut compiler = Compiler::new();
        let mut lir_components = Vec::new();
        let mut all_hir_components = Vec::new();
        let mut all_thir_components = Vec::new();
        let mut package_info: Option<yel_core::syntax::ast::PackageId> = None;

        for (_, source) in &files {
            // Parse - errors are automatically added to diagnostics
            let parsed = match compiler.parse(source) {
                Ok(p) => p,
                Err(_) => return CompileOutcome::Failure(convert_diagnostics(&compiler)),
            };

            // Extract package info from first file that has it
            if package_info.is_none() {
                package_info = parsed.package.clone();
            }

            let hir_components_file = compiler.lower_to_hir(&parsed);

            if compiler.has_errors() {
                return CompileOutcome::Failure(convert_diagnostics(&compiler));
            }

            // Store HIR for potential serialization
            all_hir_components.extend(hir_components_file.clone());

            for hir in &hir_components_file {
                let thir = compiler.type_check(hir);

                if compiler.has_errors() {
                    return CompileOutcome::Failure(convert_diagnostics(&compiler));
                }

                // Store THIR for potential serialization
                all_thir_components.push(thir.clone());

                let lir = compiler.lower_to_lir(&thir);
                lir_components.push(lir);
            }
        }

        let ctx = compiler.context();

        // Build WitOptions from package info
        let wit_options = if let Some(ref pkg) = package_info {
            codegen::WitOptions {
                namespace: pkg.namespace.clone(),
                name: pkg.name.clone(),
                version: pkg.version.clone().unwrap_or_else(|| "0.1.0".to_string()),
                include_dom_interface: true,
            }
        } else {
            codegen::WitOptions {
                namespace: "yel".to_string(),
                name: "app".to_string(),
                version: "0.1.0".to_string(),
                include_dom_interface: true,
            }
        };

        // Generate output based on format
        let compile_result = match format {
            OutputFormat::Rust => {
                // Rust codegen temporarily disabled - needs update for block-based LIR
                CompileResult {
                    rust_code: "// Rust codegen not available - use WASM output instead\n".to_string(),
                    wit_code: String::new(),
                    wasm_bytes: Vec::new(),
                    wast_code: String::new(),
                    hir_code: String::new(),
                    thir_code: String::new(),
                }
            }
            OutputFormat::Wit => {
                let mut wit_code = String::new();
                for lir in &lir_components {
                    if lir.is_export {
                        match codegen::generate_wit(lir, ctx, &wit_options) {
                            Ok(code) => {
                                wit_code.push_str(&code);
                                wit_code.push('\n');
                            }
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
                        }
                    }
                }
                CompileResult {
                    rust_code: String::new(),
                    wit_code,
                    wasm_bytes: Vec::new(),
                    wast_code: String::new(),
                    hir_code: String::new(),
                    thir_code: String::new(),
                }
            }
            OutputFormat::Wasm => {
                let wasm_options = codegen::WasmWithWitOptions {
                    namespace: wit_options.namespace.clone(),
                    name: wit_options.name.clone(),
                    version: wit_options.version.clone(),
                };
                let wasm_bytes = if !lir_components.is_empty() {
                    match codegen::generate_wasm_with_wit(&lir_components, ctx, &wasm_options) {
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
                    wast_code: String::new(),
                    hir_code: String::new(),
                    thir_code: String::new(),
                }
            }
            OutputFormat::Wast => {
                // Generate WASM bytes first, then convert to text format
                let wasm_options = codegen::WasmWithWitOptions {
                    namespace: wit_options.namespace.clone(),
                    name: wit_options.name.clone(),
                    version: wit_options.version.clone(),
                };
                let wast_code = if !lir_components.is_empty() {
                    match codegen::generate_wasm_with_wit(&lir_components, ctx, &wasm_options) {
                        Ok(bytes) => {
                            wasmprinter::print_bytes(&bytes).unwrap_or_else(|e| {
                                format!(";; WAST conversion error: {}", e)
                            })
                        }
                        Err(e) => format!(";; WASM generation error: {}", e),
                    }
                } else {
                    ";; No components to compile".to_string()
                };
                CompileResult {
                    rust_code: String::new(),
                    wit_code: String::new(),
                    wasm_bytes: Vec::new(),
                    wast_code,
                    hir_code: String::new(),
                    thir_code: String::new(),
                }
            }
            OutputFormat::Hir => {
                let hir_code = match serde_json::to_string_pretty(&all_hir_components) {
                    Ok(json) => json,
                    Err(e) => format!("// HIR serialization error: {}", e),
                };
                CompileResult {
                    rust_code: String::new(),
                    wit_code: String::new(),
                    wasm_bytes: Vec::new(),
                    wast_code: String::new(),
                    hir_code,
                    thir_code: String::new(),
                }
            }
            OutputFormat::Thir => {
                // THIR serialization would require implementing Serialize on THIR types
                CompileResult {
                    rust_code: String::new(),
                    wit_code: String::new(),
                    wasm_bytes: Vec::new(),
                    wast_code: String::new(),
                    hir_code: String::new(),
                    thir_code: "// THIR output not yet implemented".to_string(),
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
    use yel_core::{Compiler, codegen};
    use yel_core::diagnostic::Severity;

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

    /// Convert internal diagnostics to native diagnostics.
    fn convert_diagnostics(compiler: &Compiler) -> Vec<Diagnostic> {
        let ctx = compiler.context();
        ctx.diagnostics
            .iter()
            .map(|d| {
                let (line, column, length) = if let Some(span) = d.span {
                    if let Some(source) = ctx.source_map.get(span.source) {
                        let (l, c) = source.line_col(span.start);
                        let len = (span.end - span.start) as u32;
                        (l as u32, c as u32, len.max(1))
                    } else {
                        (0, 0, 1)
                    }
                } else {
                    (0, 0, 1)
                };

                let severity = match d.severity {
                    Severity::Error => "error",
                    Severity::Warning => "warning",
                    Severity::Note => "info",
                };

                Diagnostic {
                    message: d.message.clone(),
                    rendered: d.render(&ctx.source_map),
                    line,
                    column,
                    length,
                    severity: severity.to_string(),
                }
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
        let mut lir_components = Vec::new();
        let mut package_info: Option<yel_core::syntax::ast::PackageId> = None;

        for (_, source) in files {
            // Parse - errors are automatically added to diagnostics
            let parsed = match compiler.parse(source) {
                Ok(p) => p,
                Err(_) => return CompileOutcome::Failure(convert_diagnostics(&compiler)),
            };

            // Extract package info from first file that has it
            if package_info.is_none() {
                package_info = parsed.package.clone();
            }

            let hir_components_file = compiler.lower_to_hir(&parsed);

            if compiler.has_errors() {
                return CompileOutcome::Failure(convert_diagnostics(&compiler));
            }

            for hir in &hir_components_file {
                let thir = compiler.type_check(hir);

                if compiler.has_errors() {
                    return CompileOutcome::Failure(convert_diagnostics(&compiler));
                }

                let lir = compiler.lower_to_lir(&thir);
                lir_components.push(lir);
            }
        }

        let ctx = compiler.context();

        // Build WitOptions from package info
        let wit_options = if let Some(ref pkg) = package_info {
            codegen::WitOptions {
                namespace: pkg.namespace.clone(),
                name: pkg.name.clone(),
                version: pkg.version.clone().unwrap_or_else(|| "0.1.0".to_string()),
                include_dom_interface: true,
            }
        } else {
            codegen::WitOptions {
                namespace: "yel".to_string(),
                name: "app".to_string(),
                version: "0.1.0".to_string(),
                include_dom_interface: true,
            }
        };

        let compile_result = match format {
            OutputFormat::Rust => {
                // Rust codegen temporarily disabled - needs update for block-based LIR
                CompileResult {
                    rust_code: "// Rust codegen not available - use WASM output instead\n".to_string(),
                    wit_code: String::new(),
                    wasm_bytes: Vec::new(),
                }
            }
            OutputFormat::Wit => {
                let mut wit_code = String::new();
                for lir in &lir_components {
                    if lir.is_export {
                        match codegen::generate_wit(lir, ctx, &wit_options) {
                            Ok(code) => {
                                wit_code.push_str(&code);
                                wit_code.push('\n');
                            }
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
                        }
                    }
                }
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
                };
                let wasm_bytes = if !lir_components.is_empty() {
                    // Pass all components to generate_wasm_with_wit with package options
                    match codegen::generate_wasm_with_wit(&lir_components, ctx, &wasm_options) {
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
