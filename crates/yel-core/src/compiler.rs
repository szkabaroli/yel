//! Main compiler driver.
//!
//! This module provides the high-level API for compiling .yel files
//! through the entire pipeline: Parse → HIR → THIR → LIR → Codegen.

use crate::context::CompilerContext;
use crate::diagnostic::Diagnostic;
use crate::hir::{lower_file, HirComponent};
use crate::lir::{lower_component as lower_to_lir, LirComponent};
use crate::source::{SourceId, Span};
use crate::stdlib_lookup::lookup_known_definitions;
use crate::syntax::ast::File;
use crate::syntax::parser::{parse_file_with_source_id, CatchedError, ParseError};
use crate::thir::{type_check, ThirComponent};

use std::path::Path;

/// Result type for compilation.
pub type CompileResult<T> = Result<T, CompileError>;

/// Compilation error.
#[derive(Debug)]
pub enum CompileError {
    /// Parse error.
    Parse(ParseError),
    /// Type error (from diagnostics).
    Type(String),
    /// IO error.
    Io(std::io::Error),
}

impl From<ParseError> for CompileError {
    fn from(e: ParseError) -> Self {
        CompileError::Parse(e)
    }
}

impl From<std::io::Error> for CompileError {
    fn from(e: std::io::Error) -> Self {
        CompileError::Io(e)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Parse(e) => write!(f, "parse error: {}", e),
            CompileError::Type(e) => write!(f, "type error: {}", e),
            CompileError::Io(e) => write!(f, "io error: {}", e),
        }
    }
}

impl std::error::Error for CompileError {}

/// Convert a CatchedError to a Diagnostic.
fn catched_error_to_diagnostic(e: &CatchedError) -> Diagnostic {
    Diagnostic::error(&e.message).with_span(e.span)
}

/// Convert a ParseError to a Diagnostic.
fn parse_error_to_diagnostic(e: &ParseError, source_id: SourceId) -> Diagnostic {
    match e {
        ParseError::Syntax { message, span, .. } => {
            let diag = Diagnostic::error(message.clone());
            if let Some(s) = span {
                diag.with_span(*s)
            } else {
                // Use source_id with position 0 as fallback
                diag.with_span(Span::new(source_id, 0, 1))
            }
        }
        ParseError::UnexpectedRule { expected, found, span } => {
            let diag = Diagnostic::error(format!("expected {}, found {}", expected, found));
            if let Some(s) = span {
                diag.with_span(*s)
            } else {
                diag
            }
        }
        ParseError::Missing(what) => {
            Diagnostic::error(format!("missing required element: {}", what))
        }
        ParseError::InvalidCallBase { span } => {
            let diag = Diagnostic::error("invalid call base: only identifiers and member expressions can be called");
            if let Some(s) = span {
                diag.with_span(*s)
            } else {
                diag
            }
        }
    }
}

/// Compiler instance.
pub struct Compiler {
    ctx: CompilerContext,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    /// Create a new compiler.
    pub fn new() -> Self {
        let mut ctx = CompilerContext::new();
        // Register builtin elements and types
        lookup_known_definitions(&mut ctx);
        Self { ctx }
    }

    /// Get the compiler context.
    pub fn context(&self) -> &CompilerContext {
        &self.ctx
    }

    /// Get mutable compiler context.
    pub fn context_mut(&mut self) -> &mut CompilerContext {
        &mut self.ctx
    }

    /// Load and parse a source file.
    ///
    /// On parse error, adds the error to diagnostics.
    /// Caught syntax errors are also added to diagnostics.
    pub fn load_file(&mut self, path: impl AsRef<Path>) -> CompileResult<File> {
        let path = path.as_ref();
        let content = std::fs::read_to_string(path)?;
        let source_id = self.ctx.source_map.add_file(path.to_path_buf(), &content);
        match parse_file_with_source_id(&content, source_id) {
            Ok(result) => {
                // Add caught errors to diagnostics
                for e in &result.catched_errors {
                    self.ctx.diagnostics.push(catched_error_to_diagnostic(e));
                }
                Ok(result.file)
            }
            Err(e) => {
                self.ctx.diagnostics.push(parse_error_to_diagnostic(&e, source_id));
                Err(e.into())
            }
        }
    }

    /// Parse a source string.
    ///
    /// On success, returns the parsed File.
    /// On error, adds the error to diagnostics and returns an error.
    /// Caught syntax errors are also added to diagnostics.
    /// Callers should check `has_errors()` after calling this.
    pub fn parse(&mut self, source: &str) -> CompileResult<File> {
        let source_id = self.ctx.source_map.add_inline(source);
        match parse_file_with_source_id(source, source_id) {
            Ok(result) => {
                // Add caught errors to diagnostics
                for e in &result.catched_errors {
                    self.ctx.diagnostics.push(catched_error_to_diagnostic(e));
                }
                Ok(result.file)
            }
            Err(e) => {
                self.ctx.diagnostics.push(parse_error_to_diagnostic(&e, source_id));
                Err(e.into())
            }
        }
    }

    /// Parse with a specific source ID.
    ///
    /// On parse error, adds the error to diagnostics.
    /// Caught syntax errors are also added to diagnostics.
    pub fn parse_with_source_id(
        &mut self,
        source: &str,
        source_id: SourceId,
    ) -> CompileResult<File> {
        match parse_file_with_source_id(source, source_id) {
            Ok(result) => {
                // Add caught errors to diagnostics
                for e in &result.catched_errors {
                    self.ctx.diagnostics.push(catched_error_to_diagnostic(e));
                }
                Ok(result.file)
            }
            Err(e) => {
                self.ctx.diagnostics.push(parse_error_to_diagnostic(&e, source_id));
                Err(e.into())
            }
        }
    }

    /// Lower an AST file to HIR.
    pub fn lower_to_hir(&mut self, file: &File) -> Vec<HirComponent> {
        lower_file(file, &mut self.ctx)
    }

    /// Type check an HIR component to THIR.
    pub fn type_check(&mut self, hir: &HirComponent) -> ThirComponent {
        type_check(hir, &mut self.ctx)
    }

    /// Lower a THIR component to LIR.
    pub fn lower_to_lir(&self, thir: &ThirComponent) -> LirComponent {
        lower_to_lir(thir, &self.ctx)
    }
    
    /// Check if there were any errors.
    pub fn has_errors(&self) -> bool {
        self.ctx.has_errors()
    }

    /// Get the number of errors.
    pub fn error_count(&self) -> usize {
        self.ctx.error_count()
    }

    /// Render all diagnostics.
    pub fn render_diagnostics(&self) -> String {
        self.ctx.render_diagnostics()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // BASIC COMPILATION TESTS
    // ========================================================================

    #[test]
    fn test_compile_simple_component() {
        let mut compiler = Compiler::new();

        let source = r#"
            component Counter {
                count: s32 = 0;
            }
        "#;

        let file = compiler.parse(source).unwrap();
        assert_eq!(file.components.len(), 1);

        let hir = compiler.lower_to_hir(&file);
        assert_eq!(hir.len(), 1);

        // Verify definitions were registered
        let def_id = compiler.ctx.lookup_component("Counter");
        assert!(def_id.is_some());
    }

    #[test]
    fn test_compile_with_record() {
        let mut compiler = Compiler::new();

        let source = r#"
            record Point {
                x: s32,
                y: s32,
            }

            component App {
                pos: Point;
            }
        "#;

        let file = compiler.parse(source).unwrap();
        assert_eq!(file.records.len(), 1);
        assert_eq!(file.components.len(), 1);

        let hir = compiler.lower_to_hir(&file);
        assert_eq!(hir.len(), 1);

        // Verify both definitions were registered
        assert!(compiler.ctx.lookup_type("Point").is_some());
        assert!(compiler.ctx.lookup_component("App").is_some());
    }

    #[test]
    fn test_full_pipeline() {
        let mut compiler = Compiler::new();

        let source = r#"
            component Counter {
                count: s32 = 0;
            }
        "#;

        // Parse
        let file = compiler.parse(source);
        assert!(file.is_ok(), "Parse failed: {:?}", file);
        let file = file.unwrap();

        // Lower to HIR
        let hir = compiler.lower_to_hir(&file);
        assert_eq!(hir.len(), 1, "Expected 1 component");

        // Type check
        let thir = compiler.type_check(&hir[0]);

        // Lower to LIR
        let lir = compiler.lower_to_lir(&thir);

        // Verify component name
        let name = compiler.context().str(lir.name);
        assert_eq!(name, "Counter");
    }

    // ========================================================================
    // PARSE ERROR TESTS
    // ========================================================================

    #[test]
    fn test_parse_error_missing_type() {
        let mut compiler = Compiler::new();
        let result = compiler.parse("component Foo { x: ; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_missing_brace() {
        let mut compiler = Compiler::new();
        let result = compiler.parse("component Foo { x: s32;");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_missing_component_name() {
        let mut compiler = Compiler::new();
        let result = compiler.parse("component { }");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_empty_source() {
        let mut compiler = Compiler::new();
        let result = compiler.parse("");
        assert!(result.is_err());
    }

    // ========================================================================
    // MULTIPLE COMPONENT TESTS
    // ========================================================================

    #[test]
    fn test_compile_multiple_components() {
        let mut compiler = Compiler::new();

        let source = r#"
            component Child {}
            component Parent {
                Child {}
            }
        "#;

        let file = compiler.parse(source).unwrap();
        assert_eq!(file.components.len(), 2);

        let hir = compiler.lower_to_hir(&file);
        assert_eq!(hir.len(), 2);

        assert!(compiler.ctx.lookup_component("Child").is_some());
        assert!(compiler.ctx.lookup_component("Parent").is_some());
    }

    #[test]
    fn test_compile_component_reference() {
        let mut compiler = Compiler::new();

        let source = r#"
            component Button {}
            component App {
                Button {}
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 2);
        assert!(!compiler.has_errors());
    }

    // ========================================================================
    // RECORD AND ENUM TESTS
    // ========================================================================

    #[test]
    fn test_compile_enum() {
        let mut compiler = Compiler::new();

        let source = r#"
            enum Status {
                pending,
                active,
                done,
            }

            component App {
                status: Status;
            }
        "#;

        let file = compiler.parse(source).unwrap();
        assert_eq!(file.enums.len(), 1);

        let hir = compiler.lower_to_hir(&file);
        assert_eq!(hir.len(), 1);

        assert!(compiler.ctx.lookup_type("Status").is_some());
    }

    #[test]
    fn test_compile_variant() {
        let mut compiler = Compiler::new();

        let source = r#"
            variant Message {
                text(string),
                image(string),
                empty,
            }

            component Chat {
                msg: Message;
            }
        "#;

        let file = compiler.parse(source).unwrap();
        assert_eq!(file.variants.len(), 1);

        let hir = compiler.lower_to_hir(&file);
        assert_eq!(hir.len(), 1);

        assert!(compiler.ctx.lookup_type("Message").is_some());
    }

    #[test]
    fn test_record_field_lookup() {
        let mut compiler = Compiler::new();

        let source = r#"
            record Person {
                name: string,
                age: s32,
            }

            component App {
                person: Person;
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let _hir = compiler.lower_to_hir(&file);

        let person_id = compiler.ctx.lookup_type("Person").unwrap();
        let name_interned = compiler.ctx.intern("name");
        let (field_idx, _) = compiler.ctx.defs.find_field(person_id, name_interned).unwrap();
        assert_eq!(field_idx.index(), 0);

        let age_interned = compiler.ctx.intern("age");
        let (field_idx, _) = compiler.ctx.defs.find_field(person_id, age_interned).unwrap();
        assert_eq!(field_idx.index(), 1);
    }

    // ========================================================================
    // FOR LOOP TESTS
    // ========================================================================

    #[test]
    fn test_for_loop_basic() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                items: list<s32>;
                for item in items {
                    Text { "{item}" }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
        assert!(!compiler.has_errors(), "Should not have errors: {}", compiler.render_diagnostics());
    }

    #[test]
    fn test_for_loop_with_key() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                items: list<s32>;
                for item in items key(item) {
                    Text { "{item}" }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_for_loop_with_range() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                for i in 0..10 {
                    Text { "{i}" }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    // ========================================================================
    // IF CONDITION TESTS
    // ========================================================================

    #[test]
    fn test_if_condition_basic() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                show: bool = true;
                if show {
                    Text { "visible" }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
        assert!(!compiler.has_errors());
    }

    #[test]
    fn test_if_else() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                show: bool = true;
                if show {
                    Text { "yes" }
                } else {
                    Text { "no" }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_if_else_if() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                value: s32 = 0;
                if value > 0 {
                    Text { "positive" }
                } else if value < 0 {
                    Text { "negative" }
                } else {
                    Text { "zero" }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    // ========================================================================
    // HANDLER TESTS
    // ========================================================================

    #[test]
    fn test_handler_basic() {
        let mut compiler = Compiler::new();

        let source = r#"
            component Counter {
                count: s32 = 0;
                Button {
                    clicked: { count = count + 1; }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
        assert!(!compiler.has_errors());
    }

    #[test]
    fn test_handler_compound_assignment() {
        let mut compiler = Compiler::new();

        let source = r#"
            component Counter {
                count: s32 = 0;
                Button {
                    clicked: { count += 1; }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    // ========================================================================
    // TYPE CHECKING TESTS
    // ========================================================================

    #[test]
    fn test_type_check_property_reference() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                count: s32 = 0;
                Text { "{count}" }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert!(!compiler.has_errors());

        let thir = compiler.type_check(&hir[0]);
        assert!(!compiler.has_errors(), "Type check failed: {}", compiler.render_diagnostics());

        // Component should have one Text node
        assert_eq!(thir.body.len(), 1);
    }

    #[test]
    fn test_type_check_binary_expression() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                a: s32 = 1;
                b: s32 = 2;
                Text { "{a + b}" }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);
        let _thir = compiler.type_check(&hir[0]);

        assert!(!compiler.has_errors());
    }

    #[test]
    fn test_type_check_comparison() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                count: s32 = 0;
                if count > 0 {
                    Text { "positive" }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);
        let _thir = compiler.type_check(&hir[0]);

        assert!(!compiler.has_errors());
    }

    // ========================================================================
    // EXPRESSION TESTS
    // ========================================================================

    #[test]
    fn test_ternary_expression() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                show: bool = true;
                label: string = show ? "yes" : "no";
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_member_access() {
        let mut compiler = Compiler::new();

        let source = r#"
            record Point {
                x: s32,
                y: s32,
            }

            component App {
                pos: Point;
                Text { "{pos.x}" }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_string_interpolation() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                name: string = "World";
                Text { "Hello, {name}!" }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    // ========================================================================
    // FUNCTION/CALLBACK TESTS
    // ========================================================================

    #[test]
    fn test_callback_declaration() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                count: s32 = 0;
                export increment: func();
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_callback_with_params() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                count: s32 = 0;
                export add: func(value: s32);
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_callback_with_return() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                count: s32 = 0;
                export get-count: func() -> s32;
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    // ========================================================================
    // STDLIB TESTS (Basic elements should work)
    // ========================================================================

    #[test]
    fn test_stdlib_elements_parse() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                VStack {
                    Text { "Hello" }
                    HStack {
                        Button { label: "Click" }
                    }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_text_with_bindings() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                Text {
                    content: "Hello"
                    color: #ff0000
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    // ========================================================================
    // COMPLEX SCENARIOS
    // ========================================================================

    #[test]
    fn test_nested_components() {
        let mut compiler = Compiler::new();

        let source = r#"
            component Inner {}
            component Middle {
                Inner {}
            }
            component Outer {
                Middle {}
            }
        "#;

        let file = compiler.parse(source).unwrap();
        assert_eq!(file.components.len(), 3);

        let hir = compiler.lower_to_hir(&file);
        assert_eq!(hir.len(), 3);
    }

    #[test]
    fn test_list_type_in_for() {
        let mut compiler = Compiler::new();

        let source = r#"
            record Person {
                name: string,
            }

            component App {
                people: list<Person>;
                for person in people {
                    Text { "{person.name}" }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_export_component() {
        let mut compiler = Compiler::new();

        let source = r#"
            export component App {
                value: s32 = 42;
            }
        "#;

        let file = compiler.parse(source).unwrap();
        assert!(file.components[0].node.is_export);
    }

    #[test]
    fn test_option_type() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                name: option<string>;
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_result_type() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                data: result<s32, string>;
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_unit_literals() {
        let mut compiler = Compiler::new();

        let source = r#"
            component App {
                VStack {
                    width: 100px
                    height: 50%
                    padding: 8pt
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);
    }

    #[test]
    fn test_wasm_generation_and_validation() {
        use crate::codegen::generate_wasm;

        let source = r#"
            export component Counter {
                count: s32 = 0;

                VStack {
                    Text { "Count: {count}" },
                    Button {
                        "+",
                        clicked: { count += 1; }
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

        // Generate WASM
        let wasm_bytes = generate_wasm(&[lir], compiler.context()).expect("WASM generation failed");

        assert!(wasm_bytes.len() > 100, "WASM too small: {} bytes", wasm_bytes.len());

        // Write to temp file for validation
        std::fs::write("/tmp/test_counter.wasm", &wasm_bytes).expect("Failed to write WASM");

        println!("Generated {} bytes of WASM", wasm_bytes.len());
    }
}
