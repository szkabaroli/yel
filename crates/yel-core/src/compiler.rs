//! Main compiler driver.
//!
//! This module provides the high-level API for compiling .yel files
//! through the entire pipeline: Parse → HIR → THIR → LIR → Codegen.

use crate::context::CompilerContext;
use crate::diagnostic::{Diagnostic, ErrorCode};
use crate::hir::{HirItem, lower_file};
use crate::ids::DefId;
use crate::lir::{
    LirExpr, LirGlobal, LirModule, LirResource, lower_component as lower_to_lir, lower_globals,
};
use crate::source::{SourceId, Span};
use crate::stdlib_lookup::lookup_known_definitions;
use crate::syntax::ast::{File, PackageId};
use crate::syntax::parser::{CatchedError, ParseError, parse_file_with_source_id};
use crate::thir::{ThirComponent, ThirExpr, ThirItem, type_check};

use rustc_hash::FxHashMap as HashMap;
use std::path::Path;

/// Result type for compilation.
pub type CompileResult<T> = Result<T, CompileError>;

/// Compilation error.
///
/// Type-checking failures are reported through [`crate::diagnostic`], not this
/// type — it only covers the up-front parse and IO failures that abort the
/// pipeline before diagnostics accumulate. Each variant preserves its
/// underlying error as the [`std::error::Error::source`] so callers can walk
/// the chain.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum CompileError {
    /// Parse error.
    #[error("parse error: {0}")]
    Parse(#[from] ParseError),
    /// IO error.
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
}

/// Convert a CatchedError to a Diagnostic.
fn catched_error_to_diagnostic(e: &CatchedError) -> Diagnostic {
    Diagnostic::error(&e.message)
        .with_span(e.span)
        .with_code(ErrorCode::SyntaxError)
}

/// Validate that `s` is a valid WIT kebab-case identifier:
/// one or more non-empty hyphen-separated segments, each starting with an
/// ASCII letter and containing only ASCII letters/digits.
fn validate_kebab_identifier(s: &str) -> Result<(), String> {
    if s.is_empty() {
        return Err("empty".into());
    }
    for segment in s.split('-') {
        if segment.is_empty() {
            return Err("empty segment (consecutive or trailing `-`)".into());
        }
        let mut chars = segment.chars();
        let first = chars.next().unwrap();
        if !first.is_ascii_alphabetic() {
            if first == '_' {
                return Err("underscores are not allowed (use `-`)".into());
            }
            return Err(format!(
                "segment `{}` must start with an ASCII letter",
                segment
            ));
        }
        for c in chars {
            if c == '_' {
                return Err("underscores are not allowed (use `-`)".into());
            }
            if !c.is_ascii_alphanumeric() {
                return Err(format!(
                    "segment `{}` contains invalid character `{}`",
                    segment, c
                ));
            }
        }
    }
    Ok(())
}

/// Convert a ParseError to a Diagnostic.
fn parse_error_to_diagnostic(e: &ParseError, source_id: SourceId) -> Diagnostic {
    match e {
        ParseError::Syntax { message, span, .. } => {
            let diag = Diagnostic::error(message.clone()).with_code(ErrorCode::SyntaxError);
            if let Some(s) = span {
                diag.with_span(*s)
            } else {
                // Use source_id with position 0 as fallback
                diag.with_span(Span::new(source_id, 0, 1))
            }
        }
        ParseError::UnexpectedRule {
            expected,
            found,
            span,
        } => {
            let diag = Diagnostic::error(format!("expected {}, found {}", expected, found))
                .with_code(ErrorCode::SyntaxError);
            if let Some(s) = span {
                diag.with_span(*s)
            } else {
                diag
            }
        }
        ParseError::Missing(what) => {
            Diagnostic::error(format!("missing required element: {}", what))
                .with_code(ErrorCode::MissingElement)
        }
        ParseError::InvalidCallBase { span } => {
            let diag = Diagnostic::error(
                "invalid call base: only identifiers and member expressions can be called",
            )
            .with_code(ErrorCode::InvalidCallBase);
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

    /// Build the module's **import-side boundary contract** and **host-import
    /// registry** as data on the module — one unified source for every
    /// imported function and the WIT interfaces that declare them. See
    /// [`CompilerContext::build_import_contract`]. `component_def_ids` are the
    /// components in resource order (for stable import indices).
    pub fn build_import_contract(
        &self,
        component_def_ids: &[DefId],
    ) -> (
        crate::index_vec::IndexVec<crate::ids::InterfaceId, crate::lir::LirInterface>,
        Vec<crate::lir::LirImport>,
    ) {
        self.ctx.build_import_contract(component_def_ids)
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
                self.ctx
                    .diagnostics
                    .push(parse_error_to_diagnostic(&e, source_id));
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
                self.validate_package(&result.file, source_id);
                Ok(result.file)
            }
            Err(e) => {
                self.ctx
                    .diagnostics
                    .push(parse_error_to_diagnostic(&e, source_id));
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
                self.validate_package(&result.file, source_id);
                Ok(result.file)
            }
            Err(e) => {
                self.ctx
                    .diagnostics
                    .push(parse_error_to_diagnostic(&e, source_id));
                Err(e.into())
            }
        }
    }

    /// Validate that the package identifier uses WIT-compatible naming.
    /// WIT requires kebab-case: each hyphen-separated segment non-empty, starts
    /// with a letter, and contains only ASCII alphanumerics. The grammar is
    /// permissive (underscores allowed, digit-leading segments allowed) so
    /// the parser doesn't reject, but we reject here with a clear message
    /// instead of letting `wit-component` fail later with an opaque
    /// `decoding custom section component-type` error.
    fn validate_package(&mut self, file: &File, source_id: SourceId) {
        let Some(pkg) = &file.package else { return };
        // Use a zero-length span at the start of the file — diagnostics
        // need a valid Span, and the package declaration is always the
        // very first thing in the source if present.
        let full_span = Span::point(source_id, 0);
        for (label, value) in [("namespace", &pkg.namespace), ("name", &pkg.name)] {
            if let Err(reason) = validate_kebab_identifier(value) {
                self.ctx.diagnostics.error(
                    full_span,
                    ErrorCode::InvalidPackageName,
                    format!(
                        "invalid package {} `{}`: {}. WIT package identifiers \
                         must be kebab-case (ASCII letters, digits, hyphens; \
                         each segment must start with a letter)",
                        label, value, reason,
                    ),
                );
            }
        }
    }

    /// Lower an AST file to HIR, producing one item per top-level unit
    /// (components and globals alike).
    pub fn lower_to_hir(&mut self, file: &File) -> Vec<HirItem> {
        lower_file(file, &mut self.ctx)
    }

    /// Type check one HIR item to THIR. The single type-checking entry —
    /// dispatches on the item kind internally.
    pub fn type_check(&mut self, item: &HirItem) -> ThirItem {
        type_check(item, &mut self.ctx)
    }

    /// Lower a THIR component to LIR.
    pub fn lower_to_lir(&self, thir: &ThirComponent) -> LirResource {
        lower_to_lir(thir, &self.ctx)
    }

    /// Module-level post-lowering pass: synthesize per-observer global
    /// fanout blocks and expand every `LirOp::TriggerEffects`
    /// placeholder into direct `CallBlock`s. MUST run after every
    /// component has been lowered and before codegen — codegen rejects
    /// any surviving `TriggerEffects`.
    pub fn resolve_global_triggers(&self, resources: &mut [LirResource]) {
        crate::lower_to_lir::resolve_global_triggers(&self.ctx, resources)
    }

    /// Lower type-checked global blocks to first-class LIR globals, plus the
    /// shared default-expression arena they index into.
    pub fn lower_globals_to_lir(
        &self,
        thir_defaults: &HashMap<DefId, ThirExpr>,
    ) -> (Vec<LirGlobal>, Vec<LirExpr>) {
        lower_globals(thir_defaults, &self.ctx)
    }

    /// Lower a whole module's HIR top-level items to a [`LirModule`] — the
    /// single uniform spine every driver (CLI, tests, fuzzer) shares instead
    /// of hand-rolling the item match plus the trailing module passes.
    ///
    /// Type-checks each item; components lower to resources, globals'
    /// type-checked defaults accumulate; then the module-level passes run:
    /// `resolve_global_triggers`, global lowering (→ `LirGlobal`s + the shared
    /// default-expr arena), and the import contract (`imports` + WIT
    /// `interfaces`). Errors **accumulate** in the context — callers check
    /// [`Self::has_errors`] and render diagnostics themselves rather than this
    /// returning `Result`.
    pub fn lower_items_to_module(
        &mut self,
        items: &[HirItem],
        package: Option<PackageId>,
    ) -> LirModule {
        // Type-check every item first. LIR lowering assumes well-typed input,
        // so if any item errored we stop before lowering and hand back an
        // empty module — the caller bails on `has_errors()`. Checking after
        // the whole list (not per item) means a file's type errors are all
        // reported at once, matching the accumulate-diagnostics convention.
        let thir_items: Vec<ThirItem> = items.iter().map(|item| self.type_check(item)).collect();
        if self.has_errors() {
            return LirModule {
                package,
                ..LirModule::default()
            };
        }

        let mut resources = Vec::new();
        let mut global_thir_defaults: HashMap<DefId, ThirExpr> = HashMap::default();
        for thir in thir_items {
            match thir {
                ThirItem::Component(thir) => resources.push(self.lower_to_lir(&thir)),
                ThirItem::Global(global) => {
                    global_thir_defaults.extend(global.signal_defaults);
                }
            }
        }
        // Module-level passes: synthesize per-observer global fanout blocks
        // and expand `TriggerEffects` placeholders (must run after every
        // component is lowered), then lower globals and build the import
        // contract off the finished resource set.
        self.resolve_global_triggers(&mut resources);
        let (globals, mut global_exprs) = self.lower_globals_to_lir(&global_thir_defaults);
        // Plan the module-start init in LIR (a block of ops), so the backend
        // transcribes it rather than building it imperatively.
        let global_init_block =
            crate::lower_to_lir::synth_globals_init_block(&self.ctx, &globals, &mut global_exprs);
        let component_def_ids: Vec<DefId> = resources.iter().map(|r| r.def_id).collect();
        let (interfaces, imports) = self.build_import_contract(&component_def_ids);
        LirModule {
            resources,
            globals,
            global_exprs,
            global_init_block,
            imports,
            interfaces,
            package,
        }
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
        let thir = compiler
            .type_check(&hir[0])
            .into_component()
            .expect("component");

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
    fn test_parse_empty_source() {
        // Empty sources are now legal: they parse to a file with no components.
        let mut compiler = Compiler::new();
        let file = compiler
            .parse("")
            .expect("empty source should parse successfully");
        assert!(file.components.is_empty());
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
            component MyButton {}
            component App {
                MyButton {}
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 2);
        assert!(
            !compiler.has_errors(),
            "diagnostics: {}",
            compiler.render_diagnostics()
        );
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
        let (field_idx, _) = compiler
            .ctx
            .defs
            .find_field(person_id, name_interned)
            .unwrap();
        assert_eq!(field_idx.index(), 0);

        let age_interned = compiler.ctx.intern("age");
        let (field_idx, _) = compiler
            .ctx
            .defs
            .find_field(person_id, age_interned)
            .unwrap();
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
        assert!(
            !compiler.has_errors(),
            "Should not have errors: {}",
            compiler.render_diagnostics()
        );
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

        let thir = compiler
            .type_check(&hir[0])
            .into_component()
            .expect("component");
        assert!(
            !compiler.has_errors(),
            "Type check failed: {}",
            compiler.render_diagnostics()
        );

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
}
