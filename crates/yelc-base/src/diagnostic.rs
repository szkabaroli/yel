//! Diagnostic types for error reporting.

use crate::source::{SourceMap, Span};
use std::fmt;

/// Severity level of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

/// Stable diagnostic codes.
///
/// The `E####` / `W####` string returned by [`ErrorCode::code`] is the *stable
/// identity* of a diagnostic: users can search it, docs can cross-reference it,
/// and tests can assert on it — all while the human-readable message wording
/// stays free to change. Codes are grouped into bands of ten by category so a
/// category can grow without renumbering. `W`-prefixed codes are warnings.
///
/// When adding a diagnostic, add (or reuse) a variant here rather than emitting
/// an un-coded error — see the `diag-error-codes` rule.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    // — Type checking —
    /// Expected one type, found another.
    TypeMismatch,
    /// A type could not be inferred and no annotation was given.
    CannotInferType,
    /// An integer literal does not fit its declared/expected type.
    IntLiteralOutOfRange,
    /// An unknown unit suffix on a literal (e.g. `10xyz`).
    UnknownUnitSuffix,
    /// A comparison operator (`==`, `<`, …) was applied to a type that does
    /// not support comparison (a GC composite: string/list/tuple/option/
    /// result/record/variant). Only scalars and enums are comparable.
    UncomparableType,

    // — Name resolution —
    /// A name is defined more than once in the same scope.
    DuplicateDefinition,
    /// A referenced name/type/function does not resolve to any definition.
    UnresolvedName,
    /// No such case in an enum/variant.
    NoSuchCase,
    /// No such function/property/method on a value (e.g. a global).
    NoSuchMember,

    // — Records & fields —
    /// A required record field is missing from a literal.
    MissingField,
    /// A record literal was written where a record type was not expected.
    NotARecord,
    /// Field access against a type that has no such field.
    NoSuchField,

    // — Calls & arity —
    /// A call/constructor got the wrong number of arguments.
    WrongArgCount,
    /// The callee of a call is not a callable form.
    InvalidCallBase,

    // — Components & structural —
    /// More than one `@children` slot declared on a component.
    DuplicateChildrenSlot,
    /// Child nodes passed to a component that declares no `@children` slot.
    MissingChildrenSlot,
    /// A required element/declaration is missing.
    MissingElement,
    /// A component instantiates itself.
    RecursiveInstantiation,
    /// An empty aggregate (`record`/`enum`/`variant` with no fields/cases) is
    /// exposed across the component boundary — the WebAssembly component model
    /// requires each to have at least one field/case.
    EmptyTypeAtBoundary,
    /// A malformed two-way `set value:` binding.
    InvalidValueBinding,

    // — Syntax & driver —
    /// A parse-level syntax error surfaced from the front-end.
    SyntaxError,
    /// A package identifier is not a valid WIT kebab-case name.
    InvalidPackageName,
    /// A file in a package directory carries no `package` declaration.
    ///
    /// Distinct from [`ErrorCode::PackageNameMismatch`] on purpose: absence has
    /// no other file to point at, so it is reported against the one file alone.
    MissingPackageDecl,
    /// Two files in one package directory declare different packages.
    PackageNameMismatch,

    // — Warnings —
    /// A setter writes a signal that its companion getter also reads.
    SetterOverwritesGetter,
}

impl ErrorCode {
    /// The stable `E####` / `W####` string for this code.
    pub fn code(self) -> &'static str {
        match self {
            ErrorCode::TypeMismatch => "E0001",
            ErrorCode::CannotInferType => "E0002",
            ErrorCode::IntLiteralOutOfRange => "E0003",
            ErrorCode::UnknownUnitSuffix => "E0004",
            ErrorCode::UncomparableType => "E0005",
            ErrorCode::DuplicateDefinition => "E0010",
            ErrorCode::UnresolvedName => "E0011",
            ErrorCode::NoSuchCase => "E0012",
            ErrorCode::NoSuchMember => "E0013",
            ErrorCode::MissingField => "E0020",
            ErrorCode::NotARecord => "E0021",
            ErrorCode::NoSuchField => "E0022",
            ErrorCode::WrongArgCount => "E0030",
            ErrorCode::InvalidCallBase => "E0031",
            ErrorCode::DuplicateChildrenSlot => "E0040",
            ErrorCode::MissingChildrenSlot => "E0041",
            ErrorCode::MissingElement => "E0042",
            ErrorCode::RecursiveInstantiation => "E0043",
            ErrorCode::EmptyTypeAtBoundary => "E0044",
            ErrorCode::InvalidValueBinding => "E0050",
            ErrorCode::SyntaxError => "E0060",
            ErrorCode::InvalidPackageName => "E0070",
            ErrorCode::MissingPackageDecl => "E0071",
            ErrorCode::PackageNameMismatch => "E0072",
            ErrorCode::SetterOverwritesGetter => "W0001",
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.code())
    }
}

/// A single diagnostic message.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Option<Span>,
    pub code: Option<ErrorCode>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span: None,
            code: None,
            notes: Vec::new(),
        }
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            span: None,
            code: None,
            notes: Vec::new(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = Some(code);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Render the diagnostic with source context.
    pub fn render(&self, source_map: &SourceMap) -> String {
        let mut output = String::new();

        // Severity and code
        let severity_str = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        };

        if let Some(ref code) = self.code {
            output.push_str(&format!("{}[{}]: {}\n", severity_str, code, self.message));
        } else {
            output.push_str(&format!("{}: {}\n", severity_str, self.message));
        }

        // Source location
        if let Some(span) = self.span
            && let Some(source) = source_map.get(span.source)
        {
            let (line, col) = source.line_col(span.start);
            output.push_str(&format!("  --> {}:{}:{}\n", source.name(), line, col));
            output.push_str(&source.snippet(line, 1));
            output.push('\n');
        }

        // Notes
        for note in &self.notes {
            output.push_str(&format!("  = note: {}\n", note));
        }

        output
    }
}

/// A collection of diagnostics.
#[derive(Debug, Clone, Default)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag);
    }

    pub fn error(&mut self, span: Span, code: ErrorCode, message: impl Into<String>) {
        self.push(Diagnostic::error(message).with_span(span).with_code(code));
    }

    pub fn warning(&mut self, span: Span, code: ErrorCode, message: impl Into<String>) {
        self.push(Diagnostic::warning(message).with_span(span).with_code(code));
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
    }

    pub fn error_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .count()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }

    /// Render all diagnostics.
    pub fn render(&self, source_map: &SourceMap) -> String {
        self.diagnostics
            .iter()
            .map(|d| d.render(source_map))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl fmt::Display for Diagnostics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for diag in &self.diagnostics {
            writeln!(
                f,
                "{}: {}",
                match diag.severity {
                    Severity::Error => "error",
                    Severity::Warning => "warning",
                    Severity::Note => "note",
                },
                diag.message
            )?;
        }
        Ok(())
    }
}

impl std::error::Error for Diagnostics {}
