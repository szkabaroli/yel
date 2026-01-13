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

/// A single diagnostic message.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Option<Span>,
    pub code: Option<String>,
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

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
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
        if let Some(span) = self.span {
            if let Some(source) = source_map.get(span.source) {
                let (line, col) = source.line_col(span.start);
                output.push_str(&format!("  --> {}:{}:{}\n", source.name(), line, col));
                output.push_str(&source.snippet(line, 1));
                output.push('\n');
            }
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

    pub fn error(&mut self, span: Span, message: impl Into<String>) {
        self.push(Diagnostic::error(message).with_span(span));
    }

    pub fn warning(&mut self, span: Span, message: impl Into<String>) {
        self.push(Diagnostic::warning(message).with_span(span));
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
