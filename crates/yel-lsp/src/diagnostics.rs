//! Diagnostic conversion from yel-core to LSP.

use yel_core::{CompileError, Severity, SourceId, Span};
use ropey::Rope;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Convert an yel-core Diagnostic directly to LSP Diagnostics.
pub fn convert_yel_diagnostic(
    diag: &yel_core::Diagnostic,
    expected_source: SourceId,
    rope: &Rope,
) -> Vec<Diagnostic> {
    tracing::debug!(
        "Converting yel diagnostic: code={:?}, message={}, span={:?}",
        diag.code,
        diag.message,
        diag.span,
    );

    // Determine the primary range
    let range = if let Some(span) = &diag.span {
        // Only show diagnostics for the current file
        if span.source != expected_source {
            tracing::debug!("Skipping diagnostic - wrong source");
            return Vec::new();
        }
        span_to_range(span, rope)
    } else {
        // Fallback to start of document
        Range::new(Position::new(0, 0), Position::new(0, 1))
    };

    // Build the message with notes
    let mut message = diag.message.clone();

    // Add notes
    for note in &diag.notes {
        message.push_str("\n\nnote: ");
        message.push_str(note);
    }

    vec![Diagnostic {
        range,
        severity: Some(convert_severity(diag.severity)),
        code: diag
            .code
            .clone()
            .map(tower_lsp::lsp_types::NumberOrString::String),
        source: Some("yel".to_string()),
        message,
        related_information: None,
        tags: None,
        code_description: None,
        data: None,
    }]
}

/// Convert a byte offset to an LSP Position using the rope.
fn offset_to_position(offset: usize, rope: &Rope) -> Position {
    let offset = offset.min(rope.len_chars());
    let line = rope.char_to_line(offset);
    let line_start = rope.line_to_char(line);
    let character = offset - line_start;
    Position::new(line as u32, character as u32)
}

/// Convert an yel-core Span to an LSP Range.
fn span_to_range(span: &Span, rope: &Rope) -> Range {
    let start = offset_to_position(span.start, rope);
    let end = offset_to_position(span.end, rope);
    Range::new(start, end)
}

/// Convert yel-core Severity to LSP DiagnosticSeverity.
fn convert_severity(severity: Severity) -> DiagnosticSeverity {
    match severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Note => DiagnosticSeverity::INFORMATION,
    }
}

/// Convert a CompileError to LSP Diagnostics.
pub fn convert_compile_error(
    error: &CompileError,
    _expected_source: SourceId,
    _rope: &Rope,
) -> Vec<Diagnostic> {
    // CompileError doesn't have a to_diagnostic method in the current API
    // Just create a simple diagnostic from the error message
    let message = error.to_string();

    tracing::debug!("Converting compile error: {}", message);

    // Use the first line as the range since we don't have span info
    let range = Range::new(Position::new(0, 0), Position::new(0, 1));

    vec![Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        source: Some("yel".to_string()),
        message,
        related_information: None,
        tags: None,
        code_description: None,
        data: None,
    }]
}
