//! Diagnostic conversion from yel-core to LSP.

use ropey::Rope;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use yel_core::{CompileError, ParseError, Severity, SourceId, Span};

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
            .map(|c| tower_lsp::lsp_types::NumberOrString::String(c.code().to_string())),
        source: Some("yel".to_string()),
        message,
        related_information: None,
        tags: None,
        code_description: None,
        data: None,
    }]
}

/// Convert a character offset to an LSP Position using the rope.
fn offset_to_position(offset: usize, rope: &Rope) -> Position {
    let offset = offset.min(rope.len_chars());
    let line = rope.char_to_line(offset);
    let line_start = rope.line_to_char(line);
    let character = offset - line_start;
    Position::new(line as u32, character as u32)
}

/// Convert an yel-core Span (UTF-8 byte offsets) to an LSP Range.
fn span_to_range(span: &Span, rope: &Rope) -> Range {
    let len_bytes = rope.len_bytes();
    let start_b = span.start.min(len_bytes);
    let end_b = span.end.min(len_bytes);
    let start_char = rope.try_byte_to_char(start_b).unwrap_or(0);
    let end_char = rope
        .try_byte_to_char(end_b)
        .unwrap_or(start_char)
        .max(start_char);
    let start = offset_to_position(start_char, rope);
    let end = offset_to_position(end_char, rope);
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

/// Convert a CompileError to LSP Diagnostics (fallback when context diagnostics are unavailable).
pub fn convert_compile_error(
    error: &CompileError,
    expected_source: SourceId,
    rope: &Rope,
) -> Vec<Diagnostic> {
    let message = error.to_string();

    tracing::debug!("Converting compile error: {}", message);

    let range = match error {
        CompileError::Parse(pe) => parse_error_range(pe, expected_source, rope),
        _ => Range::new(Position::new(0, 0), Position::new(0, 1)),
    };

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

fn parse_error_range(pe: &ParseError, expected_source: SourceId, rope: &Rope) -> Range {
    if let Some(span) = pe.span()
        && span.source == expected_source
    {
        return span_to_range(&span, rope);
    }
    Range::new(Position::new(0, 0), Position::new(0, 1))
}
