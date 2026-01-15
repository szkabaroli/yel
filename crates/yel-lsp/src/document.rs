//! Document state management.

use yel_core::Compiler;
use ropey::Rope;
use tower_lsp::lsp_types::*;

use crate::diagnostics::{convert_compile_error, convert_yel_diagnostic};

/// An open document in the editor.
pub struct Document {
    /// The document content as a rope for efficient editing.
    pub rope: Rope,
    /// Document version for incremental sync.
    pub version: i32,
}

impl Document {
    /// Create a new document from content.
    pub fn new(content: String, version: i32) -> Self {
        Self {
            rope: Rope::from_str(&content),
            version,
        }
    }

    /// Apply a content change from the editor.
    pub fn apply_change(&mut self, change: TextDocumentContentChangeEvent, version: i32) {
        self.version = version;

        // Full document sync (range is None)
        if change.range.is_none() {
            self.rope = Rope::from_str(&change.text);
            return;
        }

        // Incremental sync
        if let Some(range) = change.range {
            let start = self.position_to_offset(range.start);
            let end = self.position_to_offset(range.end);

            // Remove old text
            if start < end && end <= self.rope.len_chars() {
                self.rope.remove(start..end);
            }
            // Insert new text
            if start <= self.rope.len_chars() {
                self.rope.insert(start, &change.text);
            }
        }
    }

    /// Get the document content as a string.
    pub fn content(&self) -> String {
        self.rope.to_string()
    }

    /// Validate the document and return diagnostics.
    pub fn validate(&self) -> Vec<Diagnostic> {
        let content = self.content();

        let mut compiler = Compiler::new();

        // Parse the source
        match compiler.parse(&content) {
            Ok(file) => {
                tracing::debug!("Parse successful, {} components", file.components.len());

                // Lower to HIR and type check
                let hir_components = compiler.lower_to_hir(&file);
                for hir in &hir_components {
                    let _thir = compiler.type_check(hir);
                }

                // Collect diagnostics from the compiler context
                let mut diagnostics = Vec::new();
                let source_id = yel_core::SourceId(0);
                
                for diag in compiler.context().diagnostics.iter() {
                    diagnostics.extend(convert_yel_diagnostic(diag, source_id, &self.rope));
                }

                tracing::debug!("Type checking produced {} diagnostics", diagnostics.len());
                diagnostics
            }
            Err(err) => {
                tracing::debug!("Parse error: {}", err);
                let source_id = yel_core::SourceId(0);
                let diagnostics = convert_compile_error(&err, source_id, &self.rope);
                tracing::debug!("Converted to {} diagnostics", diagnostics.len());
                diagnostics
            }
        }
    }

    /// Convert an LSP position to a character offset.
    pub fn position_to_offset(&self, pos: Position) -> usize {
        let line = pos.line as usize;
        if line >= self.rope.len_lines() {
            return self.rope.len_chars();
        }

        let line_start = self.rope.line_to_char(line);
        let line_len = self
            .rope
            .line(line)
            .len_chars()
            .saturating_sub(1); // Don't count newline

        let col = (pos.character as usize).min(line_len);
        line_start + col
    }

    /// Convert a character offset to an LSP position.
    #[allow(dead_code)]
    pub fn offset_to_position(&self, offset: usize) -> Position {
        let offset = offset.min(self.rope.len_chars());
        let line = self.rope.char_to_line(offset);
        let line_start = self.rope.line_to_char(line);
        let character = offset - line_start;
        Position::new(line as u32, character as u32)
    }

    /// Get the word at the given position.
    pub fn get_word_at_position(&self, pos: Position) -> Option<String> {
        let offset = self.position_to_offset(pos);
        let content = self.content();
        let bytes = content.as_bytes();

        if offset >= content.len() {
            return None;
        }

        // Check if we're inside a string literal
        if let Some(string_info) = self.get_string_at_offset(offset, &content) {
            return Some(string_info);
        }

        // Check if we're on a color literal (#rrggbb)
        if bytes[offset] == b'#' || (offset > 0 && self.find_color_start(offset, bytes).is_some()) {
            if let Some(color) = self.get_color_at_offset(offset, &content, bytes) {
                return Some(color);
            }
        }

        // Find word boundaries (including digits for unit literals like 10px)
        let mut start = offset;
        let mut end = offset;

        fn is_word_char(c: u8) -> bool {
            c.is_ascii_alphanumeric() || c == b'_' || c == b'-' || c == b'.'
        }

        if !is_word_char(bytes[offset]) {
            if offset > 0 && is_word_char(bytes[offset - 1]) {
                start = offset - 1;
                end = offset;
            } else {
                return None;
            }
        }

        // Scan backwards to find start
        while start > 0 && is_word_char(bytes[start - 1]) {
            start -= 1;
        }

        // Scan forwards to find end
        while end < bytes.len() && is_word_char(bytes[end]) {
            end += 1;
        }

        // Handle percentage (include trailing %)
        if end < bytes.len() && bytes[end] == b'%' {
            end += 1;
        }

        if start < end {
            Some(content[start..end].to_string())
        } else {
            None
        }
    }

    /// Check if offset is inside a string and return string info.
    fn get_string_at_offset(&self, offset: usize, content: &str) -> Option<String> {
        let bytes = content.as_bytes();

        // Find if we're inside quotes
        let mut in_string = false;
        let mut string_start = 0;

        for (i, &b) in bytes.iter().enumerate() {
            if i >= offset {
                break;
            }
            if b == b'"' && (i == 0 || bytes[i - 1] != b'\\') {
                if in_string {
                    in_string = false;
                } else {
                    in_string = true;
                    string_start = i;
                }
            }
        }

        if in_string {
            // Find end of string
            let mut string_end = offset;
            for (i, &b) in bytes[offset..].iter().enumerate() {
                if b == b'"' && (offset + i == 0 || bytes[offset + i - 1] != b'\\') {
                    string_end = offset + i + 1;
                    break;
                }
            }

            let string_content = &content[string_start..string_end.min(content.len())];

            // Check for interpolation
            if string_content.contains("{{") {
                return Some("__string_interpolated__".to_string());
            } else {
                return Some("__string_literal__".to_string());
            }
        }

        None
    }

    /// Find the start of a color literal.
    fn find_color_start(&self, offset: usize, bytes: &[u8]) -> Option<usize> {
        let mut pos = offset;
        while pos > 0 {
            if bytes[pos - 1] == b'#' {
                return Some(pos - 1);
            }
            if !bytes[pos - 1].is_ascii_hexdigit() {
                break;
            }
            pos -= 1;
        }
        None
    }

    /// Get color literal at offset.
    fn get_color_at_offset(&self, offset: usize, content: &str, bytes: &[u8]) -> Option<String> {
        let start = if bytes[offset] == b'#' {
            offset
        } else {
            self.find_color_start(offset, bytes)?
        };

        let mut end = start + 1;
        while end < bytes.len() && bytes[end].is_ascii_hexdigit() {
            end += 1;
        }

        let color = &content[start..end];
        if color.len() >= 4 { // #rgb minimum
            Some(color.to_string())
        } else {
            None
        }
    }
}
