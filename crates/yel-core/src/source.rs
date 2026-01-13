//! Virtual source file management for diagnostics.
//!
//! The SourceMap holds all source files and provides source IDs for
//! accurate error reporting across multiple files.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Unique identifier for a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SourceId(pub u32);

impl std::fmt::Display for SourceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "source#{}", self.0)
    }
}

/// A source file with its content and metadata.
#[derive(Debug, Clone)]
pub struct Source {
    /// Unique ID for this source.
    pub id: SourceId,
    /// Optional file path (None for inline sources).
    pub path: Option<PathBuf>,
    /// The source code content.
    pub content: String,
}

impl Source {
    /// Get a display name for this source.
    pub fn name(&self) -> String {
        self.path
            .as_ref()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|| format!("<source#{}>", self.id.0))
    }

    /// Get line and column for a byte offset.
    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;
        for (i, ch) in self.content.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (line, col)
    }

    /// Get a snippet of source code around a line.
    pub fn snippet(&self, line: usize, context: usize) -> String {
        let lines: Vec<&str> = self.content.lines().collect();
        let start = line.saturating_sub(context + 1);
        let end = (line + context).min(lines.len());

        lines[start..end]
            .iter()
            .enumerate()
            .map(|(i, l)| format!("{:4} | {}", start + i + 1, l))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/// Manages a collection of source files.
#[derive(Debug, Default, Clone)]
pub struct SourceMap {
    sources: HashMap<SourceId, Source>,
    path_to_id: HashMap<PathBuf, SourceId>,
    next_id: u32,
}

impl SourceMap {
    /// Create a new empty source map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a source file with a path.
    pub fn add_file(&mut self, path: impl Into<PathBuf>, content: impl Into<String>) -> SourceId {
        let path = path.into();
        let content = content.into();

        // Return existing ID if already added
        if let Some(&id) = self.path_to_id.get(&path) {
            return id;
        }

        let id = SourceId(self.next_id);
        self.next_id += 1;

        self.path_to_id.insert(path.clone(), id);
        self.sources.insert(
            id,
            Source {
                id,
                path: Some(path),
                content,
            },
        );

        id
    }

    /// Add an inline source (no path).
    pub fn add_inline(&mut self, content: impl Into<String>) -> SourceId {
        let id = SourceId(self.next_id);
        self.next_id += 1;

        self.sources.insert(
            id,
            Source {
                id,
                path: None,
                content: content.into(),
            },
        );

        id
    }

    /// Get a source by ID.
    pub fn get(&self, id: SourceId) -> Option<&Source> {
        self.sources.get(&id)
    }

    /// Get a source ID by path.
    pub fn get_id(&self, path: &PathBuf) -> Option<SourceId> {
        self.path_to_id.get(path).copied()
    }

    /// Get all source IDs.
    pub fn source_ids(&self) -> impl Iterator<Item = SourceId> + '_ {
        self.sources.keys().copied()
    }
}

/// A location in source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    /// Source file ID.
    pub source: SourceId,
    /// Start byte offset.
    pub start: usize,
    /// End byte offset.
    pub end: usize,
}

impl Span {
    /// Create a new span.
    pub fn new(source: SourceId, start: usize, end: usize) -> Self {
        Self { source, start, end }
    }

    /// Create a zero-length span at a position.
    pub fn point(source: SourceId, offset: usize) -> Self {
        Self::new(source, offset, offset)
    }

    /// Merge two spans (smallest start to largest end).
    pub fn merge(self, other: Self) -> Self {
        assert_eq!(
            self.source, other.source,
            "Cannot merge spans from different sources"
        );
        Self {
            source: self.source,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            source: SourceId(0),
            start: 0,
            end: 0,
        }
    }
}
