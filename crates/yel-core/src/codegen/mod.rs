//! Code generation from LIR.
//!
//! This module contains code generators for different targets:
//! - WIT (WebAssembly Interface Types) generation
//! - WebAssembly component generation

use thiserror::Error;

// TODO: lir_rust needs to be updated for block-based LIR
// pub mod lir_rust;
pub mod wasm;
pub mod wit;
pub mod wit_ast;

// pub use lir_rust::generate_rust;
pub use wasm::{generate_wasm, generate_wasm_with_wit, WasmWithWitOptions};
pub use wit::{generate_wit, WitOptions};

/// Code generation error.
#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("unsupported type: {0}")]
    UnsupportedType(String),

    #[error("unsupported expression: {0}")]
    UnsupportedExpr(String),

    #[error("missing definition: {0}")]
    MissingDefinition(String),

    #[error("internal error: {0}")]
    InternalError(String),

    #[error("encoding error: {0}")]
    EncodingError(String),

    #[error("invalid IR: {0}")]
    InvalidIR(String),

    #[error("layout missing for component {0}")]
    LayoutMissing(usize),
}

/// WASM generation error (alias for CodegenError).
pub type WasmError = CodegenError;
