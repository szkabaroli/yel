//! Code generation from LIR.
//!
//! This module contains code generators for different targets:
//! - WIT (WebAssembly Interface Types) generation
//! - WebAssembly component generation

// Codegen/lowering routines legitimately thread many positional parameters;
// bundling them into immediately-destructured structs hurt readability more
// than it helped, so the lint is allowed crate-wide.
#![allow(clippy::too_many_arguments)]

use thiserror::Error;

pub mod dot;
pub mod wasm;
pub mod wit;
pub mod wit_ast;

// pub use lir_rust::generate_rust;
pub use dot::{DotOptions, generate_dot};
pub use wasm::functions::{
    ComponentPackage, FunctionInput, generate_component, generate_function_module,
};
pub use wasm::{WasmWithWitOptions, generate_wasm, generate_wasm_module, generate_wasm_with_wit};
pub use wit::{WitOptions, generate_wit};

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
