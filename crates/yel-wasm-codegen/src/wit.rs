//! WIT (WebAssembly Interface Types) code generation from LIR.
//!
//! This module generates WIT interface definitions for Yel components,
//! producing a valid WIT file that can be used with wit-bindgen.
//!
//! Uses wit_ast.rs (wit-parser) + wit-encoder's from_parser to build
//! a proper WIT AST that can be printed to string.

use super::CodegenError;
use crate::wit_ast::WitAstBuilder;
use yel_core::context::CompilerContext;
use yel_core::lir::LirComponent;

use wit_encoder::packages_from_parsed;

/// Options for WIT generation.
#[derive(Debug, Clone, Default)]
pub struct WitOptions {
    /// Package namespace (e.g., "yel").
    pub namespace: String,
    /// Package name (e.g., "counter").
    pub name: String,
    /// Package version (e.g., "0.1.0").
    pub version: String,
    /// Whether to include the standard DOM interface.
    pub include_dom_interface: bool,
}

/// Generate WIT using the AST-based approach (wit-parser + wit-encoder).
///
/// This builds a proper WIT AST using wit-parser types (via WitAstBuilder),
/// then converts it to wit-encoder's Package type which can be printed to string.
///
/// Returns the generated WIT as a string.
pub fn generate_wit(
    component: &LirComponent,
    ctx: &CompilerContext,
    options: &WitOptions,
) -> Result<String, CodegenError> {
    let mut builder = WitAstBuilder::new(ctx, &options.namespace, &options.name, &options.version);
    builder.build_component_wit(component)?;

    let (resolve, _world_id) = builder.into_resolve_and_world();

    // Convert wit-parser Resolve to wit-encoder Package(s)
    let packages = packages_from_parsed(&resolve);

    // Return the first package's string representation
    packages
        .into_iter()
        .next()
        .map(|pkg| pkg.to_string())
        .ok_or_else(|| CodegenError::InternalError("No package generated".to_string()))
}
