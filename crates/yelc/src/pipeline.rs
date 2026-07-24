//! Shared compiler front-end, used by every output driver: the CLI binary
//! (`main.rs`), the native library API (`native`), and the WASI component
//! (`wasi_impl`). Each driver used to carry its own near-identical
//! parse → HIR → type-check → LIR loop, diagnostic conversion, and
//! `WitOptions` construction; those now live here once. Drivers differ only
//! in how they render diagnostics and emit output, so this module stays
//! free of any target- or transport-specific types.

use yel_core::{
    Compiler, Severity,
    hir::{HirComponent, HirItem},
    lir::{LirExpr, LirGlobal, LirModule, LirResource},
    syntax::ast::PackageId,
};
use yel_wasm_codegen as codegen;

/// Fully lowered front-end output shared by all drivers. Everything codegen
/// needs hangs off [`LirModule`]; `hir` is retained only for the `--hir`
/// debug dump.
pub struct Lowered {
    /// One compilation unit holding every component plus module-scope
    /// artifacts (global defaults, package header).
    pub module: LirModule,
    /// HIR components, kept for the HIR debug-dump format. Empty otherwise is
    /// not worth special-casing — retaining them is a move, not a clone.
    pub hir: Vec<HirComponent>,
}

impl Lowered {
    /// The lowered resources, the input to WIT and DOT generation.
    pub fn components(&self) -> &[LirResource] {
        &self.module.resources
    }

    /// The import-side boundary contract (foreign-package interfaces — DOM).
    pub fn interfaces(&self) -> &[yel_core::lir::LirInterface] {
        self.module.interfaces.as_slice()
    }

    /// The lowered global-singleton items.
    pub fn globals(&self) -> &[LirGlobal] {
        &self.module.globals
    }

    /// The shared expression arena backing every global's default nodes.
    pub fn global_exprs(&self) -> &[LirExpr] {
        &self.module.global_exprs
    }

    /// The package declaration, if any source declared one.
    pub fn package(&self) -> Option<&PackageId> {
        self.module.package.as_ref()
    }
}

/// Signals that a front-end phase recorded errors. The diagnostics are already
/// in the [`Compiler`]'s context; the caller renders them however it likes
/// (terminal output, WIT diagnostics, etc.).
#[derive(Debug, Clone, Copy)]
pub struct LoweringFailed;

/// Run parse → HIR → type-check → LIR over every source, then type-check and
/// lower the global-singleton property defaults once at the end.
///
/// # Errors
///
/// Returns [`LoweringFailed`] as soon as any phase records errors. Inspect
/// `compiler` for the diagnostics.
pub fn lower_all<'a>(
    compiler: &mut Compiler,
    sources: impl IntoIterator<Item = &'a str>,
) -> Result<Lowered, LoweringFailed> {
    // Parse + HIR-lower every source, accumulating one flat top-level item
    // list. Package info comes from the first file that declares one.
    let mut items: Vec<HirItem> = Vec::new();
    let mut package: Option<PackageId> = None;

    for source in sources {
        // Parse errors are recorded in the compiler's diagnostics.
        let parsed = compiler.parse(source).map_err(|_| LoweringFailed)?;

        if package.is_none() {
            package = parsed.package.clone();
        }

        items.extend(compiler.lower_to_hir(&parsed));
        if compiler.has_errors() {
            return Err(LoweringFailed);
        }
    }

    // Type-check + lower the whole module — components and globals — through
    // the one shared spine (`Compiler::lower_items_to_module`), which also
    // runs the module-level passes (global-trigger resolution, global
    // lowering, import contract).
    let module = compiler.lower_items_to_module(&items, package);
    if compiler.has_errors() {
        return Err(LoweringFailed);
    }

    // Retain HIR components for the `--hir` debug dump (globals carry no node
    // tree, so they don't appear in it).
    let hir: Vec<HirComponent> = items.into_iter().filter_map(HirItem::into_component).collect();

    Ok(Lowered { module, hir })
}

/// Build WIT package options from the package declaration, falling back to
/// `yel:app@0.1.0` when no source declares a package.
pub fn wit_options(package: Option<&PackageId>) -> codegen::WitOptions {
    match package {
        Some(pkg) => codegen::WitOptions {
            namespace: pkg.namespace.clone(),
            name: pkg.name.clone(),
            version: pkg.version.clone().unwrap_or_else(|| "0.1.0".to_string()),
            include_dom_interface: true,
        },
        None => codegen::WitOptions {
            namespace: "yel".to_string(),
            name: "app".to_string(),
            version: "0.1.0".to_string(),
            include_dom_interface: true,
        },
    }
}

/// A diagnostic flattened to plain fields, decoupled from the per-target
/// `Diagnostic` record types (the WASI bindings struct vs the native struct).
pub struct DiagnosticData {
    pub message: String,
    pub rendered: String,
    pub line: u32,
    pub column: u32,
    pub length: u32,
    pub severity: &'static str,
}

/// Flatten the compiler's diagnostics into transport-neutral records. Each
/// driver maps these into its own `Diagnostic` type.
pub fn diagnostics(compiler: &Compiler) -> Vec<DiagnosticData> {
    let ctx = compiler.context();
    ctx.diagnostics
        .iter()
        .map(|d| {
            let (line, column, length) = if let Some(span) = d.span
                && let Some(source) = ctx.source_map.get(span.source)
            {
                let (l, c) = source.line_col(span.start);
                let len = u32::try_from(span.end - span.start).unwrap_or(u32::MAX);
                (
                    u32::try_from(l).unwrap_or(u32::MAX),
                    u32::try_from(c).unwrap_or(u32::MAX),
                    len.max(1),
                )
            } else {
                (0, 0, 1)
            };

            let severity = match d.severity {
                Severity::Error => "error",
                Severity::Warning => "warning",
                Severity::Note => "info",
            };

            DiagnosticData {
                message: d.message.clone(),
                rendered: d.render(&ctx.source_map),
                line,
                column,
                length,
                severity,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    const LIB_ONLY: &str = "package yel:libonly@0.1.0;\n\nglobal Store { count: s32 = 42; }\n";

    #[test]
    fn wit_options_fall_back_to_yel_app_without_a_package() {
        // Arrange / Act
        let opts = wit_options(None);

        // Assert
        assert_eq!(opts.namespace, "yel");
        assert_eq!(opts.name, "app");
        assert_eq!(opts.version, "0.1.0");
        assert!(opts.include_dom_interface);
    }

    #[test]
    fn wit_options_use_the_declared_package_and_version() {
        let pkg = PackageId {
            namespace: "acme".to_string(),
            name: "widgets".to_string(),
            version: Some("2.3.4".to_string()),
        };

        let opts = wit_options(Some(&pkg));

        assert_eq!(opts.namespace, "acme");
        assert_eq!(opts.name, "widgets");
        assert_eq!(opts.version, "2.3.4");
    }

    #[test]
    fn wit_options_default_the_version_when_the_package_omits_it() {
        // `PackageId::new` leaves `version` as `None`.
        let pkg = PackageId::new("acme", "widgets");

        let opts = wit_options(Some(&pkg));

        assert_eq!(opts.version, "0.1.0");
    }

    #[test]
    fn lower_all_extracts_the_package_from_the_source() {
        let mut compiler = Compiler::new();

        let lowered =
            lower_all(&mut compiler, std::iter::once(LIB_ONLY)).expect("library should lower");

        let package = lowered.package().expect("package declaration present");
        assert_eq!(package.namespace, "yel");
        assert_eq!(package.name, "libonly");
        // Globals-only source has no components but still lowers successfully.
        assert!(lowered.components().is_empty());
    }

    #[test]
    fn lower_all_fails_and_records_diagnostics_on_a_parse_error() {
        let mut compiler = Compiler::new();

        let result = lower_all(&mut compiler, std::iter::once("@@@ not valid yel"));

        assert!(result.is_err());
        assert!(compiler.has_errors());
    }

    #[test]
    fn lower_all_takes_the_package_from_the_first_source() {
        let second = "package yel:second@0.1.0;\n\nglobal Other { n: s32 = 1; }\n";
        let mut compiler = Compiler::new();

        let lowered =
            lower_all(&mut compiler, [LIB_ONLY, second]).expect("both sources should lower");

        assert_eq!(lowered.package().unwrap().name, "libonly");
    }
}
