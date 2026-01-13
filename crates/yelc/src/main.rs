//! Yel DSL Compiler CLI

use anyhow::{Context, Result};
use clap::{Parser, Subcommand, ValueEnum};
use std::fs;
use std::path::PathBuf;
use yel_core::{Compiler, codegen, syntax::ast::PackageId};

#[derive(Parser)]
#[command(name = "yel")]
#[command(author, version, about = "Yel Compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile Yel source files
    Compile {
        /// Input file(s) to compile
        #[arg(required = true)]
        files: Vec<PathBuf>,

        /// Output format
        #[arg(short, long, value_enum, default_value = "rust")]
        output: OutputFormat,

        /// Package name for generated code
        #[arg(short, long, default_value = "yel_app")]
        package: String,
    },

    /// Parse and dump AST
    Ast {
        /// Input file to parse
        file: PathBuf,

        /// Pretty print
        #[arg(short, long)]
        pretty: bool,

        /// Output as JSON
        #[arg(short, long)]
        json: bool,
    },

    /// Dump intermediate representation (LIR)
    Ir {
        /// Input file to analyze
        file: PathBuf,
    },

    /// Check source files for errors
    Check {
        /// Input file(s) to check
        ///
        #[arg(required = true)]
        files: Vec<PathBuf>,
    },
}

#[derive(Clone, ValueEnum)]
enum OutputFormat {
    /// Generated Rust source code
    Rust,
    /// Generated WIT interface
    Wit,
    /// WebAssembly component
    Wasm,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile {
            files,
            output,
            package,
        } => compile(files, output, package),
        Commands::Ast { file, pretty, json } => dump_ast(file, pretty, json),
        Commands::Ir { file } => dump_ir(file),
        Commands::Check { files } => check(files),
    }
}

fn compile(files: Vec<PathBuf>, output: OutputFormat, _package: String) -> Result<()> {
    let mut compiler = Compiler::new();

    // Collect all LIR components and package info
    let mut lir_components = Vec::new();
    let mut package_info: Option<PackageId> = None;

    for file in &files {
        let source = fs::read_to_string(file)
            .with_context(|| format!("Failed to read file: {}", file.display()))?;

        let parsed = compiler
            .parse(&source)
            .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;

        // Extract package info from first file that has it
        if package_info.is_none() {
            package_info = parsed.package.clone();
        }

        let hir_components = compiler.lower_to_hir(&parsed);

        if compiler.has_errors() {
            eprintln!("{}", compiler.render_diagnostics());
            return Err(anyhow::anyhow!("Compilation failed"));
        }

        for hir in &hir_components {
            let thir = compiler.type_check(hir);

            if compiler.has_errors() {
                eprintln!("{}", compiler.render_diagnostics());
                return Err(anyhow::anyhow!("Type checking failed"));
            }

            let lir = compiler.lower_to_lir(&thir);
            lir_components.push(lir);
        }
    }

    // Generate output for each component
    let ctx = compiler.context();

    // Build WitOptions from package info
    let wit_options = if let Some(ref pkg) = package_info {
        codegen::WitOptions {
            namespace: pkg.namespace.clone(),
            name: pkg.name.clone(),
            version: pkg.version.clone().unwrap_or_else(|| "0.1.0".to_string()),
            include_dom_interface: true,
        }
    } else {
        codegen::WitOptions {
            namespace: "yel".to_string(),
            name: "app".to_string(),
            version: "0.1.0".to_string(),
            include_dom_interface: true,
        }
    };

    match output {
        OutputFormat::Rust => {
            // Rust codegen temporarily disabled - needs update for block-based LIR
            println!("// Rust codegen not available - use WASM output instead");
        }
        OutputFormat::Wit => {
            // Use the AST-based generator for cleaner output
            // It uses wit-parser + wit-encoder for proper WIT formatting
            if let Some(first_component) = lir_components.iter().find(|c| c.is_export) {
                let wit_code = codegen::generate_wit(first_component, ctx, &wit_options)
                    .map_err(|e| anyhow::anyhow!("WIT generation error: {}", e))?;
                println!("{}", wit_code);
            } else {
                eprintln!("No exported component found");
            }
        }
        OutputFormat::Wasm => {
            use std::io::Write;

            let wasm_options = codegen::WasmWithWitOptions {
                namespace: wit_options.namespace.clone(),
                name: wit_options.name.clone(),
                version: wit_options.version.clone(),
            };
            let wasm_bytes = codegen::generate_wasm_with_wit(&lir_components, ctx, &wasm_options)
                .map_err(|e| anyhow::anyhow!("WASM generation error: {}", e))?;

            std::io::stdout()
                .write_all(&wasm_bytes)
                .context("Failed to write WASM output")?;
        }
    }

    Ok(())
}

fn dump_ast(file: PathBuf, pretty: bool, json: bool) -> Result<()> {
    let source = fs::read_to_string(&file)
        .with_context(|| format!("Failed to read file: {}", file.display()))?;

    let ast = yel_core::parse(&source).map_err(|e| {
        eprintln!("Parse error: {}", e);
        anyhow::anyhow!("Failed to parse")
    })?;

    if json {
        if pretty {
            println!(
                "{}",
                serde_json::to_string_pretty(&ast).context("Failed to serialize AST to JSON")?
            );
        } else {
            println!(
                "{}",
                serde_json::to_string(&ast).context("Failed to serialize AST to JSON")?
            );
        }
    } else if pretty {
        println!("{:#?}", ast);
    } else {
        println!("{:?}", ast);
    }

    Ok(())
}

fn dump_ir(file: PathBuf) -> Result<()> {
    let source = fs::read_to_string(&file)
        .with_context(|| format!("Failed to read file: {}", file.display()))?;

    let mut compiler = Compiler::new();

    let parsed = compiler.parse(&source).map_err(|e| {
        eprintln!("Parse error: {}", e);
        anyhow::anyhow!("Failed to parse")
    })?;

    let hir_components = compiler.lower_to_hir(&parsed);

    if compiler.has_errors() {
        eprintln!("{}", compiler.render_diagnostics());
        return Err(anyhow::anyhow!("HIR lowering failed"));
    }

    // Collect all LIR components first
    let mut lir_components = Vec::new();
    for hir in &hir_components {
        let thir = compiler.type_check(hir);

        if compiler.has_errors() {
            eprintln!("{}", compiler.render_diagnostics());
            return Err(anyhow::anyhow!("Type checking failed"));
        }

        let lir = compiler.lower_to_lir(&thir);
        lir_components.push(lir);
    }

    // Now we can borrow context for printing
    let ctx = compiler.context();

    for lir in &lir_components {
        // Print LIR summary
        let name = ctx.str(lir.name);
        println!("=== Component: {} ===\n", name);

        println!("Signals ({}):", lir.signals.len());
        for s in &lir.signals {
            let sig_name = ctx.str(ctx.defs.name(s.def_id));
            println!("  {} : {:?} = {:?}", sig_name, ctx.ty_kind(s.ty), s.default);
        }

        println!("\nEffects ({}):", lir.effects.len());
        for e in &lir.effects {
            println!(
                "  update_block={:?} deps={:?}",
                e.update_block, e.dependencies
            );
        }

        println!("\nBlocks ({}):", lir.blocks.len());
        for (i, block) in lir.blocks.iter().enumerate() {
            let mount_marker = if lir.mount_block.0 as usize == i {
                " (mount)"
            } else {
                ""
            };
            println!("  Block {:?}{}:", block.id, mount_marker);
            for op in &block.ops {
                println!("    {:?}", op);
            }
        }

        println!("\nStrings ({}):", lir.strings.len());
        for (i, s) in lir.strings.iter().enumerate() {
            println!("  [{}] \"{}\"", i, s);
        }
    }

    Ok(())
}

fn print_nodes(
    nodes: &[yel_core::lir::LirNode],
    indent: usize,
    ctx: &yel_core::CompilerContext,
) {
    use yel_core::lir::LirNodeKind;

    let pad = "  ".repeat(indent);
    for node in nodes {
        match &node.kind {
            LirNodeKind::Element { tag, children, .. } => {
                println!("{}[{:?}] Element({})", pad, node.id, tag);
                print_nodes(children, indent + 1, ctx);
            }
            LirNodeKind::StaticText(text) => {
                println!("{}[{:?}] StaticText(\"{}\")", pad, node.id, text);
            }
            LirNodeKind::DynamicText { effect_id } => {
                println!("{}[{:?}] DynamicText(effect={})", pad, node.id, effect_id);
            }
            LirNodeKind::If {
                then_branch,
                else_if_branches,
                else_branch,
                ..
            } => {
                println!("{}[{:?}] If", pad, node.id);
                print_nodes(then_branch, indent + 1, ctx);
                for (_, branch) in else_if_branches {
                    println!("{}  else if:", pad);
                    print_nodes(branch, indent + 1, ctx);
                }
                if let Some(else_nodes) = else_branch {
                    println!("{}  else:", pad);
                    print_nodes(else_nodes, indent + 1, ctx);
                }
            }
            LirNodeKind::For { body, .. } => {
                println!("{}[{:?}] For", pad, node.id);
                print_nodes(body, indent + 1, ctx);
            }
        }
    }
}

fn check(files: Vec<PathBuf>) -> Result<()> {
    let mut compiler = Compiler::new();
    let mut total_components = 0;

    for file in &files {
        let source = fs::read_to_string(file)
            .with_context(|| format!("Failed to read file: {}", file.display()))?;

        let parsed = match compiler.parse(&source) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Parse error in {}: {}", file.display(), e);
                return Err(anyhow::anyhow!("Check failed"));
            }
        };

        let hir_components = compiler.lower_to_hir(&parsed);

        if compiler.has_errors() {
            eprintln!("{}", compiler.render_diagnostics());
            return Err(anyhow::anyhow!("Check failed"));
        }

        for hir in &hir_components {
            let _thir = compiler.type_check(hir);
            total_components += 1;
        }

        if compiler.has_errors() {
            eprintln!("{}", compiler.render_diagnostics());
            return Err(anyhow::anyhow!("Check failed with type errors"));
        }
    }

    eprintln!("OK: {} component(s) checked", total_components);
    Ok(())
}
