//! Yel Language Server entry point.

use tower_lsp::{LspService, Server};
use tracing_subscriber::EnvFilter;

mod ast_hover;
mod completions;
mod diagnostics;
mod document;
mod hover;
mod semantic_tokens;
mod server;

use server::YelLanguageServer;

#[tokio::main]
async fn main() {
    // Initialize logging (no colors for LSP output)
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .init();

    tracing::info!("Starting Yel Language Server");

    let stdin = tokio::io::stdin();

    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(YelLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
