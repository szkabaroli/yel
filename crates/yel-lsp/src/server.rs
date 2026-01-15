//! Main LSP server implementation.

use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::completions::provide_completions;
use crate::document::Document;
use crate::hover::get_hover_info_with_ast;
use crate::semantic_tokens::{get_legend, provide_semantic_tokens};

/// The Yel Language Server.
pub struct YelLanguageServer {
    /// LSP client for sending notifications.
    client: Client,
    /// Open documents indexed by URI.
    documents: DashMap<Url, Document>,
}

impl YelLanguageServer {
    /// Create a new language server instance.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
    }

    /// Validate a document and publish diagnostics.
    async fn validate_document(&self, uri: &Url) {
        if let Some(doc) = self.documents.get(uri) {
            let diagnostics = doc.validate();
            self.client
                .publish_diagnostics(uri.clone(), diagnostics, Some(doc.version))
                .await;
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for YelLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ".".to_string(),
                        ":".to_string(),
                        "<".to_string(),
                        " ".to_string(),
                    ]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: get_legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: Some(false),
                            ..Default::default()
                        },
                    ),
                ),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "yel-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        tracing::info!("Yel Language Server initialized");
        self.client
            .log_message(MessageType::INFO, "Yel Language Server ready")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        tracing::info!("Yel Language Server shutting down");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        tracing::debug!("Document opened: {}", uri);

        let doc = Document::new(params.text_document.text, params.text_document.version);
        self.documents.insert(uri.clone(), doc);
        self.validate_document(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        tracing::debug!("Document changed: {}", uri);

        if let Some(mut doc) = self.documents.get_mut(&uri) {
            for change in params.content_changes {
                doc.apply_change(change, params.text_document.version);
            }
        }
        self.validate_document(&uri).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        tracing::debug!("Document saved: {}", params.text_document.uri);
        self.validate_document(&params.text_document.uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        tracing::debug!("Document closed: {}", params.text_document.uri);
        self.documents.remove(&params.text_document.uri);
        // Clear diagnostics for closed document
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(doc) = self.documents.get(uri) {
            let completions = provide_completions(&doc, position);
            return Ok(Some(CompletionResponse::Array(completions)));
        }
        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(doc) = self.documents.get(uri) {
            let content = doc.content();
            let offset = doc.position_to_offset(position);

            // Get the word at the cursor position for fallback
            let word = doc.get_word_at_position(position).unwrap_or_default();
            tracing::debug!("Hover at offset {} on word: {}", offset, word);

            return Ok(get_hover_info_with_ast(&content, offset, &word));
        }
        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        if let Some(doc) = self.documents.get(uri) {
            let content = doc.content();
            return Ok(provide_semantic_tokens(&content));
        }
        Ok(None)
    }
}
