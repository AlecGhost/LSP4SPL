#![allow(dead_code)]
use lsp_types::{
    ServerCapabilities, ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions,
};
use server::LanguageServer;
mod document;
mod error;
mod io;
mod server;

#[tokio::main]
async fn main() {
    eprintln!("LSP4SPL: Startup.");
    let ls = LanguageServer::setup(
        Some(ServerInfo {
            name: "lsp4spl".to_string(),
            version: Some("0.1".to_string()),
        }),
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    will_save: None,
                    will_save_wait_until: None,
                    save: None,
                },
            )),
            selection_range_provider: None,
            hover_provider: None,
            completion_provider: None,
            signature_help_provider: None,
            definition_provider: None,
            type_definition_provider: None,
            implementation_provider: None,
            references_provider: None,
            document_highlight_provider: None,
            document_symbol_provider: None,
            workspace_symbol_provider: None,
            code_action_provider: None,
            code_lens_provider: None,
            document_formatting_provider: None,
            document_range_formatting_provider: None,
            document_on_type_formatting_provider: None,
            rename_provider: None,
            document_link_provider: None,
            color_provider: None,
            folding_range_provider: None,
            declaration_provider: None,
            execute_command_provider: None,
            workspace: None,
            call_hierarchy_provider: None,
            semantic_tokens_provider: None,
            moniker_provider: None,
            linked_editing_range_provider: None,
            experimental: None,
        },
    );
    ls.run().await;
    eprintln!("LSP4SPL: Shutdown.");
}
