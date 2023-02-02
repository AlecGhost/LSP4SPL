#![allow(dead_code)]
use color_eyre::eyre::Result;
use lsp_types::{
    DeclarationCapability, HoverProviderCapability, OneOf, ServerCapabilities, ServerInfo,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TypeDefinitionProviderCapability,
};
use server::LanguageServer;
use simplelog::{Config, LevelFilter, WriteLogger};

mod document;
mod error;
mod features;
mod io;
mod server;

#[tokio::main]
async fn main() {
    register_logger().expect("Cannot register logger");
    log::info!("Startup");
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
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            completion_provider: None,
            signature_help_provider: None,
            definition_provider: Some(OneOf::Left(true)),
            type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
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
            declaration_provider: Some(DeclarationCapability::Simple(true)),
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
    log::info!("Shutdown");
}

fn register_logger() -> Result<()> {
    WriteLogger::init(
        LevelFilter::Debug,
        Config::default(),
        std::fs::File::create("lsp4spl.log")?,
    )?;
    Ok(())
}
