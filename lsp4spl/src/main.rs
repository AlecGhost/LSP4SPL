#![allow(dead_code)]
use color_eyre::eyre::{Context, Result};
use lsp_types::{
    CompletionOptions, DeclarationCapability, FoldingRangeProviderCapability,
    HoverProviderCapability, ImplementationProviderCapability, OneOf, RenameOptions,
    ServerCapabilities, ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TypeDefinitionProviderCapability, WorkDoneProgressOptions,
};
use server::LanguageServer;
use simplelog::{Config, LevelFilter, WriteLogger};

mod document;
mod error;
mod features;
mod io;
mod server;

#[tokio::main]
async fn main() -> Result<()> {
    color_eyre::install()?;
    register_logger().wrap_err("Cannot register logger")?;
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
            completion_provider: Some(CompletionOptions {
                ..Default::default()
            }),
            signature_help_provider: None,
            definition_provider: Some(OneOf::Left(true)),
            type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
            implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
            references_provider: Some(OneOf::Left(true)),
            document_highlight_provider: None,
            document_symbol_provider: None,
            workspace_symbol_provider: None,
            code_action_provider: None,
            code_lens_provider: None,
            document_formatting_provider: None,
            document_range_formatting_provider: None,
            document_on_type_formatting_provider: None,
            rename_provider: Some(OneOf::Right(RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })),
            document_link_provider: None,
            color_provider: None,
            folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
            declaration_provider: Some(DeclarationCapability::Simple(true)),
            execute_command_provider: None,
            workspace: None,
            call_hierarchy_provider: None,
            semantic_tokens_provider: None,
            moniker_provider: None,
            linked_editing_range_provider: None,
            experimental: None,
            inlay_hint_provider: None,
            inline_value_provider: None,
            position_encoding: None,
        },
    );
    ls.run().await?;
    log::info!("Shutdown");
    Ok(())
}

fn register_logger() -> Result<()> {
    WriteLogger::init(
        LevelFilter::Debug,
        Config::default(),
        std::fs::File::create("lsp4spl.log").wrap_err("Cannot open file")?,
    )?;
    Ok(())
}
