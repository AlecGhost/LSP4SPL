#![warn(clippy::nursery)]
use clap::Parser;
use color_eyre::eyre::{Context, Result};
use lsp_types::{
    CompletionOptions, DeclarationCapability, FoldingRangeProviderCapability,
    HoverProviderCapability, ImplementationProviderCapability, OneOf, RenameOptions,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo, SignatureHelpOptions,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TypeDefinitionProviderCapability, WorkDoneProgressOptions,
};
use server::LanguageServer;
use simplelog::{Config, LevelFilter, WriteLogger};
use std::path::{Path, PathBuf};

mod document;
mod error;
mod features;
mod io;
mod server;

#[derive(Parser)]
struct Args {
    /// Log to specified file
    #[arg(short, long, value_name = "FILE")]
    log: Option<PathBuf>,

    // No use.
    // But for whatever reason VSCode passes this option.
    // stdio is the only protocol this server supports.
    #[arg(long)]
    stdio: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    color_eyre::install()?;
    let args = Args::parse();
    if let Some(log_file) = &args.log {
        register_logger(log_file).wrap_err("Cannot register logger")?;
    }
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
            signature_help_provider: Some(SignatureHelpOptions {
                ..Default::default()
            }),
            definition_provider: Some(OneOf::Left(true)),
            type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
            implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
            references_provider: Some(OneOf::Left(true)),
            document_highlight_provider: None,
            document_symbol_provider: None,
            workspace_symbol_provider: None,
            code_action_provider: None,
            code_lens_provider: None,
            document_formatting_provider: Some(OneOf::Left(true)),
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
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    legend: SemanticTokensLegend {
                        token_types: features::semantic_tokens::TOKEN_TYPES.to_vec(),
                        token_modifiers: features::semantic_tokens::TOKEN_MODIFIERS.to_vec(),
                    },
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    range: None,
                    ..Default::default()
                }),
            ),
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

fn register_logger(log_file: &Path) -> Result<()> {
    WriteLogger::init(
        LevelFilter::Debug,
        Config::default(),
        std::fs::File::create(log_file).wrap_err("Cannot open file")?,
    )?;
    Ok(())
}
