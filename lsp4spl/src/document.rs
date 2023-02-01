use color_eyre::eyre::Result;
use lsp_types::{
    notification::{Notification, PublishDiagnostics},
    *,
};
use spl_frontend::{
    ast::Program,
    error::SplError,
    parser,
    table::{self, SymbolTable},
    LocalBroker,
};
use std::collections::HashMap;
use tokio::sync::mpsc::{Receiver, Sender};

use crate::io::{self, Message, ToValue};

#[derive(Debug)]
pub(super) enum DocumentRequest {
    Open(Url, String),
    Change(Url, Vec<TextDocumentContentChangeEvent>),
    Close(Url),
}

pub(super) async fn open(
    broker: Sender<DocumentRequest>,
    params: DidOpenTextDocumentParams,
) -> Result<()> {
    broker
        .send(DocumentRequest::Open(
            params.text_document.uri,
            params.text_document.text,
        ))
        .await?;
    Ok(())
}

pub(super) async fn change(
    broker: Sender<DocumentRequest>,
    params: DidChangeTextDocumentParams,
) -> Result<()> {
    broker
        .send(DocumentRequest::Change(
            params.text_document.uri,
            params.content_changes,
        ))
        .await?;
    Ok(())
}

pub(super) async fn close(
    broker: Sender<DocumentRequest>,
    params: DidCloseTextDocumentParams,
) -> Result<()> {
    broker
        .send(DocumentRequest::Close(params.text_document.uri))
        .await?;
    Ok(())
}

pub(super) async fn broker(
    mut rx: Receiver<DocumentRequest>,
    iotx: Sender<Message>,
    send_diagnostics: bool,
) {
    use DocumentRequest::*;
    let mut docs = HashMap::new();
    while let Some(request) = rx.recv().await {
        match request {
            Open(uri, text) => {
                let doc_info = DocumentInfo::new(text);
                if send_diagnostics {
                    let notification = io::Notification::new(
                        PublishDiagnostics::METHOD.to_string(),
                        PublishDiagnosticsParams {
                            uri: uri.clone(),
                            diagnostics: doc_info.diagnostics.clone(),
                            version: None,
                        }
                        .to_value(),
                    );
                    iotx.send(Message::Notification(notification))
                        .await
                        .expect("Cannot send messages");
                }
                docs.insert(uri.path().to_string(), doc_info);
            }
            Change(uri, changes) => {
                // currently no incremental changes, so there should only be one change
                if let Some(change) = changes.into_iter().next() {
                    match change.range {
                        Some(_) => { /*TODO: Incremental editing*/ }
                        None => {
                            let doc_info = DocumentInfo::new(change.text);
                            if send_diagnostics {
                                let notification = io::Notification::new(
                                    PublishDiagnostics::METHOD.to_string(),
                                    PublishDiagnosticsParams {
                                        uri: uri.clone(),
                                        diagnostics: doc_info.diagnostics.clone(),
                                        version: None,
                                    }
                                    .to_value(),
                                );
                                iotx.send(Message::Notification(notification))
                                    .await
                                    .expect("Cannot send messages");
                            }
                            docs.insert(uri.path().to_string(), doc_info);
                        }
                    }
                }
            }
            Close(uri) => {
                docs.remove(uri.path());
            }
        }
    }
}

#[derive(Debug)]
struct DocumentInfo {
    text: String,
    ast: Program,
    table: SymbolTable,
    diagnostics: Vec<Diagnostic>,
}

impl DocumentInfo {
    fn new(text: String) -> Self {
        let broker = LocalBroker::default();
        let program = parser::parse(&text, broker.clone());
        let table = table::build(&program, broker.clone());
        table::analyze(&program, &table, broker.clone());
        let diagnostics = broker
            .errors()
            .into_iter()
            .map(|err| create_diagnostic(err, &text))
            .collect();
        Self {
            text,
            ast: program,
            table,
            diagnostics,
        }
    }
}

fn create_diagnostic(err: SplError, text: &str) -> Diagnostic {
    let range = Range {
        start: get_position(err.0.start, text),
        end: get_position(err.0.end, text),
    };
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: None,
        message: err.1,
        related_information: None,
        tags: None,
        data: None,
    }
}

fn get_position(index: usize, text: &str) -> Position {
    let mut line = 0;
    let mut character = 0;
    for (i, c) in text.char_indices() {
        if i == index {
            break;
        }
        if c == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }
    Position { line, character }
}
