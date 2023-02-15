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
    LocalBroker, lexer,
};
use std::collections::HashMap;
use tokio::sync::{
    mpsc::{Receiver, Sender},
    oneshot,
};

use crate::io::{self, Message, ToValue};

#[derive(Debug)]
pub(super) enum DocumentRequest {
    Open(Url, String),
    Change(Url, Vec<TextDocumentContentChangeEvent>),
    Close(Url),
    GetInfo(Url, oneshot::Sender<Option<DocumentInfo>>),
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
            GetInfo(uri, tx) => {
                let doc_info = docs.get(uri.path()).cloned();
                tx.send(doc_info).expect("Cannot send messages");
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct DocumentInfo {
    pub text: String,
    pub ast: Program,
    pub table: SymbolTable,
    pub diagnostics: Vec<Diagnostic>,
}

impl DocumentInfo {
    fn new(text: String) -> Self {
        let broker = LocalBroker::default();
        let tokens = lexer::lex(&text);
        let program = parser::parse(&tokens, broker.clone());
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
    let SplError(range, message) = err;
    Diagnostic {
        range: convert_range(&range, text),
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: None,
        message,
        related_information: None,
        tags: None,
        data: None,
    }
}

pub fn get_position(index: usize, text: &str) -> Position {
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

pub fn convert_range(range: &std::ops::Range<usize>, text: &str) -> Range {
    Range {
        start: get_position(range.start, text),
        end: get_position(range.end, text),
    }
}

pub fn get_index(position: Position, text: &str) -> Option<usize> {
    let mut line = 0;
    let mut character = 0;
    let pos = (position.line, position.character);
    for (i, c) in text.char_indices() {
        if (line, character) == pos {
            return Some(i);
        }
        if c == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }
    None
}
