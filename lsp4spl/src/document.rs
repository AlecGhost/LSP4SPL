use color_eyre::eyre::{Context, Result};
use lsp_types::{
    notification::{Notification, PublishDiagnostics},
    *,
};
use spl_frontend::{error::SplError, AnalyzedSource, ErrorContainer};
use std::collections::HashMap;
use tokio::sync::{
    mpsc::{Receiver, Sender},
    oneshot,
};

use crate::io::{self, Message, ToValue};

#[derive(Debug)]
pub enum DocumentRequest {
    Open(Url, String),
    Change(Url, Vec<TextDocumentContentChangeEvent>),
    Close(Url),
    GetInfo(Url, oneshot::Sender<Option<AnalyzedSource>>),
}

pub async fn open(
    broker: Sender<DocumentRequest>,
    params: DidOpenTextDocumentParams,
) -> Result<()> {
    broker
        .send(DocumentRequest::Open(
            params.text_document.uri,
            params.text_document.text,
        ))
        .await
        .wrap_err("Cannot send document request")?;
    Ok(())
}

pub async fn change(
    broker: Sender<DocumentRequest>,
    params: DidChangeTextDocumentParams,
) -> Result<()> {
    broker
        .send(DocumentRequest::Change(
            params.text_document.uri,
            params.content_changes,
        ))
        .await
        .wrap_err("Cannot send document request")?;
    Ok(())
}

pub async fn close(
    broker: Sender<DocumentRequest>,
    params: DidCloseTextDocumentParams,
) -> Result<()> {
    broker
        .send(DocumentRequest::Close(params.text_document.uri))
        .await
        .wrap_err("Cannot send document request")?;
    Ok(())
}

pub async fn broker(
    mut rx: Receiver<DocumentRequest>,
    iotx: Sender<Message>,
    send_diagnostics: bool,
) {
    use DocumentRequest::*;
    let mut docs = HashMap::new();
    while let Some(request) = rx.recv().await {
        match request {
            Open(uri, text) => {
                let doc_info = AnalyzedSource::new(text);
                if send_diagnostics {
                    let notification = io::Notification::new(
                        PublishDiagnostics::METHOD.to_string(),
                        PublishDiagnosticsParams {
                            uri: uri.clone(),
                            diagnostics: doc_info
                                .errors()
                                .iter()
                                .map(|err| create_diagnostic(err, &doc_info.text))
                                .collect(),
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
                    // currently no incremental changes, so range should be None
                    if change.range.is_none() {
                        let doc_info = AnalyzedSource::new(change.text);
                        if send_diagnostics {
                            let notification = io::Notification::new(
                                PublishDiagnostics::METHOD.to_string(),
                                PublishDiagnosticsParams {
                                    uri: uri.clone(),
                                    diagnostics: doc_info
                                        .errors()
                                        .iter()
                                        .map(|err| create_diagnostic(err, &doc_info.text))
                                        .collect(),
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

fn create_diagnostic(err: &SplError, text: &str) -> Diagnostic {
    let SplError(range, message) = err;
    Diagnostic {
        range: convert_range(range, text),
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: None,
        message: message.clone(),
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

/// Tries to convert a text `Position` to an index.
///
/// Note: This is the insertion index,
/// so it can be after the last character.
/// Therefore the text slice should not be indexed with this index
pub fn get_insertion_index(position: Position, text: &str) -> Option<usize> {
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
    if (line, character) == pos {
        Some(text.len())
    } else {
        None
    }
}
