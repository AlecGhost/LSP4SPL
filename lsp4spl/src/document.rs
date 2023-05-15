use crate::io::{self, Message, ToValue};
use color_eyre::eyre::{Context, Result};
use lsp_types::{
    notification::{Notification, PublishDiagnostics},
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, Position, PublishDiagnosticsParams, Range as PosRange,
    TextDocumentContentChangeEvent, Url,
};
use spl_frontend::{error::SplError, AnalyzedSource, Change, ErrorContainer};
use std::collections::HashMap;
use tokio::sync::{
    mpsc::{Receiver, Sender},
    oneshot,
};

type TextRange = std::ops::Range<usize>;

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
    let mut docs = HashMap::new();
    while let Some(request) = rx.recv().await {
        match request {
            DocumentRequest::Open(uri, text) => {
                let doc = AnalyzedSource::new(text);
                if send_diagnostics {
                    notify(iotx.clone(), uri.clone(), &doc).await;
                }
                docs.insert(uri.path().to_string(), doc);
            }
            DocumentRequest::Change(uri, changes) => {
                use std::collections::hash_map::Entry;
                match docs.entry(uri.path().to_string()) {
                    Entry::Occupied(mut entry) => {
                        let doc = entry.get_mut();
                        let changes = changes
                            .into_iter()
                            .filter_map(|change| {
                                if let TextDocumentContentChangeEvent {
                                    range: Some(range),
                                    text,
                                    ..
                                } = change
                                {
                                    Some(Change {
                                        range: as_index_range(&range, &doc.text),
                                        text,
                                    })
                                } else {
                                    None
                                }
                            })
                            .collect();
                        doc.update(changes);
                        if send_diagnostics {
                            notify(iotx.clone(), uri.clone(), &doc).await;
                        }
                    }
                    Entry::Vacant(_) => { /* This should not happen. Ignoring it. */ }
                };
            }
            DocumentRequest::Close(uri) => {
                docs.remove(uri.path());
            }
            DocumentRequest::GetInfo(uri, tx) => {
                let doc = docs.get(uri.path()).cloned();
                tx.send(doc).expect("Cannot send messages");
            }
        }
    }
}

async fn notify(iotx: Sender<Message>, uri: Url, doc: &AnalyzedSource) {
    let notification = io::Notification::new(
        PublishDiagnostics::METHOD.to_string(),
        PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics: doc
                .errors()
                .iter()
                .map(|err| create_diagnostic(err, &doc.text))
                .collect(),
            version: None,
        }
        .to_value(),
    );
    iotx.send(Message::Notification(notification))
        .await
        .expect("Cannot send messages");
}

fn create_diagnostic(err: &SplError, text: &str) -> Diagnostic {
    let SplError(range, message) = err;
    Diagnostic {
        range: as_pos_range(range, text),
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

/// Converts a string index to a `Position`.
/// If the index is out of bounds, the last possible position is returned.
pub fn as_position(index: usize, text: &str) -> Position {
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

pub fn as_pos_range(range: &TextRange, text: &str) -> PosRange {
    PosRange {
        start: as_position(range.start, text),
        end: as_position(range.end, text),
    }
}

fn as_index_range(pos_range: &PosRange, text: &str) -> TextRange {
    let PosRange { start, end } = pos_range;
    let start = get_insertion_index(start, text).expect("Invalid start position");
    let end = get_insertion_index(end, text).expect("Invalid end position");
    start..end
}

/// Tries to convert a text `Position` to an index.
///
/// Note: This is the insertion index,
/// so it can be after the last character.
/// Therefore the text slice should not be indexed with this index
pub fn get_insertion_index(position: &Position, text: &str) -> Option<usize> {
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
