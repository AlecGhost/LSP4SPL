use std::collections::HashMap;

use lsp_types::*;
use tokio::sync::mpsc::{Receiver, Sender};

#[derive(Debug)]
pub(super) enum DocumentStatus {
    DidOpen(TextDocumentItem),
    DidChange(
        (
            VersionedTextDocumentIdentifier,
            Vec<TextDocumentContentChangeEvent>,
        ),
    ),
    DidClose(TextDocumentIdentifier),
}

pub(super) async fn did_open(broker: Sender<DocumentStatus>, params: DidOpenTextDocumentParams) {
    broker
        .send(DocumentStatus::DidOpen(params.text_document))
        .await
        .expect("Cannot send messages");
}

pub(super) async fn did_change(
    broker: Sender<DocumentStatus>,
    params: DidChangeTextDocumentParams,
) {
    broker
        .send(DocumentStatus::DidChange((
            params.text_document,
            params.content_changes,
        )))
        .await
        .expect("Cannot send messages");
}

pub(super) async fn did_close(broker: Sender<DocumentStatus>, params: DidCloseTextDocumentParams) {
    broker
        .send(DocumentStatus::DidClose(params.text_document))
        .await
        .expect("Cannot send messages");
}

pub(super) async fn broker(mut rx: Receiver<DocumentStatus>) {
    use DocumentStatus::*;
    let mut docs = HashMap::new();
    while let Some(status) = rx.recv().await {
        match status {
            DidOpen(item) => {
                docs.insert(item.uri.path().to_string(), item.text);
            }
            DidChange((version, changes)) => {
                changes.into_iter().for_each(|change| match change.range {
                    Some(_) => { /*TODO: Incremental editing*/ }
                    None => {
                        docs.insert(version.uri.path().to_string(), change.text);
                    }
                })
            }
            DidClose(identifier) => {
                docs.remove(identifier.uri.path());
            }
        }
    }
}
