use std::collections::HashMap;

use lsp_types::*;
use tokio::sync::mpsc::{Receiver, Sender};

#[derive(Debug)]
pub(super) enum DocumentStatus {
    DidOpen((Url, String)),
    DidChange(
        (
            Url,
            Vec<TextDocumentContentChangeEvent>,
        ),
    ),
    DidClose(Url),
}

pub(super) async fn did_open(broker: Sender<DocumentStatus>, params: DidOpenTextDocumentParams) {
    broker
        .send(DocumentStatus::DidOpen((params.text_document.uri, params.text_document.text)))
        .await
        .expect("Cannot send messages");
}

pub(super) async fn did_change(
    broker: Sender<DocumentStatus>,
    params: DidChangeTextDocumentParams,
) {
    broker
        .send(DocumentStatus::DidChange((
            params.text_document.uri,
            params.content_changes,
        )))
        .await
        .expect("Cannot send messages");
}

pub(super) async fn did_close(broker: Sender<DocumentStatus>, params: DidCloseTextDocumentParams) {
    broker
        .send(DocumentStatus::DidClose(params.text_document.uri))
        .await
        .expect("Cannot send messages");
}

pub(super) async fn broker(mut rx: Receiver<DocumentStatus>) {
    use DocumentStatus::*;
    let mut docs = HashMap::new();
    while let Some(status) = rx.recv().await {
        match status {
            DidOpen((uri, text)) => {
                docs.insert(uri.path().to_string(), text);
            }
            DidChange((uri, changes)) => {
                changes.into_iter().for_each(|change| match change.range {
                    Some(_) => { /*TODO: Incremental editing*/ }
                    None => {
                        docs.insert(uri.path().to_string(), change.text);
                    }
                })
            }
            DidClose(uri) => {
                docs.remove(uri.path());
            }
        }
    }
}
