use crate::{
    document::{self, DocumentRequest},
    io::Message,
};
use color_eyre::eyre::Result;
use lsp_types::{DidOpenTextDocumentParams, Position, TextDocumentItem, Url};
use tokio::sync::mpsc::{self, Sender};

mod completion;
mod fold;
mod formatting;
mod goto;
mod references;

pub async fn test_feature<F, P, R, E>(f: F, uri: Url, text: &str, params: P) -> Result<E>
where
    F: Fn(Sender<DocumentRequest>, P) -> R,
    R: std::future::Future<Output = Result<E>>,
    E: std::fmt::Debug + std::cmp::PartialEq,
{
    let (iotx, iorx) = mpsc::channel(32);
    let doctx = start_document_broker(iotx);
    open_document(doctx.clone(), uri, text.to_string()).await;
    let result = f(doctx, params).await?;
    drop(iorx);
    Ok(result)
}

fn start_document_broker(iotx: Sender<Message>) -> Sender<DocumentRequest> {
    let (doctx, docrx) = mpsc::channel(32);
    tokio::spawn(document::broker(docrx, iotx, true));
    doctx
}

async fn open_document(doctx: Sender<DocumentRequest>, uri: Url, text: String) {
    document::open(
        doctx,
        DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri,
                language_id: "spl".to_string(),
                version: 0,
                text,
            },
        },
    )
    .await
    .unwrap();
}

pub fn pos(line: u32, character: u32) -> Position {
    Position { line, character }
}
