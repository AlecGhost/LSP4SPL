use crate::{
    document::{self, DocumentRequest},
    io::Message,
};
use color_eyre::eyre::Result;
use lsp_types::{DidOpenTextDocumentParams, TextDocumentItem, Url};
use pretty_assertions::assert_eq;
use tokio::sync::mpsc::{self, Sender};

mod goto;

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

pub async fn test_feature<P, F, R, E>(
    f: F,
    uri: Url,
    text: &str,
    params: P,
    expected: E,
) -> Result<()>
where
    F: Fn(Sender<DocumentRequest>, P) -> R,
    R: std::future::Future<Output = Result<E>>,
    E: std::fmt::Debug + std::cmp::PartialEq,
{
    let (iotx, iorx) = tokio::sync::mpsc::channel(32);
    let doctx = start_document_broker(iotx);
    open_document(doctx.clone(), uri, text.to_string()).await;
    let result = f(doctx, params).await?;
    assert_eq!(result, expected,);
    drop(iorx);
    Ok(())
}
