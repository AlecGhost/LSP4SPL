use crate::document::{self, DocumentInfo, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{TextDocumentPositionParams, Url};
use spl_frontend::table::RangedEntry;
use tokio::sync::{mpsc::Sender, oneshot};

mod completion;
pub(crate) mod goto;
mod hover;

pub(crate) use completion::completion;
pub(crate) use hover::hover;

struct DocumentCursor {
    doc_info: DocumentInfo,
    index: usize,
    context: Option<RangedEntry>,
}

async fn get_doc_info(uri: Url, doctx: Sender<DocumentRequest>) -> Result<Option<DocumentInfo>> {
    let (tx, rx) = oneshot::channel();
    doctx.send(DocumentRequest::GetInfo(uri, tx)).await?;
    let doc_info = rx.await?;
    Ok(doc_info)
}

async fn doc_cursor(
    doc_params: TextDocumentPositionParams,
    doctx: Sender<DocumentRequest>,
) -> Result<Option<DocumentCursor>> {
    let pos = doc_params.position;
    let uri = doc_params.text_document.uri;
    if let Some(doc_info) = get_doc_info(uri, doctx).await? {
        if let Some(index) = document::get_index(pos, &doc_info.text) {
            let context = doc_info
                .table
                .entries
                .values()
                .find(|entry| entry.range.contains(&index))
                .cloned();
            return Ok(Some(DocumentCursor {
                doc_info,
                index,
                context,
            }));
        }
    }
    Ok(None)
}
