pub(crate) mod goto;
mod hover;

use color_eyre::eyre::Result;
pub(crate) use hover::hover;
use lsp_types::{TextDocumentPositionParams, Url};
use spl_frontend::table::RangedEntry;
use tokio::sync::{mpsc::Sender, oneshot};

use crate::document::{self, DocumentInfo, DocumentRequest};

struct DocumentCursor {
    doc_info: DocumentInfo,
    index: usize,
    context: RangedEntry,
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
            if let Some(ranged_entry) = doc_info
                .table
                .entries
                .values()
                .find(|entry| entry.range.contains(&index))
            {
                let context = ranged_entry.clone();
                return Ok(Some(DocumentCursor {
                    doc_info,
                    index,
                    context,
                }));
            }
        }
    }
    Ok(None)
}
