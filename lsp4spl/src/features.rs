pub(crate) mod goto;
mod hover;

use color_eyre::eyre::Result;
pub(crate) use hover::hover;
use lsp_types::{TextDocumentPositionParams, Url};
use spl_frontend::{ast::Identifier, table::Entry};
use tokio::sync::{mpsc::Sender, oneshot};

use crate::document::{self, DocumentInfo, DocumentRequest};

async fn get_doc_info(uri: Url, doctx: Sender<DocumentRequest>) -> Result<Option<DocumentInfo>> {
    let (tx, rx) = oneshot::channel();
    doctx.send(DocumentRequest::GetInfo(uri, tx)).await?;
    let doc_info = rx.await?;
    Ok(doc_info)
}

struct DocumentPrelude {
    doc_info: DocumentInfo,
    index: usize,
    ident: Identifier,
    entry: Entry,
}

async fn document_prelude(
    doc_params: TextDocumentPositionParams,
    doctx: Sender<DocumentRequest>,
) -> Result<Option<DocumentPrelude>> {
    let pos = doc_params.position;
    let uri = doc_params.text_document.uri;
    if let Some(doc_info) = get_doc_info(uri, doctx).await? {
        if let Some(index) = document::get_index(pos, &doc_info.text) {
            if let Some(ident) = doc_info.ast.ident_at(index) {
                if let Some(ranged_entry) = doc_info
                    .table
                    .entries
                    .values()
                    .find(|entry| entry.range.contains(&index))
                {
                    let ident = ident.clone();
                    let entry = ranged_entry.entry.clone();
                    return Ok(Some(DocumentPrelude {
                        doc_info,
                        index,
                        ident,
                        entry,
                    }));
                }
            }
        }
    }
    Ok(None)
}
