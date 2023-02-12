pub(crate) mod goto;
mod hover;

use color_eyre::eyre::Result;
pub(crate) use hover::hover;
use lsp_types::{TextDocumentPositionParams, Url};
use spl_frontend::{ast::Identifier, table::Entry};
use tokio::sync::{mpsc::Sender, oneshot};

use crate::document::{self, DocumentInfo, DocumentRequest};

struct DocumentCursor {
    doc_info: DocumentInfo,
    index: usize,
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
            return Ok(Some(DocumentCursor { doc_info, index }));
        }
    }
    Ok(None)
}

fn ident_with_context(cursor: &DocumentCursor) -> Option<(Identifier, Entry)> {
    if let Some(ident) = cursor.doc_info.ast.ident_at(cursor.index) {
        if let Some(ranged_entry) = cursor
            .doc_info
            .table
            .entries
            .values()
            .find(|entry| entry.range.contains(&cursor.index))
        {
            let entry = ranged_entry.entry.clone();
            return Some((ident.clone(), entry));
        }
    }
    None
}
