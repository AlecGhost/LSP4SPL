use crate::document::{self, DocumentInfo, DocumentRequest};
use color_eyre::eyre::{Context, Result};
use lsp_types::{TextDocumentPositionParams, Url};
use spl_frontend::{
    ast::Identifier,
    lexer::token::{TokenList, TokenType},
    table::RangedEntry,
};
use tokio::sync::{mpsc::Sender, oneshot};

mod completion;
mod fold;
pub(crate) mod goto;
mod hover;
pub(crate) mod references;

pub(crate) use completion::completion;
pub(crate) use fold::fold;
pub(crate) use hover::hover;

struct DocumentCursor {
    doc_info: DocumentInfo,
    index: usize,
    context: Option<RangedEntry>,
}

impl DocumentCursor {
    fn ident(&self) -> Option<Identifier> {
        let token = self.doc_info.tokens.token_at(self.index)?;
        if let TokenType::Ident(name) = &token.token_type {
            Some(Identifier::new(name.clone(), &[token.clone()]))
        } else {
            None
        }
    }
}

async fn get_doc_info(uri: Url, doctx: Sender<DocumentRequest>) -> Result<Option<DocumentInfo>> {
    let (tx, rx) = oneshot::channel();
    doctx
        .send(DocumentRequest::GetInfo(uri, tx))
        .await
        .wrap_err("Cannot send document request")?;
    let doc_info = rx.await.wrap_err("Cannot recieve document request")?;
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
