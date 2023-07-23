use std::ops::Range;

use crate::document::{self, DocumentRequest};
use color_eyre::eyre::{Context, Result};
use lsp_types::{TextDocumentPositionParams, Url};
use spl_frontend::{
    ast::{GlobalDeclaration, Identifier, ProcedureDeclaration},
    table::{GlobalEntry, GlobalTable, LocalTable, SymbolTable},
    token::TokenType,
    AnalyzedSource, ToRange, ToTextRange,
};
use tokio::sync::{mpsc::Sender, oneshot};

pub mod completion;
mod fold;
mod formatting;
pub mod goto;
mod hover;
pub mod references;
pub mod semantic_tokens;
mod signature_help;

pub use fold::fold;
pub use formatting::format;
pub use hover::hover;
pub use semantic_tokens::semantic_tokens;
pub use signature_help::signature_help;

#[cfg(test)]
mod tests;

/// This `Ident` corresponds to a token of type `Ident`.
/// Therefore the range is the actual text range.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Ident {
    value: String,
    range: Range<usize>,
}

impl Ident {
    fn from_identifier(identifier: &Identifier, range: Range<usize>) -> Self {
        Self {
            value: identifier.value.clone(),
            range,
        }
    }
}

impl ToRange for Ident {
    fn to_range(&self) -> Range<usize> {
        self.range.clone()
    }
}

struct DocumentCursor {
    doc: AnalyzedSource,
    index: usize,
    context: Option<GlobalEntry>,
}

impl DocumentCursor {
    fn ident(&self) -> Option<Ident> {
        let token = self
            .doc
            .tokens
            .iter()
            .find(|token| token.range.contains(&self.index))?;
        if let TokenType::Ident(name) = &token.token_type {
            Some(Ident {
                value: name.clone(),
                range: token.range.clone(),
            })
        } else {
            None
        }
    }
}

async fn get_doc(uri: Url, doctx: Sender<DocumentRequest>) -> Result<Option<AnalyzedSource>> {
    let (tx, rx) = oneshot::channel();
    doctx
        .send(DocumentRequest::GetInfo(uri, tx))
        .await
        .wrap_err("Cannot send document request")?;
    let doc = rx.await.wrap_err("Cannot recieve document request")?;
    Ok(doc)
}

async fn doc_cursor(
    doc_params: TextDocumentPositionParams,
    doctx: Sender<DocumentRequest>,
) -> Result<Option<DocumentCursor>> {
    let pos = doc_params.position;
    let uri = doc_params.text_document.uri;
    if let Some(doc) = get_doc(uri, doctx).await? {
        let index = document::get_insertion_index(&pos, &doc.text);
        let context = doc
            .ast
            .global_declarations
            .iter()
            .find(|gd| gd.to_text_range(&doc.tokens[gd.offset..]).contains(&index))
            .and_then(|gd| {
                use GlobalDeclaration::*;
                let name = match gd.as_ref() {
                    Procedure(pd) => pd.name.as_ref(),
                    Type(td) => td.name.as_ref(),
                    Error(_) => None,
                };
                if let Some(name) = &name {
                    doc.table.lookup(&name.value).cloned()
                } else {
                    None
                }
            });
        return Ok(Some(DocumentCursor {
            doc,
            index,
            context,
        }));
    }
    Ok(None)
}

pub trait ToSpl {
    /// Turns input into a SPL markdown code block
    fn to_spl(&self) -> String;
}

impl ToSpl for String {
    fn to_spl(&self) -> Self {
        Self::new() + "```spl\n" + self + "\n```"
    }
}

fn get_local_table<'a>(
    pd: &ProcedureDeclaration,
    global_table: &'a GlobalTable,
) -> Option<&'a LocalTable> {
    if let Some(name) = &pd.name {
        if let Some(GlobalEntry::Procedure(p)) = global_table.lookup(&name.value) {
            return Some(&p.local_table);
        }
    }
    None
}
