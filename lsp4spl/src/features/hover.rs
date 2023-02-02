use crate::document::{self, convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Range};
use spl_frontend::table::{Entry, LookupTable, Table};
use tokio::sync::{mpsc::Sender, oneshot};

fn create_hover(entry: &Entry, range: Range) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: entry.to_string(),
        }),
        range: Some(range),
    }
}

pub(crate) async fn hover(
    doctx: Sender<DocumentRequest>,
    params: HoverParams,
) -> Result<Option<Hover>> {
    let doc_params = params.text_document_position_params;
    let pos = doc_params.position;
    let url = doc_params.text_document.uri;
    let (tx, rx) = oneshot::channel();
    doctx.send(DocumentRequest::GetInfo(url, tx)).await?;
    let doc_info = rx.await?;
    if let Some(doc_info) = doc_info {
        if let Some(index) = document::get_index(pos, &doc_info.text) {
            if let Some(ident) = doc_info.ast.ident_at(index) {
                if let Some(ranged_entry) = doc_info
                    .table
                    .entries
                    .values()
                    .find(|entry| entry.range.contains(&index))
                {
                    match &ranged_entry.entry {
                        Entry::Type(_) => {
                            if let Some(ranged_entry) = doc_info.table.lookup(ident) {
                                return Ok(Some(create_hover(
                                    &ranged_entry.entry,
                                    convert_range(&ident.range, &doc_info.text),
                                )));
                            }
                        }
                        Entry::Procedure(p) => {
                            let lookup_table = LookupTable {
                                global_table: &doc_info.table,
                                local_table: &p.local_table,
                            };
                            if let Some(ranged_entry) = lookup_table.lookup(ident) {
                                return Ok(Some(create_hover(
                                    &ranged_entry.entry,
                                    convert_range(&ident.range, &doc_info.text),
                                )));
                            }
                        }
                        Entry::Variable(v) => {
                            log::error!("Found illegal variable in global table {:#?}", v);
                            panic!("Found illegal variable in global table {:#?}", v);
                        }
                    };
                }
            }
        }
    }
    Ok(None)
}
