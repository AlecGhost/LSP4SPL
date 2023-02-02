use super::DocumentPrelude;
use crate::document::{convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Range};
use spl_frontend::table::{Entry, LookupTable, Table};
use tokio::sync::mpsc::Sender;

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
    if let Some(DocumentPrelude {
        doc_info,
        ident,
        entry,
    }) = super::document_prelude(doc_params, doctx).await?
    {
        match &entry {
            Entry::Type(_) => {
                if let Some(ranged_entry) = doc_info.table.lookup(&ident) {
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
                if let Some(ranged_entry) = lookup_table.lookup(&ident) {
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
        }
    }
    Ok(None)
}
