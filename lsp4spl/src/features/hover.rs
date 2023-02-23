use super::{DocumentCursor, ToSpl};
use crate::document::{convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Range};
use spl_frontend::{
    table::{Entry, LookupTable, SymbolTable, GlobalEntry, TableEntry},
    ToRange,
};
use tokio::sync::mpsc::Sender;

fn create_hover(entry: Entry, range: Range) -> Hover {
    let documentation = entry
        .doc()
        .map(|doc| String::new() + "\n---\n" + doc.trim_start() + "\n")
        .unwrap_or_else(|| "".to_string());
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: entry.to_string().to_spl() + &documentation,
        }),
        range: Some(range),
    }
}

pub(crate) async fn hover(
    doctx: Sender<DocumentRequest>,
    params: HoverParams,
) -> Result<Option<Hover>> {
    let doc_params = params.text_document_position_params;
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        if let Some(ident) = &cursor.ident() {
            let DocumentCursor {
                doc_info, context, ..
            } = cursor;
            if let Some(entry) = context {
                match &entry {
                    GlobalEntry::Type(_) => {
                        if let Some(entry) = doc_info.table.lookup(&ident.value) {
                            return Ok(Some(create_hover(
                                Entry::from(entry),
                                convert_range(&ident.to_range(), &doc_info.text),
                            )));
                        }
                    }
                    GlobalEntry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: Some(&doc_info.table),
                            local_table: Some(&p.local_table),
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            return Ok(Some(create_hover(
                                entry,
                                convert_range(&ident.to_range(), &doc_info.text),
                            )));
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}
