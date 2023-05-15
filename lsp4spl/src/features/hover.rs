use super::{DocumentCursor, ToSpl};
use crate::document::{as_pos_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Range as PosRange};
use spl_frontend::{
    table::{Entry, GlobalEntry, LookupTable, SymbolTable, TableEntry},
    ToRange,
};
use tokio::sync::mpsc::Sender;

fn create_hover(entry: &Entry, range: PosRange) -> Hover {
    let documentation = entry.doc().map_or_else(String::new, |doc| {
        String::new() + "\n---\n" + doc.trim_start() + "\n"
    });
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: entry.to_string().to_spl() + &documentation,
        }),
        range: Some(range),
    }
}

pub async fn hover(doctx: Sender<DocumentRequest>, params: HoverParams) -> Result<Option<Hover>> {
    let doc_params = params.text_document_position_params;
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        if let Some(ident) = &cursor.ident() {
            let DocumentCursor { doc, context, .. } = cursor;
            if let Some(entry) = context {
                match &entry {
                    GlobalEntry::Type(_) => {
                        if let Some(entry) = doc.table.lookup(&ident.value) {
                            return Ok(Some(create_hover(
                                &Entry::from(entry),
                                as_pos_range(&ident.to_range(), &doc.text),
                            )));
                        }
                    }
                    GlobalEntry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: Some(&doc.table),
                            local_table: Some(&p.local_table),
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            return Ok(Some(create_hover(
                                &entry,
                                as_pos_range(&ident.to_range(), &doc.text),
                            )));
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}
