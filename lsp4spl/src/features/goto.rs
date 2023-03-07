use super::DocumentCursor;
use crate::document::{convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{
    request::{GotoDeclarationParams, GotoImplementationParams, GotoTypeDefinitionParams},
    GotoDefinitionParams, Location,
};
use spl_frontend::{
    table::{DataType, Entry, GlobalEntry, LookupTable, SymbolTable},
    ToRange,
};
use tokio::sync::mpsc::Sender;

pub async fn declaration(
    doctx: Sender<DocumentRequest>,
    params: GotoDeclarationParams,
) -> Result<Option<Location>> {
    let doc_params = params.text_document_position_params;
    let uri = doc_params.text_document.uri.clone();
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        if let Some(ident) = &cursor.ident() {
            let DocumentCursor {
                doc_info, context, ..
            } = cursor;
            if let Some(entry) = context {
                match &entry {
                    GlobalEntry::Type(_) => {
                        if let Some(entry) = doc_info.table.lookup(&ident.value) {
                            return Ok(Some(Location {
                                uri,
                                range: convert_range(&entry.to_range(), &doc_info.text),
                            }));
                        }
                    }
                    GlobalEntry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: Some(&doc_info.table),
                            local_table: Some(&p.local_table),
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            if let Entry::Procedure(proc_entry) = entry {
                                if proc_entry.is_default() {
                                    return Ok(None);
                                }
                            }
                            return Ok(Some(Location {
                                uri,
                                range: convert_range(&entry.to_range(), &doc_info.text),
                            }));
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

/// Calls `goto::declaration` because in SPL, there is no conceptual difference
/// between declaration and definition
pub async fn definition(
    doctx: Sender<DocumentRequest>,
    params: GotoDefinitionParams,
) -> Result<Option<Location>> {
    declaration(doctx, params).await
}

pub async fn type_definition(
    doctx: Sender<DocumentRequest>,
    params: GotoTypeDefinitionParams,
) -> Result<Option<Location>> {
    let doc_params = params.text_document_position_params;
    let uri = doc_params.text_document.uri.clone();
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        if let Some(ident) = &cursor.ident() {
            let DocumentCursor {
                doc_info, context, ..
            } = cursor;
            if let Some(entry) = context {
                match &entry {
                    GlobalEntry::Type(_) => {
                        if let Some(entry) = doc_info.table.lookup(&ident.value) {
                            match &entry {
                                GlobalEntry::Type(_) => {
                                    return Ok(Some(Location {
                                        uri,
                                        range: convert_range(&entry.to_range(), &doc_info.text),
                                    }));
                                }
                                GlobalEntry::Procedure(_) => { /* no type definition */ }
                            }
                        }
                    }
                    GlobalEntry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: Some(&doc_info.table),
                            local_table: Some(&p.local_table),
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            match &entry {
                                Entry::Type(_) => {
                                    return Ok(Some(Location {
                                        uri,
                                        range: convert_range(&entry.to_range(), &doc_info.text),
                                    }));
                                }
                                Entry::Procedure(_) => { /* no type definition */ }
                                Entry::Variable(v) | Entry::Parameter(v) => {
                                    if let Some(DataType::Array { creator, .. }) = &v.data_type {
                                        return Ok(Some(Location {
                                            uri,
                                            range: convert_range(
                                                &creator.to_range(),
                                                &doc_info.text,
                                            ),
                                        }));
                                    }
                                    /* cannot look up primitive types */
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

/// Essentially the same as `goto::declaration`, but only for procedures
pub async fn implementation(
    doctx: Sender<DocumentRequest>,
    params: GotoImplementationParams,
) -> Result<Option<Location>> {
    let doc_params = params.text_document_position_params;
    let uri = doc_params.text_document.uri.clone();
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        if let Some(ident) = &cursor.ident() {
            let DocumentCursor {
                doc_info, context, ..
            } = cursor;
            if let Some(entry) = context {
                match &entry {
                    GlobalEntry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: Some(&doc_info.table),
                            local_table: Some(&p.local_table),
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            if let Entry::Procedure(_) = entry {
                                return Ok(Some(Location {
                                    uri,
                                    range: convert_range(&entry.to_range(), &doc_info.text),
                                }));
                            }
                            /* no implementation for types and variables */
                        }
                    }
                    GlobalEntry::Type(_) => { /* no implementation for types */ }
                }
            }
        }
    }
    Ok(None)
}
