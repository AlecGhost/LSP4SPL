use super::DocumentCursor;
use crate::document::{convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{
    request::{GotoDeclarationParams, GotoImplementationParams, GotoTypeDefinitionParams},
    GotoDefinitionParams, Location,
};
use spl_frontend::{
    table::{DataType, Entry, LookupTable, Table},
    ToRange,
};
use tokio::sync::mpsc::Sender;

pub(crate) async fn declaration(
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
                    Entry::Type(_) => {
                        if let Some(entry) = doc_info.table.lookup(&ident.value) {
                            return Ok(Some(Location {
                                uri,
                                range: convert_range(
                                    &entry.to_range(),
                                    &doc_info.text,
                                ),
                            }));
                        }
                    }
                    Entry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: &doc_info.table,
                            local_table: &p.local_table,
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            return Ok(Some(Location {
                                uri,
                                range: convert_range(
                                    &entry.to_range(),
                                    &doc_info.text,
                                ),
                            }));
                        }
                    }
                    Entry::Variable(v) => {
                        log::error!("Found illegal variable in global table {:#?}", v);
                        panic!("Found illegal variable in global table {:#?}", v);
                    }
                }
            }
        }
    }
    Ok(None)
}

/// Calls `goto::declaration` because in SPL, there is no conceptual difference
/// between declaration and definition
pub(crate) async fn definition(
    doctx: Sender<DocumentRequest>,
    params: GotoDefinitionParams,
) -> Result<Option<Location>> {
    declaration(doctx, params).await
}

pub(crate) async fn type_definition(
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
                    Entry::Type(_) => {
                        if let Some(entry) = doc_info.table.lookup(&ident.value) {
                            match &entry {
                                Entry::Type(_) => {
                                    return Ok(Some(Location {
                                        uri,
                                        range: convert_range(
                                            &entry.to_range(),
                                            &doc_info.text,
                                        ),
                                    }));
                                }
                                Entry::Procedure(_) => { /* no type definition */ }
                                Entry::Variable(v) => {
                                    log::error!("Found illegal variable in global table {:#?}", v);
                                    panic!("Found illegal variable in global table {:#?}", v);
                                }
                            }
                        }
                    }
                    Entry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: &doc_info.table,
                            local_table: &p.local_table,
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            match &entry {
                                Entry::Type(_) => {
                                    return Ok(Some(Location {
                                        uri,
                                        range: convert_range(
                                            &entry.to_range(),
                                            &doc_info.text,
                                        ),
                                    }));
                                }
                                Entry::Procedure(_) => { /* no type definition */ }
                                Entry::Variable(v) => {
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
                    Entry::Variable(v) => {
                        log::error!("Found illegal variable in global table {:#?}", v);
                        panic!("Found illegal variable in global table {:#?}", v);
                    }
                }
            }
        }
    }
    Ok(None)
}

/// Essentially the same as `goto::declaration`, but only for procedures
pub(crate) async fn implementation(
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
                    Entry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: &doc_info.table,
                            local_table: &p.local_table,
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            if let Entry::Procedure(_) = entry {
                                return Ok(Some(Location {
                                    uri,
                                    range: convert_range(
                                        &entry.to_range(),
                                        &doc_info.text,
                                    ),
                                }));
                            }
                            /* no implementation for types and variables */
                        }
                    }
                    Entry::Type(_) => { /* no implementation for types */ }
                    Entry::Variable(v) => {
                        log::error!("Found illegal variable in global table {:#?}", v);
                        panic!("Found illegal variable in global table {:#?}", v);
                    }
                }
            }
        }
    }
    Ok(None)
}
