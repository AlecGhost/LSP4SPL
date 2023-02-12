use crate::document::{convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{
    request::{GotoDeclarationParams, GotoImplementationParams, GotoTypeDefinitionParams},
    GotoDefinitionParams, Location,
};
use spl_frontend::table::{DataType, Entry, LookupTable, Table};
use tokio::sync::mpsc::Sender;

use super::DocumentCursor;

pub(crate) async fn declaration(
    doctx: Sender<DocumentRequest>,
    params: GotoDeclarationParams,
) -> Result<Option<Location>> {
    let doc_params = params.text_document_position_params;
    let uri = doc_params.text_document.uri.clone();
    if let Some(DocumentCursor {
        doc_info,
        index,
        context,
    }) = super::doc_cursor(doc_params, doctx).await?
    {
        if let Some(ident) = doc_info.ast.ident_at(index) {
            match &context.entry {
                Entry::Type(_) => {
                    if let Some((key, _)) = doc_info.table.entry(&ident) {
                        return Ok(Some(Location {
                            uri,
                            range: convert_range(&key.range, &doc_info.text),
                        }));
                    }
                }
                Entry::Procedure(p) => {
                    let lookup_table = LookupTable {
                        global_table: &doc_info.table,
                        local_table: &p.local_table,
                    };
                    if let Some((key, _)) = lookup_table.entry(&ident) {
                        return Ok(Some(Location {
                            uri,
                            range: convert_range(&key.range, &doc_info.text),
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
    if let Some(DocumentCursor {
        doc_info,
        index,
        context,
    }) = super::doc_cursor(doc_params, doctx).await?
    {
        if let Some(ident) = doc_info.ast.ident_at(index) {
            match &context.entry {
                Entry::Type(_) => {
                    if let Some((key, ranged_entry)) = doc_info.table.entry(&ident) {
                        match &ranged_entry.entry {
                            Entry::Type(_) => {
                                return Ok(Some(Location {
                                    uri,
                                    range: convert_range(&key.range, &doc_info.text),
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
                    if let Some((key, ranged_entry)) = lookup_table.entry(&ident) {
                        match &ranged_entry.entry {
                            Entry::Type(_) => {
                                return Ok(Some(Location {
                                    uri,
                                    range: convert_range(&key.range, &doc_info.text),
                                }));
                            }
                            Entry::Procedure(_) => { /* no type definition */ }
                            Entry::Variable(v) => {
                                if let Some(DataType::Array {
                                    size: _,
                                    base_type: _,
                                    creator,
                                }) = &v.data_type
                                {
                                    return Ok(Some(Location {
                                        uri,
                                        range: convert_range(&creator.range, &doc_info.text),
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
    Ok(None)
}

/// Essentially the same as `goto::declaration`, but only for procedures
pub(crate) async fn implementation(
    doctx: Sender<DocumentRequest>,
    params: GotoImplementationParams,
) -> Result<Option<Location>> {
    let doc_params = params.text_document_position_params;
    let uri = doc_params.text_document.uri.clone();
    if let Some(DocumentCursor {
        doc_info,
        index,
        context,
    }) = super::doc_cursor(doc_params, doctx).await?
    {
        if let Some(ident) = doc_info.ast.ident_at(index) {
            match &context.entry {
                Entry::Procedure(p) => {
                    let lookup_table = LookupTable {
                        global_table: &doc_info.table,
                        local_table: &p.local_table,
                    };
                    if let Some((key, ranged_entry)) = lookup_table.entry(&ident) {
                        if let Entry::Procedure(_) = ranged_entry.entry {
                            return Ok(Some(Location {
                                uri,
                                range: convert_range(&key.range, &doc_info.text),
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
    Ok(None)
}
