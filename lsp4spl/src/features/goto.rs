use super::DocumentPrelude;
use crate::document::{convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{
    request::{GotoDeclarationParams, GotoTypeDefinitionParams},
    GotoDefinitionParams, Location,
};
use spl_frontend::table::{DataType, Entry, LookupTable, Table};
use tokio::sync::mpsc::Sender;

pub(crate) async fn declaration(
    doctx: Sender<DocumentRequest>,
    params: GotoDeclarationParams,
) -> Result<Option<Location>> {
    let doc_params = params.text_document_position_params;
    let uri = doc_params.text_document.uri.clone();
    if let Some(DocumentPrelude {
        doc_info,
        ident,
        entry,
    }) = super::document_prelude(doc_params, doctx).await?
    {
        match &entry {
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
    Ok(None)
}

pub(crate) async fn definition(
    doctx: Sender<DocumentRequest>,
    params: GotoDefinitionParams,
) -> Result<Option<Location>> {
    // in SPL, there is no conceptual difference between declaration and definition
    declaration(doctx, params).await
}

pub(crate) async fn type_definition(
    doctx: Sender<DocumentRequest>,
    params: GotoTypeDefinitionParams,
) -> Result<Option<Location>> {
    let doc_params = params.text_document_position_params;
    let uri = doc_params.text_document.uri.clone();
    if let Some(DocumentPrelude {
        doc_info,
        ident,
        entry,
    }) = super::document_prelude(doc_params, doctx).await?
    {
        match &entry {
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
                            if let Some(dt) = &v.data_type {
                                match dt {
                                    DataType::Array {
                                        size: _,
                                        base_type: _,
                                        creator,
                                    } => {
                                        return Ok(Some(Location {
                                            uri,
                                            range: convert_range(&creator.range, &doc_info.text),
                                        }));
                                    }
                                    _ => { /* cannot look up primitive type */ }
                                }
                            }
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
    Ok(None)
}
