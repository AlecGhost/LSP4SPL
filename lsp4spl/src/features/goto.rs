use super::DocumentCursor;
use crate::document::{as_pos_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{
    request::{GotoDeclarationParams, GotoImplementationParams, GotoTypeDefinitionParams},
    GotoDefinitionParams, Location,
};
use spl_frontend::{
    table::{DataType, Entry, GlobalEntry, LookupTable, SymbolTable},
    ToRange, ToTextRange,
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
            let DocumentCursor { doc, context, .. } = cursor;
            if let Some(entry) = context {
                match &entry {
                    GlobalEntry::Type(t) => {
                        // early return for int;
                        if &ident.value == "int" {
                            return Ok(None);
                        }
                        if let Some(entry) = doc.table.lookup(&ident.value) {
                            let tokens = &doc.tokens[t.to_range()];
                            return Ok(Some(Location {
                                uri,
                                range: as_pos_range(&entry.to_text_range(tokens), &doc.text),
                            }));
                        }
                    }
                    GlobalEntry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: Some(&doc.table),
                            local_table: Some(&p.local_table),
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            // early return for default values
                            if entry.is_default() {
                                return Ok(None);
                            }
                            let tokens = match entry {
                                Entry::Procedure(param_dec) => &doc.tokens[param_dec.to_range()],
                                Entry::Type(type_dec) => &doc.tokens[type_dec.to_range()],
                                Entry::Variable(var) | Entry::Parameter(var) => {
                                    &doc.tokens[p.to_range()][var.to_range()]
                                }
                            };
                            return Ok(Some(Location {
                                uri,
                                range: as_pos_range(&entry.to_text_range(tokens), &doc.text),
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
            let DocumentCursor { doc, context, .. } = cursor;
            if let Some(entry) = context {
                match &entry {
                    GlobalEntry::Type(_) => {
                        // early return for int;
                        if &ident.value == "int" {
                            return Ok(None);
                        }
                        if let Some(entry) = doc.table.lookup(&ident.value) {
                            match &entry {
                                GlobalEntry::Type(t) => {
                                    let tokens = &doc.tokens[t.to_range()];
                                    return Ok(Some(Location {
                                        uri,
                                        range: as_pos_range(
                                            &entry.to_text_range(tokens),
                                            &doc.text,
                                        ),
                                    }));
                                }
                                GlobalEntry::Procedure(_) => { /* no type definition */ }
                            }
                        }
                    }
                    GlobalEntry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: Some(&doc.table),
                            local_table: Some(&p.local_table),
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            match &entry {
                                Entry::Type(t) => {
                                    // early return for int;
                                    if &ident.value == "int" {
                                        return Ok(None);
                                    }
                                    let tokens = &doc.tokens[t.to_range()];
                                    return Ok(Some(Location {
                                        uri,
                                        range: as_pos_range(
                                            &entry.to_text_range(tokens),
                                            &doc.text,
                                        ),
                                    }));
                                }
                                Entry::Procedure(_) => { /* no type definition */ }
                                Entry::Variable(v) | Entry::Parameter(v) => {
                                    if let Some(DataType::Array { creator, .. }) = &v.data_type {
                                        let entry =
                                            doc.table.lookup(creator).expect("Invalid creator");
                                        match entry {
                                            GlobalEntry::Type(t) => {
                                                return Ok(Some(Location {
                                                    uri,
                                                    range: as_pos_range(
                                                        &entry.to_text_range(
                                                            &doc.tokens[t.to_range()],
                                                        ),
                                                        &doc.text,
                                                    ),
                                                }));
                                            }
                                            _ => panic!("Creator must be a type"),
                                        }
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
            let DocumentCursor { doc, context, .. } = cursor;
            if let Some(entry) = context {
                match &entry {
                    GlobalEntry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: Some(&doc.table),
                            local_table: Some(&p.local_table),
                        };
                        if let Some(entry) = lookup_table.lookup(&ident.value) {
                            let tokens = &doc.tokens[p.to_range()];
                            if let Entry::Procedure(_) = entry {
                                return Ok(Some(Location {
                                    uri,
                                    range: as_pos_range(&entry.to_text_range(tokens), &doc.text),
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
