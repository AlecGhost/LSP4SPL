use crate::document::DocumentRequest;
use color_eyre::eyre::Result;
use lsp_types::{
    Documentation, MarkupContent, MarkupKind, ParameterInformation, ParameterLabel, SignatureHelp,
    SignatureHelpParams, SignatureInformation,
};
use spl_frontend::{
    ast::{CallStatement, GlobalDeclaration, ProcedureDeclaration, Reference, Statement},
    table::{GlobalEntry, ProcedureEntry, SymbolTable, VariableEntry},
    tokens::{Token, TokenType},
    Shiftable, ToRange, ToTextRange,
};
use tokio::sync::mpsc::Sender;

pub async fn signature_help(
    doctx: Sender<DocumentRequest>,
    params: SignatureHelpParams,
) -> Result<Option<SignatureHelp>> {
    let doc_params = params.text_document_position_params;
    super::doc_cursor(doc_params, doctx).await.map(|cursor| {
        cursor.and_then(|cursor| {
            cursor
                .doc
                .ast
                .global_declarations
                .iter()
                // find the procedure that contains the cursor index
                .find_map(|gd| match gd.as_ref() {
                    GlobalDeclaration::Procedure(pd)
                        if pd
                            .to_text_range(&cursor.doc.tokens[gd.offset..])
                            .contains(&cursor.index) =>
                    {
                        Some((pd, gd.offset))
                    }
                    _ => None,
                })
                // find the call statement that contains the cursor index
                .and_then(|(pd, pd_offset)| {
                    find_call_stmt(pd, &cursor.index, pd_offset, &cursor.doc.tokens)
                })
                // map the call statement to the signature help
                .and_then(|(call_stmt, offset)| {
                    if let Some(GlobalEntry::Procedure(proc_entry)) =
                        &cursor.doc.table.lookup(&call_stmt.name.value)
                    {
                        let parameters = get_param_info(proc_entry);
                        let documentation = proc_entry.doc.as_ref().map(|doc| {
                            Documentation::MarkupContent(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: doc.clone(),
                            })
                        });
                        let tokens = &cursor.doc.tokens[call_stmt.to_range().shift(offset)];
                        let active_parameter = get_active_param(&parameters, tokens, &cursor.index);
                        let help = SignatureInformation {
                            label: proc_entry.name.to_string(),
                            documentation,
                            parameters: Some(parameters),
                            active_parameter,
                        };
                        Some(SignatureHelp {
                            signatures: vec![help],
                            active_signature: Some(0),
                            active_parameter,
                        })
                    } else {
                        None
                    }
                })
        })
    })
}

fn find_call_stmt<'a>(
    pd: &'a ProcedureDeclaration,
    index: &usize,
    offset: usize,
    tokens: &[Token],
) -> Option<(&'a CallStatement, usize)> {
    pd.statements
        .iter()
        .find_map(|r| find_call_stmt_in_stmt(r.as_ref(), index, offset + r.offset, tokens))
}

fn find_call_stmt_in_stmt<'a>(
    stmt: &'a Statement,
    index: &usize,
    offset: usize,
    tokens: &[Token],
) -> Option<(&'a CallStatement, usize)> {
    fn get_in_option<'a>(
        opt: &'a Option<Box<Reference<Statement>>>,
        index: &usize,
        offset: usize,
        tokens: &[Token],
    ) -> Option<(&'a CallStatement, usize)> {
        opt.iter()
            .map(|boxed| boxed.as_ref())
            .find_map(|r| find_call_stmt_in_stmt(r.as_ref(), index, offset + r.offset, tokens))
    }

    use Statement::*;
    match stmt {
        Block(b) => b
            .statements
            .iter()
            .find_map(|r| find_call_stmt_in_stmt(r.as_ref(), index, offset + r.offset, tokens)),
        If(i) => get_in_option(&i.if_branch, index, offset, tokens)
            .or_else(|| get_in_option(&i.else_branch, index, offset, tokens)),
        While(w) => get_in_option(&w.statement, index, offset, tokens),
        Call(c) if c.to_text_range(&tokens[offset..]).contains(index) => Some((c, offset)),
        _ => None,
    }
}

/// Select the active parameter by checking at which token the cursor is.
fn get_active_param(
    params: &[ParameterInformation],
    tokens: &[Token],
    index: &usize,
) -> Option<u32> {
    if params.is_empty() {
        None
    } else {
        let mut param_index = 0;
        for token in tokens {
            if token.range.start >= *index {
                break;
            }
            if matches!(token.token_type, TokenType::Comma) {
                param_index += 1;
            }
        }
        Some(param_index)
    }
}

fn get_param_info(proc_entry: &ProcedureEntry) -> Vec<ParameterInformation> {
    proc_entry
        .parameters
        .iter()
        .map(|param| ParameterInformation {
            label: ParameterLabel::Simple(param.name.to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: {
                    let VariableEntry {
                        name, data_type, ..
                    } = param;
                    let type_info = data_type
                        .as_ref()
                        .map(|dt| dt.to_string())
                        .unwrap_or_else(|| "".to_string());
                    name.value.clone() + ": " + &type_info
                },
            })),
        })
        .collect()
}
