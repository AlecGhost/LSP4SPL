use crate::document::DocumentRequest;
use color_eyre::eyre::Result;
use lsp_types::{
    Documentation, MarkupContent, MarkupKind, ParameterInformation, ParameterLabel, SignatureHelp,
    SignatureHelpParams, SignatureInformation,
};
use spl_frontend::{
    ast::{CallStatement, GlobalDeclaration, ProcedureDeclaration, Reference, Statement},
    table::{GlobalEntry, SymbolTable},
    tokens::{Token, TokenType},
    ToRange,
};
use tokio::sync::mpsc::Sender;

pub async fn signature_help(
    doctx: Sender<DocumentRequest>,
    params: SignatureHelpParams,
) -> Result<Option<SignatureHelp>> {
    let doc_params = params.text_document_position_params;
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        if let Some((call_stmt, offset)) = cursor
            .doc
            .ast
            .global_declarations
            .iter()
            .filter_map(|gd| {
                if let GlobalDeclaration::Procedure(pd) = gd.as_ref() {
                    Some((pd, gd.offset))
                } else {
                    None
                }
            })
            .find_map(|(pd, offset)| {
                find_call_stmt(pd, &cursor.index).map(|call_stmt| (call_stmt, offset))
            })
        {
            if let Some(GlobalEntry::Procedure(proc_entry)) =
                &cursor.doc.table.lookup(&call_stmt.name.value)
            {
                let parameters: Vec<ParameterInformation> = proc_entry
                    .parameters
                    .iter()
                    .map(|param| ParameterInformation {
                        label: ParameterLabel::Simple(param.name.to_string()),
                        documentation: None,
                    })
                    .collect();
                let documentation = proc_entry.doc.as_ref().map(|doc| {
                    Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: doc.clone(),
                    })
                });
                let tokens = &cursor.doc.tokens[offset..];
                let active_parameter =
                    get_active_param(call_stmt.info.slice(tokens), &parameters, &cursor.index);
                let help = SignatureInformation {
                    label: proc_entry.name.to_string(),
                    documentation,
                    parameters: Some(parameters),
                    active_parameter,
                };
                return Ok(Some(SignatureHelp {
                    signatures: vec![help],
                    active_signature: Some(0),
                    active_parameter,
                }));
            }
        }
    };
    Ok(None)
}

fn find_call_stmt(pd: &ProcedureDeclaration, index: &usize) -> Option<CallStatement> {
    pd.statements
        .iter()
        .map(|r| r.as_ref())
        .flat_map(get_call_stmts)
        .find(|call_stmt| call_stmt.to_range().contains(index))
        .cloned()
}

fn get_call_stmts(stmt: &Statement) -> Vec<&CallStatement> {
    fn get_in_option(opt: &Option<Box<Reference<Statement>>>) -> Vec<&CallStatement> {
        opt.iter()
            .map(|boxed| boxed.as_ref())
            .map(|r| r.as_ref())
            .flat_map(get_call_stmts)
            .collect()
    }

    use Statement::*;
    match stmt {
        Block(b) => b
            .statements
            .iter()
            .map(|r| r.as_ref())
            .flat_map(get_call_stmts)
            .collect(),
        Call(c) => vec![c],
        If(i) => vec![get_in_option(&i.if_branch), get_in_option(&i.else_branch)].concat(),
        While(w) => get_in_option(&w.statement),
        _ => Vec::new(),
    }
}

fn get_active_param(
    tokens: &[Token],
    params: &[ParameterInformation],
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
