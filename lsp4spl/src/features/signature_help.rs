use crate::document::DocumentRequest;
use color_eyre::eyre::Result;
use lsp_types::{
    Documentation, MarkupContent, MarkupKind, ParameterInformation, ParameterLabel, SignatureHelp,
    SignatureHelpParams, SignatureInformation,
};
use spl_frontend::{
    ast::{GlobalDeclaration, Statement},
    lexer::token::TokenType,
    table::{GlobalEntry, SymbolTable},
    ToRange,
};
use tokio::sync::mpsc::Sender;

pub async fn signature_help(
    doctx: Sender<DocumentRequest>,
    params: SignatureHelpParams,
) -> Result<Option<SignatureHelp>> {
    let doc_params = params.text_document_position_params;
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        if let Some(call_stmt) = cursor
            .doc_info
            .ast
            .global_declarations
            .iter()
            .filter_map(|gd| {
                if let GlobalDeclaration::Procedure(pd) = gd {
                    Some(pd)
                } else {
                    None
                }
            })
            .find_map(|pd| {
                pd.statements
                    .iter()
                    .filter_map(|stmt| {
                        if let Statement::Call(call_stmt) = stmt {
                            Some(call_stmt)
                        } else {
                            None
                        }
                    })
                    .find(|call_stmt| call_stmt.to_range().contains(&cursor.index))
            })
        {
            if let Some(GlobalEntry::Procedure(proc_entry)) =
                &cursor.doc_info.table.lookup(&call_stmt.name.value)
            {
                let parameters: Vec<ParameterInformation> = proc_entry
                    .parameters
                    .iter()
                    .map(|param| ParameterInformation {
                        label: ParameterLabel::Simple(param.name.to_string()),
                        documentation: None,
                    })
                    .collect();
                let active_parameter = if parameters.is_empty() {
                    None
                } else {
                    let mut param_index = 0;
                    for token in &call_stmt.info.tokens {
                        if token.range.start >= cursor.index {
                            break;
                        }
                        if matches!(token.token_type, TokenType::Comma) {
                            param_index += 1;
                        }
                    }
                    Some(param_index)
                };
                let documentation = proc_entry.doc.as_ref().map(|doc| {
                    Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: doc.clone(),
                    })
                });
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
