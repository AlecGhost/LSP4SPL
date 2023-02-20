use crate::document::{get_position, DocumentInfo, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{Position, SemanticToken, SemanticTokens, SemanticTokensParams};
use spl_frontend::{
    ast::{GlobalDeclaration, Identifier, TypeExpression},
    lexer::token::{Token, TokenType},
    table::{Entry, LookupTable, SymbolTable, Table},
    ToRange,
};
use tokio::sync::mpsc::Sender;

pub const TOKEN_TYPES: [lsp_types::SemanticTokenType; 7] = [
    lsp_types::SemanticTokenType::COMMENT,
    lsp_types::SemanticTokenType::KEYWORD,
    lsp_types::SemanticTokenType::NUMBER,
    lsp_types::SemanticTokenType::TYPE,
    lsp_types::SemanticTokenType::FUNCTION,
    lsp_types::SemanticTokenType::PARAMETER,
    lsp_types::SemanticTokenType::VARIABLE,
];

pub const TOKEN_MODIFIERS: [lsp_types::SemanticTokenModifier; 1] =
    [lsp_types::SemanticTokenModifier::DECLARATION];

/// Represents index in `TOKEN_TYPES`
#[repr(u32)]
enum SemanticTokenType {
    Comment,
    Keyword,
    Number,
    Type,
    Function,
    Parameter,
    Variable,
}

/// Represents bitwise *and* of indexes in `TOKEN_MODIFIERS`
#[repr(u32)]
enum SemanticTokenModifier {
    None,
    Declaration,
}

impl From<SemanticTokenType> for u32 {
    fn from(value: SemanticTokenType) -> Self {
        value as u32
    }
}

impl From<SemanticTokenModifier> for u32 {
    fn from(value: SemanticTokenModifier) -> Self {
        value as u32
    }
}

pub(crate) async fn semantic_tokens(
    doctx: Sender<DocumentRequest>,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokens>> {
    let uri = params.text_document.uri;
    if let Some(doc_info) = super::get_doc_info(uri, doctx).await? {
        let DocumentInfo {
            ast,
            text,
            table: global_table,
            ..
        } = &doc_info;
        let mut previous_token_pos = Position {
            line: 0,
            character: 0,
        };
        let semantic_tokens: Vec<SemanticToken> = ast
            .global_declarations
            .iter()
            .flat_map(|gd| {
                use GlobalDeclaration::*;
                match gd {
                    Procedure(pd) => {
                        let empty_table = SymbolTable::default();
                        let local_table =
                            super::get_local_table(pd, global_table).unwrap_or(&empty_table);
                        let lookup_table = LookupTable {
                            local_table,
                            global_table,
                        };
                        pd.info
                            .tokens
                            .iter()
                            .filter_map(|token| {
                                let semantic_token = if matches!(&pd.name, Some(name) if name.to_range() == token.range)
                                {
                                    Some(create_semantic_token(
                                        token,
                                        &previous_token_pos,
                                        text,
                                        SemanticTokenType::Function.into(),
                                        SemanticTokenModifier::Declaration.into(),
                                    ))
                                } else if let TokenType::Ident(name) = &token.token_type {
                                    if let Some(ranged_entry) = lookup_table.lookup(name) {
                                        match &ranged_entry.entry {
                                            Entry::Type(_) => Some(create_semantic_token(
                                                token,
                                                &previous_token_pos,
                                                text,
                                                SemanticTokenType::Type.into(),
                                                SemanticTokenModifier::None.into(),
                                            )),
                                            Entry::Procedure(_) => Some(create_semantic_token(
                                                token,
                                                &previous_token_pos,
                                                text,
                                                SemanticTokenType::Function.into(),
                                                SemanticTokenModifier::None.into(),
                                            )),
                                            Entry::Variable(variable) => {
                                                let token_type = if variable.is_param {
                                                    SemanticTokenType::Parameter
                                                } else {
                                                    SemanticTokenType::Variable
                                                };
                                                let modifier = if variable.name.to_range() == token.range {
                                                    SemanticTokenModifier::Declaration
                                                } else {
                                                    SemanticTokenModifier::None
                                                };
                                                Some(create_semantic_token(
                                                    token,
                                                    &previous_token_pos,
                                                    text,
                                                    token_type.into(),
                                                    modifier.into(),
                                                ))
                                            },
                                        }
                                    } else {
                                        None
                                    }
                                } else {
                                    map_token(token, &previous_token_pos, text)
                                };
                                if semantic_token.is_some() {
                                    previous_token_pos = get_position(token.range.start, text);
                                }
                                semantic_token
                            })
                            .collect()
                    }
                    Type(td) => td
                        .info
                        .tokens
                        .iter()
                        .filter_map(|token| {
                            let semantic_token = if matches!(&td.name, Some(name) if name.to_range() == token.range) {
                                Some(create_semantic_token(
                                    token,
                                    &previous_token_pos,
                                    text,
                                    SemanticTokenType::Type.into(),
                                    SemanticTokenModifier::Declaration.into(),
                                ))
                            } else if matches!(token.token_type, TokenType::Ident(_)) {
                                Some(create_semantic_token(
                                    token,
                                    &previous_token_pos,
                                    text,
                                    SemanticTokenType::Type.into(),
                                    SemanticTokenModifier::None.into(),
                                ))
                            } else {
                                map_token(token, &previous_token_pos, text)
                            };
                            if semantic_token.is_some() {
                                previous_token_pos = get_position(token.range.start, text);
                            }
                            semantic_token
                        })
                        .collect::<Vec<SemanticToken>>(),
                    Error(info) => info
                        .tokens
                        .iter()
                        .filter_map(|token| {
                            let semantic_token = map_token(token,&previous_token_pos, text);
                            if semantic_token.is_some() {
                                previous_token_pos = get_position(token.range.start, text);
                            }
                            semantic_token
                        })
                        .collect::<Vec<SemanticToken>>(),
                }
            })
            .collect();
        log::debug!("Semantic Tokens: {:#?}", semantic_tokens);
        Ok(Some(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        }))
    } else {
        Ok(None)
    }
}

fn map_token(token: &Token, previous_token_pos: &Position, text: &str) -> Option<SemanticToken> {
    match &token.token_type {
        TokenType::Comment(_) => Some(create_semantic_token(
            token,
            previous_token_pos,
            text,
            SemanticTokenType::Comment.into(),
            SemanticTokenModifier::None.into(),
        )),
        TokenType::Hex(_) | TokenType::Char(_) | TokenType::Int(_) => Some(create_semantic_token(
            token,
            previous_token_pos,
            text,
            SemanticTokenType::Number.into(),
            SemanticTokenModifier::None.into(),
        )),
        token_type if token_type.is_keyword() => Some(create_semantic_token(
            token,
            previous_token_pos,
            text,
            SemanticTokenType::Keyword.into(),
            SemanticTokenModifier::None.into(),
        )),
        _ => None,
    }
}

/// Token type represents the index in TOKEN_TYPES.
/// Token modifier represents the bitwise AND of the indexes in TOKEN_MODIFIERS.
fn create_semantic_token(
    token: &Token,
    previous_token_pos: &Position,
    text: &str,
    token_type: u32,
    token_modifier: u32,
) -> SemanticToken {
    let Position { line, character } = get_position(token.range.start, text);
    let length = token
        .range
        .len()
        .try_into()
        .expect("Cannot convert range length to u32");
    let delta_line = line - previous_token_pos.line;
    let delta_start = if line != previous_token_pos.line {
        character
    } else {
        character - previous_token_pos.character
    };

    SemanticToken {
        delta_line,
        delta_start,
        length,
        token_type,
        token_modifiers_bitset: token_modifier,
    }
}

fn get_type_ident(type_expr: &TypeExpression) -> Option<&Identifier> {
    use TypeExpression::*;
    match type_expr {
        NamedType(ident) => Some(ident),
        ArrayType { base_type, .. } => base_type
            .as_ref()
            .and_then(|type_expr| get_type_ident(type_expr)),
    }
}
