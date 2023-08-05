use crate::document::{as_position, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{Position, SemanticToken, SemanticTokens, SemanticTokensParams};
use spl_frontend::{
    ast::{AstInfo, GlobalDeclaration, ProcedureDeclaration, TypeDeclaration},
    table::{Entry, GlobalTable, LookupTable},
    tokens::{Token, TokenType},
    AnalyzedSource, ToRange,
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
        value as Self
    }
}

impl From<SemanticTokenModifier> for u32 {
    fn from(value: SemanticTokenModifier) -> Self {
        value as Self
    }
}

pub async fn semantic_tokens(
    doctx: Sender<DocumentRequest>,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokens>> {
    let uri = params.text_document.uri;
    if let Some(AnalyzedSource {
        ast,
        text,
        table: global_table,
        tokens,
        ..
    }) = super::get_doc(uri, doctx).await?
    {
        let mut previous_token_pos = Position {
            line: 0,
            character: 0,
        };
        let semantic_tokens: Vec<SemanticToken> = ast
            .global_declarations
            .iter()
            .flat_map(|gd| {
                use GlobalDeclaration::*;
                let tokens = &tokens[gd.offset..];
                match gd.as_ref() {
                    Procedure(pd) => {
                        collect_proc_dec(pd, &global_table, &text, tokens, &mut previous_token_pos)
                    }
                    Type(td) => collect_type_dec(td, &text, tokens, &mut previous_token_pos),
                    Error(info) => collect_error(info, &text, tokens, &mut previous_token_pos),
                }
            })
            .collect();
        Ok(Some(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        }))
    } else {
        Ok(None)
    }
}

fn collect_type_dec(
    td: &TypeDeclaration,
    text: &str,
    tokens: &[Token],
    previous_token_pos: &mut Position,
) -> Vec<SemanticToken> {
    td.info
        .slice(tokens)
        .iter()
        .filter_map(|token| {
            let semantic_token = if matches!(&td.name, Some(name) if name.to_range() == token.range)
            {
                Some(create_semantic_token(
                    token,
                    *previous_token_pos,
                    text,
                    SemanticTokenType::Type.into(),
                    SemanticTokenModifier::Declaration.into(),
                ))
            } else if matches!(token.token_type, TokenType::Ident(_)) {
                Some(create_semantic_token(
                    token,
                    *previous_token_pos,
                    text,
                    SemanticTokenType::Type.into(),
                    SemanticTokenModifier::None.into(),
                ))
            } else {
                map_token(token, *previous_token_pos, text)
            };
            if semantic_token.is_some() {
                *previous_token_pos = as_position(token.range.start, text);
            }
            semantic_token
        })
        .collect::<Vec<SemanticToken>>()
}

fn collect_proc_dec(
    pd: &ProcedureDeclaration,
    global_table: &GlobalTable,
    text: &str,
    tokens: &[Token],
    previous_token_pos: &mut Position,
) -> Vec<SemanticToken> {
    let lookup_table = LookupTable {
        local_table: super::get_local_table(pd, global_table),
        global_table: Some(global_table),
    };
    pd.info
        .slice(tokens)
        .iter()
        .filter_map(|token| {
            let semantic_token = if matches!(&pd.name, Some(name) if name.to_range() == token.range)
            {
                Some(create_semantic_token(
                    token,
                    *previous_token_pos,
                    text,
                    SemanticTokenType::Function.into(),
                    SemanticTokenModifier::Declaration.into(),
                ))
            } else if let TokenType::Ident(name) = &token.token_type {
                lookup_table.lookup(name).map(|entry| match &entry {
                    Entry::Type(_) => create_semantic_token(
                        token,
                        *previous_token_pos,
                        text,
                        SemanticTokenType::Type.into(),
                        SemanticTokenModifier::None.into(),
                    ),
                    Entry::Procedure(_) => create_semantic_token(
                        token,
                        *previous_token_pos,
                        text,
                        SemanticTokenType::Function.into(),
                        SemanticTokenModifier::None.into(),
                    ),
                    Entry::Variable(variable) => {
                        let modifier = if variable.name.to_range() == token.range {
                            SemanticTokenModifier::Declaration
                        } else {
                            SemanticTokenModifier::None
                        };
                        create_semantic_token(
                            token,
                            *previous_token_pos,
                            text,
                            SemanticTokenType::Variable.into(),
                            modifier.into(),
                        )
                    }
                    Entry::Parameter(param) => {
                        let modifier = if param.name.to_range() == token.range {
                            SemanticTokenModifier::Declaration
                        } else {
                            SemanticTokenModifier::None
                        };
                        create_semantic_token(
                            token,
                            *previous_token_pos,
                            text,
                            SemanticTokenType::Parameter.into(),
                            modifier.into(),
                        )
                    }
                })
            } else {
                map_token(token, *previous_token_pos, text)
            };
            if semantic_token.is_some() {
                *previous_token_pos = as_position(token.range.start, text);
            }
            semantic_token
        })
        .collect()
}

fn collect_error(
    info: &AstInfo,
    text: &str,
    tokens: &[Token],
    previous_token_pos: &mut Position,
) -> Vec<SemanticToken> {
    info.slice(tokens)
        .iter()
        .filter_map(|token| {
            let semantic_token = map_token(token, *previous_token_pos, text);
            if semantic_token.is_some() {
                *previous_token_pos = as_position(token.range.start, text);
            }
            semantic_token
        })
        .collect::<Vec<SemanticToken>>()
}

fn map_token(token: &Token, previous_token_pos: Position, text: &str) -> Option<SemanticToken> {
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

/// Token type represents the index in `TOKEN_TYPES`.
/// Token modifier represents the bitwise AND of the indexes in `TOKEN_MODIFIERS`.
fn create_semantic_token(
    token: &Token,
    previous_token_pos: Position,
    text: &str,
    token_type: u32,
    token_modifier: u32,
) -> SemanticToken {
    let Position { line, character } = as_position(token.range.start, text);
    let length = token
        .range
        .len()
        .try_into()
        .expect("Cannot convert range length to u32");
    let delta_line = line - previous_token_pos.line;
    let delta_start = if line == previous_token_pos.line {
        character - previous_token_pos.character
    } else {
        character
    };

    SemanticToken {
        delta_line,
        delta_start,
        length,
        token_type,
        token_modifiers_bitset: token_modifier,
    }
}
