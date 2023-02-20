use crate::document::DocumentRequest;
use color_eyre::eyre::Result;
use lsp_types::{CompletionItem, CompletionItemKind, CompletionParams, Documentation, MarkupKind};
use spl_frontend::{
    ast::{GlobalDeclaration, Statement, TypeDeclaration},
    lexer::token::{Token, TokenList, TokenType},
    table::{Entry, RangedEntry, SymbolTable, Table},
    ToRange,
};
use tokio::sync::mpsc::Sender;

pub(crate) async fn propose(
    doctx: Sender<DocumentRequest>,
    params: CompletionParams,
) -> Result<Option<Vec<CompletionItem>>> {
    let doc_params = params.text_document_position;
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        let table = &cursor.doc_info.table;
        let program = &cursor.doc_info.ast;
        let position = correct_index(cursor.index);

        if let Some(gd) = program
            .global_declarations
            .iter()
            .find(|gd| gd.to_range().contains(&position))
        {
            use GlobalDeclaration::*;
            match gd {
                Type(td) => Ok(complete_type(td, position, table)),
                Procedure(pd) => {
                    if let Some(last_token) = pd.info.tokens.token_before(position) {
                        let in_signature = if let Some(procedure_end_token) =
                            pd.info.tokens.iter().find(|token| {
                                matches!(token.token_type, TokenType::RParen | TokenType::LCurly)
                            }) {
                            position < procedure_end_token.range.start
                        } else {
                            true
                        };

                        if in_signature {
                            let completions = match last_token.token_type {
                                TokenType::LParen | TokenType::Comma => Some(vec![items::r#ref()]),
                                TokenType::Colon => Some(search_types(table)),
                                _ => None,
                            };
                            Ok(completions)
                        } else {
                            // handle procedure body
                            let empty_table = SymbolTable::default();
                            let local_table =
                                super::get_local_table(pd, table).unwrap_or(&empty_table);

                            let in_statements = if let Some(first_stmt) =
                                &pd.statements.iter().find(|stmt| {
                                    // prevent that error or empty statements disturb the calculation of
                                    // the first real statement
                                    !matches!(stmt, Statement::Error(_) | Statement::Empty(_))
                                }) {
                                position >= first_stmt.to_range().start
                            } else {
                                false
                            };

                            if in_statements {
                                Ok(complete_statements(
                                    &pd.statements,
                                    position,
                                    last_token,
                                    local_table,
                                    table,
                                ))
                            } else {
                                // in variable declarations
                                let completions = match last_token.token_type {
                                    TokenType::Colon => Some(search_types(table)),
                                    TokenType::Semic | TokenType::LCurly => {
                                        let completions =
                                            vec![vec![items::var()], new_stmt(local_table, table)]
                                                .concat();
                                        Some(completions)
                                    }
                                    _ => None,
                                };
                                Ok(completions)
                            }
                        }
                    } else {
                        Ok(None)
                    }
                }
                _ => Ok(None),
            }
        } else {
            // fixes, that a cursor at the end of an unfinished type declaration at the end of the
            // file is still recognized as part of the declaration
            if let Some(GlobalDeclaration::Type(td)) = program.global_declarations.last() {
                if let Some(last_token) = td.info.tokens.last() {
                    if !matches!(last_token.token_type, TokenType::Semic) {
                        return Ok(complete_type(td, position, table));
                    }
                }
            }
            // The cursor is not inside the scope of any global declaration,
            // which means, that a new one can be startet with the `proc` or `type` keywords.
            // Furthermore, some snippets are provided for those definitions
            let mut completions = vec![
                snippets::proc(),
                snippets::r#type(),
                items::proc(),
                items::r#type(),
            ];
            if let Some(RangedEntry {
                entry: Entry::Procedure(_),
                ..
            }) = cursor.doc_info.table.lookup("main")
            {
                // main already exists
            } else {
                completions.push(snippets::main());
            }
            Ok(Some(completions))
        }
    } else {
        Ok(None)
    }
}

fn complete_type(
    td: &TypeDeclaration,
    position: usize,
    table: &SymbolTable,
) -> Option<Vec<CompletionItem>> {
    if let Some(last_token) = td.info.tokens.token_before(position) {
        use TokenType::*;
        match last_token.token_type {
            Eq => Some(vec![snippets::array(), items::array(), items::int()]),
            RBracket => Some(vec![items::of()]),
            Of => {
                let completions =
                    vec![vec![snippets::array(), items::array()], search_types(table)].concat();
                Some(completions)
            }
            _ => None,
        }
    } else {
        None
    }
}

fn complete_statements(
    stmts: &[Statement],
    position: usize,
    last_token: &Token,
    local_table: &SymbolTable,
    global_table: &SymbolTable,
) -> Option<Vec<CompletionItem>> {
    let mut last_stmt_is_if = false;
    if let Some(stmt) = stmts.iter().find(|stmt| {
        if stmt.to_range().contains(&position) {
            true
        } else {
            last_stmt_is_if = matches!(stmt, Statement::If(_));
            false
        }
    }) {
        complete_statement(
            stmt,
            position,
            last_token,
            last_stmt_is_if,
            local_table,
            global_table,
        )
    } else {
        Some(new_stmt(local_table, global_table))
    }
}

fn complete_statement(
    stmt: &Statement,
    position: usize,
    last_token: &Token,
    last_stmt_is_if: bool,
    local_table: &SymbolTable,
    global_table: &SymbolTable,
) -> Option<Vec<CompletionItem>> {
    // provide completion support for `else` after if statement
    if last_stmt_is_if && matches!(last_token.token_type, TokenType::RCurly) {
        let completions = vec![vec![items::r#else()], new_stmt(local_table, global_table)].concat();
        return Some(completions);
    }

    match stmt {
        Statement::Assignment(a) => {
            let assign_token = a
                .info
                .tokens
                .iter()
                .find(|token| matches!(token.token_type, TokenType::Assign))
                .expect("Cannot be recognized as an assignment without assign token.");
            // before assign is only an arbitrary identifier
            if position >= assign_token.range.end {
                return Some(search_variables(local_table));
            }
        }
        Statement::Block(b) => {
            return complete_statements(
                &b.statements,
                position,
                last_token,
                local_table,
                global_table,
            );
        }
        Statement::Call(c) => {
            let lparen_token = c
                .info
                .tokens
                .iter()
                .find(|token| matches!(token.token_type, TokenType::LParen))
                .expect("Cannot be recognized as a call statement without lparen token.");
            // procedure name completion is already provided by the
            // context based completions
            if position >= lparen_token.range.end {
                return Some(search_variables(local_table));
            }
        }
        Statement::If(i) => {
            if let Some(if_branch) = &i.if_branch {
                if if_branch.to_range().contains(&position) {
                    return complete_statement(
                        if_branch,
                        position,
                        last_token,
                        false,
                        local_table,
                        global_table,
                    );
                }
            }
            if let Some(else_branch) = &i.else_branch {
                if else_branch.to_range().contains(&position) {
                    return complete_statement(
                        else_branch,
                        position,
                        last_token,
                        false,
                        local_table,
                        global_table,
                    );
                }
            }
            if let Some(lparen_token) = &i
                .info
                .tokens
                .iter()
                .find(|token| matches!(token.token_type, TokenType::LParen))
            {
                if position >= lparen_token.range.end {
                    return Some(search_variables(local_table));
                }
            }
        }
        Statement::While(w) => {
            if let Some(if_branch) = &w.statement {
                if if_branch.to_range().contains(&position) {
                    return complete_statement(
                        if_branch,
                        position,
                        last_token,
                        false,
                        local_table,
                        global_table,
                    );
                }
            }
            if let Some(lparen_token) = &w
                .info
                .tokens
                .iter()
                .find(|token| matches!(token.token_type, TokenType::LParen))
            {
                if position >= lparen_token.range.end {
                    return Some(search_variables(local_table));
                }
            }
        }
        Statement::Error(_) | Statement::Empty(_) => {
            return Some(new_stmt(local_table, global_table))
        }
    }
    None
}

/// If the cursor is after a token, it should count as if it was on it.
fn correct_index(index: usize) -> usize {
    if index > 0 {
        index - 1
    } else {
        0
    }
}

fn search_types(table: &SymbolTable) -> Vec<CompletionItem> {
    table
        .entries
        .iter()
        .filter(|(_, ranged_entry)| matches!(ranged_entry.entry, Entry::Type(_)))
        .map(|(ident, ranged_entry)| CompletionItem {
            label: ident.clone(),
            kind: Some(CompletionItemKind::STRUCT),
            detail: Some(ranged_entry.entry.to_string()),
            documentation: ranged_entry.entry.documentation().map(|docu| {
                Documentation::MarkupContent(lsp_types::MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: docu.trim_start().to_string(),
                })
            }),
            ..Default::default()
        })
        .collect()
}

fn search_variables(table: &SymbolTable) -> Vec<CompletionItem> {
    table
        .entries
        .iter()
        .filter(|(_, ranged_entry)| matches!(ranged_entry.entry, Entry::Variable(_)))
        .map(|(ident, ranged_entry)| CompletionItem {
            label: ident.clone(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some(ranged_entry.entry.to_string()),
            documentation: ranged_entry.entry.documentation().map(|docu| {
                Documentation::MarkupContent(lsp_types::MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: docu.trim_start().to_string(),
                })
            }),
            ..Default::default()
        })
        .collect()
}

fn search_procedures(table: &SymbolTable) -> Vec<CompletionItem> {
    table
        .entries
        .iter()
        .filter(|(_, ranged_entry)| matches!(ranged_entry.entry, Entry::Procedure(_)))
        .map(|(ident, ranged_entry)| CompletionItem {
            label: ident.clone(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(ranged_entry.entry.to_string()),
            documentation: ranged_entry.entry.documentation().map(|docu| {
                Documentation::MarkupContent(lsp_types::MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: docu.trim_start().to_string(),
                })
            }),
            ..Default::default()
        })
        .collect()
}

fn new_stmt(local_table: &SymbolTable, global_table: &SymbolTable) -> Vec<CompletionItem> {
    vec![
        vec![items::r#if(), items::r#while()],
        search_variables(local_table),
        search_procedures(global_table),
    ]
    .concat()
}

mod items {
    use lsp_types::{CompletionItem, CompletionItemKind};
    use spl_frontend::lexer::token;

    macro_rules! item {
        ($name:ident, $label:expr) => {
            pub(super) fn $name() -> CompletionItem {
                CompletionItem {
                    label: $label.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                }
            }
        };
    }

    item!(array, token::ARRAY);
    item!(of, token::OF);
    item!(r#if, token::IF);
    item!(r#else, token::ELSE);
    item!(r#while, token::WHILE);
    item!(r#type, token::TYPE);
    item!(proc, token::PROC);
    item!(var, token::VAR);
    item!(r#ref, token::REF);

    /// int is a type, not a keyword
    pub(super) fn int() -> CompletionItem {
        CompletionItem {
            label: "int".to_string(),
            kind: Some(CompletionItemKind::STRUCT),
            ..Default::default()
        }
    }
}

mod snippets {
    use lsp_types::{CompletionItem, CompletionItemKind, InsertTextFormat};

    macro_rules! snippet {
        ($name:ident, $label:expr, $text:expr) => {
            pub(super) fn $name() -> CompletionItem {
                CompletionItem {
                    label: $label.to_string(),
                    kind: Some(CompletionItemKind::SNIPPET),
                    insert_text: Some($text.to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                }
            }
        };
        ($name:ident, $text:expr) => {
            snippet!($name, stringify!($name), $text);
        };
    }

    snippet!(main, "proc main() {\n    $0\n}");
    snippet!(array, "array [$1] of $0");
    snippet!(proc, "proc $1($2) {\n    $0\n}");
    snippet!(r#type, "type", "type $1 = $0;");
}
