use crate::document::DocumentRequest;
use color_eyre::eyre::Result;
use lsp_types::{CompletionItem, CompletionItemKind, CompletionParams, Documentation, MarkupKind};
use spl_frontend::{
    ast::{GlobalDeclaration, ProcedureDeclaration, Statement, TypeDeclaration},
    table::{
        GlobalEntry, GlobalTable, LocalEntry, LocalTable, LookupTable, SymbolTable, TableEntry,
    },
    token::{Token, TokenList, TokenType},
    ToRange,
};
use tokio::sync::mpsc::Sender;

pub async fn propose(
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
                Procedure(pd) => Ok(complete_procedure(pd, position, table)),
                Error(_) => Ok(Some(new_global_declaration(table))),
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
            Ok(Some(new_global_declaration(table)))
        }
    } else {
        Ok(None)
    }
}

fn complete_type(
    td: &TypeDeclaration,
    position: usize,
    table: &GlobalTable,
) -> Option<Vec<CompletionItem>> {
    td.info
        .tokens
        .token_before(position)
        .and_then(|last_token| {
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
        })
}

fn complete_procedure(
    pd: &ProcedureDeclaration,
    position: usize,
    table: &GlobalTable,
) -> Option<Vec<CompletionItem>> {
    pd.info
        .tokens
        .token_before(position)
        .and_then(|last_token| {
            let in_signature = pd
                .info
                .tokens
                .iter()
                .find(|token| matches!(token.token_type, TokenType::RParen | TokenType::LCurly))
                .map_or(true, |procedure_end_token| {
                    position < procedure_end_token.range.start
                });

            if in_signature {
                match last_token.token_type {
                    TokenType::LParen | TokenType::Comma => Some(vec![items::r#ref()]),
                    TokenType::Colon => Some(search_types(table)),
                    _ => None,
                }
            } else {
                // handle procedure body
                let lookup_table = LookupTable {
                    local_table: super::get_local_table(pd, table),
                    global_table: Some(table),
                };
                let in_statements = pd
                    .statements
                    .iter()
                    .find(|stmt| {
                        // prevent that error or empty statements disturb the calculation of
                        // the first real statement
                        !matches!(stmt, Statement::Error(_) | Statement::Empty(_))
                    })
                    .map_or(false, |first_stmt| position >= first_stmt.to_range().start);

                if in_statements {
                    complete_statements(&pd.statements, position, last_token, &lookup_table)
                } else {
                    // in variable declarations
                    match last_token.token_type {
                        TokenType::Colon => Some(search_types(table)),
                        TokenType::Semic | TokenType::LCurly => {
                            let completions =
                                vec![vec![snippets::var(), items::var()], new_stmt(&lookup_table)]
                                    .concat();
                            Some(completions)
                        }
                        _ => None,
                    }
                }
            }
        })
}

fn complete_statements(
    stmts: &[Statement],
    position: usize,
    last_token: &Token,
    lookup_table: &LookupTable,
) -> Option<Vec<CompletionItem>> {
    let mut last_stmt_is_if = false;
    stmts
        .iter()
        .find(|stmt| {
            if stmt.to_range().contains(&position) {
                true
            } else {
                last_stmt_is_if = matches!(stmt, Statement::If(_));
                false
            }
        })
        .map_or_else(
            || Some(new_stmt(lookup_table)),
            |stmt| complete_statement(stmt, position, last_token, last_stmt_is_if, lookup_table),
        )
}

macro_rules! complete_branch {
    ($branch:expr, $position:expr, $last_token:expr, $lookup_table:expr) => {
        if let Some(stmt) = $branch {
            if stmt.to_range().contains(&$position) {
                return complete_statement(stmt, $position, $last_token, false, $lookup_table);
            }
        }
    };
}

fn complete_statement(
    stmt: &Statement,
    position: usize,
    last_token: &Token,
    last_stmt_is_if: bool,
    lookup_table: &LookupTable,
) -> Option<Vec<CompletionItem>> {
    // provide completion support for `else` after if statement
    if last_stmt_is_if && matches!(last_token.token_type, TokenType::RCurly) {
        let completions = vec![
            vec![snippets::r#else(), items::r#else()],
            new_stmt(lookup_table),
        ]
        .concat();
        return Some(completions);
    }

    match stmt {
        Statement::Assignment(a) => {
            // before assign is only an arbitrary identifier
            complete_vars(&a.info.tokens, position, lookup_table, TokenType::Assign)
        }
        Statement::Block(b) => {
            complete_statements(&b.statements, position, last_token, lookup_table)
        }
        Statement::Call(c) => {
            // procedure name completion is already provided by the
            // context based completions
            complete_vars(&c.info.tokens, position, lookup_table, TokenType::LParen)
        }
        Statement::If(i) => {
            complete_branch!(&i.if_branch, position, last_token, lookup_table);
            complete_branch!(&i.else_branch, position, last_token, lookup_table);
            complete_vars(&i.info.tokens, position, lookup_table, TokenType::LParen)
        }
        Statement::While(w) => {
            complete_branch!(&w.statement, position, last_token, lookup_table);
            complete_vars(&w.info.tokens, position, lookup_table, TokenType::LParen)
        }
        Statement::Error(_) | Statement::Empty(_) => Some(new_stmt(lookup_table)),
    }
}

fn complete_vars(
    tokens: &[Token],
    position: usize,
    lookup_table: &LookupTable,
    start_token_type: TokenType,
) -> Option<Vec<CompletionItem>> {
    if let Some(lparen_token) = tokens
        .iter()
        .find(|token| token.token_type == start_token_type)
    {
        if position >= lparen_token.range.end {
            if let Some(local_table) = lookup_table.local_table {
                return Some(search_variables(local_table));
            }
        }
    }
    None
}

/// If the cursor is after a token, it should count as if it was on it.
const fn correct_index(index: usize) -> usize {
    if index > 0 {
        index - 1
    } else {
        0
    }
}

macro_rules! search_and_create_items {
    ($table:expr, $t:pat, $kind:expr) => {
        $table
            .entries
            .iter()
            .filter(|(_, entry)| matches!(entry, $t))
            .map(|(ident, entry)| CompletionItem {
                label: ident.clone(),
                kind: Some($kind),
                detail: Some(entry.to_string()),
                documentation: entry.doc().map(|docu| {
                    Documentation::MarkupContent(lsp_types::MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: docu.trim_start().to_string(),
                    })
                }),
                ..Default::default()
            })
            .collect()
    };
}

fn search_types(table: &GlobalTable) -> Vec<CompletionItem> {
    search_and_create_items!(table, GlobalEntry::Type(_), CompletionItemKind::STRUCT)
}

fn search_variables(table: &LocalTable) -> Vec<CompletionItem> {
    search_and_create_items!(
        table,
        LocalEntry::Variable(_) | LocalEntry::Parameter(_),
        CompletionItemKind::VARIABLE
    )
}

fn search_procedures(table: &GlobalTable) -> Vec<CompletionItem> {
    search_and_create_items!(
        table,
        GlobalEntry::Procedure(_),
        CompletionItemKind::FUNCTION
    )
}

fn new_stmt(lookup_table: &LookupTable) -> Vec<CompletionItem> {
    vec![
        vec![
            snippets::r#if(),
            snippets::r#while(),
            items::r#if(),
            items::r#while(),
        ],
        lookup_table
            .local_table
            .map_or_else(Vec::new, search_variables),
        lookup_table
            .global_table
            .map_or_else(Vec::new, search_procedures),
    ]
    .concat()
}

fn new_global_declaration(global_table: &GlobalTable) -> Vec<CompletionItem> {
    let mut completions = vec![
        snippets::proc(),
        snippets::r#type(),
        items::proc(),
        items::r#type(),
    ];
    if let Some(GlobalEntry::Procedure(_)) = global_table.lookup("main") {
        // main already exists
    } else {
        completions.push(snippets::main());
    }
    completions
}

mod items {
    use lsp_types::{CompletionItem, CompletionItemKind};
    use spl_frontend::token;

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
    snippet!(var, "var $1: $0;");
    snippet!(r#type, "type", "type $1 = $0;");
    snippet!(r#if, "while", "while ($1) {\n    $0\n}");
    snippet!(r#while, "if", "if ($1) {\n    $0\n}");
    snippet!(r#else, "else", "else {\n    $0\n}");
}
