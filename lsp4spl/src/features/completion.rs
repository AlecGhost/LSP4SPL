use crate::document::DocumentRequest;
use color_eyre::eyre::Result;
use lsp_types::{CompletionItem, CompletionItemKind, CompletionParams};
use spl_frontend::{
    ast::{GlobalDeclaration, Statement},
    table::{Entry, SymbolTable},
    ToRange,
};
use tokio::sync::mpsc::Sender;

pub(crate) async fn completion(
    doctx: Sender<DocumentRequest>,
    params: CompletionParams,
) -> Result<Option<Vec<CompletionItem>>> {
    let doc_params = params.text_document_position;
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        if let Some(ranged_entry) = &cursor.context {
            let program = cursor.doc_info.ast;
            let index = cursor.index;
            if let Some(gd) = program
                .global_declarations
                .iter()
                .find(|gd| gd.to_range().contains(&index))
            {
                match gd {
                    GlobalDeclaration::Type(type_dec) => {
                        let mut completions = vec![
                            CompletionItem {
                                label: "array".to_string(),
                                kind: Some(CompletionItemKind::KEYWORD),
                                ..Default::default()
                            },
                            CompletionItem {
                                label: "of".to_string(),
                                kind: Some(CompletionItemKind::KEYWORD),
                                ..Default::default()
                            },
                        ];
                        search_types(&cursor.doc_info.table)
                            .into_iter()
                            .filter(|item| {
                                if let Some(name) = &type_dec.name {
                                    item.label != name.value
                                } else {
                                    true
                                }
                            })
                            .for_each(|item| completions.push(item));
                        return Ok(Some(completions));
                    }
                    GlobalDeclaration::Procedure(p) => {
                        if let Entry::Procedure(entry) = &ranged_entry.entry {
                            let local_table = &entry.local_table;
                            let global_table = &cursor.doc_info.table;
                            let body_start = if let Some(vd) = p.variable_declarations.first() {
                                vd.range.start
                            } else if let Some(stmt) = p.statements.first() {
                                stmt.to_range().start
                            } else {
                                p.range.end
                            };
                            if index < body_start {
                                if let Some(param) = p
                                    .parameters
                                    .iter()
                                    .find(|param| param.range.contains(&index))
                                {
                                    if param.type_expr.is_none() {
                                        let completions = search_types(global_table);
                                        return Ok(Some(completions));
                                    }
                                }
                            } else if let Some(vd) = p
                                .variable_declarations
                                .iter()
                                .find(|vd| vd.range.contains(&index))
                            {
                                if vd.type_expr.is_none() {
                                    let completions = search_types(global_table);
                                    return Ok(Some(completions));
                                }
                                // name is a new identifier, so no completion possible
                            } else if p.statements.is_empty() {
                                return Ok(Some(vec![CompletionItem {
                                    label: "var".to_string(),
                                    kind: Some(CompletionItemKind::KEYWORD),
                                    ..Default::default()
                                }]));
                            } else if let Some(stmt) = p
                                .statements
                                .iter()
                                .find(|stmt| stmt.to_range().contains(&index))
                            {
                                fn analyze_statement(
                                    stmt: &Statement,
                                    index: usize,
                                    local_table: &SymbolTable,
                                    global_table: &SymbolTable,
                                ) -> Option<Vec<CompletionItem>> {
                                    use Statement::*;
                                    match stmt {
                                        Assignment(_) => {
                                            let completions = search_variables(local_table);
                                            Some(completions)
                                        }
                                        Block(b) => {
                                            if let Some(stmt) = b
                                                .statements
                                                .iter()
                                                .find(|stmt| stmt.to_range().contains(&index))
                                            {
                                                analyze_statement(
                                                    stmt,
                                                    index,
                                                    local_table,
                                                    global_table,
                                                )
                                            } else {
                                                new_stmt(local_table, global_table)
                                            }
                                        }
                                        Call(_) => {
                                            let completions = search_variables(local_table);
                                            Some(completions)
                                        }
                                        If(i) => {
                                            let body_start = if let Some(stmt) = &i.if_branch {
                                                stmt.to_range().start
                                            } else if let Some(range) = &i.else_kw {
                                                range.start
                                            } else {
                                                i.range.end
                                            };
                                            if index < body_start {
                                                let completions = search_variables(local_table);
                                                Some(completions)
                                            } else {
                                                match (&i.if_branch, &i.else_branch) {
                                                    (Some(if_branch), Some(else_branch)) => {
                                                        if if_branch.to_range().contains(&index) {
                                                            analyze_statement(
                                                                if_branch,
                                                                index,
                                                                local_table,
                                                                global_table,
                                                            )
                                                        } else if else_branch
                                                            .to_range()
                                                            .contains(&index)
                                                        {
                                                            analyze_statement(
                                                                else_branch,
                                                                index,
                                                                local_table,
                                                                global_table,
                                                            )
                                                        } else {
                                                            // cursor is at no useful place
                                                            None
                                                        }
                                                    }
                                                    (Some(if_branch), None) => {
                                                        if if_branch.to_range().contains(&index) {
                                                            analyze_statement(
                                                                if_branch,
                                                                index,
                                                                local_table,
                                                                global_table,
                                                            )
                                                        } else {
                                                            Some(vec![CompletionItem {
                                                                label: "else".to_string(),
                                                                kind: Some(
                                                                    CompletionItemKind::KEYWORD,
                                                                ),
                                                                ..Default::default()
                                                            }])
                                                        }
                                                    }
                                                    (None, Some(else_branch)) => {
                                                        if else_branch.to_range().contains(&index) {
                                                            analyze_statement(
                                                                else_branch,
                                                                index,
                                                                local_table,
                                                                global_table,
                                                            )
                                                        } else {
                                                            new_stmt(local_table, global_table)
                                                        }
                                                    }
                                                    (None, None) => {
                                                        // no block needed
                                                        new_stmt(local_table, global_table)
                                                    }
                                                }
                                            }
                                        }
                                        While(w) => {
                                            let body_start = if let Some(stmt) = &w.statement {
                                                stmt.to_range().start
                                            } else {
                                                w.range.end
                                            };
                                            if index < body_start {
                                                let completions = search_variables(local_table);
                                                Some(completions)
                                            } else {
                                                analyze_statement(
                                                    stmt,
                                                    index,
                                                    local_table,
                                                    global_table,
                                                )
                                            }
                                        }
                                        _ => None,
                                    }
                                }

                                if let Some(completions) =
                                    analyze_statement(stmt, index, local_table, global_table)
                                {
                                    return Ok(Some(completions));
                                }
                            } else {
                                return Ok(new_stmt(local_table, global_table));
                            }
                        }
                    }
                    GlobalDeclaration::Error(_) => {}
                }
            }
        } else {
            // outside of any global declaration,
            // so one can only start a new global declaration
            // with the keywords `type` and `proc`
            return Ok(Some(vec![
                CompletionItem {
                    label: "type".to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                },
                CompletionItem {
                    label: "proc".to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                },
            ]));
        }
    }
    Ok(None)
}

fn search_types(table: &SymbolTable) -> Vec<CompletionItem> {
    let mut completions: Vec<CompletionItem> = table
        .entries
        .iter()
        .filter(|(_, ranged_entry)| matches!(ranged_entry.entry, Entry::Type(_)))
        .map(|(ident, _)| CompletionItem {
            label: ident.value.clone(),
            kind: Some(CompletionItemKind::STRUCT),
            ..Default::default()
        })
        .collect();
    completions.push(CompletionItem {
        label: "int".to_string(),
        kind: Some(CompletionItemKind::STRUCT),
        ..Default::default()
    });
    completions
}

fn search_variables(table: &SymbolTable) -> Vec<CompletionItem> {
    table
        .entries
        .iter()
        .filter(|(_, ranged_entry)| matches!(ranged_entry.entry, Entry::Variable(_)))
        .map(|(ident, _)| CompletionItem {
            label: ident.value.clone(),
            kind: Some(CompletionItemKind::VARIABLE),
            ..Default::default()
        })
        .collect()
}

fn search_procedures(table: &SymbolTable) -> Vec<CompletionItem> {
    table
        .entries
        .iter()
        .filter(|(_, ranged_entry)| matches!(ranged_entry.entry, Entry::Procedure(_)))
        .map(|(ident, _)| CompletionItem {
            label: ident.value.clone(),
            kind: Some(CompletionItemKind::FUNCTION),
            ..Default::default()
        })
        .collect()
}

fn new_stmt(local_table: &SymbolTable, global_table: &SymbolTable) -> Option<Vec<CompletionItem>> {
    let mut completions = vec![
        CompletionItem {
            label: "if".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        },
        CompletionItem {
            label: "while".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        },
    ];
    search_variables(local_table)
        .into_iter()
        .for_each(|item| completions.push(item));
    search_procedures(global_table)
        .into_iter()
        .for_each(|item| completions.push(item));
    Some(completions)
}
