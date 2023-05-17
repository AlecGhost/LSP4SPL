use super::{DocumentCursor, Ident};
use crate::document::{as_pos_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{
    Location, Range as PosRange, ReferenceParams, RenameParams, TextDocumentPositionParams,
    TextEdit, WorkspaceEdit,
};
use spl_frontend::{
    ast::{
        Expression, GlobalDeclaration, Identifier, ParameterDeclaration, Program, Reference,
        Statement, TypeExpression, Variable, VariableDeclaration,
    },
    table::{Entry, GlobalEntry, GlobalTable, LookupTable},
    Shiftable, ToRange, ToTextRange,
};
use std::collections::HashMap;
use tokio::sync::mpsc::Sender;

pub async fn rename(
    doctx: Sender<DocumentRequest>,
    params: RenameParams,
) -> Result<Option<WorkspaceEdit>> {
    let doc_params = params.text_document_position;
    let uri = doc_params.text_document.uri.clone();
    let new_name = params.new_name;
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        if let Some(ident) = &cursor.ident() {
            let DocumentCursor { doc, context, .. } = cursor;
            if let Some(entry) = context {
                // Early return for int
                if &ident.value == "int" {
                    return Ok(None);
                }
                let idents = find_referenced_identifiers(ident, &entry, &doc.ast, &doc.table);
                // it seems like the original identifier is changed automatically,
                // so it does not need to be added to `idents`
                let text_edits = idents
                    .into_iter()
                    .map(|identifier| {
                        Ident::from_identifier(&identifier, identifier.to_text_range(&doc.tokens))
                    })
                    .map(|ident| TextEdit {
                        range: as_pos_range(&ident.to_range(), &doc.text),
                        new_text: new_name.clone(),
                    })
                    .collect();
                return Ok(Some(WorkspaceEdit {
                    changes: Some(HashMap::from([(uri, text_edits)])),
                    ..Default::default()
                }));
            }
        }
    }
    Ok(None)
}

/// Just checks whether the current cursor position is on an identifier
pub async fn prepare_rename(
    doctx: Sender<DocumentRequest>,
    params: TextDocumentPositionParams,
) -> Result<Option<PosRange>> {
    if let Some(cursor) = super::doc_cursor(params, doctx).await? {
        if let Some(ident) = &cursor.ident() {
            // Early return for int
            if &ident.value == "int" {
                return Ok(None);
            }
            let text = cursor.doc.text;
            return Ok(Some(as_pos_range(&ident.to_range(), &text)));
        }
    }
    Ok(None)
}

pub async fn find(
    doctx: Sender<DocumentRequest>,
    params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let doc_params = params.text_document_position;
    let uri = doc_params.text_document.uri.clone();
    if let Some(cursor) = super::doc_cursor(doc_params, doctx).await? {
        if let Some(ident) = &cursor.ident() {
            let DocumentCursor { doc, context, .. } = cursor;
            if let Some(entry) = context {
                let identifiers = find_referenced_identifiers(ident, &entry, &doc.ast, &doc.table);
                let references = identifiers
                    .into_iter()
                    .map(|identifier| {
                        Ident::from_identifier(&identifier, identifier.to_text_range(&doc.tokens))
                    })
                    .filter(|i| i != ident)
                    .map(|i| Location {
                        uri: uri.clone(),
                        range: as_pos_range(&i.to_range(), &doc.text),
                    })
                    .collect();
                return Ok(Some(references));
            }
        }
    }
    Ok(None)
}

fn find_referenced_identifiers(
    ident: &Ident,
    entry: &GlobalEntry,
    program: &Program,
    global_table: &GlobalTable,
) -> Vec<Identifier> {
    match entry {
        GlobalEntry::Procedure(p) => {
            if p.name.value == ident.value {
                find_procs(&ident.value, program)
            } else {
                let lookup_table = LookupTable {
                    global_table: Some(global_table),
                    local_table: Some(&p.local_table),
                };
                lookup_table
                    .lookup(&ident.value)
                    .map_or_else(Vec::new, |entry| match &entry {
                        Entry::Type(_) => find_types(&ident.value, program),
                        Entry::Procedure(_) => find_procs(&ident.value, program),
                        Entry::Variable(_) | Entry::Parameter(_) => {
                            find_vars(&ident.value, &p.name.value, program)
                        }
                    })
            }
        }
        GlobalEntry::Type(_) => find_types(&ident.value, program),
    }
}

fn find_procs(name: &str, program: &Program) -> Vec<Identifier> {
    fn find_in_statement(stmt: &Statement, name: &str) -> Vec<Identifier> {
        match stmt {
            Statement::Block(b) => b
                .statements
                .iter()
                .flat_map(|stmt| find_in_statement(stmt, name).shift(stmt.offset))
                .collect(),
            Statement::Call(c) => {
                if c.name.value == name {
                    vec![c.name.clone()]
                } else {
                    Vec::new()
                }
            }
            Statement::If(i) => {
                let mut idents = i.if_branch.as_ref().map_or(Vec::new(), |branch| {
                    find_in_statement(branch, name).shift(branch.offset)
                });
                if let Some(stmt) = &i.else_branch {
                    let new_idents = find_in_statement(stmt, name).shift(stmt.offset);
                    idents.extend(new_idents);
                }
                idents
            }
            Statement::While(w) => w.statement.as_ref().map_or(Vec::new(), |branch| {
                find_in_statement(branch, name).shift(branch.offset)
            }),
            _ => Vec::new(),
        }
    }

    use GlobalDeclaration::*;
    program
        .global_declarations
        .iter()
        .filter_map(|gd| match gd.as_ref() {
            Procedure(pd) => Some((pd, gd.offset)),
            _ => None,
        })
        .flat_map(|(pd, offset)| {
            let mut idents = Vec::new();
            if let Some(ident) = &pd.name {
                if ident.value == name {
                    idents.push(ident.clone());
                }
            }
            let new_idents: Vec<_> = pd
                .statements
                .iter()
                .flat_map(|stmt| find_in_statement(stmt, name).shift(stmt.offset))
                .collect();
            idents.extend(new_idents);
            idents.shift(offset)
        })
        .collect()
}

fn find_types(name: &str, program: &Program) -> Vec<Identifier> {
    fn get_ident_in_type_expr(type_expr: &Reference<TypeExpression>) -> Option<Identifier> {
        match type_expr.as_ref() {
            TypeExpression::NamedType(ident) => Some(ident.clone()),
            TypeExpression::ArrayType { base_type, .. } => base_type
                .as_ref()
                .map(|boxed| boxed.as_ref())
                .and_then(get_ident_in_type_expr),
        }
        .map(|ident| ident.shift(type_expr.offset))
    }

    use GlobalDeclaration::*;
    program
        .global_declarations
        .iter()
        .flat_map(|gd| {
            match gd.as_ref() {
                Type(td) => {
                    let mut idents = Vec::new();
                    if let Some(ident) = &td.name {
                        if ident.value == name {
                            idents.push(ident.clone());
                        }
                    }
                    if let Some(type_expr) = &td.type_expr {
                        if let Some(ident) = get_ident_in_type_expr(type_expr) {
                            if ident.value == name {
                                idents.push(ident);
                            }
                        }
                    }
                    idents
                }
                Procedure(pd) => {
                    let mut idents = Vec::new();
                    let param_idents: Vec<_> = pd
                        .parameters
                        .iter()
                        .filter_map(|param| match param.as_ref() {
                            ParameterDeclaration::Valid {
                                type_expr: Some(type_expr),
                                ..
                            } => get_ident_in_type_expr(type_expr)
                                .map(|ident| ident.shift(param.offset)),
                            _ => None,
                        })
                        .filter(|ident| ident.value == name)
                        .collect();
                    idents.extend(param_idents);
                    let var_idents: Vec<_> = pd
                        .variable_declarations
                        .iter()
                        .filter_map(|vd| match vd.as_ref() {
                            VariableDeclaration::Valid {
                                type_expr: Some(type_expr),
                                ..
                            } => get_ident_in_type_expr(type_expr)
                                .map(|ident| ident.shift(vd.offset)),
                            _ => None,
                        })
                        .filter(|ident| ident.value == name)
                        .collect();
                    idents.extend(var_idents);
                    idents
                }
                Error(_) => Vec::new(),
            }
            .shift(gd.offset)
        })
        .collect()
}

fn find_vars(name: &str, proc_name: &str, program: &Program) -> Vec<Identifier> {
    fn find_in_variable(var: &Variable, name: &str) -> Vec<Identifier> {
        use Variable::*;
        match var {
            NamedVariable(ident) => {
                if ident.value == name {
                    vec![ident.clone()]
                } else {
                    Vec::new()
                }
            }
            ArrayAccess(a) => {
                let mut idents = find_in_variable(&a.array, name);
                if let Some(index) = &a.index {
                    let new_idents = find_in_expression(index, name);
                    idents.extend(new_idents);
                }
                idents
            }
        }
    }

    fn find_in_expression(expr: &Expression, name: &str) -> Vec<Identifier> {
        use Expression::*;
        match expr {
            Variable(v) => find_in_variable(v, name),
            Binary(b) => {
                let mut idents = Vec::new();
                let new_idents = find_in_expression(&b.lhs, name);
                idents.extend(new_idents);
                let new_idents = find_in_expression(&b.rhs, name);
                idents.extend(new_idents);
                idents
            }
            _ => Vec::new(),
        }
    }

    fn find_in_statement(stmt: &Statement, name: &str) -> Vec<Identifier> {
        match stmt {
            Statement::Assignment(a) => {
                let mut idents = find_in_variable(&a.variable, name);
                if let Some(expr) = &a.expr {
                    let new_idents = find_in_expression(expr, name).shift(expr.offset);
                    idents.extend(new_idents);
                }
                idents
            }
            Statement::Block(b) => b
                .statements
                .iter()
                .flat_map(|stmt| find_in_statement(stmt, name).shift(stmt.offset))
                .collect(),
            Statement::Call(c) => c
                .arguments
                .iter()
                .flat_map(|expr| find_in_expression(expr, name).shift(expr.offset))
                .collect(),
            Statement::If(i) => {
                let mut idents = i.condition.as_ref().map_or(Vec::new(), |expr| {
                    find_in_expression(expr, name).shift(expr.offset)
                });
                if let Some(stmt) = &i.if_branch {
                    let new_idents = find_in_statement(stmt, name).shift(stmt.offset);
                    idents.extend(new_idents);
                }
                if let Some(stmt) = &i.else_branch {
                    let new_idents = find_in_statement(stmt, name).shift(stmt.offset);
                    idents.extend(new_idents);
                }
                idents
            }
            Statement::While(w) => {
                let mut idents = w.condition.as_ref().map_or(Vec::new(), |expr| {
                    find_in_expression(expr, name).shift(expr.offset)
                });
                let new_idents = w.statement.as_ref().map_or(Vec::new(), |branch| {
                    find_in_statement(branch, name).shift(branch.offset)
                });
                idents.extend(new_idents);
                idents
            }
            _ => Vec::new(),
        }
    }

    program
        .global_declarations
        .iter()
        .filter_map(|gd| match gd.as_ref() {
            GlobalDeclaration::Procedure(pd) => Some((pd, gd.offset)),
            _ => None,
        })
        .find(|(pd, _)| {
            pd.name
                .as_ref()
                .map_or(false, |ident| ident.value == proc_name)
        })
        .map_or_else(Vec::new, |(pd, offset)| {
            let mut idents = Vec::new();
            let param_idents: Vec<_> = pd
                .parameters
                .iter()
                .filter_map(|param| match param.as_ref() {
                    ParameterDeclaration::Valid {
                        name: Some(ident), ..
                    } if ident.value == name => Some(ident.clone().shift(param.offset)),
                    _ => None,
                })
                .collect();
            idents.extend(param_idents);
            let var_idents: Vec<_> = pd
                .variable_declarations
                .iter()
                .filter_map(|vd| match vd.as_ref() {
                    VariableDeclaration::Valid {
                        name: Some(ident), ..
                    } if ident.value == name => Some(ident.clone().shift(vd.offset)),
                    _ => None,
                })
                .collect();
            idents.extend(var_idents);
            let stmt_idents: Vec<_> = pd
                .statements
                .iter()
                .flat_map(|stmt| find_in_statement(stmt, name).shift(stmt.offset))
                .collect();
            idents.extend(stmt_idents);
            idents.shift(offset)
        })
}
