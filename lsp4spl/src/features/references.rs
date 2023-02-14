use super::DocumentCursor;
use crate::document::{convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{
    Location, Range, ReferenceParams, RenameParams, TextDocumentPositionParams, TextEdit,
    WorkspaceEdit,
};
use spl_frontend::{
    ast::{
        Expression, GlobalDeclaration, Identifier, Program, Statement, TypeExpression, Variable,
    },
    table::{Entry, LookupTable, SymbolTable, Table},
};
use std::collections::HashMap;
use tokio::sync::mpsc::Sender;

pub(crate) async fn rename(
    doctx: Sender<DocumentRequest>,
    params: RenameParams,
) -> Result<Option<WorkspaceEdit>> {
    let doc_params = params.text_document_position;
    let uri = doc_params.text_document.uri.clone();
    let new_name = params.new_name;
    if let Some(DocumentCursor {
        doc_info,
        index,
        context,
    }) = super::doc_cursor(doc_params, doctx).await?
    {
        if let Some(ident) = doc_info.ast.ident_at(index) {
            if let Some(ranged_entry) = context {
                let idents = find_referenced_idents(
                    ident,
                    &ranged_entry.entry,
                    &doc_info.ast,
                    &doc_info.table,
                );
                // it seems like the original identifier is changed automatically,
                // so it does not need to be added to `idents`
                let text_edits = idents
                    .into_iter()
                    .map(|ident| TextEdit {
                        range: convert_range(&ident.range, &doc_info.text),
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

pub(crate) async fn prepare_rename(
    doctx: Sender<DocumentRequest>,
    params: TextDocumentPositionParams,
) -> Result<Option<Range>> {
    if let Some(DocumentCursor {
        doc_info, index, ..
    }) = super::doc_cursor(params, doctx).await?
    {
        if let Some(ident) = doc_info.ast.ident_at(index) {
            return Ok(Some(convert_range(&ident.range, &doc_info.text)));
        }
    }
    Ok(None)
}

pub(crate) async fn find(
    doctx: Sender<DocumentRequest>,
    params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let doc_params = params.text_document_position;
    let uri = doc_params.text_document.uri.clone();
    if let Some(DocumentCursor {
        doc_info,
        index,
        context,
    }) = super::doc_cursor(doc_params, doctx).await?
    {
        if let Some(ident) = doc_info.ast.ident_at(index) {
            if let Some(ranged_entry) = context {
                let idents = find_referenced_idents(
                    ident,
                    &ranged_entry.entry,
                    &doc_info.ast,
                    &doc_info.table,
                );
                let references = idents
                    .into_iter()
                    .filter(|i| i != ident)
                    .map(|i| Location {
                        uri: uri.clone(),
                        range: convert_range(&i.range, &doc_info.text),
                    })
                    .collect();
                return Ok(Some(references));
            }
        }
    }
    Ok(None)
}

fn find_referenced_idents(
    ident: &Identifier,
    entry: &Entry,
    program: &Program,
    global_table: &SymbolTable,
) -> Vec<Identifier> {
    match entry {
        Entry::Procedure(p) => {
            if &p.name == ident {
                find_procs(&ident.value, program)
            } else {
                let lookup_table = LookupTable {
                    global_table,
                    local_table: &p.local_table,
                };
                if let Some((_, ranged_entry)) = lookup_table.entry(ident) {
                    match &ranged_entry.entry {
                        Entry::Type(_) => find_types(&ident.value, program),
                        Entry::Procedure(_) => find_procs(&ident.value, program),
                        Entry::Variable(_) => find_vars(&ident.value, &p.name.value, program),
                    }
                } else {
                    Vec::new()
                }
            }
        }
        Entry::Type(_) => find_types(&ident.value, program),
        Entry::Variable(v) => {
            log::error!("Found illegal variable in global table {:#?}", v);
            panic!("Found illegal variable in global table {:#?}", v);
        }
    }
}

fn find_procs(name: &str, program: &Program) -> Vec<Identifier> {
    fn find_in_statement(stmt: &Statement, name: &str) -> Vec<Identifier> {
        match stmt {
            Statement::Block(b) => b
                .statements
                .iter()
                .flat_map(|stmt| find_in_statement(stmt, name))
                .collect(),
            Statement::Call(c) => {
                if c.name.value == name {
                    vec![c.name.clone()]
                } else {
                    Vec::new()
                }
            }
            Statement::If(i) => {
                let mut idents = i
                    .if_branch
                    .as_ref()
                    .map_or(Vec::new(), |branch| find_in_statement(branch, name));
                if let Some(stmt) = &i.else_branch {
                    let mut new_idents = find_in_statement(stmt, name);
                    idents.append(&mut new_idents);
                }
                idents
            }
            Statement::While(w) => w
                .statement
                .as_ref()
                .map_or(Vec::new(), |branch| find_in_statement(branch, name)),
            _ => Vec::new(),
        }
    }

    use GlobalDeclaration::*;
    let mut idents = Vec::new();
    program
        .global_declarations
        .iter()
        .filter_map(|gd| match gd {
            Procedure(pd) => Some(pd),
            _ => None,
        })
        .for_each(|pd| {
            if let Some(ident) = &pd.name {
                if ident.value == name {
                    idents.push(ident.clone());
                }
            }
            let mut new_idents = pd
                .statements
                .iter()
                .flat_map(|stmt| find_in_statement(stmt, name))
                .collect();
            idents.append(&mut new_idents);
        });
    idents
}

fn find_types(name: &str, program: &Program) -> Vec<Identifier> {
    fn get_ident_in_type_expr(type_expr: &TypeExpression) -> Option<Identifier> {
        match type_expr {
            TypeExpression::NamedType(ident) => Some(ident.clone()),
            TypeExpression::ArrayType { base_type, .. } => base_type
                .as_ref()
                .map(|boxed| boxed.as_ref())
                .and_then(get_ident_in_type_expr),
        }
    }

    use GlobalDeclaration::*;
    let mut idents = Vec::new();
    program.global_declarations.iter().for_each(|gd| match gd {
        Type(td) => {
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
        }
        Procedure(pd) => {
            pd.parameters
                .iter()
                .flat_map(|param| &param.type_expr)
                .flat_map(get_ident_in_type_expr)
                .filter(|ident| ident.value == name)
                .for_each(|ident| idents.push(ident));
            pd.variable_declarations
                .iter()
                .flat_map(|vd| &vd.type_expr)
                .flat_map(get_ident_in_type_expr)
                .filter(|ident| ident.value == name)
                .for_each(|ident| idents.push(ident));
        }
        _ => {}
    });
    idents
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
                let mut new_idents = find_in_expression(&a.index, name);
                idents.append(&mut new_idents);
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
                let mut new_idents = find_in_expression(&b.lhs, name);
                idents.append(&mut new_idents);
                let mut new_idents = find_in_expression(&b.rhs, name);
                idents.append(&mut new_idents);
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
                    let mut new_idents = find_in_expression(expr, name);
                    idents.append(&mut new_idents);
                }
                idents
            }
            Statement::Block(b) => b
                .statements
                .iter()
                .flat_map(|stmt| find_in_statement(stmt, name))
                .collect(),
            Statement::Call(c) => c
                .arguments
                .iter()
                .flat_map(|expr| find_in_expression(expr, name))
                .collect(),
            Statement::If(i) => {
                let mut idents = i
                    .condition
                    .as_ref()
                    .map_or(Vec::new(), |expr| find_in_expression(expr, name));
                if let Some(stmt) = &i.if_branch {
                    let mut new_idents = find_in_statement(stmt, name);
                    idents.append(&mut new_idents);
                }
                if let Some(stmt) = &i.else_branch {
                    let mut new_idents = find_in_statement(stmt, name);
                    idents.append(&mut new_idents);
                }
                idents
            }
            Statement::While(w) => {
                let mut idents = w
                    .condition
                    .as_ref()
                    .map_or(Vec::new(), |expr| find_in_expression(expr, name));
                let mut new_idents = w
                    .statement
                    .as_ref()
                    .map_or(Vec::new(), |branch| find_in_statement(branch, name));
                idents.append(&mut new_idents);
                idents
            }
            _ => Vec::new(),
        }
    }

    if let Some(pd) = program
        .global_declarations
        .iter()
        .filter_map(|gd| match gd {
            GlobalDeclaration::Procedure(p) => Some(p),
            _ => None,
        })
        .find(|pd| {
            if let Some(ident) = &pd.name {
                ident.value == proc_name
            } else {
                false
            }
        })
    {
        let mut idents = Vec::new();
        pd.parameters
            .iter()
            .filter_map(|param| param.name.as_ref())
            .for_each(|ident| {
                if ident.value == name {
                    idents.push(ident.clone())
                }
            });
        pd.variable_declarations
            .iter()
            .filter_map(|vd| vd.name.as_ref())
            .for_each(|ident| {
                if ident.value == name {
                    idents.push(ident.clone())
                }
            });
        let mut new_idents = pd
            .statements
            .iter()
            .flat_map(|stmt| find_in_statement(stmt, name))
            .collect();
        idents.append(&mut new_idents);
        idents
    } else {
        Vec::new()
    }
}
