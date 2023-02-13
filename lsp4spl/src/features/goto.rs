use super::DocumentCursor;
use crate::document::{convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{
    request::{GotoDeclarationParams, GotoImplementationParams, GotoTypeDefinitionParams},
    GotoDefinitionParams, Location, ReferenceParams,
};
use spl_frontend::{
    ast::{
        Expression, GlobalDeclaration, Identifier, Program, Statement, TypeExpression, Variable,
    },
    table::{DataType, Entry, LookupTable, Table},
};
use tokio::sync::mpsc::Sender;

pub(crate) async fn declaration(
    doctx: Sender<DocumentRequest>,
    params: GotoDeclarationParams,
) -> Result<Option<Location>> {
    let doc_params = params.text_document_position_params;
    let uri = doc_params.text_document.uri.clone();
    if let Some(DocumentCursor {
        doc_info,
        index,
        context,
    }) = super::doc_cursor(doc_params, doctx).await?
    {
        if let Some(ident) = doc_info.ast.ident_at(index) {
            if let Some(ranged_entry) = context {
                match &ranged_entry.entry {
                    Entry::Type(_) => {
                        if let Some((key, _)) = doc_info.table.entry(ident) {
                            return Ok(Some(Location {
                                uri,
                                range: convert_range(&key.range, &doc_info.text),
                            }));
                        }
                    }
                    Entry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: &doc_info.table,
                            local_table: &p.local_table,
                        };
                        if let Some((key, _)) = lookup_table.entry(ident) {
                            return Ok(Some(Location {
                                uri,
                                range: convert_range(&key.range, &doc_info.text),
                            }));
                        }
                    }
                    Entry::Variable(v) => {
                        log::error!("Found illegal variable in global table {:#?}", v);
                        panic!("Found illegal variable in global table {:#?}", v);
                    }
                }
            }
        }
    }
    Ok(None)
}

/// Calls `goto::declaration` because in SPL, there is no conceptual difference
/// between declaration and definition
pub(crate) async fn definition(
    doctx: Sender<DocumentRequest>,
    params: GotoDefinitionParams,
) -> Result<Option<Location>> {
    declaration(doctx, params).await
}

pub(crate) async fn type_definition(
    doctx: Sender<DocumentRequest>,
    params: GotoTypeDefinitionParams,
) -> Result<Option<Location>> {
    let doc_params = params.text_document_position_params;
    let uri = doc_params.text_document.uri.clone();
    if let Some(DocumentCursor {
        doc_info,
        index,
        context,
    }) = super::doc_cursor(doc_params, doctx).await?
    {
        if let Some(ident) = doc_info.ast.ident_at(index) {
            if let Some(ranged_entry) = context {
                match &ranged_entry.entry {
                    Entry::Type(_) => {
                        if let Some((key, ranged_entry)) = doc_info.table.entry(ident) {
                            match &ranged_entry.entry {
                                Entry::Type(_) => {
                                    return Ok(Some(Location {
                                        uri,
                                        range: convert_range(&key.range, &doc_info.text),
                                    }));
                                }
                                Entry::Procedure(_) => { /* no type definition */ }
                                Entry::Variable(v) => {
                                    log::error!("Found illegal variable in global table {:#?}", v);
                                    panic!("Found illegal variable in global table {:#?}", v);
                                }
                            }
                        }
                    }
                    Entry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: &doc_info.table,
                            local_table: &p.local_table,
                        };
                        if let Some((key, ranged_entry)) = lookup_table.entry(ident) {
                            match &ranged_entry.entry {
                                Entry::Type(_) => {
                                    return Ok(Some(Location {
                                        uri,
                                        range: convert_range(&key.range, &doc_info.text),
                                    }));
                                }
                                Entry::Procedure(_) => { /* no type definition */ }
                                Entry::Variable(v) => {
                                    if let Some(DataType::Array {
                                        size: _,
                                        base_type: _,
                                        creator,
                                    }) = &v.data_type
                                    {
                                        return Ok(Some(Location {
                                            uri,
                                            range: convert_range(&creator.range, &doc_info.text),
                                        }));
                                    }
                                    /* cannot look up primitive types */
                                }
                            }
                        }
                    }
                    Entry::Variable(v) => {
                        log::error!("Found illegal variable in global table {:#?}", v);
                        panic!("Found illegal variable in global table {:#?}", v);
                    }
                }
            }
        }
    }
    Ok(None)
}

/// Essentially the same as `goto::declaration`, but only for procedures
pub(crate) async fn implementation(
    doctx: Sender<DocumentRequest>,
    params: GotoImplementationParams,
) -> Result<Option<Location>> {
    let doc_params = params.text_document_position_params;
    let uri = doc_params.text_document.uri.clone();
    if let Some(DocumentCursor {
        doc_info,
        index,
        context,
    }) = super::doc_cursor(doc_params, doctx).await?
    {
        if let Some(ident) = doc_info.ast.ident_at(index) {
            if let Some(ranged_entry) = context {
                match &ranged_entry.entry {
                    Entry::Procedure(p) => {
                        let lookup_table = LookupTable {
                            global_table: &doc_info.table,
                            local_table: &p.local_table,
                        };
                        if let Some((key, ranged_entry)) = lookup_table.entry(ident) {
                            if let Entry::Procedure(_) = ranged_entry.entry {
                                return Ok(Some(Location {
                                    uri,
                                    range: convert_range(&key.range, &doc_info.text),
                                }));
                            }
                            /* no implementation for types and variables */
                        }
                    }
                    Entry::Type(_) => { /* no implementation for types */ }
                    Entry::Variable(v) => {
                        log::error!("Found illegal variable in global table {:#?}", v);
                        panic!("Found illegal variable in global table {:#?}", v);
                    }
                }
            }
        }
    }
    Ok(None)
}

pub(crate) async fn references(
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
                match &ranged_entry.entry {
                    Entry::Procedure(p) => {
                        if &p.name == ident {
                            let idents = find_procs(&ident.value, &doc_info.ast);
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
                        let lookup_table = LookupTable {
                            global_table: &doc_info.table,
                            local_table: &p.local_table,
                        };
                        if let Some((_, ranged_entry)) = lookup_table.entry(ident) {
                            let idents = match &ranged_entry.entry {
                                Entry::Type(_) => find_types(&ident.value, &doc_info.ast),
                                Entry::Procedure(_) => find_procs(&ident.value, &doc_info.ast),
                                Entry::Variable(_) => {
                                    find_vars(&ident.value, &p.name.value, &doc_info.ast)
                                }
                            };
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
                    Entry::Type(_) => {
                        let idents = find_types(&ident.value, &doc_info.ast);
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
                    Entry::Variable(v) => {
                        log::error!("Found illegal variable in global table {:#?}", v);
                        panic!("Found illegal variable in global table {:#?}", v);
                    }
                }
            }
        }
    }
    Ok(None)
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
            TypeExpression::ArrayType {
                array_kw: _,
                of_kw: _,
                size: _,
                base_type,
                range: _,
            } => base_type
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
