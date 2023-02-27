use crate::document::{convert_range, DocumentInfo, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{DocumentFormattingParams, TextEdit};
use spl_frontend::{
    ast::{
        Expression, GlobalDeclaration, IntLiteral, ProcedureDeclaration, Statement,
        TypeDeclaration, TypeExpression, Variable,
    },
    lexer::token::{Token, TokenType},
    ToRange,
};
use tokio::sync::mpsc::Sender;

const MAX_LINE_LEN: usize = 100;

pub(crate) async fn format(
    doctx: Sender<DocumentRequest>,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = params.text_document.uri;
    let options = params.options;
    if let Some(DocumentInfo { ast, text, .. }) = super::get_doc_info(uri, doctx).await? {
        let indentation = if options.insert_spaces {
            let spaces = vec![' '; options.tab_size as usize];
            String::from_iter(spaces)
        } else {
            "\t".to_string()
        };
        let text_edits: Vec<TextEdit> = ast
            .global_declarations
            .iter()
            .filter_map(|gd| {
                if let Some(new_text) = format_global_dec(gd, &indentation) {
                    let string_range = gd.to_range();
                    if new_text == text[string_range.clone()] {
                        None
                    } else {
                        let range = convert_range(&string_range, &text);
                        Some(TextEdit { range, new_text })
                    }
                } else {
                    None
                }
            })
            .collect();
        if text_edits.is_empty() {
            Ok(None)
        } else {
            Ok(Some(text_edits))
        }
    } else {
        Ok(None)
    }
}

fn format_global_dec(gd: &GlobalDeclaration, indentation: &str) -> Option<String> {
    use GlobalDeclaration::*;
    match gd {
        Type(td) => Some(format_type_dec(td, indentation)),
        Procedure(pd) => Some(format_proc_dec(pd, indentation)),
        Error(_) => None,
    }
}

fn format_type_dec(td: &TypeDeclaration, indentation: &str) -> String {
    let mut new_text = "type ".to_string();
    if let Some(name) = &td.name {
        new_text += name.value.as_str();
        new_text += " ";
    }
    new_text += "=";
    if let Some(type_expr) = &td.type_expr {
        let expr_texts = split_type_expr(type_expr);
        // plus 1 because every string needs an additional space in front of it
        let expr_texts_len = expr_texts.iter().fold(0, |acc, text| acc + text.len() + 1);
        let splitting_symbol = if new_text.len() + expr_texts_len > MAX_LINE_LEN
            || expr_texts.iter().any(|text| text.starts_with('/'))
        {
            String::new() + "\n" + indentation
        } else {
            " ".to_string()
        };
        for text in expr_texts {
            new_text += splitting_symbol.as_str();
            new_text += text.as_str();
        }
    }
    new_text += ";";
    // collect afterwards, so comments do not count in the calculation of new_text length
    let mut comments = String::new();
    if let Some(comments_vec) = get_leading_comments(&td.info.tokens) {
        add_comments(&mut comments, comments_vec);
    }
    comments + new_text.as_str()
}

fn format_proc_dec(pd: &ProcedureDeclaration, indentation: &str) -> String {
    let mut new_text = "proc ".to_string();
    if let Some(name) = &pd.name {
        new_text += name.value.as_str();
    }
    new_text += "(";
    if !pd.parameters.is_empty() {
        let mut multi_line = false;
        let params: Vec<String> = pd
            .parameters
            .iter()
            .map(|param_dec| {
                let mut param_text = String::new();
                if let Some(comments) = get_leading_comments(&param_dec.info.tokens) {
                    multi_line = true;
                    add_comments(&mut param_text, comments);
                    param_text += indentation;
                }
                if param_dec.is_ref {
                    param_text += "ref ";
                }
                if let Some(name) = &param_dec.name {
                    param_text += name.value.as_str();
                }
                param_text += ":";
                if let Some(type_expr) = &param_dec.type_expr {
                    let expr_texts = split_type_expr(type_expr);
                    let splitting_symbol = if expr_texts.iter().any(|text| text.starts_with('/')) {
                        multi_line = true;
                        String::new() + "\n" + indentation
                    } else {
                        " ".to_string()
                    };
                    for text in expr_texts {
                        param_text += splitting_symbol.as_str();
                        param_text += text.as_str();
                    }
                }
                param_text
            })
            .collect();
        // + 2 for each ", " after each param and ") " at the end
        let params_text_len = params.iter().fold(0, |acc, text| acc + text.len() + 2);
        if !multi_line {
            // + 1 for the opening "{"
            multi_line = new_text.len() + params_text_len + 1 > MAX_LINE_LEN;
        }
        let splitting_symbol = if multi_line {
            String::new() + ",\n" + indentation
        } else {
            ", ".to_string()
        };
        if multi_line {
            new_text += "\n";
            new_text += indentation;
        }
        if let Some(first_param) = params.first() {
            new_text += first_param;
        }
        for param in params.iter().skip(1) {
            new_text += splitting_symbol.as_str();
            new_text += param;
        }
        if multi_line {
            new_text += "\n";
        }
    }
    new_text += ") {";
    if !pd.variable_declarations.is_empty() {
        let var_decs: Vec<String> = pd
            .variable_declarations
            .iter()
            .map(|var_dec| {
                let mut var_dec_text = String::new();
                if let Some(comments) = get_leading_comments(&var_dec.info.tokens) {
                    add_comments(&mut var_dec_text, comments);
                    var_dec_text += indentation;
                }
                var_dec_text += "var ";
                if let Some(name) = &var_dec.name {
                    var_dec_text += name.value.as_str();
                }
                var_dec_text += ":";
                if let Some(type_expr) = &var_dec.type_expr {
                    let expr_texts = split_type_expr(type_expr);
                    let splitting_symbol = if expr_texts.iter().any(|text| text.starts_with('/')) {
                        // add indentation twice, because we are already inside a proc body
                        String::new() + "\n" + indentation + indentation
                    } else {
                        " ".to_string()
                    };
                    for text in expr_texts {
                        var_dec_text += splitting_symbol.as_str();
                        var_dec_text += text.as_str();
                    }
                }
                var_dec_text += ";";
                var_dec_text
            })
            .collect();
        for var_dec in var_decs.iter() {
            new_text += "\n";
            new_text += indentation;
            new_text += var_dec;
        }
        new_text += "\n";
    }
    if !pd.statements.is_empty() {
        let stmts: Vec<String> = pd
            .statements
            .iter()
            .map(|stmt| {
                let mut stmt_text = String::new();
                stmt_text += format_stmt(stmt, indentation, indentation).as_str();
                stmt_text
            })
            .collect();

        for stmt in stmts.iter() {
            new_text += stmt;
        }
        new_text += "\n";
    }
    new_text += "}";
    // collect afterwards, so comments do not count in the calculation of new_text length
    let mut comments = String::new();
    if let Some(comments_vec) = get_leading_comments(&pd.info.tokens) {
        add_comments(&mut comments, comments_vec);
    }
    comments + new_text.as_str()
}

fn format_stmt(stmt: &Statement, current_indentation: &str, indentation: &str) -> String {
    let mut text = format!("\n{}", current_indentation);
    use Statement::*;
    text += match stmt {
        Assignment(a) => {
            let mut text = String::new();
            if let Some(comments) = get_leading_comments(&a.info.tokens) {
                add_comments(&mut text, comments);
                text += current_indentation;
            }
            text += format!("{} :=", format_variable(&a.variable)).as_str();
            if let Some(expr) = &a.expr {
                text += " ";
                text += format_expr(expr).as_str();
            }
            text += ";";
            text
        }
        Block(b) => {
            let mut text = "{".to_string();
            if !b.statements.is_empty() {
                let new_indentation = current_indentation.to_string() + indentation;
                for stmt in b.statements.iter() {
                    text += format_stmt(stmt, &new_indentation, indentation).as_str();
                }
                text += "\n";
                text += current_indentation;
            }
            text += "}";
            text
        }
        Call(c) => {
            let mut text = String::new();
            if let Some(comments) = get_leading_comments(&c.info.tokens) {
                add_comments(&mut text, comments);
                text += current_indentation;
            }
            text += format!("{}(", c.name.value).as_str();
            if let Some(first_arg) = &c.arguments.first() {
                text += format_expr(first_arg).as_str();
                for arg in c.arguments.iter().skip(1) {
                    text += ", ";
                    text += format_expr(arg).as_str();
                }
            }
            text += ");";
            text
        }
        Empty(info) => {
            let mut text = String::new();
            if let Some(comments) = get_leading_comments(&info.tokens) {
                add_comments(&mut text, comments);
                text += current_indentation;
            }
            text += ";";
            text
        }
        If(i) => {
            let mut text = String::new();
            if let Some(comments) = get_leading_comments(&i.info.tokens) {
                add_comments(&mut text, comments);
                text += current_indentation;
            }
            text += format!(
                "if ({})",
                i.condition
                    .as_ref()
                    .map(format_expr)
                    .unwrap_or("".to_string())
            )
            .as_str();
            if let Some(if_branch) = &i.if_branch {
                text += format_branch(if_branch, current_indentation, indentation).as_str();
            }
            if let Some(else_branch) = &i.else_branch {
                if matches!(
                    i.if_branch.as_ref().map(|boxed| boxed.as_ref()),
                    Some(Statement::Block(_))
                ) {
                    text += " "
                } else {
                    text += "\n";
                    text += current_indentation;
                }
                text += "else";
                text += format_branch(else_branch, current_indentation, indentation).as_str();
            }
            text
        }
        While(w) => {
            let mut text = String::new();
            if let Some(comments) = get_leading_comments(&w.info.tokens) {
                add_comments(&mut text, comments);
                text += current_indentation;
            }
            text += format!(
                "while ({})",
                w.condition
                    .as_ref()
                    .map(format_expr)
                    .unwrap_or("".to_string())
            )
            .as_str();
            if let Some(stmt) = &w.statement {
                text += format_branch(stmt, current_indentation, indentation).as_str();
            }
            text
        }
        Error(_) => "".to_string(),
    }
    .as_str();
    text
}

fn format_branch(branch: &Statement, current_indentation: &str, indentation: &str) -> String {
    let mut text = String::new();
    let new_indentation = current_indentation.to_string() + indentation;
    match branch {
        Statement::Block(b) => {
            text += " {";
            if !b.statements.is_empty() {
                for stmt in b.statements.iter() {
                    text += format_stmt(stmt, &new_indentation, indentation).as_str();
                }
                text += "\n";
                text += current_indentation;
            }
            text += "}";
        }
        stmt => {
            text += format_stmt(stmt, &new_indentation, indentation).as_str();
        }
    }
    text
}

fn format_variable(variable: &Variable) -> String {
    use Variable::*;
    match variable {
        NamedVariable(ident) => ident.value.to_string(),
        ArrayAccess(access) => {
            format!(
                "{}[{}]",
                format_variable(&access.array),
                format_expr(&access.index)
            )
        }
    }
}

fn format_expr(expr: &Expression) -> String {
    use Expression::*;
    match expr {
        Binary(binary_expr) => format!(
            "{} {} {}",
            format_expr(&binary_expr.lhs),
            binary_expr.operator,
            format_expr(&binary_expr.rhs)
        ),
        IntLiteral(int_lit) => int_lit
            .value
            .map(|value| value.to_string())
            .unwrap_or("".to_string()),
        Variable(var) => format_variable(var),
        Error(_) => "".to_string(),
    }
}

fn get_leading_comments(tokens: &[Token]) -> Option<Vec<String>> {
    let comments: Vec<String> = tokens
        .iter()
        .map_while(|token| match &token.token_type {
            TokenType::Comment(comment) => Some(String::new() + "// " + comment.trim()),
            _ => None,
        })
        .collect();

    if comments.is_empty() {
        None
    } else {
        Some(comments)
    }
}

fn add_comments(new_text: &mut String, comments: Vec<String>) {
    *new_text += comments
        .into_iter()
        .map(|comment| comment + "\n")
        .collect::<String>()
        .as_str();
}

fn split_type_expr(type_expr: &TypeExpression) -> Vec<String> {
    let mut strings = Vec::new();
    use TypeExpression::*;
    match type_expr {
        NamedType(ident) => strings.push(ident.value.clone()),
        ArrayType {
            base_type,
            size,
            info,
        } => {
            if let Some(mut comments) = get_leading_comments(&info.tokens) {
                strings.append(&mut comments);
            }
            let size = 'size: {
                if let Some(IntLiteral { info, .. }) = &size {
                    for token in info.tokens.iter() {
                        match &token.token_type {
                            TokenType::Int(int_result) => {
                                break 'size match int_result {
                                    Ok(value) => value.to_string(),
                                    Err(string) => string.to_string(),
                                };
                            }
                            TokenType::Hex(hex_result) => {
                                let hex = match hex_result {
                                    Ok(value) => value.to_string(),
                                    Err(hex_string) => hex_string.to_string(),
                                };
                                break 'size format!("0x{}", hex);
                            }
                            TokenType::Char(c) => {
                                break 'size format!("'{}'", c);
                            }
                            _ => {}
                        }
                    }
                }
                "".to_string()
            };
            let self_representation = format!("array [{}] of", size);
            strings.push(self_representation);
            if let Some(base_type) = base_type {
                strings.append(&mut split_type_expr(base_type));
            }
        }
    }
    strings
}
