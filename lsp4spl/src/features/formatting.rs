use crate::document::{as_pos_range, DocumentRequest};
use color_eyre::eyre::Result;
use fmt::Format;
use lsp_types::{DocumentFormattingParams, TextEdit};
use spl_frontend::AnalyzedSource;
use tokio::sync::mpsc::Sender;

pub async fn format(
    doctx: Sender<DocumentRequest>,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = params.text_document.uri;
    let options = params.options;
    if let Some(AnalyzedSource {
        ast, text, tokens, ..
    }) = super::get_doc(uri, doctx).await?
    {
        let formatting_options = if options.insert_spaces {
            fmt::FormattingOptions::new(' ', options.tab_size as usize)
        } else {
            fmt::FormattingOptions::new('\t', 1)
        };
        let new_text = ast.fmt(&tokens, &formatting_options);
        if new_text == text {
            Ok(None)
        } else {
            let range = as_pos_range(&(0..text.len()), &text);
            Ok(Some(vec![TextEdit { range, new_text }]))
        }
    } else {
        Ok(None)
    }
}

mod fmt {
    use spl_frontend::{
        ast::*,
        token::{Token, TokenType},
    };

    #[derive(Clone, Debug)]
    pub struct FormattingOptions {
        indent_symbol: char,
        indent_depth: usize,
    }

    impl FormattingOptions {
        pub const fn new(indent_symbol: char, indent_depth: usize) -> Self {
            Self {
                indent_symbol,
                indent_depth,
            }
        }

        pub fn indentation(&self) -> String {
            let indentation_characters = vec![self.indent_symbol; self.indent_depth];
            String::from_iter(indentation_characters)
        }
    }

    fn indent(text: String, f: &FormattingOptions) -> String {
        text.lines()
            .map(|line| f.indentation() + line + "\n")
            .collect()
    }

    fn add_leading_comments(text: String, tokens: &[Token]) -> String {
        tokens
            .iter()
            .map_while(|token| {
                if matches!(&token.token_type, TokenType::Comment(_)) {
                    Some(token.to_string())
                } else {
                    None
                }
            })
            .collect::<String>()
            + &text
    }

    fn add_all_comments(text: String, tokens: &[Token]) -> String {
        tokens
            .iter()
            .filter_map(|token| {
                if matches!(&token.token_type, TokenType::Comment(_)) {
                    Some(token.to_string())
                } else {
                    None
                }
            })
            .collect::<String>()
            + &text
    }

    pub trait Format {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String;
    }

    impl Format for Program {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            self.global_declarations
                .iter()
                .map(|gd| gd.fmt(&tokens[gd.offset..], f))
                .reduce(|acc, gd| acc + "\n" + &gd)
                .unwrap_or_default()
        }
    }

    impl Format for GlobalDeclaration {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            match self {
                Self::Type(td) => td.fmt(tokens, f),
                Self::Procedure(pd) => pd.fmt(tokens, f),
                Self::Error(info) => info.fmt(tokens, f),
            }
        }
    }

    impl Format for ProcedureDeclaration {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            let name = fmt_or_empty(&self.name, tokens, f);
            let param_vec: Vec<String> = self
                .parameters
                .iter()
                .map(|param| {
                    let tokens = &tokens[param.offset..];
                    add_all_comments(
                        param.fmt(tokens, f),
                        match param.as_ref() {
                            ParameterDeclaration::Valid { info, .. } => &info.slice(tokens),
                            ParameterDeclaration::Error(info) => &info.slice(tokens),
                        },
                    )
                })
                .collect();
            let params = if param_vec.is_empty() {
                String::new()
            } else {
                let len = param_vec.len();
                let has_comments = param_vec.iter().any(|param| param.contains("//"));
                // if there are more than three parameters,
                // or any of them is commented,
                // parameters should be in separate lines and indented
                if len > 3 || has_comments {
                    let params = param_vec
                        .into_iter()
                        .reduce(|acc, param| acc + ",\n" + param.as_str())
                        .expect("Params not empty was checked before");
                    "\n".to_string() + &indent(params, f)
                } else {
                    param_vec
                        .into_iter()
                        .reduce(|acc, param| acc + ", " + param.as_str())
                        .expect("Params not empty was checked before")
                }
            };
            let var_decs: String = self
                .variable_declarations
                .iter()
                .map(|var_dec| {
                    let tokens = &tokens[var_dec.offset..];
                    add_all_comments(
                        var_dec.fmt(tokens, f),
                        match var_dec.as_ref() {
                            VariableDeclaration::Valid { info, .. } => info.slice(tokens),
                            VariableDeclaration::Error(info) => info.slice(tokens),
                        },
                    )
                })
                .collect();
            let var_decs = indent(var_decs, f);
            let stmts: String = self
                .statements
                .iter()
                .map(|stmt| stmt.fmt(&tokens[stmt.offset..], f))
                .collect();
            let stmts = indent(stmts, f);
            let pd = match (var_decs.is_empty(), stmts.is_empty()) {
                (true, true) => format!("proc {}({}) {{}}\n", name, params),
                (true, false) => format!("proc {}({}) {{\n{}}}\n", name, params, stmts),
                (false, true) => format!("proc {}({}) {{\n{}}}\n", name, params, var_decs),
                (false, false) => {
                    format!("proc {}({}) {{\n{}\n{}}}\n", name, params, var_decs, stmts)
                }
            };
            add_leading_comments(pd, self.info.slice(tokens))
        }
    }

    impl Format for TypeDeclaration {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            let type_expr = fmt_ref_or_empty(&self.type_expr, tokens, f);
            let td = self.name.as_ref().map_or_else(
                || format!("type = {};\n", type_expr),
                |name| format!("type {} = {};\n", name, type_expr),
            );
            add_leading_comments(td, self.info.slice(tokens))
        }
    }

    impl Format for Statement {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            match self {
                Self::Assignment(a) => add_all_comments(a.fmt(tokens, f), a.info.slice(tokens)),
                Self::Block(b) => b.fmt(tokens, f),
                Self::Call(c) => add_all_comments(c.fmt(tokens, f), c.info.slice(tokens)),
                Self::If(i) => i.fmt(tokens, f),
                Self::While(w) => w.fmt(tokens, f),
                Self::Empty(info) => add_all_comments(";\n".to_string(), info.slice(tokens)),
                Self::Error(info) => format!("{}\n", info.fmt(tokens, f)),
            }
        }
    }

    impl Format for BlockStatement {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            let stmt = if self.statements.is_empty() {
                "{}\n".to_string()
            } else {
                let stmts: String = self
                    .statements
                    .iter()
                    .map(|stmt| stmt.fmt(&tokens[stmt.offset..], f))
                    .collect();
                let stmts = indent(stmts, f);
                format!("{{\n{}}}\n", stmts)
            };
            add_leading_comments(stmt, self.info.slice(tokens))
        }
    }

    impl Format for IfStatement {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            let condition = fmt_ref_or_empty(&self.condition, tokens, f);
            let stmt = self.else_branch.as_ref().map_or_else(
                || {
                    format!(
                        "if ({}){}",
                        condition,
                        fmt_branch(&self.if_branch, tokens, f, '\n')
                    )
                },
                |else_branch| {
                    if let Reference {
                        reference: Statement::If(else_if),
                        offset,
                    } = else_branch.as_ref()
                    {
                        format!(
                            "if ({}){}else {}",
                            condition,
                            fmt_branch(&self.if_branch, tokens, f, ' '),
                            else_if.fmt(&tokens[*offset..], f),
                        )
                    } else {
                        format!(
                            "if ({}){}else{}",
                            condition,
                            fmt_branch(&self.if_branch, tokens, f, ' '),
                            fmt_branch(&self.else_branch, tokens, f, '\n'),
                        )
                    }
                },
            );
            add_leading_comments(stmt, self.info.slice(tokens))
        }
    }

    impl Format for WhileStatement {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            let condition = fmt_ref_or_empty(&self.condition, tokens, f);
            let stmt = format!(
                "while ({}){}",
                condition,
                fmt_branch(&self.statement, tokens, f, '\n')
            );
            add_leading_comments(stmt, self.info.slice(tokens))
        }
    }

    fn fmt_branch(
        branch: &Option<Box<Reference<Statement>>>,
        tokens: &[Token],
        f: &FormattingOptions,
        ending: char,
    ) -> String {
        branch.as_ref().map_or_else(
            || format!("{}", ending),
            |stmt| {
                let tokens = &tokens[stmt.offset..];
                if let Statement::Block(b) = stmt.as_ref().as_ref() {
                    // similar to `BlockStatement::fmt`,
                    // except that a space is inserted before the brackets
                    // and if there are statements inside the block,
                    // `ending` is appended instead of a newline
                    if b.statements.is_empty() {
                        " {}\n".to_string()
                    } else {
                        let stmts: String = b
                            .statements
                            .iter()
                            .map(|stmt| stmt.fmt(&tokens[stmt.offset..], f))
                            .collect();
                        let stmts = indent(stmts, f);
                        format!(" {{\n{}}}{}", stmts, ending)
                    }
                } else {
                    let stmt = indent(stmt.fmt(tokens, f), f);
                    format!("\n{}", stmt)
                }
            },
        )
    }

    impl Format for ParameterDeclaration {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            match self {
                Self::Valid {
                    is_ref,
                    name,
                    type_expr,
                    ..
                } => {
                    let r#ref = if *is_ref { "ref " } else { "" };
                    let name = fmt_or_empty(name, tokens, f);
                    let type_expr = fmt_ref_or_empty(type_expr, tokens, f);
                    format!("{}{}: {}", r#ref, name, type_expr)
                }
                Self::Error(info) => info.fmt(tokens, f),
            }
        }
    }

    impl Format for Identifier {
        fn fmt(&self, _: &[Token], _: &FormattingOptions) -> String {
            self.to_string()
        }
    }

    impl Format for IntLiteral {
        fn fmt(&self, tokens: &[Token], _: &FormattingOptions) -> String {
            self.info
                .slice(tokens)
                .iter()
                .find_map(|token| match &token.token_type {
                    TokenType::Int(_) | TokenType::Hex(_) | TokenType::Char(_) => {
                        Some(token.to_string())
                    }
                    _ => None,
                })
                .expect("IntLiteral must contain a `Int`, `Hex` or `Char` token")
        }
    }

    impl Format for AstInfo {
        fn fmt(&self, tokens: &[Token], _: &FormattingOptions) -> String {
            self.slice(tokens)
                .iter()
                .map(|token| token.to_string())
                .reduce(|acc, token| {
                    if acc.ends_with('\n') {
                        acc + &token
                    } else {
                        acc + " " + &token
                    }
                })
                .unwrap_or_default()
        }
    }

    impl Format for ArrayAccess {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            let index = self
                .index
                .as_ref()
                .map_or_else(String::new, |inner| inner.fmt(&tokens[inner.offset..], f));
            format!("{}[{}]", self.array.fmt(tokens, f), index)
        }
    }

    impl Format for Variable {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            match self {
                Self::NamedVariable(ident) => ident.to_string(),
                Self::ArrayAccess(access) => access.fmt(tokens, f),
            }
        }
    }

    impl Format for BinaryExpression {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            format!(
                "{} {} {}",
                self.lhs.fmt(tokens, f),
                self.operator,
                self.rhs.fmt(tokens, f)
            )
        }
    }

    impl Format for BracketedExpression {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            format!("({})", self.expr.fmt(tokens, f))
        }
    }

    impl Format for UnaryExpression {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            format!("{}{}", self.operator, self.expr.fmt(tokens, f))
        }
    }

    impl Format for Expression {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            match self {
                Self::Binary(binary) => binary.fmt(tokens, f),
                Self::Bracketed(bracketed) => bracketed.fmt(tokens, f),
                Self::IntLiteral(int_lit) => int_lit.fmt(tokens, f),
                Self::Unary(unary) => unary.fmt(tokens, f),
                Self::Variable(var) => var.fmt(tokens, f),
                Self::Error(info) => info.fmt(tokens, f),
            }
        }
    }

    impl Format for TypeExpression {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            match self {
                Self::NamedType(ident) => ident.to_string(),
                Self::ArrayType {
                    size, base_type, ..
                } => {
                    let size = fmt_or_empty(size, tokens, f);
                    base_type.as_ref().map_or_else(
                        || format!("array [{}] of", size),
                        |type_expr| {
                            format!(
                                "array [{}] of {}",
                                size,
                                type_expr.fmt(&tokens[type_expr.offset..], f)
                            )
                        },
                    )
                }
            }
        }
    }

    impl Format for VariableDeclaration {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            match self {
                Self::Valid {
                    name, type_expr, ..
                } => {
                    let name = fmt_or_empty(name, tokens, f);
                    let type_expr = fmt_ref_or_empty(type_expr, tokens, f);
                    format!("var {}: {};\n", name, type_expr)
                }
                Self::Error(info) => info.fmt(tokens, f),
            }
        }
    }

    impl Format for Assignment {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            let expr = fmt_ref_or_empty(&self.expr, tokens, f);
            format!("{} := {};\n", self.variable.fmt(tokens, f), expr)
        }
    }

    impl Format for CallStatement {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            let args = self
                .arguments
                .iter()
                .map(|arg| arg.fmt(&tokens[arg.offset..], f))
                .reduce(|acc, arg| acc + ", " + arg.as_str())
                .unwrap_or_default();
            format!("{}({});\n", self.name, args)
        }
    }

    impl<T: Format> Format for Reference<T> {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            self.as_ref().fmt(tokens, f)
        }
    }

    impl<T: Format> Format for Box<T> {
        fn fmt(&self, tokens: &[Token], f: &FormattingOptions) -> String {
            self.as_ref().fmt(tokens, f)
        }
    }

    fn fmt_ref_or_empty<T: Format>(
        opt: &Option<Reference<T>>,
        tokens: &[Token],
        f: &FormattingOptions,
    ) -> String {
        opt.as_ref()
            .map_or_else(String::new, |inner| inner.fmt(&tokens[inner.offset..], f))
    }

    fn fmt_or_empty<T: Format>(opt: &Option<T>, tokens: &[Token], f: &FormattingOptions) -> String {
        opt.as_ref()
            .map_or_else(String::new, |inner| inner.fmt(tokens, f))
    }

}
