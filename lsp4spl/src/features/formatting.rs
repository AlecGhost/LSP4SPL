use crate::document::{convert_range, DocumentRequest};
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
    if let Some(AnalyzedSource { ast, text, .. }) = super::get_doc_info(uri, doctx).await? {
        let formatting_options = if options.insert_spaces {
            fmt::FormattingOptions::new(' ', options.tab_size as usize)
        } else {
            fmt::FormattingOptions::new('\t', 1)
        };
        let new_text = ast.fmt(&formatting_options);
        if new_text == text {
            Ok(None)
        } else {
            let range = convert_range(&(0..text.len()), &text);
            Ok(Some(vec![TextEdit { range, new_text }]))
        }
    } else {
        Ok(None)
    }
}

mod fmt {
    use spl_frontend::{
        ast::{
            BlockStatement, GlobalDeclaration, IfStatement, ProcedureDeclaration, Program,
            Statement, TypeDeclaration, WhileStatement,
        },
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
            .map_while(|token| match &token.token_type {
                TokenType::Comment(comment) => Some(String::new() + "// " + comment.trim() + "\n"),
                _ => None,
            })
            .collect::<String>()
            + &text
    }

    fn add_all_comments(text: String, tokens: &[Token]) -> String {
        tokens
            .iter()
            .filter_map(|token| match &token.token_type {
                TokenType::Comment(comment) => Some(String::new() + "// " + comment.trim() + "\n"),
                _ => None,
            })
            .collect::<String>()
            + &text
    }

    pub trait Format {
        fn fmt(&self, f: &FormattingOptions) -> String;
    }

    impl Format for Program {
        fn fmt(&self, f: &FormattingOptions) -> String {
            self.global_declarations
                .iter()
                .map(|gd| gd.fmt(f))
                .reduce(|acc, gd| acc + "\n" + &gd)
                .unwrap_or_default()
        }
    }

    impl Format for GlobalDeclaration {
        fn fmt(&self, f: &FormattingOptions) -> String {
            match self {
                Self::Type(td) => td.fmt(f),
                Self::Procedure(pd) => pd.fmt(f),
                Self::Error(info) => info
                    .tokens
                    .iter()
                    .map(|token| token.to_string())
                    .reduce(|acc, token| {
                        if acc.ends_with('\n') {
                            acc + &token
                        } else {
                            acc + " " + &token
                        }
                    })
                    .unwrap_or_default(),
            }
        }
    }

    impl Format for ProcedureDeclaration {
        fn fmt(&self, f: &FormattingOptions) -> String {
            let name = fmt_or_empty(&self.name);
            let param_vec: Vec<String> = self
                .parameters
                .iter()
                .map(|param| add_all_comments(param.to_string(), &param.info.tokens))
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
                .map(|var_dec| add_all_comments(var_dec.to_string(), &var_dec.info.tokens))
                .collect();
            let var_decs = indent(var_decs, f);
            let stmts: String = self.statements.iter().map(|stmt| stmt.fmt(f)).collect();
            let stmts = indent(stmts, f);
            let pd = match (var_decs.is_empty(), stmts.is_empty()) {
                (true, true) => format!("proc {}({}) {{}}\n", name, params),
                (true, false) => format!("proc {}({}) {{\n{}}}\n", name, params, stmts),
                (false, true) => format!("proc {}({}) {{\n{}}}\n", name, params, var_decs),
                (false, false) => {
                    format!("proc {}({}) {{\n{}\n{}}}\n", name, params, var_decs, stmts)
                }
            };
            add_leading_comments(pd, &self.info.tokens)
        }
    }

    impl Format for TypeDeclaration {
        fn fmt(&self, _f: &FormattingOptions) -> String {
            let type_expr = fmt_or_empty(&self.type_expr);
            let td = self.name.as_ref().map_or_else(
                || format!("type = {};\n", type_expr),
                |name| format!("type {} = {};\n", name, type_expr),
            );
            add_leading_comments(td, &self.info.tokens)
        }
    }

    impl Format for Statement {
        fn fmt(&self, f: &FormattingOptions) -> String {
            match self {
                Self::Assignment(a) => add_all_comments(a.to_string(), &a.info.tokens),
                Self::Block(b) => b.fmt(f),
                Self::Call(c) => add_all_comments(c.to_string(), &c.info.tokens),
                Self::If(i) => i.fmt(f),
                Self::While(w) => w.fmt(f),
                Self::Empty(info) => add_all_comments(";\n".to_string(), &info.tokens),
                Self::Error(_) => String::new(),
            }
        }
    }

    impl Format for BlockStatement {
        fn fmt(&self, f: &FormattingOptions) -> String {
            let stmt = if self.statements.is_empty() {
                "{}\n".to_string()
            } else {
                let stmts: String = self.statements.iter().map(|stmt| stmt.fmt(f)).collect();
                let stmts = indent(stmts, f);
                format!("{{\n{}}}\n", stmts)
            };
            add_leading_comments(stmt, &self.info.tokens)
        }
    }

    impl Format for IfStatement {
        fn fmt(&self, f: &FormattingOptions) -> String {
            let condition = fmt_or_empty(&self.condition);
            let stmt = if self.else_branch.is_some() {
                format!(
                    "if ({}){}else{}",
                    condition,
                    fmt_branch(f, &self.if_branch, ' '),
                    fmt_branch(f, &self.else_branch, '\n'),
                )
            } else {
                format!("if ({}){}", condition, fmt_branch(f, &self.if_branch, '\n'))
            };
            add_leading_comments(stmt, &self.info.tokens)
        }
    }

    impl Format for WhileStatement {
        fn fmt(&self, f: &FormattingOptions) -> String {
            let condition = fmt_or_empty(&self.condition);
            let stmt = format!(
                "while ({}){}",
                condition,
                fmt_branch(f, &self.statement, '\n')
            );
            add_leading_comments(stmt, &self.info.tokens)
        }
    }

    fn fmt_branch(f: &FormattingOptions, branch: &Option<Box<Statement>>, ending: char) -> String {
        branch.as_ref().map_or_else(
            || format!("{}", ending),
            |stmt| {
                if let Statement::Block(b) = stmt.as_ref() {
                    // similar to `BlockStatement::fmt`,
                    // except that a space is inserted before the brackets
                    // and if there are statements inside the block,
                    // `ending` is appended instead of a newline
                    if b.statements.is_empty() {
                        " {}\n".to_string()
                    } else {
                        let stmts: String = b.statements.iter().map(|stmt| stmt.fmt(f)).collect();
                        let stmts = indent(stmts, f);
                        format!(" {{\n{}}}{}", stmts, ending)
                    }
                } else {
                    let stmt = indent(stmt.fmt(f), f);
                    format!("\n{}", stmt)
                }
            },
        )
    }

    fn fmt_or_empty<T: std::fmt::Display>(opt: &Option<T>) -> String {
        opt.as_ref()
            .map_or_else(String::new, |inner| inner.to_string())
    }
}
