#![warn(clippy::nursery)]
#![allow(clippy::redundant_pub_crate)]
use ast::Program;
use error::SplError;
use lexer::token::Token;
use std::ops::Range;
use table::GlobalTable;

pub mod ast;
pub mod error;
mod lexer;
mod parser;
pub use lexer::token;
pub mod table;

pub trait ToRange {
    fn to_range(&self) -> Range<usize>;
}

pub trait ToTextRange {
    fn to_text_range(&self, tokens: &[Token]) -> Range<usize>;
}

pub trait ErrorContainer {
    fn errors(&self) -> Vec<SplError>;
}

pub trait Shiftable {
    fn shift(self, offset: usize) -> Self;
}

impl Shiftable for Range<usize> {
    fn shift(self, offset: usize) -> Self {
        (self.start + offset)..(self.end + offset)
    }
}

/// Change in source code.
/// `range` is the text range in the original source code.
/// `text` is the replacement text.
#[derive(Clone, Debug)]
pub struct Change {
    pub range: Range<usize>,
    pub text: String,
}

/// Contains all information extracted from a given SPL source file.
#[derive(Clone, Debug)]
pub struct AnalyzedSource {
    pub text: String,
    pub tokens: Vec<Token>,
    pub ast: Program,
    pub table: GlobalTable,
}

impl AnalyzedSource {
    pub fn new(text: String) -> Self {
        let tokens = lexer::lex(&text);
        let mut program = parser::parse(&tokens);
        let table = table::build(&mut program);
        table::analyze(&mut program, &table);
        Self {
            text,
            tokens,
            ast: program,
            table,
        }
    }

    pub fn update(&mut self, changes: Vec<Change>) {
        // Temporary implementation
        let mut new_text = self.text.clone();
        changes
            .into_iter()
            .for_each(|change| new_text.replace_range(change.range, &change.text));
        *self = AnalyzedSource::new(new_text);
    }
}

impl ErrorContainer for AnalyzedSource {
    fn errors(&self) -> Vec<SplError> {
        let token_ranged_errors = self.ast.errors();
        token_ranged_errors
            .into_iter()
            .map(|error| match error.to_range() {
                range if range.len() == 0 => {
                    let token = &self.tokens[range.end];
                    let end_pos = token.range.end;
                    SplError(end_pos..end_pos, error.1)
                }
                range => {
                    let tokens = &self.tokens[range];
                    let start_pos = tokens.first().expect("Token slice is empty").range.start;
                    let end_pos = tokens.last().expect("Token slice is empty").range.end;
                    SplError(start_pos..end_pos, error.1)
                }
            })
            .collect()
    }
}
