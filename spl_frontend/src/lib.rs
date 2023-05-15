#![warn(clippy::nursery)]
#![allow(clippy::redundant_pub_crate)]
use ast::Program;
use error::SplError;
use lexer::token::Token;
use std::{cell::RefCell, ops::Range, rc::Rc};
use table::GlobalTable;

pub mod ast;
pub mod error;
mod lexer;
mod parser;
pub use lexer::token;
pub mod table;

trait DiagnosticsBroker: Clone + std::fmt::Debug {
    fn report_error(&self, error: SplError);
}

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
        let broker = LocalBroker::default();
        let tokens = lexer::lex(&text, broker.clone());
        let mut program = parser::parse(&tokens, broker.clone());
        let table = table::build(&mut program, &broker);
        table::analyze(&mut program, &table, &broker);
        Self {
            text,
            tokens,
            ast: program,
            table,
        }
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

#[derive(Debug, Default)]
struct LocalBroker(Rc<RefCell<Vec<SplError>>>);

impl Clone for LocalBroker {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl LocalBroker {
    pub fn errors(&self) -> Vec<SplError> {
        self.0.borrow().clone()
    }
}

impl DiagnosticsBroker for LocalBroker {
    fn report_error(&self, error: SplError) {
        self.0.borrow_mut().push(error);
    }
}
