#![warn(clippy::nursery)]
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

/// Contains all information extracted from a given SPL source file.
#[derive(Clone, Debug)]
pub struct AnalyzedSource {
    pub text: String,
    pub tokens: Vec<Token>,
    pub ast: Program,
    pub table: GlobalTable,
    pub errors: Vec<SplError>,
}

impl AnalyzedSource {
    pub fn new(text: String) -> Self {
        let broker = LocalBroker::default();
        let tokens = lexer::lex(&text, broker.clone());
        let program = parser::parse(&tokens, broker.clone());
        let table = table::build(&program, &broker);
        table::analyze(&program, &table, &broker);
        let errors = broker.errors();
        Self {
            text,
            tokens,
            ast: program,
            table,
            errors,
        }
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
