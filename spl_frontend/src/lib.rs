#![warn(clippy::nursery)]
use error::SplError;
use std::{cell::RefCell, ops::Range, rc::Rc};

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod table;

pub trait DiagnosticsBroker: Clone + std::fmt::Debug {
    fn report_error(&self, error: SplError);
}

// source: https://github.com/ebkalderon/example-fault-tolerant-parser/blob/master/src/main.rs
// see also: https://eyalkalderon.com/blog/nom-error-recovery/
pub trait ToRange {
    fn to_range(&self) -> Range<usize>;
}

#[derive(Debug, Default)]
pub struct LocalBroker(Rc<RefCell<Vec<SplError>>>);

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
