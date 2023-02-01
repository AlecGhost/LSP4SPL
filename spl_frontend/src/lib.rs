#![allow(dead_code)]
use error::SplError;
use std::{cell::RefCell, rc::Rc};

pub mod ast;
pub mod error;
pub mod parser;
pub mod table;

pub trait DiagnosticsBroker: Clone + std::fmt::Debug {
    fn report_error(&self, error: SplError);
}

#[derive(Debug)]
pub struct LocalBroker(Rc<RefCell<Vec<SplError>>>);

impl Clone for LocalBroker {
    fn clone(&self) -> Self {
        LocalBroker(Rc::clone(&self.0))
    }
}

impl LocalBroker {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(Vec::new())))
    }

    pub fn errors(&self) -> Vec<SplError> {
        self.0.borrow().clone()
    }
}

impl DiagnosticsBroker for LocalBroker {
    fn report_error(&self, error: SplError) {
        self.0.borrow_mut().push(error);
    }
}
