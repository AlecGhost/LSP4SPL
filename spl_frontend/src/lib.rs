#![allow(dead_code)]
pub mod parser;
pub mod ast;

pub trait DiagnosticsBroker<E> {
    fn report_error(&self, error: E);
}

