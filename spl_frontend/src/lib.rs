#![allow(dead_code)]
pub mod ast;
pub mod parser;
pub mod table;

pub trait DiagnosticsBroker<E> {
    fn report_error(&self, error: E);
}
