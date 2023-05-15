use crate::{
    ast::Identifier,
    error::{BuildErrorMessage, SplError},
    table::{
        DataType, GlobalEntry, GlobalTable, LocalEntry, LocalTable, ProcedureEntry, TypeEntry,
        VariableEntry,
    },
    ErrorContainer, LocalBroker,
};
#[cfg(test)]
use pretty_assertions::assert_eq as eq;
use std::collections::HashMap;

fn test(src: &str) -> (GlobalTable, Vec<SplError>) {
    eprintln!("Testing: {}", src);
    let lex_broker = LocalBroker::default();
    let tokens = crate::lexer::lex(src, lex_broker.clone());
    eq!(lex_broker.errors(), Vec::new(), "lexing failed");
    let parse_broker = LocalBroker::default();
    let mut program = crate::parser::parse(&tokens, parse_broker.clone());
    eq!(parse_broker.errors(), Vec::new(), "parsing failed");
    let broker = LocalBroker::default();
    let table = crate::table::build(&mut program, &broker);
    let errors = program.errors();
    (table, errors)
}

mod main;
mod redeclaration;
mod type_declarations;
