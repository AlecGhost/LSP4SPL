use crate::{
    ast::Identifier,
    error::{BuildErrorMessage, SplError},
    lexer::token::Token,
    table::{
        DataType, GlobalEntry, GlobalTable, LocalEntry, LocalTable, ProcedureEntry, TypeEntry,
        VariableEntry,
    },
    LocalBroker,
};
#[cfg(test)]
use pretty_assertions::assert_eq as eq;
use std::collections::HashMap;

fn test(src: &str) -> (GlobalTable, Vec<Token>, LocalBroker) {
    eprintln!("Testing: {}", src);
    let lex_broker = LocalBroker::default();
    let tokens = crate::lexer::lex(src, lex_broker.clone());
    eq!(lex_broker.errors(), Vec::new(), "lexing failed");
    let parse_broker = LocalBroker::default();
    let program = crate::parser::parse(&tokens, parse_broker.clone());
    eq!(parse_broker.errors(), Vec::new(), "parsing failed");
    let broker = LocalBroker::default();
    let table = crate::table::build(&program, &broker);
    (table, tokens, broker)
}

mod main;
mod redeclaration;
mod type_declarations;
