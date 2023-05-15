use crate::{
    ast::Identifier,
    error::{BuildErrorMessage, SplError},
    table::{
        DataType, GlobalEntry, GlobalTable, LocalEntry, LocalTable, ProcedureEntry, TypeEntry,
        VariableEntry,
    },
    ErrorContainer,
};
#[cfg(test)]
use pretty_assertions::assert_eq as eq;
use std::collections::HashMap;

fn test(src: &str) -> (GlobalTable, Vec<SplError>) {
    eprintln!("Testing: {}", src);
    let tokens = crate::lexer::lex(src);
    let mut program = crate::parser::parse(&tokens);
    let table = crate::table::build(&mut program);
    let errors = program.errors();
    (table, errors)
}

mod main;
mod redeclaration;
mod type_declarations;
