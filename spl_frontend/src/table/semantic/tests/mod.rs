use crate::{
    error::{SemanticErrorMessage, SplError},
    lexer, parser, table, ErrorContainer,
};

#[cfg(test)]
use pretty_assertions::assert_eq as eq;

fn test(src: &str) -> Vec<SplError> {
    eprintln!("Testing: {}", src);
    let tokens = lexer::lex(src);
    let mut program = parser::parse(&tokens);
    let table = table::build(&mut program);
    table::semantic::analyze(&mut program, &table);
    program.errors()
}

mod arrays;
mod call_statements;
mod comparisons;
mod complete;
mod expressions;
