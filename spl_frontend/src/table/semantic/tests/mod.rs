use crate::{
    error::{SemanticErrorMessage, SplError},
    lexer, parser, table, ErrorContainer, LocalBroker,
};
#[cfg(test)]
use pretty_assertions::assert_eq as eq;

fn test(src: &str) -> Vec<SplError> {
    eprintln!("Testing: {}", src);
    let lex_broker = LocalBroker::default();
    let tokens = lexer::lex(src, lex_broker.clone());
    eq!(lex_broker.errors(), Vec::new(), "lexing failed");
    let parse_broker = LocalBroker::default();
    let mut program = parser::parse(&tokens, parse_broker.clone());
    eq!(parse_broker.errors(), Vec::new(), "parsing failed");
    let build_broker = LocalBroker::default();
    let table = table::build(&mut program, &build_broker);
    eq!(build_broker.errors(), Vec::new(), "building failed");
    let semantic_broker = LocalBroker::default();
    table::semantic::analyze(&mut program, &table, &semantic_broker);
    program.errors()
}

mod arrays;
mod call_statements;
mod complete;
mod comparisons;
mod expressions;
