use crate::{
    error::{SemanticErrorMessage, SplError},
    lexer, parser, table, LocalBroker,
};
#[cfg(test)]
use pretty_assertions::assert_eq as eq;

fn test(src: &str) -> Vec<SplError> {
    eprintln!("Testing: {}", src);
    let lex_broker = LocalBroker::default();
    let tokens = lexer::lex(src, lex_broker.clone());
    eq!(lex_broker.errors(), Vec::new(), "lexing failed");
    let parse_broker = LocalBroker::default();
    let program = parser::parse(&tokens, parse_broker.clone());
    eq!(parse_broker.errors(), Vec::new(), "parsing failed");
    let build_broker = LocalBroker::default();
    let table = table::build(&program, &build_broker);
    eq!(build_broker.errors(), Vec::new(), "building failed");
    let semantic_broker = LocalBroker::default();
    table::semantic::analyze(&program, &table, &semantic_broker);
    semantic_broker.errors()
}

mod complete;
mod arrays;
mod call_statements;
mod comparisons;
mod expressions;
