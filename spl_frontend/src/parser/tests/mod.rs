use crate::{
    ast::*,
    error::*,
    lexer::{
        lex,
        token::{Token, Tokens},
    },
    parser::{eof, Parser},
    LocalBroker,
};
use nom::{combinator::all_consuming, sequence::terminated};
#[cfg(test)]
use pretty_assertions::assert_eq as eq;

trait ToTokens<B> {
    fn to_tokens(&self) -> Tokens<B>;
}

impl ToTokens<LocalBroker> for Vec<Token> {
    fn to_tokens(&self) -> Tokens<LocalBroker> {
        Tokens::new(self, LocalBroker::default())
    }
}

fn int_lit(value: u32, tokens: &[Token]) -> Box<Expression> {
    Box::new(Expression::IntLiteral(IntLiteral::new(
        value,
        AstInfo::new(tokens),
    )))
}

mod acker;
mod assignments;
mod call_statements;
mod expressions;
mod idents;
mod if_statements;
mod keywords;
mod type_declarations;
