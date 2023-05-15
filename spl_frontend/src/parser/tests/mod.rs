use crate::{
    ast::*,
    error::{ParseErrorMessage, SplError},
    lexer::{
        lex,
        token::{Token, TokenStream},
    },
    parser::{eof, Parser},
    LocalBroker,
};
use nom::{combinator::all_consuming, sequence::terminated};
#[cfg(test)]
use pretty_assertions::assert_eq as eq;
use std::ops::Range;

trait ToTokens<B> {
    fn to_tokens(&self) -> TokenStream<B>;
}

impl ToTokens<LocalBroker> for Vec<Token> {
    fn to_tokens(&self) -> TokenStream<LocalBroker> {
        TokenStream::new(self, LocalBroker::default())
    }
}

fn int_lit(value: u32, range: Range<usize>) -> Box<Expression> {
    Box::new(Expression::IntLiteral(IntLiteral::new(
        value,
        AstInfo::new(range),
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
