use crate::{
    ast::*,
    error::{ParseErrorMessage, SplError},
    lexer::{
        lex,
        token::{Token, TokenStream},
    },
    parser::{markers::eof, Parser},
};
use nom::{combinator::all_consuming, sequence::terminated};
#[cfg(test)]
use pretty_assertions::assert_eq as eq;
use std::ops::Range;

trait ToTokens {
    fn to_tokens(&self) -> TokenStream;
}

impl ToTokens for Vec<Token> {
    fn to_tokens(&self) -> TokenStream {
        TokenStream::new(self)
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
mod incremental;
mod keywords;
mod type_declarations;
