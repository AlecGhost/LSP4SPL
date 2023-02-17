use std::ops::Range;

use super::ToRange;
use nom::combinator::{eof, peek};
use nom::sequence::terminated;
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till, take_while},
    character::{
        complete::{alpha1, anychar, digit1, hex_digit1, multispace0},
        is_alphanumeric,
    },
    combinator::map,
    error::ErrorKind,
    multi::many0,
    sequence::preceded,
};
use token::{Token, TokenType};

#[cfg(test)]
mod tests;
pub mod token;

pub(crate) type Span<'a> = nom_locate::LocatedSpan<&'a str>;

impl ToRange for Span<'_> {
    fn to_range(&self) -> Range<usize> {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        start..end
    }
}

type IResult<'a, T> = nom::IResult<Span<'a>, T>;

pub fn lex(input: &str) -> Vec<Token> {
    let span = Span::new(input);
    let (input, mut tokens) =
        many0(preceded(multispace0, Token::lex))(span).expect("Lexing must not fail.");
    let (_, eof_token) = preceded(multispace0, Eof::lex)(input).expect("Lexing must not fail.");
    tokens.push(eof_token);
    tokens
}

trait Lexer: Sized {
    fn lex(input: Span) -> IResult<Token>;
}

macro_rules! lex_symbol {
    ($token_type:expr) => {{
        map(tag($token_type.as_static_str()), |span: Span| -> Token {
            Token::new($token_type, span.to_range())
        })
    }};
}

macro_rules! lex_keyword {
    ($token_type:expr) => {{
        terminated(
            map(tag($token_type.as_static_str()), |span: Span| -> Token {
                Token::new($token_type, span.to_range())
            }),
            peek(alt((
                eof,
                verify(take(1u8), |span| !span.starts_with(is_alpha_numeric)),
            ))),
        )
    }};
}

impl Lexer for Token {
    fn lex(input: Span) -> IResult<Self> {
        alt((
            Comment::lex,
            alt((
                lex_symbol!(TokenType::LParen),
                lex_symbol!(TokenType::RParen),
                lex_symbol!(TokenType::RParen),
                lex_symbol!(TokenType::LBracket),
                lex_symbol!(TokenType::RBracket),
                lex_symbol!(TokenType::LCurly),
                lex_symbol!(TokenType::RCurly),
                lex_symbol!(TokenType::Eq),
                lex_symbol!(TokenType::Neq),
                lex_symbol!(TokenType::Le),
                lex_symbol!(TokenType::Lt),
                lex_symbol!(TokenType::Ge),
                lex_symbol!(TokenType::Gt),
                lex_symbol!(TokenType::Assign),
                lex_symbol!(TokenType::Colon),
                lex_symbol!(TokenType::Comma),
                lex_symbol!(TokenType::Semic),
                lex_symbol!(TokenType::Plus),
                lex_symbol!(TokenType::Minus),
                lex_symbol!(TokenType::Times),
                lex_symbol!(TokenType::Divide),
            )),
            alt((
                lex_keyword!(TokenType::If),
                lex_keyword!(TokenType::Else),
                lex_keyword!(TokenType::While),
                lex_keyword!(TokenType::Array),
                lex_keyword!(TokenType::Of),
                lex_keyword!(TokenType::Proc),
                lex_keyword!(TokenType::Ref),
                lex_keyword!(TokenType::Type),
                lex_keyword!(TokenType::Var),
            )),
            alt((Char::lex, Hex::lex, Int::lex, Ident::lex)),
            Unknown::lex,
        ))(input)
    }
}

struct Ident;
impl Lexer for Ident {
    fn lex(input: Span) -> IResult<Token> {
        let start = input.location_offset();
        let (input, first_letter) = alt((alpha1, tag("_")))(input)?;
        let (input, rest) = alpha_numeric0(input)?;
        let end = input.location_offset();
        let ident = String::new() + *first_letter + *rest;
        Ok((input, Token::new(TokenType::Ident(ident), start..end)))
    }
}

struct Char;
impl Lexer for Char {
    fn lex(input: Span) -> IResult<Token> {
        let start = input.location_offset();
        let (input, _) = tag("'")(input)?;
        let (input, c) = alt((map(tag("\\n"), |_| '\n'), anychar))(input)?;
        let (input, _) = tag("'")(input)?;
        let end = input.location_offset();
        Ok((input, Token::new(TokenType::Char(c), start..end)))
    }
}

struct Int;
impl Lexer for Int {
    fn lex(input: Span) -> IResult<Token> {
        let (input, span) = digit1(input)?;
        let int = span.parse().expect("Parsing digit failed");
        Ok((input, Token::new(TokenType::Int(int), span.to_range())))
    }
}

struct Hex;
impl Lexer for Hex {
    fn lex(input: Span) -> IResult<Token> {
        let start = input.location_offset();
        let (input, hex) = preceded(tag("0x"), hex_digit1)(input)?;
        let value = u32::from_str_radix(&hex, 16).expect("Parsing hex digit failed");
        let end = input.location_offset();
        Ok((input, Token::new(TokenType::Hex(value), start..end)))
    }
}

struct Comment;
impl Lexer for Comment {
    fn lex(input: Span) -> IResult<Token> {
        let start = input.location_offset();
        let (input, _) = tag("//")(input)?;
        let (input, comment) = take_till(|c| c == '\n')(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Token::new(TokenType::Comment(comment.to_string()), start..end),
        ))
    }
}

struct Unknown;
impl Lexer for Unknown {
    fn lex(input: Span) -> IResult<Token> {
        map(take(1u8), |span: Span| {
            Token::new(TokenType::Unknown(span.to_string()), span.to_range())
        })(input)
    }
}

struct Eof;
impl Lexer for Eof {
    fn lex(input: Span) -> IResult<Token> {
        map(eof, |span: Span| {
            Token::new(TokenType::Eof, span.to_range())
        })(input)
    }
}

/// Parser for alphanumeric characters or underscores
fn alpha_numeric0(input: Span) -> IResult<Span> {
    take_while(is_alpha_numeric)(input)
}

fn is_alpha_numeric(c: char) -> bool {
    is_alphanumeric(c as u8) || c == '_'
}

/// Tries to parse the input with the given parser.
/// If parsing succeeds and the output matches the given verification function,
/// the result is returned.
fn verify<'a, O, F, G>(mut parser: F, verification: G) -> impl FnMut(Span<'a>) -> IResult<O>
where
    F: FnMut(Span<'a>) -> IResult<'a, O>,
    G: Fn(&O) -> bool,
{
    move |input: Span| match parser(input) {
        Ok((input, out)) => {
            if verification(&out) {
                Ok((input, out))
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Verify,
                )))
            }
        }
        Err(err) => Err(err),
    }
}
