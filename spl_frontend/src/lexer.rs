use crate::error::{LexErrorMessage, SplError};
use crate::{DiagnosticsBroker, ToRange};
use nom::combinator::{eof, peek};
use nom::sequence::{pair, terminated};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till},
    character::complete::{alpha1, anychar, digit1, hex_digit1, multispace0},
    combinator::map,
    multi::many0,
    sequence::preceded,
};
use std::ops::Range;
use token::{Token, TokenType};
use utility::{alpha_numeric0, expect, is_alpha_numeric, verify};

#[cfg(test)]
mod tests;
pub mod token;
mod utility;

pub(crate) type Span<'a, B> = nom_locate::LocatedSpan<&'a str, B>;

impl<B: DiagnosticsBroker> ToRange for Span<'_, B> {
    fn to_range(&self) -> Range<usize> {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        start..end
    }
}

type IResult<'a, B> = nom::IResult<Span<'a, B>, Token>;

pub fn lex<B: DiagnosticsBroker>(src: &str, broker: B) -> Vec<Token> {
    let input = Span::new_extra(src, broker);
    let (_, (mut tokens, eof_token)) = pair(
        many0(preceded(multispace0, Token::lex)),
        preceded(multispace0, Eof::lex),
    )(input)
    .expect("Lexing must not fail.");
    tokens.push(eof_token);
    tokens
}

trait Lexer<B>: Sized {
    fn lex(input: Span<B>) -> IResult<B>;
}

macro_rules! lex_symbol {
    ($token_type:expr) => {{
        map(tag($token_type.as_static_str()), |span: Span<B>| -> Token {
            Token::new($token_type, span.to_range())
        })
    }};
}

macro_rules! lex_keyword {
    ($token_type:expr) => {{
        terminated(
            map(tag($token_type.as_static_str()), |span: Span<B>| -> Token {
                Token::new($token_type, span.to_range())
            }),
            peek(alt((
                eof,
                verify(take(1u8), |span| !span.starts_with(is_alpha_numeric)),
            ))),
        )
    }};
}

impl<B: DiagnosticsBroker> Lexer<B> for Token {
    fn lex(input: Span<B>) -> IResult<B> {
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
impl<B: DiagnosticsBroker> Lexer<B> for Ident {
    fn lex(input: Span<B>) -> IResult<B> {
        let start = input.location_offset();
        let (input, first_letter) = alt((alpha1, tag("_")))(input)?;
        let (input, rest) = alpha_numeric0(input)?;
        let end = input.location_offset();
        let ident = String::new() + *first_letter + *rest;
        Ok((input, Token::new(TokenType::Ident(ident), start..end)))
    }
}

struct Char;
impl<B: DiagnosticsBroker> Lexer<B> for Char {
    fn lex(input: Span<B>) -> IResult<B> {
        let start = input.location_offset();
        let (input, _) = tag("'")(input)?;
        let (input, c) = alt((map(tag("\\n"), |_| '\n'), anychar))(input)?;
        let pos = input.location_offset();
        let (input, _) = expect(tag("'"), LexErrorMessage::MissingClosingTick, pos)(input)?;
        let end = input.location_offset();
        Ok((input, Token::new(TokenType::Char(c), start..end)))
    }
}

struct Int;
impl<B: DiagnosticsBroker> Lexer<B> for Int {
    fn lex(input: Span<B>) -> IResult<B> {
        let (input, int_span) = digit1(input)?;
        let result = match int_span.parse() {
            Ok(value) => Ok(value),
            Err(_) => {
                input.extra.report_error(SplError(
                    int_span.to_range(),
                    LexErrorMessage::InvalidIntLit(int_span.to_string()).to_string(),
                ));
                Err(int_span.to_string())
            }
        };
        Ok((
            input,
            Token::new(TokenType::Int(result), int_span.to_range()),
        ))
    }
}

struct Hex;
impl<B: DiagnosticsBroker> Lexer<B> for Hex {
    fn lex(input: Span<B>) -> IResult<B> {
        let start = input.location_offset();
        let (input, _) = tag("0x")(input)?;
        let pos = input.location_offset();
        let (input, opt_hex) = expect(hex_digit1, LexErrorMessage::ExpectedHexNumber, pos)(input)?;
        let result = if let Some(hex_span) = opt_hex {
            match u32::from_str_radix(&hex_span, 16) {
                Ok(value) => Ok(value),
                Err(_) => {
                    input.extra.report_error(SplError(
                        hex_span.to_range(),
                        LexErrorMessage::InvalidIntLit("0x".to_string() + &hex_span).to_string(),
                    ));
                    Err(hex_span.to_string())
                }
            }
        } else {
            Err(String::new())
        };
        let end = input.location_offset();
        Ok((input, Token::new(TokenType::Hex(result), start..end)))
    }
}

struct Comment;
impl<B: DiagnosticsBroker> Lexer<B> for Comment {
    fn lex(input: Span<B>) -> IResult<B> {
        let start = input.location_offset();
        let (input, comment) = preceded(tag("//"), take_till(|c| c == '\n'))(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Token::new(TokenType::Comment(comment.to_string()), start..end),
        ))
    }
}

struct Unknown;
impl<B: DiagnosticsBroker> Lexer<B> for Unknown {
    fn lex(input: Span<B>) -> IResult<B> {
        map(take(1u8), |span: Span<B>| {
            Token::new(TokenType::Unknown(span.to_string()), span.to_range())
        })(input)
    }
}

struct Eof;
impl<B: DiagnosticsBroker> Lexer<B> for Eof {
    fn lex(input: Span<B>) -> IResult<B> {
        map(eof, |span: Span<B>| {
            Token::new(TokenType::Eof, span.to_range())
        })(input)
    }
}
