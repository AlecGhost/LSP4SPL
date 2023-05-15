use crate::error::{LexErrorMessage, SplError};
use crate::ToRange;
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

use self::token::IntResult;

#[cfg(test)]
mod tests;
pub mod token;
mod utility;

/// Type alias for nom_locate::LocatedSpan.
/// Tracks range inside source code during lexical analysis.
pub type Span<'a> = nom_locate::LocatedSpan<&'a str>;

impl ToRange for Span<'_> {
    fn to_range(&self) -> Range<usize> {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        start..end
    }
}

type IResult<'a> = nom::IResult<Span<'a>, Token>;

/// Tokenizes the given source code.
///
/// # Panics
///
/// Panics if lexing fails.
pub(crate) fn lex(src: &str) -> Vec<Token> {
    let input = Span::new(src);
    let (_, (mut tokens, eof_token)) = pair(
        many0(preceded(multispace0, Token::lex)),
        preceded(multispace0, Eof::lex),
    )(input)
    .expect("Lexing must not fail.");
    tokens.push(eof_token);
    tokens
}

/// Try to parse `Span` into `Token`
trait Lexer: Sized {
    fn lex(input: Span) -> IResult;
}

/// Recognize symbol and map to `Token`
macro_rules! lex_symbol {
    ($token_type:expr) => {{
        map(
            tag($token_type
                .as_static_str()
                .expect("No static representation available")),
            |span: Span| -> Token { Token::new($token_type, span.to_range()) },
        )
    }};
}

/// Recognize keyword and map to `Token`
/// Same as `lex_symbol`, except that the next character must not be alpha_numeric
macro_rules! lex_keyword {
    ($token_type:expr) => {{
        terminated(
            map(
                tag($token_type
                    .as_static_str()
                    .expect("No static representation available")),
                |span: Span| -> Token { Token::new($token_type, span.to_range()) },
            ),
            peek(alt((
                eof,
                verify(take(1u8), |span| !span.starts_with(is_alpha_numeric)),
            ))),
        )
    }};
}

impl Lexer for Token {
    fn lex(input: Span) -> IResult {
        alt((
            Comment::lex,
            alt((
                lex_symbol!(TokenType::LParen),
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
    fn lex(input: Span) -> IResult {
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
    fn lex(input: Span) -> IResult {
        let start = input.location_offset();
        let (input, _) = tag("'")(input)?;
        let (input, c) = alt((map(tag("\\n"), |_| '\n'), anychar))(input)?;
        let pos = input.location_offset();
        let (input, closing_tick) = expect(tag("'"), LexErrorMessage::MissingClosingTick, pos)(input)?;
        let end = input.location_offset();
        match closing_tick {
            Ok(_) => Ok((input, Token::new(TokenType::Char(c), start..end))),
            Err(err) => Ok((
                input,
                Token::new_with_errors(TokenType::Char(c), start..end, vec![err]),
            )),
        }
    }
}

struct Int;
impl Lexer for Int {
    fn lex(input: Span) -> IResult {
        let (input, int_span) = digit1(input)?;
        match int_span.parse() {
            Ok(value) => Ok((
                input,
                Token::new(TokenType::Int(IntResult::Int(value)), int_span.to_range()),
            )),
            Err(_) => {
                let err = SplError(
                    int_span.to_range(),
                    LexErrorMessage::InvalidIntLit(int_span.to_string()).to_string(),
                );
                Ok((
                    input,
                    Token::new_with_errors(
                        TokenType::Int(IntResult::Err(int_span.to_string())),
                        int_span.to_range(),
                        vec![err],
                    ),
                ))
            }
        }
    }
}

struct Hex;
impl Lexer for Hex {
    fn lex(input: Span) -> IResult {
        let start = input.location_offset();
        let (input, _) = tag("0x")(input)?;
        let pos = input.location_offset();
        let (input, opt_hex) = expect(hex_digit1, LexErrorMessage::ExpectedHexNumber, pos)(input)?;
        let mut errors = Vec::new();
        let result = match opt_hex {
            Ok(hex_span) => match u32::from_str_radix(&hex_span, 16) {
                Ok(value) => IntResult::Int(value),
                Err(_) => {
                    let err = SplError(
                        hex_span.to_range(),
                        LexErrorMessage::InvalidIntLit("0x".to_string() + &hex_span).to_string(),
                    );
                    errors.push(err);
                    IntResult::Err(hex_span.to_string())
                }
            },
            Err(err) => {
                errors.push(err);
                IntResult::Err(String::new())
            }
        };
        let end = input.location_offset();
        Ok((
            input,
            Token::new_with_errors(TokenType::Hex(result), start..end, errors),
        ))
    }
}

struct Comment;
impl Lexer for Comment {
    fn lex(input: Span) -> IResult {
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
impl Lexer for Unknown {
    fn lex(input: Span) -> IResult {
        map(take(1u8), |span: Span| {
            Token::new(TokenType::Unknown(span.to_string()), span.to_range())
        })(input)
    }
}

struct Eof;
impl Lexer for Eof {
    fn lex(input: Span) -> IResult {
        map(eof, |span: Span| {
            Token::new(TokenType::Eof, span.to_range())
        })(input)
    }
}
