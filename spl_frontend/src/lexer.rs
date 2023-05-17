use crate::error::{LexErrorMessage, SplError};
use crate::{Shiftable, TextChange, ToRange};
use nom::combinator::{eof, iterator, peek};
use nom::sequence::{delimited, pair, terminated};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till},
    character::complete::{alpha1, anychar, digit1, hex_digit1, multispace0},
    combinator::map,
    multi::many0,
    sequence::preceded,
};
use std::ops::Range;
use token::{IntResult, Token, TokenType};
use utility::{alpha_numeric0, expect, is_alpha_numeric, verify};

#[cfg(test)]
mod tests;
pub mod token;
mod utility;

/// Type alias for nom_locate::LocatedSpan.
/// Tracks range inside source code during lexical analysis.
type Span<'a> = nom_locate::LocatedSpan<&'a str>;

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
pub fn lex(src: &str) -> Vec<Token> {
    let input = Span::new(src);
    let (_, (mut tokens, eof_token)) = pair(
        many0(preceded(multispace0, Token::lex)),
        preceded(multispace0, Eof::lex),
    )(input)
    .expect("Lexing must not fail.");
    tokens.push(eof_token);
    tokens
}

pub fn update(new_text: &str, mut tokens: Vec<Token>, change: &TextChange) -> Vec<Token> {
    /// This can also shift in negative direction, unlike `Shiftable::shift`.
    ///
    /// # Panics
    ///
    /// Panics if ranges cannot be converted to `isize`
    /// or if the resulting range does not fit into `usize`.
    fn shift_token(token: Token, offset: isize) -> Token {
        let start: isize = token.range.start.try_into().expect("Range is too big");
        let end: isize = token.range.end.try_into().expect("Range is too big");
        let new_start: usize = (start + offset).try_into().expect("Range is too big");
        let new_end: usize = (end + offset).try_into().expect("Range is too big");
        Token {
            range: new_start..new_end,
            ..token
        }
    }

    let offset: isize = {
        let insertion_len: isize = change.text.len().try_into().expect("Insertion is too big");
        let deletion_len: isize = change.range.len().try_into().expect("Deletion is too big");
        insertion_len - deletion_len
    };

    // pop EOF token, so that the rest of the logic doesn't need to care.
    let eof = match tokens.pop() {
        Some(eof) if matches!(eof.token_type, TokenType::Eof) => shift_token(eof, offset),
        _ => panic!("Must contain EOF token"),
    };

    // Alternative implementation:
    let (unaffected_head, tokens): (Vec<Token>, Vec<Token>) = tokens
        .into_iter()
        .partition(|token| !token.is_affected_by(change.range.start));
    let (_, reusable_tokens): (Vec<Token>, Vec<Token>) = tokens
        .into_iter()
        .partition(|token| token.range.start < change.range.end);
    let reusable_tokens: Vec<Token> = reusable_tokens
        .into_iter()
        .map(|token| shift_token(token, offset))
        .collect();

    let reanalysis_start = unaffected_head
        .last()
        .map(|token| token.range.end)
        .unwrap_or(0);
    let reanalysis_text = &new_text[reanalysis_start..];

    let input = Span::new(reanalysis_text);
    let new_tokens: Vec<_> = iterator(input, preceded(multispace0, Token::lex))
        .map(|token| token.shift(reanalysis_start))
        .take_while(|token| !reusable_tokens.contains(token))
        .collect();

    let unaffected_tail = if let Some(last_new_token) = new_tokens.last() {
        reusable_tokens
            .into_iter()
            .skip_while(|token| token.range.start < last_new_token.range.end)
            .collect()
    } else {
        reusable_tokens
    };

    vec![unaffected_head, new_tokens, unaffected_tail, vec![eof]].concat()
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
        let (input, closing_tick) =
            expect(tag("'"), LexErrorMessage::MissingClosingTick, pos)(input)?;
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
        int_span.parse().map_or_else(
            |_| {
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
            },
            |value| {
                Ok((
                    input,
                    Token::new(TokenType::Int(IntResult::Int(value)), int_span.to_range()),
                ))
            },
        )
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
            Ok(hex_span) => u32::from_str_radix(&hex_span, 16).map_or_else(
                |_| {
                    let err = SplError(
                        hex_span.to_range(),
                        LexErrorMessage::InvalidIntLit("0x".to_string() + &hex_span).to_string(),
                    );
                    errors.push(err);
                    IntResult::Err(hex_span.to_string())
                },
                IntResult::Int,
            ),
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
        let (input, comment) = delimited(tag("//"), take_till(|c| c == '\n'), tag("\n"))(input)?;
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
