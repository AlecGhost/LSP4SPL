use super::ToRange;
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till, take_while},
    character::{
        complete::{alpha1, anychar, digit1, hex_digit1, multispace0},
        is_alphanumeric,
    },
    combinator::{all_consuming, map},
    error::ErrorKind,
    multi::many0,
    sequence::preceded,
};
use token::{Token, TokenType};

#[cfg(test)]
mod tests;
mod token;

pub(crate) type Span<'a> = nom_locate::LocatedSpan<&'a str>;

type IResult<'a, T> = nom::IResult<Span<'a>, T>;

pub fn lex(input: &str) -> Vec<Token> {
    let span = Span::new(input);
    let (_, tokens) = all_consuming(many0(Token::lex))(span).expect("Lexing must not fail.");
    tokens
}

trait Lexer: Sized {
    fn lex(input: Span) -> IResult<Token>;
}

macro_rules! token {
    ($token_type:expr) => {
        |span: Span| Token::new($token_type, span.to_range())
    };
    ($span:ident, $token_type:expr) => {
        |$span: Span| Token::new($token_type, $span.to_range())
    };
}

impl Lexer for Token {
    fn lex(input: Span) -> IResult<Self> {
        let (input, _) = multispace0(input)?;
        alt((
            Comment::lex,
            alt((
                map(tag("("), token!(TokenType::LParen)),
                map(tag(")"), token!(TokenType::RParen)),
                map(tag("["), token!(TokenType::LBracket)),
                map(tag("]"), token!(TokenType::RBracket)),
                map(tag("{"), token!(TokenType::LCurly)),
                map(tag("}"), token!(TokenType::RCurly)),
                map(tag("="), token!(TokenType::Eq)),
                map(tag("#"), token!(TokenType::Neq)),
                map(tag("<="), token!(TokenType::Le)),
                map(tag("<"), token!(TokenType::Lt)),
                map(tag(">="), token!(TokenType::Ge)),
                map(tag(">"), token!(TokenType::Gt)),
                map(tag(":="), token!(TokenType::Assign)),
                map(tag(":"), token!(TokenType::Colon)),
                map(tag(","), token!(TokenType::Comma)),
                map(tag(";"), token!(TokenType::Semic)),
                map(tag("+"), token!(TokenType::Plus)),
                map(tag("-"), token!(TokenType::Minus)),
                map(tag("*"), token!(TokenType::Times)),
                map(tag("/"), token!(TokenType::Divide)),
            )),
            alt((
                map(keyword::r#if, token!(TokenType::If)),
                map(keyword::r#else, token!(TokenType::Else)),
                map(keyword::array, token!(TokenType::Array)),
                map(keyword::of, token!(TokenType::Of)),
                map(keyword::proc, token!(TokenType::Proc)),
                map(keyword::r#ref, token!(TokenType::Ref)),
                map(keyword::r#type, token!(TokenType::Type)),
                map(keyword::var, token!(TokenType::Var)),
            )),
            alt((Char::lex, Int::lex, Hex::lex, Ident::lex)),
            map(
                take(1u8),
                token!(span, TokenType::Unknown(span.to_string())),
            ),
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

/// Parser for alphanumeric characters or underscores
fn alpha_numeric0(input: Span) -> IResult<Span> {
    take_while(is_alpha_numeric)(input)
}

fn is_alpha_numeric(c: char) -> bool {
    is_alphanumeric(c as u8) || c == '_'
}

trait InnerParser<'a, O> {
    fn parse(&mut self, input: Span<'a>) -> IResult<'a, O>;
}

impl<'a, O, F> InnerParser<'a, O> for F
where
    F: FnMut(Span<'a>) -> IResult<'a, O>,
{
    fn parse(&mut self, input: Span<'a>) -> IResult<'a, O> {
        self(input)
    }
}

/// Tries to parse the input with the given parser.
/// If parsing succeeds and the output matches the given verification function,
/// the result is returned.
fn verify<'a, O, F, G>(mut parser: F, verification: G) -> impl FnMut(Span<'a>) -> IResult<O>
where
    F: InnerParser<'a, O>,
    G: Fn(&O) -> bool,
{
    move |input: Span| match parser.parse(input) {
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

mod keyword {
    macro_rules! keyword_lexer {
        ($name: ident, $pattern: literal) => {
            pub fn $name(input: super::Span) -> super::IResult<super::Span> {
                use super::{is_alpha_numeric, verify};
                use nom::branch::alt;
                use nom::bytes::complete::{tag, take};
                use nom::combinator::{eof, peek};
                use nom::sequence::terminated;
                terminated(
                    tag($pattern),
                    peek(alt((
                        eof,
                        verify(take(1u8), |span| !span.starts_with(is_alpha_numeric)),
                    ))),
                )(input)
            }
        };
    }

    keyword_lexer!(array, "array");
    keyword_lexer!(of, "of");
    keyword_lexer!(var, "var");
    keyword_lexer!(proc, "proc");
    keyword_lexer!(r#ref, "ref");
    keyword_lexer!(r#type, "type");
    keyword_lexer!(r#if, "if");
    keyword_lexer!(r#else, "else");
}
