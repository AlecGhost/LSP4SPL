use super::{IResult, Span};
use crate::{error::ParseErrorMessage, DiagnosticsBroker};
use nom::{
    bytes::complete::{tag, take, take_till, take_while},
    character::{complete::multispace0, is_alphanumeric},
    error::ErrorKind,
    multi::many0,
    {InputTake, Offset},
};

pub(super) trait MutParser<'a, O, B> {
    fn parse(&mut self, input: Span<'a, B>) -> IResult<'a, O, B>;
}

impl<'a, O, B, F> MutParser<'a, O, B> for F
where
    F: FnMut(Span<'a, B>) -> IResult<'a, O, B>,
{
    fn parse(&mut self, input: Span<'a, B>) -> IResult<'a, O, B> {
        self(input)
    }
}

pub(super) fn comment<B: Clone>(input: Span<B>) -> IResult<Span<B>, B> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("//")(input)?;
    let (input, comment) = take_till(|c| c == '\n')(input)?;
    Ok((input, comment))
}

// Source: https://github.com/Geal/nom/blob/main/doc/nom_recipes.md#whitespace
/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub(super) fn ws<'a, O, B: Clone, F>(mut inner: F) -> impl FnMut(Span<'a, B>) -> IResult<O, B>
where
    F: MutParser<'a, O, B>,
{
    move |input: Span<B>| {
        let (input, _) = many0(comment)(input)?;
        let (input, _) = multispace0(input)?;
        let (input, result) = inner.parse(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = many0(comment)(input)?;
        Ok((input, result))
    }
}

pub(super) fn alpha_numeric0<B: Clone>(input: Span<B>) -> IResult<Span<B>, B> {
    take_while(is_alpha_numeric)(input)
}

pub(super) fn is_alpha_numeric(c: char) -> bool {
    is_alphanumeric(c as u8) || c == '_'
}

pub(super) fn ignore_until<'a, B: Clone, F>(
    mut f: F,
) -> impl FnMut(Span<'a, B>) -> IResult<Span<'a, B>, B>
where
    F: MutParser<'a, Span<'a, B>, B>,
{
    move |mut i: Span<B>| {
        let original_input = i.clone();
        loop {
            match f.parse(i.clone()) {
                Ok((i1, _)) => {
                    // source: https://stackoverflow.com/a/73004814
                    // compares remaining input with original input and returns the difference
                    let offset = original_input.offset(&i1);
                    let output = original_input.take(offset);
                    return Ok((i1, output));
                }
                Err(nom::Err::Error(_)) => match take(1u32)(i.clone()) {
                    Ok((i1, _)) => i = i1,
                    Err(e) => return Err(e),
                },
                Err(e) => return Err(e),
            };
        }
    }
}

pub(super) fn ignore_until1<'a, B: Clone, F>(
    mut f: F,
) -> impl FnMut(Span<'a, B>) -> IResult<Span<'a, B>, B>
where
    F: MutParser<'a, Span<'a, B>, B>,
{
    move |mut i: Span<B>| {
        if let Ok((i1, _)) = f.parse(i.clone()) {
            return Err(nom::Err::Error(nom::error::ParseError::from_error_kind(
                i1,
                ErrorKind::ManyTill,
            )));
        };
        let original_input = i.clone();
        loop {
            match f.parse(i.clone()) {
                Ok((i1, _)) => {
                    // source: https://stackoverflow.com/a/73004814
                    // compares remaining input with original input and returns the difference
                    let offset = original_input.offset(&i1);
                    let output = original_input.take(offset);
                    return Ok((i1, output));
                }
                Err(nom::Err::Error(_)) => match take(1u32)(i.clone()) {
                    Ok((i1, _)) => i = i1,
                    Err(e) => return Err(e),
                },
                Err(e) => return Err(e),
            };
        }
    }
}

pub(super) fn expect<'a, O, B: DiagnosticsBroker, F>(
    mut parser: F,
    error_msg: ParseErrorMessage,
) -> impl FnMut(Span<'a, B>) -> IResult<Option<O>, B>
where
    F: MutParser<'a, O, B>,
{
    move |input: Span<B>| match parser.parse(input.clone()) {
        Ok((input, out)) => Ok((input, Some(out))),
        Err(_) => {
            // TODO: look into error range reporting
            let pos = input.location_offset();
            let err = crate::error::SplError(pos..pos, error_msg.to_string());
            input.extra.report_error(err);
            Ok((input, None))
        }
    }
}

pub(super) fn verify<'a, O, B: DiagnosticsBroker, F, G>(
    mut parser: F,
    verification: G,
) -> impl FnMut(Span<'a, B>) -> IResult<O, B>
where
    F: MutParser<'a, O, B>,
    G: Fn(&O) -> bool,
{
    move |input: Span<B>| match parser.parse(input.clone()) {
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

macro_rules! keyword_parsers {
    ($($name: ident: $pattern: literal),*) => {
        use crate::parser::{Span, DiagnosticsBroker, utility};
        $(
        pub fn $name<B: DiagnosticsBroker>(input: Span<B>)
        -> nom::IResult<Span<B>, Span<B>> {
            use nom::bytes::complete::{tag, take};
            use nom::combinator::{peek, eof};
            use nom::branch::alt;
            use nom::sequence::terminated;
            use utility::{ws, is_alpha_numeric, verify};
            ws(terminated(tag($pattern), peek(alt((eof, verify(take(1u8), |span| !span.starts_with(is_alpha_numeric)))))))(input)
        }
        )*
    };
}

macro_rules! symbol_parsers {
    ($($name: ident: $pattern: literal),*) => {
        use crate::parser::{Span, utility};
        $(
        pub fn $name<B: Clone>(input: Span<B>) -> nom::IResult<Span<B>, Span<B>> {
            use nom::bytes::complete::tag;
            use utility::ws;
            ws(tag($pattern))(input)
        }
        )*
    };
}

pub(super) mod keywords {
    keyword_parsers!(
        array: "array",
        r#else: "else",
        r#if: "if",
        of: "of",
        proc: "proc",
        r#ref: "ref",
        r#type: "type",
        var: "var",
        r#while: "while"
    );
}

pub(super) mod primitives {
    keyword_parsers!(
        int: "int"
    );
}

pub(super) mod symbols {
    symbol_parsers!(
        lparen: "(",
        rparen: ")",
        lbracket: "[",
        rbracket: "]",
        lcurly: "{",
        rcurly: "}",
        eq: "=",
        neq: "#",
        lt: "<",
        le: "<=",
        gt: ">",
        ge: ">=",
        assign: ":=",
        colon: ":",
        comma: ",",
        semic: ";",
        plus: "+",
        minus: "-",
        times: "*",
        divide: "/"
    );
}
