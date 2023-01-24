use super::{IResult, ParseErrorBroker, Span};
use crate::error::ParseErrorMessage;
use nom::{
    bytes::complete::{tag, take, take_till},
    character::complete::{multispace0, multispace1},
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

pub(super) fn ws_enclosed<'a, O, B: Clone, F>(
    mut inner: F,
) -> impl FnMut(Span<'a, B>) -> IResult<O, B>
where
    F: MutParser<'a, O, B>,
{
    move |input: Span<B>| {
        let (input, _) = multispace1(input)?;
        let (input, result) = inner.parse(input)?;
        let (input, _) = multispace1(input)?;
        Ok((input, result))
    }
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

pub(super) fn expect<'a, O, B: ParseErrorBroker, F>(
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
            let err = crate::error::ParseError(pos..pos, error_msg.clone());
            input.extra.report_error(err);
            Ok((input, None))
        }
    }
}

macro_rules! simple_parsers {
    ($($name: ident: $pattern: literal),*) => {
        $(
        pub(crate) fn $name<B: Clone>(input: crate::parser::Span<B>) -> nom::IResult<crate::parser::Span<B>, crate::parser::Span<B>> {
            crate::parser::ws(nom::bytes::complete::tag($pattern))(input)
        }
        )*
    };
}

pub(crate) mod keywords {
    simple_parsers!(
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

pub(crate) mod primitives {
    simple_parsers!(
        int: "int",
        bool: "bool"
    );
}

pub(crate) mod symbols {
    simple_parsers!(
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
