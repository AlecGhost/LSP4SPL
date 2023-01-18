use nom::{
    bytes::complete::take,
    character::complete::{multispace0, multispace1},
    error::ErrorKind,
    {InputTake, Offset},
};
use super::{DiagnosticsBroker, ErrorMessage, IResult, Span};

pub trait MutParser<'a, O> {
    fn parse(&mut self, input: Span<'a>) -> IResult<'a, O>;
}

impl<'a, O, F> MutParser<'a, O> for F
where
    F: FnMut(Span<'a>) -> IResult<'a, O>,
{
    fn parse(&mut self, input: Span<'a>) -> IResult<'a, O> {
        self(input)
    }
}

// Source: https://github.com/Geal/nom/blob/main/doc/nom_recipes.md#whitespace
/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, O, F>(mut inner: F) -> impl FnMut(Span<'a>) -> IResult<O>
where
    F: MutParser<'a, O>,
{
    move |input: Span| {
        let (input, _) = multispace0(input)?;
        let (input, result) = inner.parse(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, result))
    }
}

pub fn ws_enclosed<'a, O, F>(mut inner: F) -> impl FnMut(Span<'a>) -> IResult<O>
where
    F: MutParser<'a, O>,
{
    move |input: Span| {
        let (input, _) = multispace1(input)?;
        let (input, result) = inner.parse(input)?;
        let (input, _) = multispace1(input)?;
        Ok((input, result))
    }
}

pub fn ignore_until<'a, F>(mut f: F) -> impl FnMut(Span<'a>) -> IResult<Span<'a>>
where
    F: MutParser<'a, Span<'a>>,
{
    move |mut i: Span| {
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

pub fn ignore_until1<'a, F>(mut f: F) -> impl FnMut(Span<'a>) -> IResult<Span<'a>>
where
    F: MutParser<'a, Span<'a>>,
{
    move |mut i: Span| {
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

pub fn expect<'a, O, F>(
    mut parser: F,
    error_msg: ErrorMessage,
) -> impl FnMut(Span<'a>) -> IResult<Option<O>>
where
    F: MutParser<'a, O>,
{
    move |input| match parser.parse(input.clone()) {
        Ok((input, out)) => Ok((input, Some(out))),
        Err(_) => {
            // TODO: look into error range reporting
            let pos = input.location_offset();
            let err = super::ParseError(pos..pos, error_msg.clone());
            input.extra.report_error(err);
            Ok((input, None))
        }
    }
}

macro_rules! simple_parsers {
    ($($name: ident: $pattern: literal),*) => {
        $(
        pub(crate) fn $name(input: crate::parser::Span) -> nom::IResult<crate::parser::Span, crate::parser::Span> {
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
