use super::IResult;
use crate::{error::ParseErrorMessage, lexer::token::TokenStream, DiagnosticsBroker, ToRange};
use nom::{
    bytes::complete::take,
    error::ErrorKind,
    multi::many0,
    sequence::preceded,
    {InputTake, Offset},
};

pub(super) trait InnerParser<'a, O, B> {
    fn parse(&mut self, input: TokenStream<'a, B>) -> IResult<'a, O, B>;
}

impl<'a, O, B, F> InnerParser<'a, O, B> for F
where
    F: FnMut(TokenStream<'a, B>) -> IResult<'a, O, B>,
{
    fn parse(&mut self, input: TokenStream<'a, B>) -> IResult<'a, O, B> {
        self(input)
    }
}

/// Consumes tokens until the given pattern matches,
/// succeeds if it matches immediately
/// Returns all consumed tokens as `TokenStream`.
/// Remember: `TokenStream` can be empty.
pub(super) fn ignore_until0<'a, B: Clone, F>(
    mut pattern: F,
) -> impl FnMut(TokenStream<'a, B>) -> IResult<TokenStream<'a, B>, B>
where
    F: InnerParser<'a, TokenStream<'a, B>, B>,
{
    move |mut i: TokenStream<B>| {
        let original_input = i.clone();
        loop {
            match pattern.parse(i.clone()) {
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

/// Consumes tokens until the given pattern matches,
/// but fails if the pattern does not match at least once.
/// Returns all consumed tokens as `TokenStream`.
pub(super) fn ignore_until1<'a, B: Clone, F>(
    mut pattern: F,
) -> impl FnMut(TokenStream<'a, B>) -> IResult<TokenStream<'a, B>, B>
where
    F: InnerParser<'a, TokenStream<'a, B>, B>,
{
    move |mut i: TokenStream<B>| {
        if let Ok((i1, _)) = pattern.parse(i.clone()) {
            return Err(nom::Err::Error(nom::error::ParseError::from_error_kind(
                i1,
                ErrorKind::ManyTill,
            )));
        };
        let original_input = i.clone();
        loop {
            match pattern.parse(i.clone()) {
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

/// Tries to parse the input with the given parser.
/// If parsing succeeds, the result of inner is returned.
/// If parsing fails, an error with the given message is reported.
/// Source: [Eyal Kalderon](https://eyalkalderon.com/blog/nom-error-recovery/)
pub(super) fn expect<'a, O, B: DiagnosticsBroker, F>(
    mut parser: F,
    error_msg: ParseErrorMessage,
) -> impl FnMut(TokenStream<'a, B>) -> IResult<Option<O>, B>
where
    F: InnerParser<'a, O, B>,
{
    move |input: TokenStream<B>| match parser.parse(input) {
        Ok((input, out)) => Ok((input, Some(out))),
        Err(nom::Err::Failure(err) | nom::Err::Error(err)) => {
            let pos = err.input.error_pos;
            let spl_error = crate::error::SplError(pos..pos, error_msg.to_string());
            err.input.broker.report_error(spl_error);
            Ok((err.input, None))
        }
        Err(_) => panic!("Incomplete data"),
    }
}

/// Parses a comma separated list of parsers
pub(super) fn parse_list<'a, O, B: DiagnosticsBroker, F>(
    mut parser: F,
) -> impl FnMut(TokenStream<'a, B>) -> IResult<Vec<O>, B>
where
    F: InnerParser<'a, O, B>,
{
    move |input: TokenStream<B>| {
        // Create new parser from closure because `InnerParser` must be used with `parse` function
        let mut parser = |input| parser.parse(input);
        let (input, head) = parser(input)?;
        let (input, tail) = many0(preceded(super::symbols::comma, &mut parser))(input)?;
        let mut list = vec![head];
        list.extend(tail);
        Ok((input, list))
    }
}

pub(super) fn confusable<'a, O, B: DiagnosticsBroker, F>(
    mut parser: F,
    error_msg: ParseErrorMessage,
) -> impl FnMut(TokenStream<'a, B>) -> IResult<O, B>
where
    F: InnerParser<'a, O, B>,
{
    move |input: TokenStream<B>| {
        let error_start = input.to_range().start;
        let (input, out) = parser.parse(input)?;
        let spl_error = crate::error::SplError(error_start..input.error_pos, error_msg.to_string());
        input.broker.report_error(spl_error);
        Ok((input, out))
    }
}
