use super::IResult;
use crate::{error::ParseErrorMessage, lexer::token::Tokens, DiagnosticsBroker};
use nom::{
    bytes::complete::take,
    error::ErrorKind,
    {InputTake, Offset},
};

pub(super) trait InnerParser<'a, O, B> {
    fn parse(&mut self, input: Tokens<'a, B>) -> IResult<'a, O, B>;
}

impl<'a, O, B, F> InnerParser<'a, O, B> for F
where
    F: FnMut(Tokens<'a, B>) -> IResult<'a, O, B>,
{
    fn parse(&mut self, input: Tokens<'a, B>) -> IResult<'a, O, B> {
        self(input)
    }
}

/// Consumes tokens until the given pattern matches.
/// Returns all consumed tokens as `Tokens`.
pub(super) fn ignore_until<'a, B: Clone, F>(
    mut pattern: F,
) -> impl FnMut(Tokens<'a, B>) -> IResult<Tokens<'a, B>, B>
where
    F: InnerParser<'a, Tokens<'a, B>, B>,
{
    move |mut i: Tokens<B>| {
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

/// Like `ignore_until`, but fails if the pattern does not match at least once.
pub(super) fn ignore_until1<'a, B: Clone, F>(
    mut pattern: F,
) -> impl FnMut(Tokens<'a, B>) -> IResult<Tokens<'a, B>, B>
where
    F: InnerParser<'a, Tokens<'a, B>, B>,
{
    move |mut i: Tokens<B>| {
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
pub(super) fn expect<'a, O, B: DiagnosticsBroker, F>(
    mut parser: F,
    error_msg: ParseErrorMessage,
) -> impl FnMut(Tokens<'a, B>) -> IResult<Option<O>, B>
where
    F: InnerParser<'a, O, B>,
{
    move |input: Tokens<B>| match parser.parse(input.clone()) {
        Ok((input, out)) => Ok((input, Some(out))),
        Err(_) => {
            // TODO: look into error range reporting
            let pos = input.location_offset();
            let err = crate::error::SplError(pos..pos, error_msg.to_string());
            input.broker.report_error(err);
            Ok((input, None))
        }
    }
}
