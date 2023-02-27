use super::Span;
use crate::{error::ParseErrorMessage, DiagnosticsBroker};
use nom::{bytes::complete::take_while, character::is_alphanumeric, error::ErrorKind};
use std::ops::Range;

type IResult<'a, O, B> = nom::IResult<Span<'a, B>, O>;

/// Parser for alphanumeric characters or underscores
pub(super) fn alpha_numeric0<B: DiagnosticsBroker>(input: Span<B>) -> IResult<Span<B>, B> {
    take_while(is_alpha_numeric)(input)
}

/// Checks if provided char is alphanumeric or an underscore.
pub(super) fn is_alpha_numeric(c: char) -> bool {
    is_alphanumeric(c as u8) || c == '_'
}

/// Tries to parse the input with the given parser.
/// If parsing succeeds and the output matches the given verification function,
/// the result is returned.
pub(super) fn verify<'a, O, F, G, B: DiagnosticsBroker>(
    mut parser: F,
    verification: G,
) -> impl FnMut(Span<'a, B>) -> IResult<O, B>
where
    F: FnMut(Span<'a, B>) -> IResult<'a, O, B>,
    G: Fn(&O) -> bool,
{
    move |input: Span<B>| match parser(input) {
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

/// Tries to parse the input with the given parser.
/// If parsing succeeds, the result of inner is returned.
/// If parsing fails, an error with the provided message is reported.
pub(super) fn expect<'a, O, B: DiagnosticsBroker, F>(
    mut inner: F,
    error_msg: ParseErrorMessage,
    error_range: Range<usize>,
) -> impl FnMut(Span<'a, B>) -> IResult<Option<O>, B>
where
    F: FnMut(Span<'a, B>) -> IResult<'a, O, B>,
{
    move |input: Span<B>| match inner(input.clone()) {
        Ok((input, out)) => Ok((input, Some(out))),
        Err(_) => {
            let err = crate::error::SplError(error_range.clone(), error_msg.to_string());
            input.extra.report_error(err);
            Ok((input, None))
        }
    }
}
