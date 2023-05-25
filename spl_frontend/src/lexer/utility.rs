use super::Span;
use crate::error::{LexErrorMessage, SplError};
use nom::{bytes::complete::take_while, character::is_alphanumeric, error::ErrorKind};

type IResult<'a, O> = nom::IResult<Span<'a>, O>;

/// Parser for alphanumeric characters or underscores
pub(super) fn alpha_numeric0(input: Span) -> IResult<Span> {
    take_while(is_alpha_numeric)(input)
}

/// Checks if provided char is alphanumeric or an underscore.
pub(super) fn is_alpha_numeric(c: char) -> bool {
    is_alphanumeric(c as u8) || c == '_'
}

/// Tries to parse the input with the given parser.
/// If parsing succeeds and the output matches the given verification function,
/// the result is returned.
pub(super) fn verify<'a, O, F, G>(
    mut parser: F,
    verification: G,
) -> impl FnMut(Span<'a>) -> IResult<O>
where
    F: FnMut(Span<'a>) -> IResult<'a, O>,
    G: Fn(&O) -> bool,
{
    move |input: Span| match parser(input) {
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
pub(super) fn expect<'a, O, F>(
    mut inner: F,
    error_msg: LexErrorMessage,
    error_pos: usize,
) -> impl FnMut(Span<'a>) -> IResult<Result<O, SplError>>
where
    F: FnMut(Span<'a>) -> IResult<'a, O>,
{
    move |input: Span| match inner(input) {
        Ok((input, out)) => Ok((input, Ok(out))),
        Err(_) => {
            let err = crate::error::SplError(error_pos..error_pos, error_msg.clone().into());
            Ok((input, Err(err)))
        }
    }
}
