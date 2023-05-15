use super::IResult;
use crate::{
    ast::{AstInfo, Reference},
    error::ParseErrorMessage,
    lexer::token::TokenStream,
    token::Token,
    ToRange,
};
use nom::{
    bytes::complete::take,
    error::ErrorKind,
    multi::many0,
    sequence::preceded,
    {InputTake, Offset},
};

pub(super) trait InnerParser<'a, O> {
    fn parse(&mut self, input: TokenStream<'a>) -> IResult<'a, O>;
}

impl<'a, O, F> InnerParser<'a, O> for F
where
    F: FnMut(TokenStream<'a>) -> IResult<'a, O>,
{
    fn parse(&mut self, input: TokenStream<'a>) -> IResult<'a, O> {
        self(input)
    }
}

/// Consumes tokens until the given pattern matches,
/// succeeds if it matches immediately
/// Returns all consumed tokens as `TokenStream`.
/// Remember: `TokenStream` can be empty.
pub(super) fn ignore_until0<'a, F>(
    mut pattern: F,
) -> impl FnMut(TokenStream<'a>) -> IResult<TokenStream<'a>>
where
    F: InnerParser<'a, TokenStream<'a>>,
{
    move |mut i: TokenStream| {
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
pub(super) fn ignore_until1<'a, F>(
    mut pattern: F,
) -> impl FnMut(TokenStream<'a>) -> IResult<Vec<Token>>
where
    F: InnerParser<'a, TokenStream<'a>>,
{
    move |mut i: TokenStream| {
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
                    let ignored = original_input.take(offset);
                    return Ok((i1, ignored.tokens()));
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
pub(super) fn expect<'a, O, F>(
    mut parser: F,
    error_msg: ParseErrorMessage,
) -> impl FnMut(TokenStream<'a>) -> IResult<Option<O>>
where
    F: InnerParser<'a, O>,
{
    move |input: TokenStream| match parser.parse(input) {
        Ok((input, out)) => Ok((input, Some(out))),
        Err(nom::Err::Failure(mut err) | nom::Err::Error(mut err)) => {
            let pos = err.input.location_offset() - err.input.reference_pos;
            let error_pos = if pos > 0 { pos - 1 } else { 0 };
            let spl_error = crate::error::SplError(error_pos..error_pos, error_msg.to_string());
            err.input.error_buffer.push(spl_error);
            Ok((err.input, None))
        }
        Err(_) => panic!("Incomplete data"),
    }
}

/// Parses a comma separated list of parsers
pub(super) fn parse_list<'a, O, F>(
    mut parser: F,
) -> impl FnMut(TokenStream<'a>) -> IResult<Vec<O>>
where
    F: InnerParser<'a, O>,
{
    move |input: TokenStream| {
        // Create new parser from closure because `InnerParser` must be used with `parse` function
        let mut parser = |input| parser.parse(input);
        let (input, head) = parser(input)?;
        let (input, tail) = many0(preceded(super::symbols::comma, &mut parser))(input)?;
        let mut list = vec![head];
        list.extend(tail);
        Ok((input, list))
    }
}

pub(super) fn confusable<'a, O, F>(
    mut parser: F,
    error_msg: ParseErrorMessage,
) -> impl FnMut(TokenStream<'a>) -> IResult<O>
where
    F: InnerParser<'a, O>,
{
    move |input: TokenStream| {
        let (mut input, (out, info)) = info(|input| parser.parse(input))(input)?;
        let spl_error = crate::error::SplError(info.to_range(), error_msg.to_string());
        input.error_buffer.push(spl_error);
        Ok((input, out))
    }
}

pub(super) fn info<'a, O, F>(
    mut parser: F,
) -> impl FnMut(TokenStream<'a>) -> IResult<'a, (O, AstInfo)>
where
    F: InnerParser<'a, O>,
{
    move |mut input: TokenStream| {
        let reference_pos = input.reference_pos;
        let start_pos = input.location_offset() - reference_pos;
        let error_backup = input.error_buffer;
        input.error_buffer = Vec::new();
        match parser.parse(input) {
            Ok((mut input, out)) => {
                let errors = input.error_buffer;
                input.error_buffer = error_backup;
                let end_pos = input.location_offset() - reference_pos;
                let range = start_pos..end_pos;
                let info = if errors.is_empty() {
                    AstInfo::new(range)
                } else {
                    AstInfo::new_with_errors(range, errors)
                };
                Ok((input, (out, info)))
            }
            Err(nom::Err::Failure(mut err) | nom::Err::Error(mut err)) => {
                // recover backup
                err.input.error_buffer = error_backup;
                Err(nom::Err::Error(err))
            }
            Err(_) => panic!("Incomplete data"),
        }
    }
}

pub(super) fn reference<'a, O, F>(
    mut parser: F,
) -> impl FnMut(TokenStream<'a>) -> IResult<'a, Reference<O>>
where
    F: InnerParser<'a, O>,
{
    move |mut input: TokenStream| {
        let reference_backup = input.reference_pos;
        input.reference_pos = input.location_offset();
        let offset = input.reference_pos - reference_backup;
        match parser.parse(input) {
            Ok((mut input, out)) => {
                input.reference_pos = reference_backup;
                Ok((
                    input,
                    Reference {
                        reference: out,
                        offset,
                    },
                ))
            }
            Err(nom::Err::Failure(mut err) | nom::Err::Error(mut err)) => {
                // recover backup
                err.input.reference_pos = reference_backup;
                Err(nom::Err::Error(err))
            }
            Err(_) => panic!("Incomplete data"),
        }
    }
}
