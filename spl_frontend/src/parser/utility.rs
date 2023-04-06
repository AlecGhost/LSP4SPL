use super::IResult;
use crate::{
    ast::AstInfo,
    error::{ParseErrorMessage, SplError},
    lexer::token::TokenStream,
    DiagnosticsBroker, ToRange,
};
use nom::{
    bytes::complete::take,
    combinator::{opt, peek},
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
/// but fails if the pattern does not match at least once.
/// Returns all consumed tokens as `TokenStream`.
pub(super) fn ignore_until<'a, B: Clone, F>(
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
/// If the list is not followed by a `terminating_parser`,
/// input is consumed and reported until it is.
pub(super) fn parse_list<'a, O, B: DiagnosticsBroker, F, G>(
    mut parser: F,
    mut terminating_parser: G,
    error_msg: ParseErrorMessage,
) -> impl FnMut(TokenStream<'a, B>) -> IResult<Vec<O>, B>
where
    F: InnerParser<'a, O, B>,
    G: InnerParser<'a, TokenStream<'a, B>, B>,
{
    fn ignore_if_not_finished<'a, B: DiagnosticsBroker, G>(
        terminating_parser: &mut G,
        input: TokenStream<'a, B>,
    ) -> TokenStream<'a, B>
    where
        G: InnerParser<'a, TokenStream<'a, B>, B>,
    {
        let terminating_parser = |input| terminating_parser.parse(input);
        match ignore_until(peek(terminating_parser))(input) {
            Ok((input, ignored)) => {
                // convert into AstInfo, so that `to_range()` and `to_string()` are available
                let ignored_info = AstInfo::new(&ignored[..]);
                let err = SplError(
                    ignored_info.to_range(),
                    ParseErrorMessage::UnexpectedCharacters(ignored_info.to_string()).to_string(),
                );
                input.broker.report_error(err);
                input
            }
            Err(nom::Err::Failure(err) | nom::Err::Error(err)) => err.input,
            Err(_) => panic!("Incomplete data"),
        }
    }

    move |input: TokenStream<B>| {
        // Create new parser from closure because `InnerParser` must be used with `parse` function
        let mut parser = |input| parser.parse(input);
        let (input, first) = opt(&mut parser)(input)?;
        if let Some(head) = first {
            let (input, tail) = many0(preceded(
                super::symbols::comma,
                expect(&mut parser, error_msg.clone()),
            ))(input)?;
            let mut list = vec![head];
            list.append(&mut tail.into_iter().flatten().collect());
            let input = ignore_if_not_finished(&mut terminating_parser, input);
            Ok((input, list))
        } else {
            let input = ignore_if_not_finished(&mut terminating_parser, input);
            Ok((input, Vec::new()))
        }
    }
}
