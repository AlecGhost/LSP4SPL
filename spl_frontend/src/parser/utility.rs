use crate::{
    ast::{AstInfo, AstInfoTraverser, Reference},
    error::{ErrorMessage, ParseErrorMessage, ParserError, ParserErrorKind},
    lexer::token::TokenStream,
    parser::{IResult, Parser},
    token::{Token, TokenChange},
    Shiftable, ToRange,
};
use nom::{
    bytes::complete::take,
    combinator::map,
    multi::many0,
    sequence::preceded,
    {InputTake, Offset},
};
use std::{collections::VecDeque, ops::Range};

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

pub(super) trait IncInnerParser<'a, O> {
    fn parse(&mut self, this: Option<&'a O>, input: TokenStream<'a>) -> IResult<'a, O>;
}

impl<'a, O, F> IncInnerParser<'a, O> for F
where
    F: FnMut(Option<&'a O>, TokenStream<'a>) -> IResult<'a, O>,
    O: Clone + 'a,
{
    fn parse(&mut self, this: Option<&'a O>, input: TokenStream<'a>) -> IResult<'a, O> {
        self(this, input)
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
            return Err(nom::Err::Error(ParserError {
                input: i1,
                kind: ParserErrorKind::IgnoreUntil,
            }));
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
    this: Option<&'a O>,
    mut parser: F,
    error_msg: ParseErrorMessage,
) -> impl FnMut(TokenStream<'a>) -> IResult<Option<O>>
where
    F: IncInnerParser<'a, O>,
    O: Clone,
{
    fn expect_error(input: &mut TokenStream, error_msg: ParseErrorMessage) {
        let pos = input.location_offset() - input.reference_pos;
        let error_pos = if pos > 0 { pos - 1 } else { 0 };
        let spl_error = crate::error::SplError(error_pos..error_pos, error_msg.into());
        input.error_buffer.push(spl_error);
    }

    move |input: TokenStream| match parser.parse(this, input) {
        Ok((input, out)) => Ok((input, Some(out))),
        Err(nom::Err::Error(ParserError {
            kind: ParserErrorKind::Affected,
            input,
        })) => match parser.parse(None, input) {
            Ok((input, out)) => Ok((input, Some(out))),
            Err(nom::Err::Error(mut err)) => {
                expect_error(&mut err.input, error_msg.clone());
                Ok((err.input, None))
            }
            Err(_) => panic!("Incomplete data"),
        },
        Err(nom::Err::Error(mut err)) => {
            expect_error(&mut err.input, error_msg.clone());
            Ok((err.input, None))
        }
        Err(_) => panic!("Incomplete data"),
    }
}

#[derive(Debug, Clone)]
struct CommaPreceded<O> {
    inner: Reference<O>,
}

impl<O> CommaPreceded<O> {
    fn new_wrapped(inner: Reference<O>) -> Reference<Self> {
        assert_ne!(
            inner.offset, 0,
            "There must be a comma in front of this parser"
        );
        Reference {
            offset: inner.offset - 1,
            reference: Self {
                inner: Reference {
                    offset: 1,
                    reference: inner.reference,
                },
            },
        }
    }
}

impl<O: Parser + Clone + ToRange> Parser for CommaPreceded<O> {
    fn parse<'a>(this: Option<&'a Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        let (input, parsed_inner) = preceded(super::symbols::comma, |input| {
            Reference::parse(this.as_ref().map(|this| &this.inner), input)
        })(input)?;
        Ok((
            input,
            Self {
                inner: parsed_inner,
            },
        ))
    }
}

impl<O: Parser + Clone + ToRange> ToRange for CommaPreceded<O> {
    fn to_range(&self) -> std::ops::Range<usize> {
        let Range { start, end } = self.inner.to_range();
        // add one to fill in for added comma
        start..(end + 1)
    }
}

/// Parses a comma separated list of parsers
pub(super) fn parse_list<'a, O>(
    inner_parsers: Option<Vec<Reference<O>>>,
) -> impl FnMut(TokenStream<'a>) -> IResult<Vec<Reference<O>>>
where
    O: Parser + ToRange + Clone + std::fmt::Debug + 'a,
{
    move |input: TokenStream| {
        let mut parsers: VecDeque<_> = inner_parsers.clone().unwrap_or_default().into();
        let first_parser = parsers.pop_front();
        let (input, head) = match Reference::parse(first_parser.as_ref(), input.clone()) {
            Ok((i, head)) => {
                let offset = input.offset(&i);
                let input = input.advance(offset);
                (input, head)
            }
            Err(nom::Err::Error(err)) => {
                let offset = input.offset(&err.input);
                let input = input.advance(offset);
                return Err(nom::Err::Error(ParserError { input, ..err }));
            }
            Err(_) => panic!("Incomplete data"),
        };
        let rest_parsers: Vec<_> = parsers.into();
        let comma_preceded: Vec<_> = rest_parsers
            .into_iter()
            .map(|r| CommaPreceded::new_wrapped(r))
            .collect();
        let (input, tail) = match map(many(Some(&comma_preceded)), |comma_preceded| {
            comma_preceded
                .into_iter()
                .map(|r| Reference {
                    reference: r.reference.inner.reference,
                    offset: r.offset + r.reference.inner.offset,
                })
                .collect::<Vec<_>>()
        })(input.clone())
        {
            Ok((i, head)) => {
                let offset = input.offset(&i);
                let input = input.advance(offset);
                (input, head)
            }
            Err(nom::Err::Error(err)) => {
                let offset = input.offset(&err.input);
                let input = input.advance(offset);
                return Err(nom::Err::Error(ParserError { input, ..err }));
            }
            Err(_) => panic!("Incomplete data"),
        };
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
        let spl_error = crate::error::SplError(info.to_range(), error_msg.clone().into());
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
                // recover backup
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
            Err(nom::Err::Error(mut err)) => {
                // recover backup
                err.input.error_buffer = error_backup;
                Err(nom::Err::Error(err))
            }
            Err(_) => panic!("Incomplete data"),
        }
    }
}

pub(super) fn affected<'a, F, O>(
    mut inner_parser: F,
) -> impl FnMut(Option<&'a O>, TokenStream<'a>) -> IResult<'a, O>
where
    F: InnerParser<'a, O>,
    O: Parser + ToRange + AstInfoTraverser + Clone,
{
    /// True if part of the tokens, that this node pointed to,
    /// were already consumed by previous parsers.
    /// So this node is no longer valid.
    fn is_partially_consumed(
        location_offset: usize,
        token_change: &TokenChange,
        parser_start: usize,
    ) -> bool {
        if token_change.out_of_range(location_offset) {
            let new_start_pos = token_change.new_token_pos(parser_start);
            if location_offset > new_start_pos {
                return true;
            }
        }
        false
    }

    /// True if the current location is inside the area of insertion
    fn is_insertion_here(location_offset: usize, token_change: &TokenChange) -> bool {
        let change_start = token_change.deletion_range.start;
        let insertion_range = change_start..(change_start + token_change.insertion_len);
        insertion_range.contains(&location_offset)
    }

    /// If true, there is no need to re-parse.
    /// This node can neither be reused nor rebuilt
    fn invalid(input: &TokenStream, this_range: &Range<usize>) -> bool {
        let location_offset = input.location_offset();
        let token_change = &input.token_change;

        token_change.deletes(this_range)
            || is_insertion_here(location_offset, token_change)
            || is_partially_consumed(location_offset, token_change, this_range.start)
    }

    const fn affected_error<O>(input: TokenStream) -> IResult<O> {
        Err(nom::Err::Error(ParserError {
            input,
            kind: ParserErrorKind::Affected,
        }))
    }

    move |this, input| {
        if let Some(this) = this {
            let this_range = this.to_range().shift(input.get_old_reference());
            if invalid(&input, &this_range) {
                return affected_error(input);
            }
            // TODO: maybe dynamic affection range
            let affected_range = this_range.start..(this_range.end + 1);
            if input.token_change.overlaps(&affected_range) {
                match inner_parser.parse(input) {
                    Ok(result) => Ok(result),
                    Err(nom::Err::Error(err)) => affected_error(err.input),
                    Err(_) => panic!("Incomplete data"),
                }
            } else {
                fn remove_messages(info: &mut AstInfo) {
                    info.errors.retain(|err| {
                        !matches!(
                            err.1,
                            ErrorMessage::BuildErrorMessage(_)
                                | ErrorMessage::SemanticErrorMessage(_)
                        )
                    });
                }

                // Delete build and semantic errors from unaffected node.
                // If the error persists, it will be re-added in the next phase.
                let mut this_clone = this.clone();
                this_clone.traverse_mut(remove_messages);
                Ok((input.advance(this_range.len()), this_clone))
            }
        } else {
            // parsing from scratch
            inner_parser.parse(input)
        }
    }
}

pub(super) fn many<'a, O>(
    inner_parser: Option<&'a [Reference<O>]>,
) -> impl FnMut(TokenStream<'a>) -> IResult<Vec<Reference<O>>>
where
    O: Parser + ToRange + Clone + std::fmt::Debug,
{
    fn parse_insertion<'a, O: Parser + 'a>(
        mut input: TokenStream<'a>,
        end_pos: usize,
        acc: &mut Vec<O>,
    ) -> IResult<'a, ()> {
        while input.location_offset() < end_pos {
            let (i, out) = O::parse(None, input.clone())?;
            acc.push(out);
            input = i;
        }
        Ok((input, ()))
    }

    fn handle_insertions<'a, O: Parser + 'a>(
        input: TokenStream<'a>,
        parser_start: usize,
        acc: &mut Vec<O>,
    ) -> IResult<'a, ()> {
        let location_offset = input.location_offset();
        let token_change = &input.token_change;
        let change_start = token_change.deletion_range.start;
        let insertion_range = change_start..(change_start + token_change.insertion_len);

        let end_pos = if insertion_range.contains(&location_offset) {
            insertion_range.end
        } else {
            // unaffected by change,
            // but there are unconsumed tokens before this parsers starting position
            token_change.new_token_pos(parser_start)
        };
        parse_insertion(input, end_pos, acc)
    }

    move |mut input| {
        let mut acc = Vec::new();
        let parsers = inner_parser.unwrap_or_default();
        for parser in parsers {
            let parser_start = parser.to_range().shift(parser.offset).start;
            let (i, _) = match handle_insertions(input.clone(), parser_start, &mut acc) {
                Ok(result) => result,
                Err(nom::Err::Error(err)) => return Ok((err.input, acc)),
                Err(e) => return Err(e),
            };
            input = i;
            let (i, out) = match Reference::parse(Some(parser), input.clone()) {
                Err(nom::Err::Error(ParserError {
                    kind: ParserErrorKind::Affected,
                    ..
                })) => {
                    continue;
                }
                Err(nom::Err::Error(_)) => {
                    return Ok((input, acc));
                }
                Err(e) => {
                    return Err(e);
                }
                Ok(result) => result,
            };
            acc.push(out);
            input = i;
        }
        let (input, out) = many0(|input| Reference::parse(None, input))(input)?;
        acc.extend(out);
        Ok((input, acc))
    }
}
