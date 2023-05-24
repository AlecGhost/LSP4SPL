use super::IResult;
use crate::{
    ast::{AstInfo, Reference},
    error::{ParseErrorMessage, ParserError, ParserErrorKind},
    lexer::token::TokenStream,
    token::Token,
    Shiftable, ToRange,
};
use nom::{
    bytes::complete::take,
    error::ParseError,
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
pub(super) fn parse_list<'a, O, F>(mut parser: F) -> impl FnMut(TokenStream<'a>) -> IResult<Vec<O>>
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
                // recover backup
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

pub(super) fn affected<'a, F, O>(
    mut inner_parser: F,
) -> impl FnMut(Option<O>, TokenStream<'a>) -> IResult<O>
where
    F: InnerParser<'a, O>,
    O: super::Parser + ToRange,
{
    fn is_partially_consumed(input: &TokenStream, parser_start: usize) -> bool {
        let token_change = &input.token_change;
        let location_offset = input.location_offset();

        if token_change.out_of_range(location_offset) {
            let new_start_pos = token_change.new_token_pos(parser_start);
            if location_offset > new_start_pos {
                return true;
            }
        }
        false
    }

    fn affected_error<'a, O>(input: TokenStream<'a>) -> IResult<O> {
        Err(nom::Err::Error(ParserError {
            input,
            kind: ParserErrorKind::Affected,
        }))
    }
    move |this, input| {
        if let Some(this) = this {
            let this_range = this.to_range().shift(input.old_reference_pos);
            if is_partially_consumed(&input, this_range.start) {
                // Part of the tokens, that this node pointed to,
                // were already consumed by previous parsers.
                // So this node is no longer valid.
                return affected_error(input);
            }
            // TODO: maybe dynamic affection range
            let affected_range = this_range.start..(this_range.end + 1);
            if input.token_change.overlaps(&affected_range) {
                if input.token_change.deletes(&this_range) {
                    // If all tokens of this node were deleted by the change,
                    // there is no need to try to re-parsing.
                    // This also prevents parsing of the next node.
                    return affected_error(input);
                }
                match inner_parser.parse(input) {
                    Ok(result) => Ok(result),
                    Err(nom::Err::Failure(err) | nom::Err::Error(err)) => affected_error(err.input),
                    Err(_) => panic!("Incomplete data"),
                }
            } else {
                Ok((input.advance(this_range.len()), this))
            }
        } else {
            // parsing from scratch
            inner_parser.parse(input)
        }
    }
}

pub(super) fn many<'a, O>(
    inner_parser: Option<Vec<O>>,
) -> impl FnMut(TokenStream<'a>) -> IResult<Vec<O>>
where
    O: super::Parser + ToRange + Clone + std::fmt::Debug,
{
    fn parse_insertion<O: super::Parser>(
        mut input: TokenStream,
        end_pos: usize,
    ) -> IResult<Vec<O>> {
        let mut acc = Vec::new();
        while input.location_offset() < end_pos {
            let (i, out) = match O::parse(None, input.clone()) {
                Err(nom::Err::Error(_)) => return Ok((input, acc)),
                Err(e) => return Err(e),
                Ok((i, out)) => (i, out),
            };
            acc.push(out);
            input = i;
        }
        Ok((input, acc))
    }

    fn handle_insertions<O: super::Parser>(
        input: TokenStream,
        parser_start: usize,
    ) -> IResult<Vec<O>> {
        let location_offset = input.location_offset();
        let token_change = &input.token_change;
        let change_start = token_change.deletion_range.start;
        let insertion_range = change_start..(change_start + token_change.insertion_len);

        if insertion_range.contains(&location_offset) {
            parse_insertion(input, insertion_range.end)
        } else {
            let new_start_pos = token_change.new_token_pos(parser_start);
            if location_offset < new_start_pos {
                // unaffected by change,
                // but only part of the tokens of the previous parser were consumed
                return parse_insertion(input, new_start_pos);
            }
            Ok((input, Vec::new()))
        }
    }

    move |mut input| {
        let mut acc = Vec::new();
        let parsers = inner_parser.clone().unwrap_or_default();
        for parser in parsers {
            let parser_start = parser.to_range().start;
            let (i, out) = handle_insertions(input.clone(), parser_start)?;
            acc.extend(out);
            input = i;
            let (i, out) = match O::parse(Some(parser), input.clone()) {
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
                Ok((i, out)) => (i, out),
            };
            acc.push(out);
            input = i;
        }
        // TODO: handle properly, only when really needed
        let (input, out) = match many0(|input| O::parse(None, input))(input) {
            Ok(result) => result,
            Err(nom::Err::Error(err)) => return Ok((err.input, acc)),
            Err(err) => return Err(err),
        };
        acc.extend(out);
        Ok((input, acc))
    }
}

/// Copy of `nom::branch::alt`.
/// Important difference:
/// Fails if one of the alternatives produces an `Affected` error.
pub(super) fn alt<'a, O, List: Alt<'a, O>>(
    mut l: List,
) -> impl FnMut(TokenStream<'a>) -> IResult<O> {
    move |i| l.choice(i)
}

// Trait implementation and macros also from `nom::branch::alt`.
pub(super) trait Alt<'a, O> {
    /// Tests each parser in the tuple and returns the result of the first one that succeeds
    fn choice(&mut self, input: TokenStream<'a>) -> IResult<'a, O>;
}

macro_rules! succ (
    (0, $submac:ident ! ($($rest:tt)*)) => ($submac!(1, $($rest)*));
    (1, $submac:ident ! ($($rest:tt)*)) => ($submac!(2, $($rest)*));
    (2, $submac:ident ! ($($rest:tt)*)) => ($submac!(3, $($rest)*));
    (3, $submac:ident ! ($($rest:tt)*)) => ($submac!(4, $($rest)*));
    (4, $submac:ident ! ($($rest:tt)*)) => ($submac!(5, $($rest)*));
    (5, $submac:ident ! ($($rest:tt)*)) => ($submac!(6, $($rest)*));
    (6, $submac:ident ! ($($rest:tt)*)) => ($submac!(7, $($rest)*));
    (7, $submac:ident ! ($($rest:tt)*)) => ($submac!(8, $($rest)*));
    (8, $submac:ident ! ($($rest:tt)*)) => ($submac!(9, $($rest)*));
    (9, $submac:ident ! ($($rest:tt)*)) => ($submac!(10, $($rest)*));
    (10, $submac:ident ! ($($rest:tt)*)) => ($submac!(11, $($rest)*));
    (11, $submac:ident ! ($($rest:tt)*)) => ($submac!(12, $($rest)*));
    (12, $submac:ident ! ($($rest:tt)*)) => ($submac!(13, $($rest)*));
    (13, $submac:ident ! ($($rest:tt)*)) => ($submac!(14, $($rest)*));
    (14, $submac:ident ! ($($rest:tt)*)) => ($submac!(15, $($rest)*));
    (15, $submac:ident ! ($($rest:tt)*)) => ($submac!(16, $($rest)*));
    (16, $submac:ident ! ($($rest:tt)*)) => ($submac!(17, $($rest)*));
    (17, $submac:ident ! ($($rest:tt)*)) => ($submac!(18, $($rest)*));
    (18, $submac:ident ! ($($rest:tt)*)) => ($submac!(19, $($rest)*));
    (19, $submac:ident ! ($($rest:tt)*)) => ($submac!(20, $($rest)*));
    (20, $submac:ident ! ($($rest:tt)*)) => ($submac!(21, $($rest)*));
);

macro_rules! alt_trait(
    ($first:ident $second:ident $($id: ident)+) => (
        alt_trait!(__impl $first $second; $($id)+);
    );
    (__impl $($current:ident)*; $head:ident $($id: ident)+) => (
        alt_trait_impl!($($current)*);

        alt_trait!(__impl $($current)* $head; $($id)+);
    );
    (__impl $($current:ident)*; $head:ident) => (
        alt_trait_impl!($($current)*);
        alt_trait_impl!($($current)* $head);
    );
);

macro_rules! alt_trait_impl(
    ($($id:ident)+) => (
        impl<'a, Output, $($id: InnerParser<'a, Output>),+> Alt<'a, Output> for ( $($id),+ ) {
            fn choice(&mut self, input: TokenStream<'a>) -> IResult<'a, Output> {
                match self.0.parse(input.clone()) {
                    Err(nom::Err::Error(ParserError {
                        kind: ParserErrorKind::Affected,
                        input
                    })) => Err(nom::Err::Error(ParserError {
                        kind: ParserErrorKind::Affected,
                        input
                    })),
                    Err(nom::Err::Error(e)) => {
                        alt_trait_inner!(1, self, input, e, $($id)+)
                    },
                    res => res,
                }
            }
        }
    );
);

macro_rules! alt_trait_inner(
    ($it:tt, $self:expr, $input:expr, $err:expr, $head:ident $($id:ident)+) => (
        match $self.$it.parse($input.clone()) {
            Err(nom::Err::Error(ParserError {
                kind: ParserErrorKind::Affected,
                input
            })) => Err(nom::Err::Error(ParserError {
                kind: ParserErrorKind::Affected,
                input
            })),
            Err(nom::Err::Error(e)) => {
                let err = $err.or(e);
                succ!($it, alt_trait_inner!($self, $input, err, $($id)+))
            }
            res => res,
    }
    );
    ($it:tt, $self:expr, $input:expr, $err:expr, $head:ident) => (
        Err(nom::Err::Error(ParserError::append($input, nom::error::ErrorKind::Alt, $err)))
    );
);

alt_trait!(A B C D E F G H I J K L M N O P Q R S T U);
