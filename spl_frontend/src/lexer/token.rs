use crate::{error::SplError, ErrorContainer, Shiftable, ToRange};
use nom::Needed;
use std::{
    iter::Enumerate,
    ops::{Range, RangeTo},
};

pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACKET: &str = "[";
pub const RBRACKET: &str = "]";
pub const LCURLY: &str = "{";
pub const RCURLY: &str = "}";
pub const EQ: &str = "=";
pub const NEQ: &str = "#";
pub const LT: &str = "<";
pub const LE: &str = "<=";
pub const GT: &str = ">";
pub const GE: &str = ">=";
pub const ASSIGN: &str = ":=";
pub const COLON: &str = ":";
pub const COMMA: &str = ",";
pub const SEMIC: &str = ";";
pub const PLUS: &str = "+";
pub const MINUS: &str = "-";
pub const TIMES: &str = "*";
pub const DIVIDE: &str = "/";
pub const IF: &str = "if";
pub const ELSE: &str = "else";
pub const WHILE: &str = "while";
pub const ARRAY: &str = "array";
pub const OF: &str = "of";
pub const PROC: &str = "proc";
pub const REF: &str = "ref";
pub const TYPE: &str = "type";
pub const VAR: &str = "var";

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Token {
    pub token_type: TokenType,
    pub range: Range<usize>,
    pub errors: Vec<SplError>,
}

impl Token {
    pub const fn new(token_type: TokenType, range: Range<usize>) -> Self {
        Self {
            token_type,
            range,
            errors: Vec::new(),
        }
    }

    pub const fn new_with_errors(
        token_type: TokenType,
        range: Range<usize>,
        errors: Vec<SplError>,
    ) -> Self {
        Self {
            token_type,
            range,
            errors,
        }
    }

    pub(super) fn is_affected_by(&self, index: usize) -> bool {
        self.range.end + usize::from(self.token_type.look_ahead()) > index
    }
}

impl ToRange for Token {
    fn to_range(&self) -> Range<usize> {
        self.range.clone()
    }
}

impl ErrorContainer for Token {
    fn errors(&self) -> Vec<SplError> {
        self.errors.clone()
    }
}

impl Shiftable for Token {
    fn shift(self, offset: usize) -> Self {
        Self {
            range: self.range.shift(offset),
            ..self
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type)
    }
}

pub trait TokenList {
    fn token_before(&self, index: usize) -> Option<&Token>;
}

impl TokenList for &[Token] {
    fn token_before(&self, index: usize) -> Option<&Token> {
        if let Some(first) = self.first() {
            if first.range.start > index {
                return None;
            }
            let mut current = first;
            for token in self.iter() {
                if token.range.start >= index {
                    break;
                }
                current = token;
            }
            Some(current)
        } else {
            None
        }
    }
}

impl ErrorContainer for Vec<Token> {
    fn errors(&self) -> Vec<SplError> {
        self.iter().flat_map(|token| token.errors()).collect()
    }
}

impl ToRange for Vec<Token> {
    fn to_range(&self) -> Range<usize> {
        let start = self.first().map_or(0, |token| token.range.start);
        let end = self.last().map_or(0, |token| token.range.end);
        start..end
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntResult {
    Int(u32),
    Err(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenType {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LCurly,
    RCurly,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    Assign,
    Colon,
    Comma,
    Semic,
    Plus,
    Minus,
    Times,
    Divide,
    If,
    Else,
    While,
    Array,
    Of,
    Proc,
    Ref,
    Type,
    Var,
    Ident(String),
    Char(char),
    Int(IntResult),
    Hex(IntResult),
    Comment(String),
    Unknown(String),
    Eof,
}

impl TokenType {
    pub const fn is_symbol(&self) -> bool {
        use TokenType::*;
        matches!(
            self,
            LParen
                | RParen
                | LBracket
                | RBracket
                | LCurly
                | RCurly
                | Eq
                | Neq
                | Lt
                | Le
                | Gt
                | Ge
                | Assign
                | Colon
                | Comma
                | Semic
                | Plus
                | Minus
                | Times
                | Divide
        )
    }

    pub const fn is_keyword(&self) -> bool {
        use TokenType::*;
        matches!(
            self,
            If | Else | While | Array | Of | Proc | Ref | Type | Var
        )
    }

    const fn look_ahead(&self) -> u8 {
        use TokenType::*;
        match self {
            If | Else | While | Array | Of | Proc | Ref | Type | Var | Colon | Divide | Lt | Gt
            | Int(_) | Ident(_) | Hex(_) => 1,
            LParen | RParen | LBracket | RBracket | LCurly | RCurly | Eq | Neq | Le | Ge
            | Assign | Comma | Semic | Plus | Minus | Times | Comment(_) | Unknown(_) | Eof => 0,
            Char(_) => {
                1 // this is a worst case look ahead.
            }
        }
    }

    pub(super) const fn as_static_str(&self) -> Option<&'static str> {
        use TokenType::*;
        match self {
            LParen => Some(LPAREN),
            RParen => Some(RPAREN),
            LBracket => Some(LBRACKET),
            RBracket => Some(RBRACKET),
            LCurly => Some(LCURLY),
            RCurly => Some(RCURLY),
            Eq => Some(EQ),
            Neq => Some(NEQ),
            Lt => Some(LT),
            Le => Some(LE),
            Gt => Some(GT),
            Ge => Some(GE),
            Assign => Some(ASSIGN),
            Colon => Some(COLON),
            Comma => Some(COMMA),
            Semic => Some(SEMIC),
            Plus => Some(PLUS),
            Minus => Some(MINUS),
            Times => Some(TIMES),
            Divide => Some(DIVIDE),
            If => Some(IF),
            Else => Some(ELSE),
            While => Some(WHILE),
            Array => Some(ARRAY),
            Of => Some(OF),
            Proc => Some(PROC),
            Ref => Some(REF),
            Type => Some(TYPE),
            Var => Some(VAR),
            Eof => Some(""),
            _ => None,
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenType::*;
        let string = match self {
            Ident(s) | Unknown(s) => s.to_string(),
            Comment(s) => format!("// {}\n", s.trim()),
            Char(c) => {
                if c == &'\n' {
                    "'\\n'".to_string()
                } else {
                    format!("'{}'", c)
                }
            }
            Int(int_result) => match int_result {
                IntResult::Int(i) => i.to_string(),
                IntResult::Err(err) => err.to_string(),
            },
            Hex(int_result) => match int_result {
                IntResult::Int(i) => format!("{:#04X}", i), // prints in the format 0x0A
                IntResult::Err(err) => format!("0x{}", err),
            },
            token_type => token_type
                .as_static_str()
                .expect("No static representation available")
                .to_string(),
        };
        write!(f, "{}", string)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenChange {
    pub deletion_range: Range<usize>,
    pub insertion_len: usize,
}

impl TokenChange {
    pub fn new(deletion_range: Range<usize>, insertion_len: usize) -> Self {
        Self {
            deletion_range,
            insertion_len,
        }
    }

    /// Returns true, if this range and the other range overlap.
    /// Empty changes count as overlapping,
    /// if they are inside the other range,
    /// except at the very start.
    pub fn overlaps(&self, other_range: &Range<usize>) -> bool {
        let this_range = &self.deletion_range;
        if this_range.is_empty() {
            if this_range.end == other_range.start {
                // insertion at start does not affect nonterminal
                // it should already be handled by a previous rule
                false
            } else {
                other_range.contains(&this_range.start)
            }
        } else {
            this_range.start.max(other_range.start) < this_range.end.min(other_range.end)
        }
    }

    /// Returns true, if the tokens in the other range are completely deleted by this change.
    pub fn deletes(&self, other_range: &Range<usize>) -> bool {
        let this_range = &self.deletion_range;
        this_range.start <= other_range.start && other_range.end <= this_range.end
    }

    /// Returns true, if the provided position is greater or equal
    /// to the position of the first unchanged token in the new token stream.
    pub fn out_of_range(&self, position: usize) -> bool {
        let first_unchanged_token =
            self.deletion_range.end + self.insertion_len - self.deletion_range.len();
        position >= first_unchanged_token
    }

    /// Calculates the (theoretical) position of a token based on this change.
    pub fn new_token_pos(&self, old_token_pos: usize) -> usize {
        if old_token_pos >= self.deletion_range.end {
            old_token_pos + self.insertion_len - self.deletion_range.len()
        } else {
            old_token_pos
        }
    }
}

#[derive(Clone, Debug)]
pub struct TokenStream<'a> {
    tokens: &'a [Token],
    first_ptr: *const Token,
    pub token_change: TokenChange,
    pub error_buffer: Vec<SplError>,
    pub inc_references: Vec<usize>,
    pub reference_pos: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let token_change = TokenChange::new(0..0, tokens.len());
        Self {
            tokens,
            first_ptr: tokens.as_ptr(),
            token_change,
            error_buffer: Vec::new(),
            inc_references: Vec::new(),
            reference_pos: 0,
        }
    }

    pub const fn new_with_change(tokens: &'a [Token], token_change: TokenChange) -> Self {
        Self {
            tokens,
            first_ptr: tokens.as_ptr(),
            token_change,
            error_buffer: Vec::new(),
            inc_references: Vec::new(),
            reference_pos: 0,
        }
    }

    /// Access to first token
    pub fn fragment(&self) -> Option<&Token> {
        self.tokens.get(0)
    }
}

impl TokenStream<'_> {
    fn distance(first: *const Token, second: *const Token) -> usize {
        // because we do pointer arithmetic, the size of `Token` in memory is needed,
        // to calculate the offset.
        let size = std::mem::size_of::<Token>();

        (second as usize - first as usize) / size
    }

    pub fn location_offset(&self) -> usize {
        TokenStream::distance(self.first_ptr, self.tokens.as_ptr())
    }

    pub fn tokens(&self) -> Vec<Token> {
        self.tokens.to_owned()
    }

    /// Advances the token stream by the provided offset.
    /// The other values do not change.
    pub fn advance(self, offset: usize) -> Self {
        Self {
            tokens: &self.tokens[offset..],
            ..self
        }
    }

    pub fn get_old_reference(&self) -> usize {
        self.inc_references.iter().fold(0, |acc, i| acc + i)
    }
}

impl<'a> ToRange for TokenStream<'a> {
    fn to_range(&self) -> Range<usize> {
        self.fragment().map_or(0..0, |token| token.range.clone())
    }
}

/// source: [Stackoverflow](https://stackoverflow.com/a/57203324)
/// enables indexing and slicing
impl<'a, Idx> std::ops::Index<Idx> for TokenStream<'a>
where
    Idx: std::slice::SliceIndex<[Token]>,
{
    type Output = Idx::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        &self.tokens[index]
    }
}

impl<'a> nom::InputLength for TokenStream<'a> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a> nom::InputTake for TokenStream<'a> {
    fn take(&self, count: usize) -> Self {
        Self {
            tokens: &self.tokens[0..count],
            ..self.clone()
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        (
            Self {
                tokens: &self.tokens[count..],
                ..self.clone()
            },
            Self {
                tokens: &self.tokens[0..count],
                ..self.clone()
            },
        )
    }
}

/// source: [nom traits](https://docs.rs/nom/latest/src/nom/traits.rs.html#62-69)
impl<'a> nom::Offset for TokenStream<'a> {
    fn offset(&self, second: &Self) -> usize {
        let fst = self.tokens.as_ptr();
        let snd = second.tokens.as_ptr();
        TokenStream::distance(fst, snd)
    }
}

impl<'a> nom::Slice<RangeTo<usize>> for TokenStream<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self {
            tokens: &self.tokens[range],
            ..self.clone()
        }
    }
}

/// source: [Monkey Rust lexer](https://github.com/Rydgel/monkey-rust/blob/master/lib/lexer/token.rs)
impl<'a> nom::InputIter for TokenStream<'a> {
    type Item = &'a Token;
    type Iter = Enumerate<::std::slice::Iter<'a, Token>>;
    type IterElem = ::std::slice::Iter<'a, Token>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, Token>> {
        self.tokens.iter().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, Token> {
        self.tokens.iter()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tokens.iter().position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tokens.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }
}
