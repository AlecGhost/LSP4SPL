use crate::ToRange;
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
}

impl Token {
    pub fn new(token_type: TokenType, range: Range<usize>) -> Self {
        Self { token_type, range }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type)
    }
}

pub trait TokenList {
    fn token_at(&self, index: usize) -> Option<&Token>;
    fn token_before(&self, index: usize) -> Option<&Token>;
}

impl TokenList for Vec<Token> {
    fn token_at(&self, index: usize) -> Option<&Token> {
        self.iter().find(|token| token.range.contains(&index))
    }

    fn token_before(&self, index: usize) -> Option<&Token> {
        if let Some(first) = self.first() {
            if first.range.start > index {
                return None;
            }
            let mut current = first;
            for token in self.iter() {
                if token.range.start >= index {
                    break;
                } else {
                    current = token;
                }
            }
            Some(current)
        } else {
            None
        }
    }
}

impl ToRange for Vec<Token> {
    fn to_range(&self) -> Range<usize> {
        let start = self.first().map(|token| token.range.start).unwrap_or(0);
        let end = self.last().map(|token| token.range.end).unwrap_or(0);
        start..end
    }
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
    Int(Result<u32, String>),
    Hex(Result<u32, String>),
    Comment(String),
    Unknown(String),
    Eof,
}

impl TokenType {
    pub fn is_symbol(&self) -> bool {
        use TokenType::*;
        matches!(
            self,
            LParen
                | RParen
                | LBracket
                | RBracket
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

    pub fn is_keyword(&self) -> bool {
        use TokenType::*;
        matches!(
            self,
            If | Else | While | Array | Of | Proc | Ref | Type | Var
        )
    }

    pub(super) fn as_static_str(&self) -> &'static str {
        use TokenType::*;
        match self {
            LParen => LPAREN,
            RParen => RPAREN,
            LBracket => LBRACKET,
            RBracket => RBRACKET,
            LCurly => LCURLY,
            RCurly => RCURLY,
            Eq => EQ,
            Neq => NEQ,
            Lt => LT,
            Le => LE,
            Gt => GT,
            Ge => GE,
            Assign => ASSIGN,
            Colon => COLON,
            Comma => COMMA,
            Semic => SEMIC,
            Plus => PLUS,
            Minus => MINUS,
            Times => TIMES,
            Divide => DIVIDE,
            If => IF,
            Else => ELSE,
            While => WHILE,
            Array => ARRAY,
            Of => OF,
            Proc => PROC,
            Ref => REF,
            Type => TYPE,
            Var => VAR,
            _ => panic!("No static representation available"),
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenType::*;
        let string = match self {
            Ident(s) => s.to_string(),
            Char(c) => c.to_string(),
            Int(result) => match result {
                Ok(value) => value.to_string(),
                Err(int_string) => int_string.to_string(),
            },
            Hex(result) => match result {
                Ok(value) => value.to_string(),
                Err(hex_string) => hex_string.to_string(),
            },
            Comment(s) => s.to_string(),
            Unknown(s) => s.to_string(),
            token_type => token_type.as_static_str().to_string(),
        };
        write!(f, "{}", string)
    }
}

#[derive(Clone, Debug)]
pub struct Tokens<'a, B> {
    tokens: &'a [Token],
    pub broker: B,
    // error_pos is the end position of the previous token
    pub error_pos: usize,
}

/// source: https://stackoverflow.com/a/57203324
/// enables indexing and slicing
impl<'a, B, Idx> std::ops::Index<Idx> for Tokens<'a, B>
where
    Idx: std::slice::SliceIndex<[Token]>,
{
    type Output = Idx::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        &self.tokens[index]
    }
}

impl<'a, B: Clone> Tokens<'a, B> {
    pub fn new(tokens: &'a [Token], broker: B) -> Self {
        Self {
            tokens,
            broker,
            error_pos: 0,
        }
    }

    pub fn fragment(&self) -> &Token {
        assert!(!self.tokens.is_empty(), "Token slice must not be empty");
        &self.tokens[0]
    }
}

impl<'a, B: Clone> ToRange for Tokens<'a, B> {
    fn to_range(&self) -> Range<usize> {
        self.fragment().range.clone()
    }
}

impl<'a, B: Clone> nom::InputLength for Tokens<'a, B> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a, B: Clone> nom::InputTake for Tokens<'a, B> {
    fn take(&self, count: usize) -> Self {
        Self {
            tokens: &self.tokens[0..count],
            ..self.clone()
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        // if no previous token exists, error_pos stays 0
        let error_pos = if count > 0 {
            self.tokens[count - 1].range.end
        } else {
            0
        };
        (
            Self {
                tokens: &self.tokens[count..],
                error_pos,
                ..self.clone()
            },
            Self {
                tokens: &self.tokens[0..count],
                ..self.clone()
            },
        )
    }
}

/// source: https://docs.rs/nom/latest/src/nom/traits.rs.html#62-69
impl<'a, B> nom::Offset for Tokens<'a, B> {
    fn offset(&self, second: &Self) -> usize {
        let fst = self.tokens.as_ptr();
        let snd = second.tokens.as_ptr();
        // because we do pointer arithmetic, the size of `Token` in memory is needed,
        // to calculate the offset.
        let size = std::mem::size_of::<Token>();

        (snd as usize - fst as usize) / size
    }
}

impl<'a, B: Clone> nom::Slice<RangeTo<usize>> for Tokens<'a, B> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        // if no previous token exists, error_pos stays 0
        let error_pos = if range.end > 0 {
            self.tokens[range.end - 1].range.end
        } else {
            0
        };
        Self {
            tokens: &self.tokens[range],
            error_pos,
            ..self.clone()
        }
    }
}

/// source: https://github.com/Rydgel/monkey-rust/blob/master/lib/lexer/token.rs
impl<'a, B: Clone> nom::InputIter for Tokens<'a, B> {
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
