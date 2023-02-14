use std::ops::Range;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    token_type: TokenType,
    range: Range<usize>,
}

impl Token {
    pub fn new(token_type: TokenType, range: Range<usize>) -> Self {
        Self { token_type, range }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    Array,
    Of,
    Proc,
    Ref,
    Type,
    Var,
    Ident(String),
    Char(char),
    Int(u32),
    Hex(u32),
    Comment(String),
    Unknown(String),
}
