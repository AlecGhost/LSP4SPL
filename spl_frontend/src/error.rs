use crate::token::TokenStream;
use crate::Shiftable;
use crate::{ast::Identifier, ToRange};
use std::fmt::{Debug, Display};
use std::ops::Range;
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq, Eq)]
#[error("{1}")]
pub struct SplError(pub Range<usize>, pub String);

impl ToRange for SplError {
    fn to_range(&self) -> Range<usize> {
        self.0.clone()
    }
}

impl Shiftable for SplError {
    fn shift(self, offset: usize) -> Self {
        Self(self.0.shift(offset), self.1)
    }
}

impl Shiftable for Vec<SplError> {
    fn shift(self, offset: usize) -> Self {
        self.into_iter().map(|err| err.shift(offset)).collect()
    }
}

impl Identifier {
    pub fn to_error<M, T>(&self, msg: M) -> SplError
    where
        M: Fn(String) -> T,
        T: ToString,
    {
        // Identifier position is the last in the range (which might contain comments)
        let end_pos = self.to_range().end;
        assert!(end_pos > 0, "Identifier must contain at least one token");
        let start_pos = end_pos - 1;
        SplError(start_pos..end_pos, msg(self.value.clone()).to_string())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LexErrorMessage {
    MissingClosingTick,
    ExpectedHexNumber,
    InvalidIntLit(String),
}
impl Display for LexErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::MissingClosingTick => "missing closing `'`".to_string(),
            Self::ExpectedHexNumber => "expected `hexadecimal number`".to_string(),
            Self::InvalidIntLit(i) => format!("invalid integer literal: `{}`", i),
        };
        writeln!(f, "{}", display)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseErrorMessage {
    MissingOpening(char),
    MissingClosing(char),
    MissingTrailingSemic,
    UnexpectedCharacters(String),
    ExpectedToken(String),
    ConfusedToken(String, String),
}

impl Display for ParseErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::MissingOpening(c) => format!("missing opening `{}`", c),
            Self::MissingClosing(c) => format!("missing closing `{}`", c),
            Self::MissingTrailingSemic => "missing trailing `;`".to_string(),
            Self::UnexpectedCharacters(s) => format!("unexpected `{}`", s),
            Self::ExpectedToken(t) => format!("expected `{}`", t),
            Self::ConfusedToken(expected, got) => {
                format!("expected `{}`, but got `{}`", expected, got)
            }
        };
        writeln!(f, "{}", display)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(usize)]
pub enum BuildErrorMessage {
    UndefinedType(String) = 101,
    NotAType(String),
    RedeclarationAsType(String),
    MustBeAReferenceParameter(String),
    RedeclarationAsProcedure(String),
    RedeclarationAsParameter(String),
    RedeclarationAsVariable(String),
    MainIsMissing = 125,
    MainIsNotAProcedure,
    MainMustNotHaveParameters,
}

impl Display for BuildErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::UndefinedType(name) => format!("undefined type `{}`", name),
            Self::NotAType(name) => format!("`{}` is not a type", name),
            Self::RedeclarationAsType(name) => format!("redeclaration of `{}` as type", name),
            Self::MustBeAReferenceParameter(name) => {
                format!("parameter `{}` must be a reference parameter", name)
            }
            Self::RedeclarationAsProcedure(name) => {
                format!("redeclaration of `{}` as procedure", name)
            }
            Self::RedeclarationAsParameter(name) => {
                format!("redeclaration of `{}` as parameter", name)
            }
            Self::RedeclarationAsVariable(name) => {
                format!("redeclaration of `{}` as variable", name)
            }
            Self::MainIsMissing => "procedure `main` is missing".to_string(),
            Self::MainIsNotAProcedure => "`main` is not a procedure".to_string(),
            Self::MainMustNotHaveParameters => {
                "procedure `main` must not have any parameters".to_string()
            }
        };
        writeln!(f, "{}", display)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(usize)]
pub enum SemanticErrorMessage {
    AssignmentHasDifferentTypes = 108,
    AssignmentRequiresIntegers,
    IfConditionMustBeBoolean,
    WhileConditionMustBeBoolean,
    UndefinedProcedure(String),
    CallOfNoneProcedure(String),
    ArgumentsTypeMismatch(String, usize),
    ArgumentMustBeAVariable(String, usize),
    TooFewArguments(String),
    TooManyArguments(String),
    OperatorDifferentTypes,
    ComparisonNonInteger,
    ArithmeticOperatorNonInteger,
    UndefinedVariable(String),
    NotAVariable(String),
    IndexingNonArray,
    IndexingWithNonInteger,
}

impl Display for SemanticErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::AssignmentHasDifferentTypes => "assignment has different types".to_string(),
            Self::AssignmentRequiresIntegers => "assignment requires integer variable".to_string(),
            Self::IfConditionMustBeBoolean => {
                "`if` test expression must be of type boolean".to_string()
            }
            Self::WhileConditionMustBeBoolean => {
                "`while` test expression must be of type boolean".to_string()
            }
            Self::UndefinedProcedure(name) => format!("undefined procedure `{}`", name),
            Self::CallOfNoneProcedure(name) => format!("call of non-procedure `{}`", name),
            Self::ArgumentsTypeMismatch(name, index) => {
                format!("procedure `{}` argument `{}` type mismatch", name, index)
            }
            Self::ArgumentMustBeAVariable(name, index) => {
                format!(
                    "procedure `{}` argument `{}` must be a variable",
                    name, index
                )
            }
            Self::TooFewArguments(name) => {
                format!("procedure `{}` called with too few arguments", name)
            }
            Self::TooManyArguments(name) => {
                format!("procedure `{}` called with too many arguments", name)
            }
            Self::OperatorDifferentTypes => "expression combines different types".to_string(),
            Self::ComparisonNonInteger => "comparison requires integer operands".to_string(),
            Self::ArithmeticOperatorNonInteger => {
                "arithmetic operation requires integer operands".to_string()
            }
            Self::UndefinedVariable(name) => format!("undefined variable `{}`", name),
            Self::NotAVariable(name) => format!("`{}` is not a variable", name),
            Self::IndexingNonArray => "illegal indexing a non-array".to_string(),
            Self::IndexingWithNonInteger => "illegal indexing with a non-integer".to_string(),
        };
        writeln!(f, "{}", display)
    }
}

#[derive(Debug, Error)]
pub struct OperatorConversionError<T: Debug> {
    item: T,
}

impl<T: Debug> OperatorConversionError<T> {
    pub const fn new(item: T) -> Self {
        Self { item }
    }
}

impl<T: Debug> Display for OperatorConversionError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Cannot convert `{:?}` to an operator", self.item)
    }
}

#[derive(Debug, Error)]
pub struct KeyAlreadyExistsError<T: Debug> {
    key: T,
}

impl<T: Debug> KeyAlreadyExistsError<T> {
    pub const fn new(key: T) -> Self {
        Self { key }
    }
}

impl<T: Debug> Display for KeyAlreadyExistsError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Key `{:?}` already exists", self.key)
    }
}

#[derive(Clone, Debug)]
pub struct ParserError<'a> {
    pub kind: ParserErrorKind,
    pub input: TokenStream<'a>,
}

impl<'a> nom::error::ParseError<TokenStream<'a>> for ParserError<'a> {
    fn append(_: TokenStream, _: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn from_error_kind(input: TokenStream<'a>, kind: nom::error::ErrorKind) -> Self {
        Self { kind: ParserErrorKind::Nom(kind), input }
    }
}

#[derive(Clone, Debug)]
pub enum ParserErrorKind {
    Token,
    Affected,
    Expect,
    IgnoreUntil,
    Nom(nom::error::ErrorKind),
}
