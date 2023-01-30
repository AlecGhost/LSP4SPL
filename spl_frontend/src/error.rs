use crate::ast::Identifier;
use std::fmt::Display;
use std::ops::Range;
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq, Eq)]
#[error("Range {} - {}: {1}", .0.start, .0.end)]
pub struct ParseError(pub Range<usize>, pub ParseErrorMessage);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseErrorMessage {
    MissingOpening(char),
    MissingClosing(char),
    MissingTrailingSemic,
    UnexpectedCharacters(String),
    ExpectedToken(String),
}

impl Display for ParseErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::MissingOpening(c) => format!("missing opening '{}'", c),
            Self::MissingClosing(c) => format!("missing closing '{}'", c),
            Self::MissingTrailingSemic => "missing trailing ';'".to_string(),
            Self::UnexpectedCharacters(s) => format!("unexpected '{}'", s),
            Self::ExpectedToken(t) => format!("expected '{}'", t),
        };
        writeln!(f, "{}", display)
    }
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
#[error("Range {} - {}: {1}", .0.start, .0.end)]
pub struct BuildError(pub Range<usize>, pub BuildErrorMessage);

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
            Self::UndefinedType(name) => format!("undefined type {}", name),
            Self::NotAType(name) => format!("{} is not a type", name),
            Self::RedeclarationAsType(name) => format!("redeclaration of {} as type", name),
            Self::MustBeAReferenceParameter(name) => {
                format!("parameter {} mus be a reference parameter", name)
            }
            Self::RedeclarationAsProcedure(name) => {
                format!("redeclaration of {} as procedure", name)
            }
            Self::RedeclarationAsParameter(name) => {
                format!("redeclaration of {} as parameter", name)
            }
            Self::RedeclarationAsVariable(name) => {
                format!("redeclaration of {} as variable", name)
            }
            Self::MainIsMissing => "procedure 'main' is missing".to_string(),
            Self::MainIsNotAProcedure => "'main' is not a procedure".to_string(),
            Self::MainMustNotHaveParameters => {
                "procedure 'main' must not have any parameters".to_string()
            }
        };
        writeln!(f, "{}", display)
    }
}

impl Identifier {
    pub fn to_build_error(&self, msg: impl Fn(String) -> BuildErrorMessage) -> BuildError {
        BuildError(self.range.clone(), msg(self.value.clone()))
    }
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
#[error("Range {} - {}: {1}", .0.start, .0.end)]
pub struct SemanticError(pub Range<usize>, pub SemanticErrorMessage);

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
                "'if' test expression must be of type boolean".to_string()
            }
            Self::WhileConditionMustBeBoolean => {
                "'while' test expression must be of type boolean".to_string()
            }
            Self::UndefinedProcedure(name) => format!("undefined procedure {}", name),
            Self::CallOfNoneProcedure(name) => format!("call of non-procedure {}", name),
            Self::ArgumentsTypeMismatch(name, index) => {
                format!("procedure {} argument {} type mismatch", name, index)
            }
            Self::ArgumentMustBeAVariable(name, index) => {
                format!("procedure {} argument {} must be a variable", name, index)
            }
            Self::TooFewArguments(name) => {
                format!("procedure {} called with too few arguments", name)
            }
            Self::TooManyArguments(name) => {
                format!("procedure {} called with too many arguments", name)
            }
            Self::OperatorDifferentTypes => "expression combines different types".to_string(),
            Self::ComparisonNonInteger => "comparison requires integer operands".to_string(),
            Self::ArithmeticOperatorNonInteger => {
                "arithmetic operation requires integer operands".to_string()
            }
            Self::UndefinedVariable(name) => format!("undefined variable {}", name),
            Self::NotAVariable(name) => format!("{} is not a variable", name),
            Self::IndexingNonArray => "illegal indexing a non-array".to_string(),
            Self::IndexingWithNonInteger => "illegal indexing with a non-integer".to_string(),
        };
        writeln!(f, "{}", display)
    }
}

impl Identifier {
    pub fn to_semantic_error(&self, msg: impl Fn(String) -> SemanticErrorMessage) -> SemanticError {
        SemanticError(self.range.clone(), msg(self.value.clone()))
    }
}
