use std::ops::Range;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError(pub Range<usize>, pub ParseErrorMessage);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseErrorMessage {
    MissingOpening(char),
    MissingClosing(char),
    MissingTrailingSemic,
    UnexpectedCharacters(String),
    ExpectedToken(String),
}

impl ToString for ParseErrorMessage {
    fn to_string(&self) -> String {
        match self {
            Self::MissingOpening(c) => format!("missing opening `{}`", c),
            Self::MissingClosing(c) => format!("missing closing `{}`", c),
            Self::MissingTrailingSemic => "missing trailing `;`".to_string(),
            Self::UnexpectedCharacters(s) => format!("unexpected `{}`", s),
            Self::ExpectedToken(t) => format!("expected `{}`", t),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuildError(pub Range<usize>, pub BuildErrorMessage);

#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(usize)]
pub enum BuildErrorMessage {
    UndefinedType(String) = 101,
    RedeclarationAsType(String) = 103,
    MustBeAReferenceParameter(String) = 104,
    RedeclarationAsProcedure(String) = 105,
    RedeclarationAsParameter(String) = 106,
    RedeclarationAsVariable(String) = 107,
    MainIsMissing = 125,
    MainIsNotAProcedure = 126,
    MainMustNotHaveParameters = 127,
}

impl ToString for BuildErrorMessage {
    fn to_string(&self) -> String {
        match self {
            Self::UndefinedType(name) => format!("undefined type {}", name),
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
        }
    }
}
