use std::ops::Range;
use crate::DiagnosticsBroker;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TableError(Range<usize>, ErrorMessage);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorMessage {
    MissingOpening(char),
    MissingClosing(char),
    MissingTrailingSemic,
    UnexpectedCharacters(String),
    ExpectedToken(String),
}

impl ToString for ErrorMessage {
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

trait TableErrorBroker: Clone + std::fmt::Debug + DiagnosticsBroker<TableError> {}

impl<T> TableErrorBroker for T where T: Clone + std::fmt::Debug + DiagnosticsBroker<TableError> {}
