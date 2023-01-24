use crate::{
    ast::{Identifier, TypeExpression},
    DiagnosticsBroker,
};
pub use build::build;
use std::{collections::HashMap, ops::Range};

mod build;
#[cfg(test)]
mod tests;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TableError(Range<usize>, ErrorMessage);

trait TableErrorBroker: Clone + std::fmt::Debug + DiagnosticsBroker<TableError> {}

impl<T> TableErrorBroker for T where T: Clone + std::fmt::Debug + DiagnosticsBroker<TableError> {}

impl Identifier {
    fn to_table_error(&self, msg: impl Fn(String) -> ErrorMessage) -> TableError {
        TableError(self.range.clone(), msg(self.value.clone()))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(usize)]
pub enum ErrorMessage {
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

impl ToString for ErrorMessage {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableEntry {
    pub is_ref: bool,
    pub type_expr: Option<TypeExpression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcedureEntry {
    pub local_table: SymbolTable,
    pub parameters: Vec<VariableEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Entry {
    Type(Option<TypeExpression>),
    Variable(VariableEntry),
    Parameter(VariableEntry),
    Procedure(ProcedureEntry),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolTable {
    entries: HashMap<Identifier, Entry>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    fn enter(&mut self, key: Identifier, value: Entry, mut on_error: impl FnMut()) {
        // TODO: More effective lookup
        if !self.entries.keys().any(|ident| ident.value == key.value) {
            self.entries.insert(key, value);
        } else {
            on_error();
        }
    }

    fn lookup(&self, key: &Identifier) -> Option<&Entry> {
        self.entries.get(key)
    }
}
