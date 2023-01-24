use crate::{
    ast::{Identifier, TypeExpression},
    error::{BuildError, BuildErrorMessage},
    DiagnosticsBroker,
};
pub use build::build;
use std::collections::HashMap;

mod build;
#[cfg(test)]
mod tests;

trait BuildErrorBroker: Clone + std::fmt::Debug + DiagnosticsBroker<BuildError> {}

impl<T> BuildErrorBroker for T where T: Clone + std::fmt::Debug + DiagnosticsBroker<BuildError> {}

impl Identifier {
    fn to_table_error(&self, msg: impl Fn(String) -> BuildErrorMessage) -> BuildError {
        BuildError(self.range.clone(), msg(self.value.clone()))
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
