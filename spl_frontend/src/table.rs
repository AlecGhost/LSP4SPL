use crate::{ast::Identifier, error::BuildErrorMessage, DiagnosticsBroker};
pub use build::build;
pub use semantic::analyze;
use std::{collections::HashMap, ops::Range};

mod build;
mod initialization;
mod semantic;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataType {
    Int,
    Bool,
    Array {
        size: u32,
        base_type: Box<Self>,
        creator: Identifier,
    },
}

impl DataType {
    fn is_primitive(&self) -> bool {
        matches!(self, Self::Int | Self::Bool)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableEntry {
    pub is_ref: bool,
    pub data_type: Option<DataType>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcedureEntry {
    pub local_table: SymbolTable,
    pub parameters: Vec<VariableEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Entry {
    Type(Option<DataType>),
    Variable(VariableEntry),
    Procedure(ProcedureEntry),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RangedEntry {
    pub range: Range<usize>,
    pub entry: Entry,
}

trait Table {
    fn lookup(&self, key: &Identifier) -> Option<&RangedEntry>;
}

#[derive(Clone, PartialEq, Eq)]
pub struct SymbolTable {
    entries: HashMap<Identifier, RangedEntry>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    fn enter(&mut self, key: Identifier, value: RangedEntry, mut on_error: impl FnMut()) {
        // TODO: More effective lookup
        if !self.entries.keys().any(|ident| ident.value == key.value) {
            self.entries.insert(key, value);
        } else {
            on_error();
        }
    }
}

impl Table for SymbolTable {
    fn lookup(&self, key: &Identifier) -> Option<&RangedEntry> {
        self.entries
            .iter()
            .find(|(k, _)| k.value == key.value)
            .map(|(_, v)| v)
    }
}

#[derive(Debug)]
struct LookupTable<'a> {
    local_table: &'a SymbolTable,
    global_table: &'a SymbolTable,
}

impl<'a> Table for LookupTable<'a> {
    fn lookup(&self, key: &Identifier) -> Option<&RangedEntry> {
        let mut value = self.local_table.lookup(key);
        if value.is_none() {
            value = self.global_table.lookup(key);
        }
        value
    }
}
