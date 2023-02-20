use crate::{ast::Identifier, error::BuildErrorMessage, DiagnosticsBroker, ToRange};
pub use build::build;
pub use semantic::analyze;
use std::{collections::HashMap, fmt::Display, ops::Range};

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
pub struct TypeEntry {
    pub name: Identifier,
    pub data_type: Option<DataType>,
    pub documentation: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableEntry {
    pub name: Identifier,
    pub is_ref: bool,
    pub is_param: bool,
    pub data_type: Option<DataType>,
    pub documentation: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcedureEntry {
    pub name: Identifier,
    pub local_table: SymbolTable,
    pub parameters: Vec<VariableEntry>,
    pub documentation: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Entry {
    Type(TypeEntry),
    Variable(VariableEntry),
    Procedure(ProcedureEntry),
}

impl ToRange for Entry {
    fn to_range(&self) -> Range<usize> {
        use Entry::*;
        match self {
            Type(t) => t.name.to_range(),
            Procedure(p) => p.name.to_range(),
            _ => 0..0,
        }
    }
}

impl Entry {
    pub fn documentation(&self) -> Option<String> {
        match self {
            Entry::Procedure(p) => p.documentation.clone(),
            Entry::Type(t) => t.documentation.clone(),
            Entry::Variable(v) => v.documentation.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RangedEntry {
    pub range: Range<usize>,
    pub entry: Entry,
}

pub trait Table {
    fn lookup(&self, key: &str) -> Option<&RangedEntry>;
    fn entry(&self, key: &str) -> Option<(&String, &RangedEntry)>;
}

#[derive(Clone, Default, PartialEq, Eq)]
pub struct SymbolTable {
    pub entries: HashMap<String, RangedEntry>,
}

impl SymbolTable {
    fn enter(&mut self, key: String, value: RangedEntry, mut on_error: impl FnMut()) {
        if let std::collections::hash_map::Entry::Vacant(v) = self.entries.entry(key) {
            v.insert(value);
        } else {
            on_error();
        }
    }
}

impl Table for SymbolTable {
    fn lookup(&self, key: &str) -> Option<&RangedEntry> {
        self.entries
            .iter()
            .find(|(k, _)| k.as_str() == key)
            .map(|(_, v)| v)
    }

    fn entry(&self, key: &str) -> Option<(&String, &RangedEntry)> {
        self.entries.iter().find(|(k, _)| k.as_str() == key)
    }
}

#[derive(Debug)]
pub struct LookupTable<'a> {
    pub local_table: &'a SymbolTable,
    pub global_table: &'a SymbolTable,
}

impl<'a> Table for LookupTable<'a> {
    fn lookup(&self, key: &str) -> Option<&RangedEntry> {
        let mut value = self.local_table.lookup(key);
        if value.is_none() {
            value = self.global_table.lookup(key);
        }
        value
    }

    fn entry(&self, key: &str) -> Option<(&String, &RangedEntry)> {
        let mut result = self.local_table.entry(key);
        if result.is_none() {
            result = self.global_table.entry(key);
        }
        result
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::Int => "int".to_string(),
            Self::Bool => "boolean".to_string(),
            Self::Array {
                size,
                base_type,
                creator: _,
            } => format!("array [{}] of {}", size, base_type),
        };
        write!(f, "{}", display)
    }
}

impl Display for VariableEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}: {}",
            if self.is_ref { "ref " } else { "" },
            self.name,
            self.data_type
                .as_ref()
                .map(|dt| dt.to_string())
                .unwrap_or_else(|| "_".to_string())
        )
    }
}

impl Display for ProcedureEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "proc {}({})",
            self.name,
            self.parameters
                .iter()
                .map(|param| param.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl Display for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Entry::Procedure(p) => p.to_string(),
            Entry::Type(t) => t
                .data_type
                .as_ref()
                .map(|dt| dt.to_string())
                .unwrap_or_else(|| "_".to_string()),
            Entry::Variable(v) => v.to_string(),
        };
        write!(f, "{}", display)
    }
}
