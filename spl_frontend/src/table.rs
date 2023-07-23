use crate::{ast::Identifier, error::KeyAlreadyExistsError, ToRange, ToTextRange};
pub(crate) use build::build;
pub(crate) use semantic::analyze;
use std::{
    collections::{hash_map, HashMap},
    fmt::Display,
    ops::Range,
};

mod build;
mod initialization;
mod semantic;

pub trait SymbolTable {
    type Value;
    fn lookup(&self, key: &str) -> Option<&Self::Value>;
    fn enter(
        &mut self,
        key: String,
        value: Self::Value,
    ) -> Result<(), KeyAlreadyExistsError<String>>;
}

#[derive(Clone, PartialEq, Eq)]
pub struct GlobalTable {
    pub entries: HashMap<String, GlobalEntry>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct LocalTable {
    pub entries: HashMap<String, LocalEntry>,
}

#[derive(Debug)]
pub struct LookupTable<'a> {
    pub local_table: Option<&'a LocalTable>,
    pub global_table: Option<&'a GlobalTable>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GlobalEntry {
    Type(TypeEntry),
    Procedure(ProcedureEntry),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LocalEntry {
    Variable(VariableEntry),
    Parameter(VariableEntry),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Entry<'a> {
    Type(&'a TypeEntry),
    Procedure(&'a ProcedureEntry),
    Variable(&'a VariableEntry),
    Parameter(&'a VariableEntry),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeEntry {
    pub name: Identifier,
    pub data_type: Option<DataType>,
    pub range: Range<usize>,
    pub doc: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcedureEntry {
    pub name: Identifier,
    pub local_table: LocalTable,
    pub parameters: Vec<VariableEntry>,
    pub range: Range<usize>,
    pub doc: Option<String>,
}

impl Entry<'_> {
    pub fn is_default(&self) -> bool {
        match self {
            Self::Type(TypeEntry { name, .. }) | Self::Procedure(ProcedureEntry { name, .. }) => {
                initialization::DEFAULT_ENTRIES.contains(&name.value.as_str())
            }
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableEntry {
    pub name: Identifier,
    pub is_ref: bool,
    pub data_type: Option<DataType>,
    pub range: Range<usize>,
    pub doc: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataType {
    Int,
    Bool,
    Array {
        size: u32,
        base_type: Box<Self>,
        creator: String,
    },
}

impl DataType {
    const fn is_primitive(&self) -> bool {
        matches!(self, Self::Int | Self::Bool)
    }
}

impl SymbolTable for GlobalTable {
    type Value = GlobalEntry;

    fn lookup(&self, key: &str) -> Option<&GlobalEntry> {
        self.entries.get(key)
    }

    fn enter(
        &mut self,
        key: String,
        value: GlobalEntry,
    ) -> Result<(), KeyAlreadyExistsError<String>> {
        match self.entries.entry(key) {
            hash_map::Entry::Vacant(v) => {
                v.insert(value);
                Ok(())
            }
            hash_map::Entry::Occupied(o) => Err(KeyAlreadyExistsError::new(o.key().clone())),
        }
    }
}

impl SymbolTable for LocalTable {
    type Value = LocalEntry;

    fn lookup(&self, key: &str) -> Option<&LocalEntry> {
        self.entries.get(key)
    }

    fn enter(
        &mut self,
        key: String,
        value: LocalEntry,
    ) -> Result<(), KeyAlreadyExistsError<String>> {
        match self.entries.entry(key) {
            hash_map::Entry::Vacant(v) => {
                v.insert(value);
                Ok(())
            }
            hash_map::Entry::Occupied(o) => Err(KeyAlreadyExistsError::new(o.key().clone())),
        }
    }
}

pub trait TableEntry {
    fn doc(&self) -> Option<String>;
}

impl TableEntry for Entry<'_> {
    fn doc(&self) -> Option<String> {
        match self {
            Entry::Procedure(p) => p.doc.clone(),
            Entry::Type(t) => t.doc.clone(),
            Entry::Variable(v) | Entry::Parameter(v) => v.doc.clone(),
        }
    }
}

impl TableEntry for GlobalEntry {
    fn doc(&self) -> Option<String> {
        match self {
            Self::Procedure(p) => p.doc.clone(),
            Self::Type(t) => t.doc.clone(),
        }
    }
}

impl TableEntry for LocalEntry {
    fn doc(&self) -> Option<String> {
        match self {
            Self::Variable(v) | Self::Parameter(v) => v.doc.clone(),
        }
    }
}

impl<'a> LookupTable<'a> {
    pub fn lookup(&self, key: &str) -> Option<Entry<'a>> {
        self.local_table
            .and_then(|table| table.lookup(key))
            .map(Entry::from)
            .map_or_else(
                || {
                    self.global_table
                        .and_then(|table| table.lookup(key))
                        .map(Entry::from)
                },
                Some,
            )
    }
}

impl<'a> From<&'a GlobalEntry> for Entry<'a> {
    fn from(value: &'a GlobalEntry) -> Self {
        match value {
            GlobalEntry::Type(t) => Self::Type(t),
            GlobalEntry::Procedure(p) => Self::Procedure(p),
        }
    }
}

impl<'a> From<&'a LocalEntry> for Entry<'a> {
    fn from(value: &'a LocalEntry) -> Self {
        match value {
            LocalEntry::Variable(v) => Self::Variable(v),
            LocalEntry::Parameter(p) => Self::Parameter(p),
        }
    }
}

impl ToRange for Entry<'_> {
    fn to_range(&self) -> Range<usize> {
        use Entry::*;
        match self {
            Type(t) => t.to_range(),
            Procedure(p) => p.to_range(),
            Variable(v) | Parameter(v) => v.to_range(),
        }
    }
}

// TODO: Remove
impl ToTextRange for Entry<'_> {
    fn to_text_range(&self, tokens: &[crate::token::Token]) -> Range<usize> {
        use Entry::*;
        match self {
            Type(t) => t.name.to_text_range(tokens),
            Procedure(p) => p.name.to_text_range(tokens),
            Variable(v) | Parameter(v) => v.name.to_text_range(tokens),
        }
    }
}

impl ToRange for VariableEntry {
    fn to_range(&self) -> Range<usize> {
        self.range.clone()
    }
}

impl ToRange for ProcedureEntry {
    fn to_range(&self) -> Range<usize> {
        self.range.clone()
    }
}

impl ToRange for TypeEntry {
    fn to_range(&self) -> Range<usize> {
        self.range.clone()
    }
}

// TODO: Remove
impl ToTextRange for GlobalEntry {
    fn to_text_range(&self, tokens: &[crate::token::Token]) -> Range<usize> {
        use GlobalEntry::*;
        match self {
            Procedure(p) => p.name.to_text_range(tokens),
            Type(t) => t.name.to_text_range(tokens),
        }
    }
}

impl ToRange for GlobalEntry {
    fn to_range(&self) -> Range<usize> {
        use GlobalEntry::*;
        match self {
            Procedure(p) => p.to_range(),
            Type(t) => t.to_range(),
        }
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
                .map_or_else(|| "_".to_string(), |dt| dt.to_string())
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

impl Display for TypeEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.data_type
                .as_ref()
                .map_or_else(|| "_".to_string(), |dt| dt.to_string())
        )
    }
}

impl Display for Entry<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::Procedure(p) => p.to_string(),
            Self::Type(t) => t.to_string(),
            Self::Variable(v) | Self::Parameter(v) => v.to_string(),
        };
        write!(f, "{}", display)
    }
}

impl Display for GlobalEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::Type(t) => t.to_string(),
            Self::Procedure(p) => p.to_string(),
        };
        write!(f, "{}", display)
    }
}

impl Display for LocalEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::Variable(v) => v.to_string(),
            Self::Parameter(p) => p.to_string(),
        };
        write!(f, "{}", display)
    }
}
