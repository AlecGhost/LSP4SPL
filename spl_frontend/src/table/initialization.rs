use super::SymbolTable;
use crate::{
    ast::{Identifier, TypeExpression},
    table::{Entry, ProcedureEntry, VariableEntry},
};
use std::collections::HashMap;

impl SymbolTable {
    pub fn initialized() -> Self {
        fn procedure_entry(parameters: Vec<VariableEntry>) -> Entry {
            Entry::Procedure(ProcedureEntry {
                local_table: SymbolTable::new(),
                parameters,
            })
        }

        Self {
            entries: HashMap::from([
                // printi(i: int)
                (
                    Identifier::new("printi", 0..0),
                    procedure_entry(vec![VariableEntry {
                        is_ref: false,
                        type_expr: Some(TypeExpression::IntType),
                    }]),
                ),
                // printc(i: int)
                (
                    Identifier::new("printc", 0..0),
                    procedure_entry(vec![VariableEntry {
                        is_ref: false,
                        type_expr: Some(TypeExpression::IntType),
                    }]),
                ),
                // readi(ref i: int)
                (
                    Identifier::new("readi", 0..0),
                    procedure_entry(vec![VariableEntry {
                        is_ref: true,
                        type_expr: Some(TypeExpression::IntType),
                    }]),
                ),
                // readc(ref i: int)
                (
                    Identifier::new("readc", 0..0),
                    procedure_entry(vec![VariableEntry {
                        is_ref: true,
                        type_expr: Some(TypeExpression::IntType),
                    }]),
                ),
                // exit()
                (Identifier::new("exit", 0..0), procedure_entry(vec![])),
                // time(ref i: int)
                (
                    Identifier::new("time", 0..0),
                    procedure_entry(vec![VariableEntry {
                        is_ref: true,
                        type_expr: Some(TypeExpression::IntType),
                    }]),
                ),
                // clearAll(color: int)
                (
                    Identifier::new("time", 0..0),
                    procedure_entry(vec![VariableEntry {
                        is_ref: false,
                        type_expr: Some(TypeExpression::IntType),
                    }]),
                ),
                // setPixel(x: int, y: int, color: int)
                (
                    Identifier::new("setPixel", 0..0),
                    procedure_entry(vec![
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                    ]),
                ),
                // drawLine(x1: int, y1: int, x2: int, y2: int, color: int)
                (
                    Identifier::new("drawLine", 0..0),
                    procedure_entry(vec![
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                    ]),
                ),
                // drawCircle(x0: int, y0: int, radius: int, color: int)
                (
                    Identifier::new("drawCircle", 0..0),
                    procedure_entry(vec![
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                        VariableEntry {
                            is_ref: false,
                            type_expr: Some(TypeExpression::IntType),
                        },
                    ]),
                ),
            ]),
        }
    }

    pub fn initialize(entries: Vec<(Identifier, Entry)>) -> Self {
        let mut table = Self::initialized();
        for (k, v) in entries {
            table.entries.insert(k, v);
        }
        table
    }
}

impl std::fmt::Debug for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const DEFAULT_ENTRIES: [&str; 10] = [
            "printi",
            "printc",
            "readi",
            "readc",
            "exit",
            "time",
            "clearAll",
            "setPixel",
            "drawLine",
            "drawCircle",
        ];
        f.debug_map()
            .entries(
                self.entries
                    .iter()
                    .filter(|(k, _)| !DEFAULT_ENTRIES.contains(&k.value.as_str())),
            )
            .finish()
    }
}
