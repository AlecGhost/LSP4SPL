use super::{DataType, SymbolTable};
use crate::{
    ast::Identifier,
    table::{Entry, ProcedureEntry, RangedEntry, VariableEntry},
};
use std::collections::HashMap;

impl SymbolTable {
    pub fn initialized() -> Self {
        fn procedure_entry(name: Identifier, parameters: Vec<VariableEntry>) -> RangedEntry {
            RangedEntry {
                range: 0..0,
                entry: Entry::Procedure(ProcedureEntry {
                    name,
                    local_table: SymbolTable::new(),
                    parameters,
                }),
            }
        }

        Self {
            entries: HashMap::from([
                // printi(i: int)
                (
                    Identifier::new("printi", 0..0),
                    procedure_entry(
                        Identifier::new("printi", 0..0),
                        vec![VariableEntry {
                            name: Some(Identifier::new("i", 0..0)),
                            is_ref: false,
                            data_type: Some(DataType::Int),
                        }],
                    ),
                ),
                // printc(i: int)
                (
                    Identifier::new("printc", 0..0),
                    procedure_entry(
                        Identifier::new("printc", 0..0),
                        vec![VariableEntry {
                            name: Some(Identifier::new("i", 0..0)),
                            is_ref: false,
                            data_type: Some(DataType::Int),
                        }],
                    ),
                ),
                // readi(ref i: int)
                (
                    Identifier::new("readi", 0..0),
                    procedure_entry(
                        Identifier::new("readi", 0..0),
                        vec![VariableEntry {
                            name: Some(Identifier::new("i", 0..0)),
                            is_ref: true,
                            data_type: Some(DataType::Int),
                        }],
                    ),
                ),
                // readc(ref i: int)
                (
                    Identifier::new("readc", 0..0),
                    procedure_entry(
                        Identifier::new("readc", 0..0),
                        vec![VariableEntry {
                            name: Some(Identifier::new("i", 0..0)),
                            is_ref: true,
                            data_type: Some(DataType::Int),
                        }],
                    ),
                ),
                // exit()
                (
                    Identifier::new("exit", 0..0),
                    procedure_entry(Identifier::new("exit", 0..0), vec![]),
                ),
                // time(ref i: int)
                (
                    Identifier::new("time", 0..0),
                    procedure_entry(
                        Identifier::new("time", 0..0),
                        vec![VariableEntry {
                            name: Some(Identifier::new("i", 0..0)),
                            is_ref: true,
                            data_type: Some(DataType::Int),
                        }],
                    ),
                ),
                // clearAll(color: int)
                (
                    Identifier::new("clearAll", 0..0),
                    procedure_entry(
                        Identifier::new("clearAll", 0..0),
                        vec![VariableEntry {
                            name: Some(Identifier::new("color", 0..0)),
                            is_ref: false,
                            data_type: Some(DataType::Int),
                        }],
                    ),
                ),
                // setPixel(x: int, y: int, color: int)
                (
                    Identifier::new("setPixel", 0..0),
                    procedure_entry(
                        Identifier::new("setPixel", 0..0),
                        vec![
                            VariableEntry {
                                name: Some(Identifier::new("x", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                            VariableEntry {
                                name: Some(Identifier::new("y", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                            VariableEntry {
                                name: Some(Identifier::new("z", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                        ],
                    ),
                ),
                // drawLine(x1: int, y1: int, x2: int, y2: int, color: int)
                (
                    Identifier::new("drawLine", 0..0),
                    procedure_entry(
                        Identifier::new("drawLine", 0..0),
                        vec![
                            VariableEntry {
                                name: Some(Identifier::new("x1", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                            VariableEntry {
                                name: Some(Identifier::new("y1", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                            VariableEntry {
                                name: Some(Identifier::new("x2", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                            VariableEntry {
                                name: Some(Identifier::new("y2", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                            VariableEntry {
                                name: Some(Identifier::new("color", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                        ],
                    ),
                ),
                // drawCircle(x0: int, y0: int, radius: int, color: int)
                (
                    Identifier::new("drawCircle", 0..0),
                    procedure_entry(
                        Identifier::new("drawCircle", 0..0),
                        vec![
                            VariableEntry {
                                name: Some(Identifier::new("x0", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                            VariableEntry {
                                name: Some(Identifier::new("y0", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                            VariableEntry {
                                name: Some(Identifier::new("radius", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                            VariableEntry {
                                name: Some(Identifier::new("color", 0..0)),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                            },
                        ],
                    ),
                ),
            ]),
        }
    }

    pub fn initialize(entries: Vec<(Identifier, RangedEntry)>) -> Self {
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
