use super::{DataType, SymbolTable, TypeEntry};
use crate::{
    ast::Identifier,
    table::{Entry, ProcedureEntry, RangedEntry, VariableEntry},
};
use std::collections::HashMap;

const PRINTI: &str = "printi";
const PRINTC: &str = "printc";
const READI: &str = "readi";
const READC: &str = "readc";
const EXIT: &str = "exit";
const TIME: &str = "time";
const CLEARALL: &str = "clearAll";
const SETPIXEL: &str = "setPixel";
const DRAWLINE: &str = "drawLine";
const DRAWCIRCLE: &str = "drawCircle";
const INT: &str = "int";

const DEFAULT_ENTRIES: [&str; 11] = [
    PRINTI, PRINTC, READI, READC, EXIT, TIME, CLEARALL, SETPIXEL, DRAWLINE, DRAWCIRCLE, INT,
];

impl SymbolTable {
    pub fn initialized() -> Self {
        fn procedure_entry(
            name: Identifier,
            documentation: &str,
            parameters: Vec<VariableEntry>,
        ) -> RangedEntry {
            RangedEntry {
                range: 0..0,
                entry: Entry::Procedure(ProcedureEntry {
                    name,
                    local_table: SymbolTable::default(),
                    parameters,
                    documentation: Some(documentation.to_string()),
                }),
            }
        }

        Self {
            entries: HashMap::from([
                // basic type int
                (
                    INT.to_string(),
                    RangedEntry {
                        range: 0..0,
                        entry: Entry::Type(TypeEntry {
                            name: Identifier::new(INT, &[]),
                            data_type: Some(DataType::Int),
                            documentation: None,
                        }),
                    },
                ),
                // printi(i: int)
                (
                    PRINTI.to_string(),
                    procedure_entry(
                        Identifier::new(PRINTI, &[]),
                        "Gibt den Wert von i auf dem Textbildschirm aus.",
                        vec![VariableEntry {
                            name: Identifier::new("i", &[]),
                            is_ref: false,
                            is_param: true,
                            data_type: Some(DataType::Int),
                            documentation: None,
                        }],
                    ),
                ),
                // printc(i: int)
                (
                   PRINTC.to_string(),
                    procedure_entry(
                        Identifier::new(PRINTC, &[]),
                        "Gibt das Zeichen mit dem ASCII-Code i auf dem Textbildschirm aus.",
                        vec![VariableEntry {
                            name: Identifier::new("i", &[]),
                            is_ref: false,
                            is_param: true,
                            data_type: Some(DataType::Int),
                            documentation: None,
                        }],
                ),
                ),
                // readi(ref i: int)
                (
                    READI.to_string(),
                    procedure_entry(
                        Identifier::new(READI, &[]),
                        "Liest eine ganze Zahl von der Tastatur ein und speichert sie in i.
Die Eingabe erfolgt zeilenweise gepuffert mit Echo.",
                        vec![VariableEntry {
                            name: Identifier::new("i", &[]),
                            is_ref: true,
                            is_param: true,
                            data_type: Some(DataType::Int),
                            documentation: None,
                        }],
                    ),
                ),
                // readc(ref i: int)
                (
                    READC.to_string(),
                    procedure_entry(
                        Identifier::new(READC, &[]),
                        "Liest ein Zeichen von der Tastatur ein und speichert seinen ASCII-Code in i.
Die Eingabe erfolgt ungepuffert und ohne Echo.",
                        vec![VariableEntry {
                            name: Identifier::new("i", &[]),
                            is_ref: true,
                            is_param: true,
                            data_type: Some(DataType::Int),
                            documentation: None,
                        }],
                    ),
                ),
                // exit()
                (
                    EXIT.to_string(),
                    procedure_entry(
                        Identifier::new(EXIT, &[]),
"Beendet das laufende Programm und kehrt nicht zum Aufrufer zurück.",
                        vec![]),
                ),
                // time(ref i: int)
                (
                    TIME.to_string(),
                    procedure_entry(
                        Identifier::new(TIME, &[]),
                        "Gibt in i die seit dem Start des Programms vergangene Zeit in Sekun- den zurück.",
                        vec![VariableEntry {
                            name: Identifier::new("i", &[]),
                            is_ref: true,
                            is_param: true,
                            data_type: Some(DataType::Int),
                            documentation: None,
                        }],
                    ),
                ),
                // clearAll(color: int)
                (
                    CLEARALL.to_string(),
                    procedure_entry(
                        Identifier::new(CLEARALL, &[]),
                        "Löscht den Graphikbildschirm mit der Farbe color.
Farben werden durch Angabe der R-, G- und B-Komponenten nach dem Muster 0x00RRGGBB gebildet.
Es stehen also für jede Komponente die Werte 0..255 zur Verfügung.",
                        vec![VariableEntry {
                            name: Identifier::new("color", &[]),
                            is_ref: false,
                            is_param: true,
                            data_type: Some(DataType::Int),
                            documentation: None,
                        }],
                    ),
                ),
                // setPixel(x: int, y: int, color: int)
                (
                  SETPIXEL.to_string(),
                    procedure_entry(
                        Identifier::new(SETPIXEL, &[]),
                        "Setzt den Pixel mit den Koordinaten x und y auf die Farbe color.
Grenzen: 0<= x <640, 0 <= y < 480.",
                        vec![
                            VariableEntry {
                                name: Identifier::new("x", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                            VariableEntry {
                                name: Identifier::new("y", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                            VariableEntry {
                                name: Identifier::new("z", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                        ],
                    ),
                ),
                // drawLine(x1: int, y1: int, x2: int, y2: int, color: int)
                (
                   DRAWLINE.to_string(),
                    procedure_entry(
                        Identifier::new(DRAWLINE, &[]),
                        "Zeichnet eine gerade Linie von (x1|y1) nach (x2|y2) mit der Farbe color.
Grenzen wie bei setPixel.",
                        vec![
                            VariableEntry {
                                name: Identifier::new("x1", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                            VariableEntry {
                                name: Identifier::new("y1", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                            VariableEntry {
                                name: Identifier::new("x2", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                            VariableEntry {
                                name: Identifier::new("y2", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                            VariableEntry {
                                name: Identifier::new("color", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                        ],
                    ),
                ),
                // drawCircle(x0: int, y0: int, radius: int, color: int)
                (
                  DRAWCIRCLE.to_string(),
                    procedure_entry(
                        Identifier::new(DRAWCIRCLE, &[]),
                        "Zeichnet einen Kreis um den Mittelpunkt (x0|y0) mit dem Radius radius und der Farbe color.",
                        vec![
                            VariableEntry {
                                name: Identifier::new("x0", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                            VariableEntry {
                                name: Identifier::new("y0", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                            VariableEntry {
                                name: Identifier::new("radius", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                            VariableEntry {
                                name: Identifier::new("color", &[]),
                                is_ref: false,
                                is_param: true,
                                data_type: Some(DataType::Int),
                                documentation: None,
                            },
                        ],
                    ),
                ),
            ]),
        }
    }

    pub fn initialize(entries: Vec<(String, RangedEntry)>) -> Self {
        let mut table = Self::initialized();
        for (k, v) in entries {
            table.entries.insert(k, v);
        }
        table
    }
}

impl std::fmt::Debug for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(
                self.entries
                    .iter()
                    .filter(|(k, _)| !DEFAULT_ENTRIES.contains(&k.as_str())),
            )
            .finish()
    }
}
