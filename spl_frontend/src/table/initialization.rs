use super::{DataType, GlobalTable, LocalTable, TypeEntry};
use crate::{
    ast::Identifier,
    table::{GlobalEntry, LocalEntry, ProcedureEntry, VariableEntry},
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

pub const DEFAULT_ENTRIES: [&str; 11] = [
    PRINTI, PRINTC, READI, READC, EXIT, TIME, CLEARALL, SETPIXEL, DRAWLINE, DRAWCIRCLE, INT,
];

impl GlobalTable {
    pub fn initialized() -> Self {
        fn procedure_entry(
            name: Identifier,
            documentation: &str,
            parameters: Vec<VariableEntry>,
        ) -> GlobalEntry {
            let param_map = parameters
                .into_iter()
                .map(|param| (param.name.value.clone(), LocalEntry::Parameter(param)))
                .collect();
            GlobalEntry::Procedure(ProcedureEntry {
                name,
                local_table: LocalTable { entries: param_map },
                doc: Some(documentation.to_string()),
            })
        }

        Self {
            entries: HashMap::from([
                // basic type int
                (
                    INT.to_string(),
                    GlobalEntry::Type(TypeEntry {
                        name: Identifier::new(INT.to_string(), &[]),
                        data_type: Some(DataType::Int),
                        doc: None,
                    }),
                ),
                // printi(i: int)
                (
                    PRINTI.to_string(),
                    procedure_entry(
                        Identifier::new(PRINTI.to_string(), &[]),
                        "Gibt den Wert von i auf dem Textbildschirm aus.",
                        vec![VariableEntry {
                            name: Identifier::new("i".to_string(), &[]),
                            is_ref: false,
                            data_type: Some(DataType::Int),
                            doc: None,
                        }],
                    ),
                ),
                // printc(i: int)
                (
                   PRINTC.to_string(),
                    procedure_entry(
                        Identifier::new(PRINTC.to_string(), &[]),
                        "Gibt das Zeichen mit dem ASCII-Code i auf dem Textbildschirm aus.",
                        vec![VariableEntry {
                            name: Identifier::new("i".to_string(), &[]),
                            is_ref: false,
                            data_type: Some(DataType::Int),
                            doc: None,
                        }],
                ),
                ),
                // readi(ref i: int)
                (
                    READI.to_string(),
                    procedure_entry(
                        Identifier::new(READI.to_string(), &[]),
                        "Liest eine ganze Zahl von der Tastatur ein und speichert sie in i.
Die Eingabe erfolgt zeilenweise gepuffert mit Echo.",
                        vec![VariableEntry {
                            name: Identifier::new("i".to_string(), &[]),
                            is_ref: true,
                            data_type: Some(DataType::Int),
                            doc: None,
                        }],
                    ),
                ),
                // readc(ref i: int)
                (
                    READC.to_string(),
                    procedure_entry(
                        Identifier::new(READC.to_string(), &[]),
                        "Liest ein Zeichen von der Tastatur ein und speichert seinen ASCII-Code in i.
Die Eingabe erfolgt ungepuffert und ohne Echo.",
                        vec![VariableEntry {
                            name: Identifier::new("i".to_string(), &[]),
                            is_ref: true,
                            data_type: Some(DataType::Int),
                            doc: None,
                        }],
                    ),
                ),
                // exit()
                (
                    EXIT.to_string(),
                    procedure_entry(
                        Identifier::new(EXIT.to_string(), &[]),
"Beendet das laufende Programm und kehrt nicht zum Aufrufer zurück.",
                        vec![]),
                ),
                // time(ref i: int)
                (
                    TIME.to_string(),
                    procedure_entry(
                        Identifier::new(TIME.to_string(), &[]),
                        "Gibt in i die seit dem Start des Programms vergangene Zeit in Sekun- den zurück.",
                        vec![VariableEntry {
                            name: Identifier::new("i".to_string(), &[]),
                            is_ref: true,
                            data_type: Some(DataType::Int),
                            doc: None,
                        }],
                    ),
                ),
                // clearAll(color: int)
                (
                    CLEARALL.to_string(),
                    procedure_entry(
                        Identifier::new(CLEARALL.to_string(), &[]),
                        "Löscht den Graphikbildschirm mit der Farbe color.
Farben werden durch Angabe der R-, G- und B-Komponenten nach dem Muster 0x00RRGGBB gebildet.
Es stehen also für jede Komponente die Werte 0..255 zur Verfügung.",
                        vec![VariableEntry {
                            name: Identifier::new("color".to_string(), &[]),
                            is_ref: false,
                            data_type: Some(DataType::Int),
                            doc: None,
                        }],
                    ),
                ),
                // setPixel(x: int, y: int, color: int)
                (
                  SETPIXEL.to_string(),
                    procedure_entry(
                        Identifier::new(SETPIXEL.to_string(), &[]),
                        "Setzt den Pixel mit den Koordinaten x und y auf die Farbe color.
Grenzen: 0<= x <640, 0 <= y < 480.",
                        vec![
                            VariableEntry {
                                name: Identifier::new("x".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                            VariableEntry {
                                name: Identifier::new("y".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                            VariableEntry {
                                name: Identifier::new("z".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                        ],
                    ),
                ),
                // drawLine(x1: int, y1: int, x2: int, y2: int, color: int)
                (
                   DRAWLINE.to_string(),
                    procedure_entry(
                        Identifier::new(DRAWLINE.to_string(), &[]),
                        "Zeichnet eine gerade Linie von (x1|y1) nach (x2|y2) mit der Farbe color.
Grenzen wie bei setPixel.",
                        vec![
                            VariableEntry {
                                name: Identifier::new("x1".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                            VariableEntry {
                                name: Identifier::new("y1".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                            VariableEntry {
                                name: Identifier::new("x2".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                            VariableEntry {
                                name: Identifier::new("y2".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                            VariableEntry {
                                name: Identifier::new("color".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                        ],
                    ),
                ),
                // drawCircle(x0: int, y0: int, radius: int, color: int)
                (
                  DRAWCIRCLE.to_string(),
                    procedure_entry(
                        Identifier::new(DRAWCIRCLE.to_string(), &[]),
                        "Zeichnet einen Kreis um den Mittelpunkt (x0|y0) mit dem Radius radius und der Farbe color.",
                        vec![
                            VariableEntry {
                                name: Identifier::new("x0".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                            VariableEntry {
                                name: Identifier::new("y0".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                            VariableEntry {
                                name: Identifier::new("radius".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                            VariableEntry {
                                name: Identifier::new("color".to_string(), &[]),
                                is_ref: false,
                                data_type: Some(DataType::Int),
                                doc: None,
                            },
                        ],
                    ),
                ),
            ]),
        }
    }

    pub fn initialize(entries: Vec<(String, GlobalEntry)>) -> Self {
        let mut table = Self::initialized();
        for (k, v) in entries {
            table.entries.insert(k, v);
        }
        table
    }
}

impl std::fmt::Debug for GlobalTable {
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
