use crate::{
    ast::Identifier,
    error::SplError,
    table::{
        BuildErrorMessage, DataType, Entry, ProcedureEntry, RangedEntry, SymbolTable, VariableEntry,
    },
    LocalBroker,
};
#[cfg(test)]
use pretty_assertions::assert_eq;
use std::collections::HashMap;

fn test(src: &str) -> (SymbolTable, LocalBroker) {
    eprintln!("Testing: {}", src);
    let program = crate::parser::parse(src, LocalBroker::default());
    let broker = LocalBroker::default();
    let table = crate::table::build(&program, broker.clone());
    (table, broker)
}

#[test]
fn type_decs() {
    let (table, broker) = test("type a = int;");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            Identifier::new("a", 5..6),
            RangedEntry {
                range: 0..13,
                entry: Entry::Type(Some(DataType::Int)),
            },
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );

    let (table, broker) = test("type a = array [5] of int");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            Identifier::new("a", 5..6),
            RangedEntry {
                range: 0..25,
                entry: Entry::Type(Some(DataType::Array {
                    size: 5,
                    base_type: Box::new(DataType::Int),
                    creator: Identifier::new("a", 5..6),
                })),
            }
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );

    let (table, broker) = test("type a = bool;");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            Identifier::new("a", 5..6),
            RangedEntry {
                range: 0..14,
                entry: Entry::Type(None),
            },
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![
            SplError(
                9..13,
                BuildErrorMessage::UndefinedType("bool".to_string()).to_string()
            ),
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())
        ]
    );
}

#[test]
fn test_main() {
    let (table, broker) = test("proc main() {}");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            Identifier::new("main", 5..9),
            RangedEntry {
                range: 0..14,
                entry: Entry::Procedure(ProcedureEntry {
                    name: Identifier::new("main", 5..9),
                    local_table: SymbolTable::new(),
                    parameters: Vec::new(),
                }),
            }
        )])
    );
    assert_eq!(broker.errors(), Vec::new());

    let (table, broker) = test("");
    assert_eq!(table, SymbolTable::initialized());
    assert_eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );

    let (table, broker) = test("type main = int;");
    assert_eq!(table, SymbolTable::initialized());
    assert_eq!(
        broker.errors(),
        vec![
            SplError(5..9, BuildErrorMessage::MainIsNotAProcedure.to_string()),
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())
        ]
    );

    let (table, broker) = test("type main = int; proc main() {}");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            Identifier::new("main", 22..26),
            RangedEntry {
                range: 17..31,
                entry: Entry::Procedure(ProcedureEntry {
                    name: Identifier::new("main", 22..26),
                    local_table: SymbolTable::new(),
                    parameters: Vec::new(),
                }),
            }
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(
            5..9,
            BuildErrorMessage::MainIsNotAProcedure.to_string()
        ),]
    );

    let (table, broker) = test("proc main(a: int) {}");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            Identifier::new("main", 5..9),
            RangedEntry {
                range: 0..20,
                entry: Entry::Procedure(ProcedureEntry {
                    name: Identifier::new("main", 5..9),
                    local_table: SymbolTable {
                        entries: HashMap::from([(
                            Identifier::new("a", 10..11),
                            RangedEntry {
                                range: 10..16,
                                entry: Entry::Variable(VariableEntry {
                                    name: Some(Identifier::new("a", 10..11)),
                                    is_ref: false,
                                    data_type: Some(DataType::Int)
                                }),
                            }
                        )])
                    },
                    parameters: vec![VariableEntry {
                        name: Some(Identifier::new("a", 10..11)),
                        is_ref: false,
                        data_type: Some(DataType::Int)
                    }],
                }),
            }
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(
            0..0,
            BuildErrorMessage::MainMustNotHaveParameters.to_string()
        )]
    );
}

#[test]
fn redeclaration() {
    let (_, broker) = test("type a = int; proc a() {}");
    assert_eq!(
        broker.errors(),
        vec![
            SplError(
                19..20,
                BuildErrorMessage::RedeclarationAsProcedure("a".to_string()).to_string()
            ),
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())
        ]
    );

    let (_, broker) = test("proc a() {}\ntype a = int; ");
    assert_eq!(
        broker.errors(),
        vec![
            SplError(
                17..18,
                BuildErrorMessage::RedeclarationAsType("a".to_string()).to_string()
            ),
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())
        ]
    );

    let (_, broker) = test("proc a(i: int, i: int) {}");
    assert_eq!(
        broker.errors(),
        vec![
            SplError(
                15..16,
                BuildErrorMessage::RedeclarationAsParameter("i".to_string()).to_string()
            ),
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())
        ]
    );

    let (_, broker) = test(
        "proc main() {
            var i: int;
            var i: int;
        }",
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(
            54..55,
            BuildErrorMessage::RedeclarationAsVariable("i".to_string()).to_string()
        )]
    );
}
