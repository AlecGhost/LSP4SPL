use crate::{
    ast::{Identifier, TypeExpression},
    table::{Entry, ErrorMessage, ProcedureEntry, SymbolTable, TableError, VariableEntry},
    test::LocalBroker,
};
use std::collections::HashMap;

fn test(src: &str) -> (SymbolTable, LocalBroker<TableError>) {
    eprintln!("Testing: {}", src);
    let program = crate::parser::parse(src, LocalBroker::new());
    let broker = LocalBroker::new();
    let table = crate::table::build(&program, broker.clone());
    (table, broker)
}

#[test]
fn type_decs() {
    let (table, broker) = test("type a = int;");
    assert_eq!(
        table,
        SymbolTable {
            entries: HashMap::from([(
                Identifier::new("a", 5..6),
                Entry::Type(Some(TypeExpression::IntType))
            )])
        }
    );
    assert_eq!(
        broker.errors(),
        vec![TableError(0..0, ErrorMessage::MainIsMissing)]
    );

    let (table, broker) = test("type a = array [5] of int");
    assert_eq!(
        table,
        SymbolTable {
            entries: HashMap::from([(
                Identifier::new("a", 5..6),
                Entry::Type(Some(TypeExpression::ArrayType {
                    size: Some(5),
                    base_type: Some(Box::new(TypeExpression::IntType)),
                }))
            )])
        }
    );
    assert_eq!(
        broker.errors(),
        vec![TableError(0..0, ErrorMessage::MainIsMissing)]
    );

    let (table, broker) = test("type a = bool;");
    assert_eq!(
        table,
        SymbolTable {
            entries: HashMap::from([(
                Identifier::new("a", 5..6),
                Entry::Type(Some(TypeExpression::NamedType(Identifier::new(
                    "bool",
                    9..13
                )))),
            )])
        }
    );
    assert_eq!(
        broker.errors(),
        vec![
            TableError(9..13, ErrorMessage::UndefinedType("bool".to_string())),
            TableError(0..0, ErrorMessage::MainIsMissing)
        ]
    );
}

#[test]
fn test_main() {
    let (table, broker) = test("proc main() {}");
    assert_eq!(
        table,
        SymbolTable {
            entries: HashMap::from([(
                Identifier::new("main", 5..9),
                Entry::Procedure(ProcedureEntry {
                    local_table: SymbolTable::new(),
                    parameters: Vec::new(),
                })
            )])
        }
    );
    assert_eq!(broker.errors(), Vec::new());

    let (table, broker) = test("");
    assert_eq!(
        table,
        SymbolTable {
            entries: HashMap::new()
        }
    );
    assert_eq!(
        broker.errors(),
        vec![TableError(0..0, ErrorMessage::MainIsMissing)]
    );

    let (table, broker) = test("type main = int;");
    assert_eq!(
        table,
        SymbolTable {
            entries: HashMap::new()
        }
    );
    assert_eq!(
        broker.errors(),
        vec![
            TableError(5..9, ErrorMessage::MainIsNotAProcedure),
            TableError(0..0, ErrorMessage::MainIsMissing)
        ]
    );

    let (table, broker) = test("type main = int; proc main() {}");
    assert_eq!(
        table,
        SymbolTable {
            entries: HashMap::from([(
                Identifier::new("main", 22..26),
                Entry::Procedure(ProcedureEntry {
                    local_table: SymbolTable::new(),
                    parameters: Vec::new(),
                })
            )])
        }
    );
    assert_eq!(
        broker.errors(),
        vec![TableError(5..9, ErrorMessage::MainIsNotAProcedure),]
    );

    let (table, broker) = test("proc main(a: int) {}");
    assert_eq!(
        table,
        SymbolTable {
            entries: HashMap::from([(
                Identifier::new("main", 5..9),
                Entry::Procedure(ProcedureEntry {
                    local_table: SymbolTable {
                        entries: HashMap::from([(
                            Identifier::new("a", 10..11),
                            Entry::Parameter(VariableEntry {
                                is_ref: false,
                                type_expr: Some(TypeExpression::IntType)
                            })
                        )])
                    },
                    parameters: vec![VariableEntry {
                        is_ref: false,
                        type_expr: Some(TypeExpression::IntType)
                    }],
                })
            )])
        }
    );
    assert_eq!(
        broker.errors(),
        vec![TableError(0..0, ErrorMessage::MainMustNotHaveParameters)]
    );
}

#[test]
fn redeclaration() {
    let (_, broker) = test("type a = int; proc a() {}");
    assert_eq!(
        broker.errors(),
        vec![
            TableError(
                19..20,
                ErrorMessage::RedeclarationAsProcedure("a".to_string())
            ),
            TableError(0..0, ErrorMessage::MainIsMissing)
        ]
    );

    let (_, broker) = test("proc a() {}\ntype a = int; ");
    assert_eq!(
        broker.errors(),
        vec![
            TableError(17..18, ErrorMessage::RedeclarationAsType("a".to_string())),
            TableError(0..0, ErrorMessage::MainIsMissing)
        ]
    );

    let (_, broker) = test("proc a(i: int, i: int) {}");
    assert_eq!(
        broker.errors(),
        vec![
            TableError(
                15..16,
                ErrorMessage::RedeclarationAsParameter("i".to_string())
            ),
            TableError(0..0, ErrorMessage::MainIsMissing)
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
        vec![TableError(
            54..55,
            ErrorMessage::RedeclarationAsVariable("i".to_string())
        )]
    );
}
