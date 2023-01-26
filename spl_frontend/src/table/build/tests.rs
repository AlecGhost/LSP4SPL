use crate::{
    ast::{Identifier, TypeExpression},
    table::{BuildError, BuildErrorMessage, Entry, ProcedureEntry, SymbolTable, VariableEntry},
    test::LocalBroker,
};
#[cfg(test)]
use pretty_assertions::assert_eq;
use std::collections::HashMap;

fn test(src: &str) -> (SymbolTable, LocalBroker<BuildError>) {
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
        SymbolTable::initialize(vec![(
            Identifier::new("a", 5..6),
            Entry::Type(Some(TypeExpression::IntType))
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![BuildError(0..0, BuildErrorMessage::MainIsMissing)]
    );

    let (table, broker) = test("type a = array [5] of int");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            Identifier::new("a", 5..6),
            Entry::Type(Some(TypeExpression::ArrayType {
                size: Some(5),
                base_type: Some(Box::new(TypeExpression::IntType)),
            }))
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![BuildError(0..0, BuildErrorMessage::MainIsMissing)]
    );

    let (table, broker) = test("type a = bool;");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            Identifier::new("a", 5..6),
            Entry::Type(Some(TypeExpression::NamedType(Identifier::new(
                "bool",
                9..13
            )))),
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![
            BuildError(9..13, BuildErrorMessage::UndefinedType("bool".to_string())),
            BuildError(0..0, BuildErrorMessage::MainIsMissing)
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
            Entry::Procedure(ProcedureEntry {
                local_table: SymbolTable::new(),
                parameters: Vec::new(),
            })
        )])
    );
    assert_eq!(broker.errors(), Vec::new());

    let (table, broker) = test("");
    assert_eq!(table, SymbolTable::initialized());
    assert_eq!(
        broker.errors(),
        vec![BuildError(0..0, BuildErrorMessage::MainIsMissing)]
    );

    let (table, broker) = test("type main = int;");
    assert_eq!(table, SymbolTable::initialized());
    assert_eq!(
        broker.errors(),
        vec![
            BuildError(5..9, BuildErrorMessage::MainIsNotAProcedure),
            BuildError(0..0, BuildErrorMessage::MainIsMissing)
        ]
    );

    let (table, broker) = test("type main = int; proc main() {}");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            Identifier::new("main", 22..26),
            Entry::Procedure(ProcedureEntry {
                local_table: SymbolTable::new(),
                parameters: Vec::new(),
            })
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![BuildError(5..9, BuildErrorMessage::MainIsNotAProcedure),]
    );

    let (table, broker) = test("proc main(a: int) {}");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            Identifier::new("main", 5..9),
            Entry::Procedure(ProcedureEntry {
                local_table: SymbolTable {
                    entries: HashMap::from([(
                        Identifier::new("a", 10..11),
                        Entry::Variable(VariableEntry {
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
    );
    assert_eq!(
        broker.errors(),
        vec![BuildError(
            0..0,
            BuildErrorMessage::MainMustNotHaveParameters
        )]
    );
}

#[test]
fn redeclaration() {
    let (_, broker) = test("type a = int; proc a() {}");
    assert_eq!(
        broker.errors(),
        vec![
            BuildError(
                19..20,
                BuildErrorMessage::RedeclarationAsProcedure("a".to_string())
            ),
            BuildError(0..0, BuildErrorMessage::MainIsMissing)
        ]
    );

    let (_, broker) = test("proc a() {}\ntype a = int; ");
    assert_eq!(
        broker.errors(),
        vec![
            BuildError(
                17..18,
                BuildErrorMessage::RedeclarationAsType("a".to_string())
            ),
            BuildError(0..0, BuildErrorMessage::MainIsMissing)
        ]
    );

    let (_, broker) = test("proc a(i: int, i: int) {}");
    assert_eq!(
        broker.errors(),
        vec![
            BuildError(
                15..16,
                BuildErrorMessage::RedeclarationAsParameter("i".to_string())
            ),
            BuildError(0..0, BuildErrorMessage::MainIsMissing)
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
        vec![BuildError(
            54..55,
            BuildErrorMessage::RedeclarationAsVariable("i".to_string())
        )]
    );
}
