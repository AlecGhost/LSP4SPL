use crate::lexer::token::Token;
use crate::table::{GlobalEntry, LocalEntry, LocalTable};
use crate::{
    ast::Identifier,
    error::SplError,
    table::{BuildErrorMessage, DataType, GlobalTable, ProcedureEntry, TypeEntry, VariableEntry},
    LocalBroker,
};
#[cfg(test)]
use pretty_assertions::assert_eq;
use std::collections::HashMap;

fn test(src: &str) -> (GlobalTable, Vec<Token>, LocalBroker) {
    eprintln!("Testing: {}", src);
    let tokens = crate::lexer::lex(src, LocalBroker::default());
    let program = crate::parser::parse(&tokens, LocalBroker::default());
    let broker = LocalBroker::default();
    let table = crate::table::build(&program, &broker);
    (table, tokens, broker)
}

#[test]
fn type_decs() {
    let (table, tokens, broker) = test("type a = int;");
    assert_eq!(
        table,
        GlobalTable::initialize(vec![(
            "a".to_string(),
            GlobalEntry::Type(TypeEntry {
                name: Identifier::new("a".to_string(), &tokens[1..2]),
                data_type: Some(DataType::Int),
                doc: None
            })
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );

    let (table, tokens, broker) = test("type a = array [5] of int");
    assert_eq!(
        table,
        GlobalTable::initialize(vec![(
            "a".to_string(),
            GlobalEntry::Type(TypeEntry {
                name: Identifier::new("a".to_string(), &tokens[1..2]),
                data_type: Some(DataType::Array {
                    size: 5,
                    base_type: Box::new(DataType::Int),
                    creator: Identifier::new("a".to_string(), &tokens[1..2]),
                }),
                doc: None
            })
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );

    let (table, tokens, broker) = test("type a = bool;");
    assert_eq!(
        table,
        GlobalTable::initialize(vec![(
            "a".to_string(),
            GlobalEntry::Type(TypeEntry {
                name: Identifier::new("a".to_string(), &tokens[1..2]),
                data_type: None,
                doc: None
            }),
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
    let (table, tokens, broker) = test("proc main() {}");
    assert_eq!(
        table,
        GlobalTable::initialize(vec![(
            "main".to_string(),
            GlobalEntry::Procedure(ProcedureEntry {
                name: Identifier::new("main".to_string(), &tokens[1..2]),
                local_table: LocalTable::default(),
                parameters: Vec::new(),
                doc: None,
            })
        )])
    );
    assert_eq!(broker.errors(), Vec::new());

    let (table, _, broker) = test("");
    assert_eq!(table, GlobalTable::initialized());
    assert_eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );

    let (table, _, broker) = test("type main = int;");
    assert_eq!(table, GlobalTable::initialized());
    assert_eq!(
        broker.errors(),
        vec![
            SplError(5..9, BuildErrorMessage::MainIsNotAProcedure.to_string()),
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())
        ]
    );

    let (table, tokens, broker) = test("type main = int; proc main() {}");
    assert_eq!(
        table,
        GlobalTable::initialize(vec![(
            "main".to_string(),
            GlobalEntry::Procedure(ProcedureEntry {
                name: Identifier::new("main".to_string(), &tokens[6..7]),
                local_table: LocalTable::default(),
                parameters: Vec::new(),
                doc: None,
            })
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(
            5..9,
            BuildErrorMessage::MainIsNotAProcedure.to_string()
        ),]
    );

    let (table, tokens, broker) = test("proc main(a: int) {}");
    assert_eq!(
        table,
        GlobalTable::initialize(vec![(
            "main".to_string(),
            GlobalEntry::Procedure(ProcedureEntry {
                name: Identifier::new("main".to_string(), &tokens[1..2]),
                local_table: LocalTable {
                    entries: HashMap::from([(
                        "a".to_string(),
                        LocalEntry::Parameter(VariableEntry {
                            name: Identifier::new("a".to_string(), &tokens[3..4]),
                            is_ref: false,
                            data_type: Some(DataType::Int),
                            doc: None,
                        })
                    )])
                },
                parameters: vec![VariableEntry {
                    name: Identifier::new("a".to_string(), &tokens[3..4]),
                    is_ref: false,
                    data_type: Some(DataType::Int),
                    doc: None,
                }],
                doc: None,
            })
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(
            5..9,
            BuildErrorMessage::MainMustNotHaveParameters.to_string()
        )]
    );
}

#[test]
fn redeclaration() {
    let (_, _, broker) = test("type a = int; proc a() {}");
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

    let (_, _, broker) = test("proc a() {}\ntype a = int; ");
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

    let (_, _, broker) = test("proc a(i: int, i: int) {}");
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

    let (_, _, broker) = test(
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
