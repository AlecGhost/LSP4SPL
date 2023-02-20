use crate::lexer::token::Token;
use crate::table::TypeEntry;
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

fn test(src: &str) -> (SymbolTable, Vec<Token>, LocalBroker) {
    eprintln!("Testing: {}", src);
    let tokens = crate::lexer::lex(src);
    let program = crate::parser::parse(&tokens, LocalBroker::default());
    let broker = LocalBroker::default();
    let table = crate::table::build(&program, broker.clone());
    (table, tokens, broker)
}

#[test]
fn type_decs() {
    let (table, tokens, broker) = test("type a = int;");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            "a".to_string(),
            RangedEntry {
                range: 0..13,
                entry: Entry::Type(TypeEntry {
                    name: Identifier::new("a", &tokens[1..2]),
                    data_type: Some(DataType::Int),
                    documentation: None
                }),
            },
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );

    let (table, tokens, broker) = test("type a = array [5] of int");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            "a".to_string(),
            RangedEntry {
                range: 0..25,
                entry: Entry::Type(TypeEntry {
                    name: Identifier::new("a", &tokens[1..2]),
                    data_type: Some(DataType::Array {
                        size: 5,
                        base_type: Box::new(DataType::Int),
                        creator: Identifier::new("a", &tokens[1..2]),
                    }),
                    documentation: None
                }),
            }
        )])
    );
    assert_eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );

    let (table, tokens, broker) = test("type a = bool;");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            "a".to_string(),
            RangedEntry {
                range: 0..14,
                entry: Entry::Type(TypeEntry {
                    name: Identifier::new("a", &tokens[1..2]),
                    data_type: None,
                    documentation: None
                }),
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
    let (table, tokens, broker) = test("proc main() {}");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            "main".to_string(),
            RangedEntry {
                range: 0..14,
                entry: Entry::Procedure(ProcedureEntry {
                    name: Identifier::new("main", &tokens[1..2]),
                    local_table: SymbolTable::default(),
                    parameters: Vec::new(),
                    documentation: None,
                }),
            }
        )])
    );
    assert_eq!(broker.errors(), Vec::new());

    let (table, _, broker) = test("");
    assert_eq!(table, SymbolTable::initialized());
    assert_eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );

    let (table, _, broker) = test("type main = int;");
    assert_eq!(table, SymbolTable::initialized());
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
        SymbolTable::initialize(vec![(
            "main".to_string(),
            RangedEntry {
                range: 17..31,
                entry: Entry::Procedure(ProcedureEntry {
                    name: Identifier::new("main", &tokens[6..7]),
                    local_table: SymbolTable::default(),
                    parameters: Vec::new(),
                    documentation: None,
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

    let (table, tokens, broker) = test("proc main(a: int) {}");
    assert_eq!(
        table,
        SymbolTable::initialize(vec![(
            "main".to_string(),
            RangedEntry {
                range: 0..20,
                entry: Entry::Procedure(ProcedureEntry {
                    name: Identifier::new("main", &tokens[1..2]),
                    local_table: SymbolTable {
                        entries: HashMap::from([(
                            "a".to_string(),
                            RangedEntry {
                                range: 10..16,
                                entry: Entry::Variable(VariableEntry {
                                    name: Identifier::new("a", &tokens[3..4]),
                                    is_ref: false,
                                    data_type: Some(DataType::Int),
                                    documentation: None,
                                }),
                            }
                        )])
                    },
                    parameters: vec![VariableEntry {
                        name: Identifier::new("a", &tokens[3..4]),
                        is_ref: false,
                        data_type: Some(DataType::Int),
                        documentation: None,
                    }],
                    documentation: None,
                }),
            }
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
