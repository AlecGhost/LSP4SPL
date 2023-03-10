use super::*;

#[test]
fn simple() {
    let (table, tokens, broker) = test("type a = int;");
    eq!(
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
    eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );
}

#[test]
fn array() {
    let (table, tokens, broker) = test("type a = array [5] of int;");
    eq!(
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
    eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );
}

#[test]
fn invalid_bool() {
    let (table, tokens, broker) = test("type a = bool;");
    eq!(
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
    eq!(
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
