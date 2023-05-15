use super::*;

#[test]
fn simple() {
    let (table, errors) = test("type a = int;");
    eq!(
        table,
        GlobalTable::initialize(vec![(
            "a".to_string(),
            GlobalEntry::Type(TypeEntry {
                name: Identifier::new("a".to_string(), 1..2),
                data_type: Some(DataType::Int),
                range: 0..5,
                doc: None
            })
        )])
    );
    eq!(
        errors,
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );
}

#[test]
fn array() {
    let (table, errors) = test("type a = array [5] of int;");
    eq!(
        table,
        GlobalTable::initialize(vec![(
            "a".to_string(),
            GlobalEntry::Type(TypeEntry {
                name: Identifier::new("a".to_string(), 1..2),
                data_type: Some(DataType::Array {
                    size: 5,
                    base_type: Box::new(DataType::Int),
                    creator: "a".to_string(),
                }),
                range: 0..10,
                doc: None
            })
        )])
    );
    eq!(
        errors,
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );
}

#[test]
fn invalid_bool() {
    let (table, errors) = test("type a = bool;");
    eq!(
        table,
        GlobalTable::initialize(vec![(
            "a".to_string(),
            GlobalEntry::Type(TypeEntry {
                name: Identifier::new("a".to_string(), 1..2),
                data_type: None,
                range: 0..5,
                doc: None
            }),
        )])
    );
    eq!(
        errors,
        vec![
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string()),
            SplError(
                3..4,
                BuildErrorMessage::UndefinedType("bool".to_string()).to_string()
            ),
        ]
    );
}
