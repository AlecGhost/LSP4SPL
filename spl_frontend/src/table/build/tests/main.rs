use super::*;

#[test]
fn simple() {
    let (table, tokens, broker) = test("proc main() {}");
    eq!(
        table,
        GlobalTable::initialize(vec![(
            "main".to_string(),
            GlobalEntry::Procedure(ProcedureEntry {
                name: Identifier::new("main".to_string(), &tokens[1..2]),
                local_table: LocalTable::default(),
                doc: None,
            })
        )])
    );
    eq!(broker.errors(), Vec::new());
}

#[test]
fn empty() {
    let (table, _, broker) = test("");
    eq!(table, GlobalTable::initialized());
    eq!(
        broker.errors(),
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())]
    );
}

#[test]
fn is_type_declaration() {
    let (table, _, broker) = test("type main = int;");
    eq!(table, GlobalTable::initialized());
    eq!(
        broker.errors(),
        vec![
            SplError(5..9, BuildErrorMessage::MainIsNotAProcedure.to_string()),
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())
        ]
    );
}

#[test]
fn redeclaration() {
    let (table, tokens, broker) = test("type main = int; proc main() {}");
    eq!(
        table,
        GlobalTable::initialize(vec![(
            "main".to_string(),
            GlobalEntry::Procedure(ProcedureEntry {
                name: Identifier::new("main".to_string(), &tokens[6..7]),
                local_table: LocalTable::default(),
                doc: None,
            })
        )])
    );
    eq!(
        broker.errors(),
        vec![SplError(
            5..9,
            BuildErrorMessage::MainIsNotAProcedure.to_string()
        ),]
    );
}

#[test]
fn main_with_params() {
    let (table, tokens, broker) = test("proc main(a: int) {}");
    eq!(
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
                doc: None,
            })
        )])
    );
    eq!(
        broker.errors(),
        vec![SplError(
            5..9,
            BuildErrorMessage::MainMustNotHaveParameters.to_string()
        )]
    );
}
