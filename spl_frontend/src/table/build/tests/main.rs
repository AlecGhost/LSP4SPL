use super::*;

#[test]
fn simple() {
    let (table, errors) = test("proc main() {}");
    eq!(
        table,
        GlobalTable::initialize(vec![(
            "main".to_string(),
            GlobalEntry::Procedure(ProcedureEntry {
                name: Identifier::new("main".to_string(), 1..2),
                local_table: LocalTable::default(),
                parameters: Vec::new(),
                range: 0..6,
                doc: None,
            })
        )])
    );
    eq!(errors, Vec::new());
}

#[test]
fn empty() {
    let (table, errors) = test("");
    eq!(table, GlobalTable::initialized());
    eq!(
        errors,
        vec![SplError(0..0, BuildErrorMessage::MainIsMissing.into())]
    );
}

#[test]
fn is_type_declaration() {
    let (table, errors) = test("type main = int;");
    eq!(table, GlobalTable::initialized());
    eq!(
        errors,
        vec![
            SplError(0..0, BuildErrorMessage::MainIsMissing.into()),
            SplError(1..2, BuildErrorMessage::MainIsNotAProcedure.into()),
        ]
    );
}

#[test]
fn redeclaration() {
    let (table, errors) = test("type main = int; proc main() {}");
    eq!(
        table,
        GlobalTable::initialize(vec![(
            "main".to_string(),
            GlobalEntry::Procedure(ProcedureEntry {
                name: Identifier::new("main".to_string(), 1..2),
                local_table: LocalTable::default(),
                parameters: Vec::new(),
                range: 5..11,
                doc: None,
            })
        )])
    );
    eq!(
        errors,
        vec![SplError(
            1..2,
            BuildErrorMessage::MainIsNotAProcedure.into()
        ),]
    );
}

#[test]
fn main_with_params() {
    let (table, errors) = test("proc main(a: int) {}");
    eq!(
        table,
        GlobalTable::initialize(vec![(
            "main".to_string(),
            GlobalEntry::Procedure(ProcedureEntry {
                name: Identifier::new("main".to_string(), 1..2),
                local_table: LocalTable {
                    entries: HashMap::from([(
                        "a".to_string(),
                        LocalEntry::Parameter(VariableEntry {
                            name: Identifier::new("a".to_string(), 0..1),
                            is_ref: false,
                            data_type: Some(DataType::Int),
                            range: 3..6,
                            doc: None,
                        })
                    )])
                },
                parameters: vec![VariableEntry {
                    name: Identifier::new("a".to_string(), 0..1),
                    is_ref: false,
                    data_type: Some(DataType::Int),
                    range: 3..6,
                    doc: None,
                }],
                range: 0..9,
                doc: None,
            })
        )])
    );
    eq!(
        errors,
        vec![SplError(
            1..2,
            BuildErrorMessage::MainMustNotHaveParameters.into()
        )]
    );
}
