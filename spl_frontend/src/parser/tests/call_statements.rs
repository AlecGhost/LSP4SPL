use super::*;

#[test]
fn simple() {
    let broker = LocalBroker::default();
    let stmt = "a();";
    let tokens = lex(stmt, broker);
    let (input, cs) =
        all_consuming(terminated(CallStatement::parse, eof))(tokens.to_tokens()).unwrap();
    eq!(
        cs,
        CallStatement {
            name: Identifier::new("a".to_string(), &tokens[0..1]),
            arguments: Vec::new(),
            info: AstInfo::new(&tokens[0..4]),
        },
        "CallStatement: {}",
        stmt
    );
    assert!(input.broker.errors().is_empty(), "CallStatement: {}", stmt);
}

#[test]
fn with_args() {
    let stmt = "a(1, 2, 3);";
    let broker = LocalBroker::default();
    let tokens = lex(stmt, broker);
    let (input, cs) =
        all_consuming(terminated(CallStatement::parse, eof))(tokens.to_tokens()).unwrap();
    eq!(
        cs,
        CallStatement {
            name: Identifier::new("a".to_string(), &tokens[0..1]),
            arguments: vec![
                *int_lit(1, &tokens[2..3]),
                *int_lit(2, &tokens[4..5]),
                *int_lit(3, &tokens[6..7]),
            ],
            info: AstInfo::new(&tokens[..9]),
        },
        "CallStatement: {}",
        stmt
    );
    assert!(input.broker.errors().is_empty(), "CallStatement: {}", stmt);
}

#[test]
fn trailing_comma() {
    let stmt = "a(1,)";
    let broker = LocalBroker::default();
    let tokens = lex(stmt, broker);
    let (input, cs) =
        all_consuming(terminated(CallStatement::parse, eof))(tokens.to_tokens()).unwrap();
    eq!(
        cs,
        CallStatement {
            name: Identifier::new("a".to_string(), &tokens[0..1]),
            arguments: vec![*int_lit(1, &tokens[2..3])],
            info: AstInfo::new(&tokens[..5]),
        },
        "CallStatement: {}",
        stmt
    );
    eq!(
        input.broker.errors(),
        vec![
            SplError(
                4..4,
                ParseErrorMessage::ExpectedToken("expression".to_string()).to_string()
            ),
            SplError(5..5, ParseErrorMessage::MissingTrailingSemic.to_string()),
        ],
        "CallStatement: {}",
        stmt
    );
}
