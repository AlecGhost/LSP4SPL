use super::*;

#[test]
fn correct_alphanumeric() {
    let i = "ab1";
    let broker = LocalBroker::default();
    let tokens = lex(i, broker);
    eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("ab1".to_string(), &tokens[0..1]),
        "Identifier: {}",
        i
    );
}

#[test]
fn underscore_in_middle() {
    let i = "test_ident";
    let broker = LocalBroker::default();
    let tokens = lex(i, broker);
    eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("test_ident".to_string(), &tokens[0..1]),
        "Identifier: {}",
        i
    );
}

#[test]
fn underscore_in_front() {
    let i = "_a";
    let broker = LocalBroker::default();
    let tokens = lex(i, broker);
    eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("_a".to_string(), &tokens[0..1]),
        "Identifier: {}",
        i
    );
}

#[test]
fn invalid_number_in_front() {
    let i = "1a";
    let broker = LocalBroker::default();
    let tokens = lex(i, broker);
    assert!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens()).is_err(),
        "Identifier: {}",
        i
    );
}
