use super::*;

#[test]
fn simple() {
    let broker = LocalBroker::default();
    let asgn = "a := 1;";
    let tokens = lex(asgn, broker);
    let (input, assignment) =
        all_consuming(terminated(Assignment::parse, eof))(tokens.to_tokens()).unwrap();
    eq!(
        assignment,
        Assignment {
            variable: Variable::NamedVariable(Identifier::new("a".to_string(), &tokens[0..1])),
            expr: Some(*int_lit(1, &tokens[2..3])),
            info: AstInfo::new(&tokens[..4]),
        },
        "Assignment: {}",
        asgn
    );
    assert!(input.broker.errors().is_empty(), "Assignment: {}", asgn);
}

#[test]
fn invalid_equals_symbol() {
    let asgn = "a = 1;";
    let broker = LocalBroker::default();
    let tokens = lex(asgn, broker);
    assert!(
        Assignment::parse(tokens.to_tokens()).is_err(),
        "Assignment: {}",
        asgn
    );
}
