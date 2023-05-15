use crate::token;

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
            variable: Variable::NamedVariable(Identifier::new("a".to_string(), 0..1)),
            expr: Some(Reference::new(*int_lit(1, 0..1), 2)),
            info: AstInfo::new(0..4),
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
    let (_, assignment) =
        all_consuming(terminated(Assignment::parse, eof))(tokens.to_tokens()).unwrap();
    eq!(
        assignment,
        Assignment {
            variable: Variable::NamedVariable(Identifier::new("a".to_string(), 0..1)),
            expr: Some(Reference::new(*int_lit(1, 0..1), 2)),
            info: AstInfo::new_with_errors(
                0..4,
                vec![SplError(
                    1..2,
                    ParseErrorMessage::ConfusedToken(
                        token::ASSIGN.to_string(),
                        token::EQ.to_string()
                    )
                    .to_string()
                )],
            ),
        },
        "Assignment: {}",
        asgn
    );
}
