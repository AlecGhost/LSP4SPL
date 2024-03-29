use crate::tokens;

use super::*;

#[test]
fn simple() {
    let asgn = "a := 1;";
    let tokens = lex(asgn);
    let (_, assignment) =
        all_consuming(terminated(|input| Assignment::parse(None, input), eof))(tokens.to_tokens())
            .unwrap();
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
}

#[test]
fn invalid_equals_symbol() {
    let asgn = "a = 1;";
    let tokens = lex(asgn);
    let (_, assignment) =
        all_consuming(terminated(|input| Assignment::parse(None, input), eof))(tokens.to_tokens())
            .unwrap();
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
                        tokens::ASSIGN.to_string(),
                        tokens::EQ.to_string()
                    )
                    .into()
                )],
            ),
        },
        "Assignment: {}",
        asgn
    );
}
