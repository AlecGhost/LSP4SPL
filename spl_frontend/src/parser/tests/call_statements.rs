use crate::parser::inc;
use super::*;

#[test]
fn simple() {
    let stmt = "a();";
    let tokens = lex(stmt);
    let (_, cs) =
        all_consuming(terminated(inc::<CallStatement>(None), eof))(tokens.to_tokens()).unwrap();
    eq!(
        cs,
        CallStatement {
            name: Identifier::new("a".to_string(), 0..1),
            arguments: Vec::new(),
            info: AstInfo::new(0..4),
        },
        "CallStatement: {}",
        stmt
    );
}

#[test]
fn with_args() {
    let stmt = "a(1, 2, 3);";
    let tokens = lex(stmt);
    let (_, cs) = all_consuming(terminated(inc::<CallStatement>(None), eof))(tokens.to_tokens()).unwrap();
    eq!(
        cs,
        CallStatement {
            name: Identifier::new("a".to_string(), 0..1),
            arguments: vec![
                Reference::new(*int_lit(1, 0..1), 2),
                Reference::new(*int_lit(2, 0..1), 4),
                Reference::new(*int_lit(3, 0..1), 6),
            ],
            info: AstInfo::new(0..9),
        },
        "CallStatement: {}",
        stmt
    );
}

#[test]
fn trailing_comma() {
    let stmt = "a(1,)";
    let tokens = lex(stmt);
    let (_, cs) = all_consuming(terminated(inc::<CallStatement>(None), eof))(tokens.to_tokens()).unwrap();
    eq!(
        cs,
        CallStatement {
            name: Identifier::new("a".to_string(), 0..1),
            arguments: vec![
                Reference::new(*int_lit(1, 0..1), 2),
                Reference::new(
                    Expression::Error(AstInfo::new_with_errors(
                        0..0,
                        vec![SplError(
                            0..0,
                            ParseErrorMessage::ExpectedToken("expression".to_string()).into()
                        ),],
                    )),
                    4
                ),
            ],
            info: AstInfo::new_with_errors(
                0..5,
                vec![SplError(
                    4..4,
                    ParseErrorMessage::MissingTrailingSemic.into()
                ),],
            ),
        },
        "CallStatement: {}",
        stmt
    );
}
