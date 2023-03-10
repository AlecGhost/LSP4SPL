use super::*;

type TD = TypeDeclaration;
type TE = TypeExpression;

#[test]
fn simple() {
    let dec = "type a = int;";
    let broker = LocalBroker::default();
    let tokens = lex(dec, broker);
    eq!(
        all_consuming(terminated(TD::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        TD {
            name: Some(Identifier::new("a".to_string(), &tokens[1..2])),
            type_expr: Some(TE::NamedType(Identifier::new(
                "int".to_string(),
                &tokens[3..4]
            ))),
            info: AstInfo::new(&tokens[..5]),
        },
        "Declaration: {}",
        dec
    );
}

#[test]
fn nested_arrays() {
    let dec = "type a = array [2] of array [3] of int;";
    let broker = LocalBroker::default();
    let tokens = lex(dec, broker);
    eq!(
        all_consuming(terminated(TD::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        TD {
            name: Some(Identifier::new("a".to_string(), &tokens[1..2])),
            type_expr: Some(TE::ArrayType {
                size: Some(IntLiteral {
                    value: Some(2),
                    info: AstInfo::new(&tokens[5..6])
                }),
                base_type: Some(Box::new(TE::ArrayType {
                    size: Some(IntLiteral {
                        value: Some(3),
                        info: AstInfo::new(&tokens[10..11]),
                    }),
                    base_type: Some(Box::new(TE::NamedType(Identifier::new(
                        "int".to_string(),
                        &tokens[13..14]
                    )))),
                    info: AstInfo::new(&tokens[8..14]),
                })),
                info: AstInfo::new(&tokens[3..14]),
            }),
            info: AstInfo::new(&tokens[..15]),
        },
        "Declaration: {}",
        dec
    );
}

#[test]
fn missing_array_index() {
    let dec = "type = array [] of array [] of;";
    let broker = LocalBroker::default();
    let tokens = lex(dec, broker);
    let (input, td) = all_consuming(terminated(TD::parse, eof))(tokens.to_tokens()).unwrap();
    eq!(
        td,
        TD {
            name: None,
            type_expr: Some(TE::ArrayType {
                size: None,
                base_type: Some(Box::new(TE::ArrayType {
                    size: None,
                    base_type: None,
                    info: AstInfo::new(&tokens[6..10]),
                })),
                info: AstInfo::new(&tokens[2..10]),
            }),
            info: AstInfo::new(&tokens[..11]),
        },
        "Declaration: {}",
        dec
    );
    eq!(
        input.broker.errors(),
        vec![
            SplError(
                4..4,
                ParseErrorMessage::ExpectedToken("identifier".to_string()).to_string()
            ),
            SplError(
                14..14,
                ParseErrorMessage::ExpectedToken("int literal".to_string()).to_string()
            ),
            SplError(
                26..26,
                ParseErrorMessage::ExpectedToken("int literal".to_string()).to_string()
            ),
            SplError(
                30..30,
                ParseErrorMessage::ExpectedToken("type expression".to_string()).to_string()
            ),
        ],
        "Declaration: {}",
        dec
    );
}
