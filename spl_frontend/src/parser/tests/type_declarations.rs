use super::*;

type TD = TypeDeclaration;
type TE = TypeExpression;

#[test]
fn simple() {
    let dec = "type a = int;";
    let tokens = lex(dec);
    eq!(
        all_consuming(terminated(|input| TD::parse(None, input), eof))(tokens.to_tokens())
            .unwrap()
            .1,
        TD {
            doc: Vec::new(),
            name: Some(Identifier::new("a".to_string(), 1..2)),
            type_expr: Some(Reference::new(
                TE::NamedType(Identifier::new("int".to_string(), 0..1)),
                3
            )),
            info: AstInfo::new(0..5),
        },
        "Declaration: {}",
        dec
    );
}

#[test]
fn nested_arrays() {
    let dec = "type a = array [2] of array [3] of int;";
    let tokens = lex(dec);
    eq!(
        all_consuming(terminated(|input| TD::parse(None, input), eof))(tokens.to_tokens())
            .unwrap()
            .1,
        TD {
            doc: Vec::new(),
            name: Some(Identifier::new("a".to_string(), 1..2)),
            type_expr: Some(Reference::new(
                TE::ArrayType {
                    size: Some(IntLiteral {
                        value: Some(2),
                        info: AstInfo::new(2..3)
                    }),
                    base_type: Some(Box::new(Reference::new(
                        TE::ArrayType {
                            size: Some(IntLiteral {
                                value: Some(3),
                                info: AstInfo::new(2..3),
                            }),
                            base_type: Some(Box::new(Reference::new(
                                TE::NamedType(Identifier::new("int".to_string(), 0..1)),
                                5
                            ))),
                            info: AstInfo::new(0..6),
                        },
                        5
                    ))),
                    info: AstInfo::new(0..11),
                },
                3
            )),
            info: AstInfo::new(0..15),
        },
        "Declaration: {}",
        dec
    );
}

#[test]
fn missing_array_index() {
    let dec = "type = array [] of array [] of;";
    let tokens = lex(dec);
    let (_, td) =
        all_consuming(terminated(|input| TD::parse(None, input), eof))(tokens.to_tokens()).unwrap();
    eq!(
        td,
        TD {
            doc: Vec::new(),
            name: None,
            type_expr: Some(Reference::new(
                TE::ArrayType {
                    size: None,
                    base_type: Some(Box::new(Reference::new(
                        TE::ArrayType {
                            size: None,
                            base_type: None,
                            info: AstInfo::new_with_errors(
                                0..4,
                                vec![
                                    SplError(
                                        1..1,
                                        ParseErrorMessage::ExpectedToken("int literal".to_string())
                                            .into()
                                    ),
                                    SplError(
                                        3..3,
                                        ParseErrorMessage::ExpectedToken(
                                            "type expression".to_string()
                                        )
                                        .into()
                                    ),
                                ]
                            ),
                        },
                        4
                    ))),
                    info: AstInfo::new_with_errors(
                        0..8,
                        vec![SplError(
                            1..1,
                            ParseErrorMessage::ExpectedToken("int literal".to_string()).into()
                        ),]
                    ),
                },
                2
            )),
            info: AstInfo::new_with_errors(
                0..11,
                vec![SplError(
                    0..0,
                    ParseErrorMessage::ExpectedToken("identifier".to_string()).into()
                ),]
            ),
        },
        "Declaration: {}",
        dec
    );
}
