use super::*;
use crate::{lexer::lex, LocalBroker};
use nom::combinator::all_consuming;
#[cfg(test)]
use pretty_assertions::assert_eq;
use std::ops::Range;

trait ToTokens<B> {
    fn to_tokens(&self) -> Tokens<B>;
}

impl ToTokens<LocalBroker> for Vec<Token> {
    fn to_tokens(&self) -> Tokens<LocalBroker> {
        Tokens::new(self, LocalBroker::default())
    }
}

fn int_lit(value: u32, range: Range<usize>) -> Box<Expression> {
    Box::new(Expression::IntLiteral(IntLiteral::new(value, range)))
}

#[test]
fn idents() {
    let i = "ab1";
    let tokens = lex(i);
    assert_eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("ab1", 0..3),
        "Identifier: {}",
        i
    );

    let i = "test_ident";
    let tokens = lex(i);
    assert_eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("test_ident", 0..10),
        "Identifier: {}",
        i
    );

    let i = "_a";
    let tokens = lex(i);
    assert_eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("_a", 0..2),
        "Identifier: {}",
        i
    );

    let i = "1a";
    let tokens = lex(i);
    assert!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens()).is_err(),
        "Identifier: {}",
        i
    );
}

#[test]
fn keywords() {
    let kw = "array";
    let tokens = lex(kw);
    assert!(
        all_consuming(terminated(keywords::array, eof))(tokens.to_tokens()).is_ok(),
        "Keyword: {}",
        kw
    );

    let kw = "array_";
    let tokens = lex(kw);
    assert!(
        all_consuming(terminated(keywords::array, eof))(tokens.to_tokens()).is_err(),
        "Keyword: {}",
        kw
    );

    let kw = "type a=int;";
    let tokens = lex(kw);
    assert!(
        all_consuming(terminated(TypeDeclaration::parse, eof))(tokens.to_tokens()).is_ok(),
        "Keyword: {}",
        kw
    );

    let kw = "typea=int;";
    let tokens = lex(kw);
    assert!(
        all_consuming(terminated(TypeDeclaration::parse, eof))(tokens.to_tokens()).is_err(),
        "Keyword: {}",
        kw
    );
}

#[test]
fn expressions() {
    type E = Expression;

    let expr = "1";
    let tokens = lex(expr);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::IntLiteral(IntLiteral::new(1, 0..1)),
        "Expression: {}",
        expr
    );
    let expr = "1 + 2";
    let tokens = lex(expr);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: int_lit(1, 0..1),
            rhs: int_lit(2, 4..5),
            range: 0..5,
        }),
        "Expression: {}",
        expr
    );
    let expr = "1 + 2 * 3";
    let tokens = lex(expr);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: int_lit(1, 0..1),
            rhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Mul,
                lhs: int_lit(2, 4..5),
                rhs: int_lit(3, 8..9),
                range: 4..9,
            })),
            range: 0..9,
        }),
        "Expression: {}",
        expr
    );
    let expr = "1 / 2 + 3";
    let tokens = lex(expr);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Div,
                lhs: int_lit(1, 0..1),
                rhs: int_lit(2, 4..5),
                range: 0..5,
            })),
            rhs: int_lit(3, 8..9),
            range: 0..9,
        }),
        "Expression: {}",
        expr
    );
    let expr = "1 * 2 / 3 * 4";
    let tokens = lex(expr);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Mul,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Div,
                lhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Mul,
                    lhs: int_lit(1, 0..1),
                    rhs: int_lit(2, 4..5),
                    range: 0..5,
                })),
                rhs: int_lit(3, 8..9),
                range: 0..9,
            })),
            rhs: int_lit(4, 12..13),
            range: 0..13,
        }),
        "Expression: {}",
        expr
    );
    let expr = "1 - 2 + 3 - 4";
    let tokens = lex(expr);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Sub,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Add,
                lhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Sub,
                    lhs: int_lit(1, 0..1),
                    rhs: int_lit(2, 4..5),
                    range: 0..5,
                })),
                rhs: int_lit(3, 8..9),
                range: 0..9,
            })),
            rhs: int_lit(4, 12..13),
            range: 0..13,
        }),
        "Expression: {}",
        expr
    );
    let expr = "(1 + 2) * 3 = 4 + 5 * 6 / 6";
    let tokens = lex(expr);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Equ,
            lhs: Box::new(Expression::Binary(BinaryExpression {
                operator: Operator::Mul,
                lhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Add,
                    lhs: int_lit(1, 1..2),
                    rhs: int_lit(2, 5..6),
                    range: 1..6,
                })),
                rhs: int_lit(3, 10..11),
                range: 1..11,
            })),
            rhs: Box::new(Expression::Binary(BinaryExpression {
                operator: Operator::Add,
                lhs: int_lit(4, 14..15),
                rhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Div,
                    lhs: Box::new(E::Binary(BinaryExpression {
                        operator: Operator::Mul,
                        lhs: int_lit(5, 18..19),
                        rhs: int_lit(6, 22..23),
                        range: 18..23,
                    })),
                    rhs: int_lit(6, 26..27),
                    range: 18..27,
                })),
                range: 14..27,
            })),
            range: 1..27,
        }),
        "Expression: {}",
        expr
    );
    let expr = "a < b > c";
    let tokens = lex(expr);
    assert!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens()).is_err(),
        "Expression: {}",
        expr
    );
}

#[test]
fn type_declarations() {
    type TD = TypeDeclaration;
    type TE = TypeExpression;

    let dec = "type a = int;";
    let tokens = lex(dec);
    assert_eq!(
        all_consuming(terminated(TD::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        TD {
            type_kw: 0..4,
            name: Some(Identifier::new("a", 5..6)),
            type_expr: Some(TE::NamedType(Identifier::new("int", 9..12))),
            range: 0..13,
        },
        "Declaration: {}",
        dec
    );

    let dec = "type a = array [2] of array [3] of int;";
    let tokens = lex(dec);
    assert_eq!(
        all_consuming(terminated(TD::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        TD {
            type_kw: 0..4,
            name: Some(Identifier::new("a", 5..6)),
            type_expr: Some(TE::ArrayType {
                array_kw: 9..14,
                of_kw: Some(19..21),
                size: Some(2),
                base_type: Some(Box::new(TE::ArrayType {
                    array_kw: 22..27,
                    of_kw: Some(32..34),
                    size: Some(3),
                    base_type: Some(Box::new(TE::NamedType(Identifier::new("int", 35..38)))),
                    range: 22..38,
                })),
                range: 9..38,
            }),
            range: 0..39,
        },
        "Declaration: {}",
        dec
    );

    let dec = "type = array [] of array [] of;";
    let tokens = lex(dec);
    let (input, td) = all_consuming(terminated(TD::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        td,
        TD {
            type_kw: 0..4,
            name: None,
            type_expr: Some(TE::ArrayType {
                array_kw: 7..12,
                of_kw: Some(16..18),
                size: None,
                base_type: Some(Box::new(TE::ArrayType {
                    array_kw: 19..24,
                    of_kw: Some(28..30),
                    size: None,
                    base_type: None,
                    range: 19..30,
                })),
                range: 7..30,
            }),
            range: 0..31
        },
        "Declaration: {}",
        dec
    );
    assert_eq!(
        input.broker.errors(),
        vec![
            SplError(
                5..5,
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

#[test]
fn assignments() {
    let asgn = "a := 1;";
    let tokens = lex(asgn);
    let (input, assignment) =
        all_consuming(terminated(Assignment::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        assignment,
        Assignment {
            variable: Variable::NamedVariable(Identifier::new("a", 0..1)),
            expr: Some(Expression::IntLiteral(IntLiteral::new(1, 5..6))),
            range: 0..7,
        },
        "Assignment: {}",
        asgn
    );
    assert!(input.broker.errors().is_empty(), "Assignment: {}", asgn);

    let asgn = "a = 1;";
    let tokens = lex(asgn);
    assert!(
        Assignment::parse(tokens.to_tokens()).is_err(),
        "Assignment: {}",
        asgn
    );
}

#[test]
fn call_statements() {
    let stmt = "a();";
    let tokens = lex(stmt);
    let (input, cs) =
        all_consuming(terminated(CallStatement::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a", 0..1),
            arguments: Vec::new(),
            range: 0..4,
        },
        "CallStatement: {}",
        stmt
    );
    assert!(input.broker.errors().is_empty(), "CallStatement: {}", stmt);

    let stmt = "a(1, 2, 3);";
    let tokens = lex(stmt);
    let (input, cs) =
        all_consuming(terminated(CallStatement::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a", 0..1),
            arguments: vec![
                Expression::IntLiteral(IntLiteral::new(1, 2..3)),
                Expression::IntLiteral(IntLiteral::new(2, 5..6)),
                Expression::IntLiteral(IntLiteral::new(3, 8..9)),
            ],
            range: 0..11,
        },
        "CallStatement: {}",
        stmt
    );
    assert!(input.broker.errors().is_empty(), "CallStatement: {}", stmt);

    let stmt = "a(1,)";
    let tokens = lex(stmt);
    let (input, cs) =
        all_consuming(terminated(CallStatement::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a", 0..1),
            arguments: vec![Expression::IntLiteral(IntLiteral::new(1, 2..3)),],
            range: 0..5,
        },
        "CallStatement: {}",
        stmt
    );
    assert_eq!(
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

#[test]
fn if_statements() {
    let stmt = "if (1 = 2) {}";
    let tokens = lex(stmt);
    let (input, is) =
        all_consuming(terminated(IfStatement::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        is,
        IfStatement {
            if_kw: 0..2,
            condition: Some(Expression::Binary(BinaryExpression {
                operator: Operator::Equ,
                lhs: int_lit(1, 4..5),
                rhs: int_lit(2, 8..9),
                range: 4..9,
            })),
            if_branch: Some(Box::new(Statement::Block(BlockStatement {
                statements: Vec::new(),
                range: 11..13,
            }))),
            else_kw: None,
            else_branch: None,
            range: 0..13,
        },
        "IfStatement: {}",
        stmt
    );
    assert!(input.broker.errors().is_empty(), "IfStatement: {}", stmt);
}

#[test]
fn acker() {
    let acker = std::fs::read_to_string("/Users/alex/dev/compiler/programs/acker.spl").unwrap();
    let tokens = lex(&acker);
    let (input, program) = all_consuming(Program::parse)(tokens.to_tokens()).unwrap();

    // variables for use in assertion
    let int_type = |range| Some(TypeExpression::NamedType(Identifier::new("int", range)));
    let a = |range| Some(Identifier::new("a", range));
    let i = |range| Some(Identifier::new("i", range));
    let j = |range| Some(Identifier::new("j", range));
    let k = |range| Some(Identifier::new("k", range));
    let var_i = |range| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("i", range),
        )))
    };
    let var_a = |range| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("a", range),
        )))
    };
    let var_j = |range| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("j", range),
        )))
    };
    let var_k = |range| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("k", range),
        )))
    };
    fn call_ackermann(
        range_ident: Range<usize>,
        range: Range<usize>,
        arg0: Expression,
        arg1: Expression,
        arg2: Expression,
    ) -> Statement {
        Statement::Call(CallStatement {
            name: Identifier::new("ackermann", range_ident),
            arguments: vec![arg0, arg1, arg2],
            range,
        })
    }

    assert_eq!(
        program,
        Program {
            global_declarations: vec![
                GlobalDeclaration::Procedure(ProcedureDeclaration {
                    proc_kw: 45..49,
                    name: Some(Identifier::new("ackermann", 50..59)),
                    parameters: vec![
                        ParameterDeclaration {
                            ref_kw: None,
                            name: i(60..61),
                            type_expr: int_type(63..66),
                            range: 60..66,
                        },
                        ParameterDeclaration {
                            ref_kw: None,
                            name: j(68..69),
                            type_expr: int_type(71..74),
                            range: 68..74,
                        },
                        ParameterDeclaration {
                            ref_kw: Some(76..79),
                            name: k(80..81),
                            type_expr: int_type(83..86),
                            range: 76..86,
                        },
                    ],
                    variable_declarations: vec![VariableDeclaration {
                        var_kw: 92..95,
                        name: a(96..97),
                        type_expr: int_type(99..102),
                        range: 92..103,
                    }],
                    statements: vec![Statement::If(IfStatement {
                        if_kw: 107..109,
                        condition: Some(Expression::Binary(BinaryExpression {
                            operator: Operator::Equ,
                            lhs: var_i(111..112),
                            rhs: int_lit(0, 115..116),
                            range: 111..116,
                        })),
                        if_branch: Some(Box::new(Statement::Block(BlockStatement {
                            statements: vec![Statement::Assignment(Assignment {
                                variable: Variable::NamedVariable(Identifier::new("k", 124..125)),
                                expr: Some(Expression::Binary(BinaryExpression {
                                    operator: Operator::Add,
                                    lhs: var_j(129..130),
                                    rhs: int_lit(1, 133..134),
                                    range: 129..134,
                                })),
                                range: 124..135,
                            })],
                            range: 118..139,
                        }))),
                        else_kw: Some(140..144),
                        else_branch: Some(Box::new(Statement::Block(BlockStatement {
                            statements: vec![Statement::If(IfStatement {
                                if_kw: 151..153,
                                condition: Some(Expression::Binary(BinaryExpression {
                                    operator: Operator::Equ,
                                    lhs: var_j(155..156),
                                    rhs: int_lit(0, 159..160),
                                    range: 155..160,
                                })),
                                if_branch: Some(Box::new(Statement::Block(BlockStatement {
                                    statements: vec![call_ackermann(
                                        170..179,
                                        170..193,
                                        Expression::Binary(BinaryExpression {
                                            operator: Operator::Sub,
                                            lhs: var_i(180..181),
                                            rhs: int_lit(1, 184..185),
                                            range: 180..185,
                                        }),
                                        Expression::IntLiteral(IntLiteral::new(1, 187..188)),
                                        *var_k(190..191)
                                    )],
                                    range: 162..199,
                                }))),
                                else_kw: Some(200..204),
                                else_branch: Some(Box::new(Statement::Block(BlockStatement {
                                    statements: vec![
                                        call_ackermann(
                                            213..222,
                                            213..236,
                                            *var_i(223..224),
                                            Expression::Binary(BinaryExpression {
                                                operator: Operator::Sub,
                                                lhs: var_j(226..227),
                                                rhs: int_lit(1, 230..231),
                                                range: 226..231,
                                            }),
                                            *var_a(233..234)
                                        ),
                                        call_ackermann(
                                            243..252,
                                            243..266,
                                            Expression::Binary(BinaryExpression {
                                                operator: Operator::Sub,
                                                lhs: var_i(253..254),
                                                rhs: int_lit(1, 257..258),
                                                range: 253..258,
                                            }),
                                            *var_a(260..261),
                                            *var_k(263..264)
                                        )
                                    ],
                                    range: 205..272,
                                }))),

                                range: 151..272,
                            })],
                            range: 145..276,
                        }))),
                        range: 107..276,
                    })],
                    range: 45..278,
                }),
                GlobalDeclaration::Procedure(ProcedureDeclaration {
                    proc_kw: 281..285,
                    name: Some(Identifier::new("main", 286..290)),
                    parameters: Vec::new(),
                    variable_declarations: vec![
                        VariableDeclaration {
                            var_kw: 297..300,
                            name: i(301..302),
                            type_expr: int_type(304..307),
                            range: 297..308,
                        },
                        VariableDeclaration {
                            var_kw: 311..314,
                            name: j(315..316),
                            type_expr: int_type(318..321),
                            range: 311..322,
                        },
                        VariableDeclaration {
                            var_kw: 325..328,
                            name: k(329..330),
                            type_expr: int_type(332..335),
                            range: 325..336,
                        }
                    ],
                    statements: vec![
                        Statement::Assignment(Assignment {
                            variable: Variable::NamedVariable(Identifier::new("i", 340..341)),
                            expr: Some(*int_lit(0, 345..346).clone()),
                            range: 340..347,
                        }),
                        Statement::While(WhileStatement {
                            while_kw: 350..355,
                            condition: Some(Expression::Binary(BinaryExpression {
                                operator: Operator::Lse,
                                lhs: var_i(357..358),
                                rhs: int_lit(3, 362..363),
                                range: 357..363,
                            })),
                            statement: Some(Box::new(Statement::Block(BlockStatement {
                                statements: vec![
                                    Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new(
                                            "j",
                                            371..372
                                        )),
                                        expr: Some(*int_lit(0, 376..377)),
                                        range: 371..378,
                                    }),
                                    Statement::While(WhileStatement {
                                        while_kw: 383..388,
                                        condition: Some(Expression::Binary(BinaryExpression {
                                            operator: Operator::Lse,
                                            lhs: var_j(390..391),
                                            rhs: int_lit(6, 395..396),
                                            range: 390..396,
                                        })),
                                        statement: Some(Box::new(Statement::Block(
                                            BlockStatement {
                                                statements: vec![
                                                    call_ackermann(
                                                        406..415,
                                                        406..425,
                                                        *var_i(416..417),
                                                        *var_j(419..420),
                                                        *var_k(422..423)
                                                    ),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printi", 432..438),
                                                        arguments: vec![*var_i(439..440)],
                                                        range: 432..442,
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printc", 449..455),
                                                        arguments: vec![*int_lit(32, 456..459)],
                                                        range: 449..461,
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printi", 468..474),
                                                        arguments: vec![*var_j(475..476)],
                                                        range: 468..478,
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printc", 485..491),
                                                        arguments: vec![*int_lit(32, 492..495)],
                                                        range: 485..497,
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printi", 504..510),
                                                        arguments: vec![*var_k(511..512)],
                                                        range: 504..514,
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printc", 521..527),
                                                        arguments: vec![*int_lit(10, 528..532)],
                                                        range: 521..534,
                                                    }),
                                                    Statement::Assignment(Assignment {
                                                        variable: Variable::NamedVariable(
                                                            Identifier::new("j", 541..542)
                                                        ),
                                                        expr: Some(Expression::Binary(
                                                            BinaryExpression {
                                                                operator: Operator::Add,
                                                                lhs: var_j(546..547),
                                                                rhs: int_lit(1, 550..551),
                                                                range: 546..551,
                                                            }
                                                        )),
                                                        range: 541..552,
                                                    })
                                                ],
                                                range: 398..558,
                                            }
                                        ))),
                                        range: 383..558,
                                    }),
                                    Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new(
                                            "i",
                                            563..564
                                        )),
                                        expr: Some(Expression::Binary(BinaryExpression {
                                            operator: Operator::Add,
                                            lhs: var_i(568..569),
                                            rhs: int_lit(1, 572..573),
                                            range: 568..573,
                                        })),
                                        range: 563..574,
                                    })
                                ],
                                range: 365..578,
                            }))),
                            range: 350..578,
                        })
                    ],
                    range: 281..582,
                }),
            ],
        },
        "Acker: {}",
        acker
    );
    assert!(input.broker.errors().is_empty(), "Acker: {}", acker);
}
