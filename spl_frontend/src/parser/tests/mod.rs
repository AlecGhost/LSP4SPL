use super::*;
use crate::{lexer::lex, LocalBroker};
use nom::combinator::all_consuming;
#[cfg(test)]
use pretty_assertions::assert_eq;

trait ToTokens<B> {
    fn to_tokens(&self) -> Tokens<B>;
}

impl ToTokens<LocalBroker> for Vec<Token> {
    fn to_tokens(&self) -> Tokens<LocalBroker> {
        Tokens::new(self, LocalBroker::default())
    }
}

fn int_lit(value: u32, tokens: &[Token]) -> Box<Expression> {
    Box::new(Expression::IntLiteral(IntLiteral::new(
        value,
        AstInfo::new(tokens),
    )))
}

#[test]
fn idents() {
    let i = "ab1";
    let broker = LocalBroker::default();
    let tokens = lex(i, broker);
    assert_eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("ab1", &tokens[0..1]),
        "Identifier: {}",
        i
    );

    let i = "test_ident";
    let broker = LocalBroker::default();
    let tokens = lex(i, broker);
    assert_eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("test_ident", &tokens[0..1]),
        "Identifier: {}",
        i
    );

    let i = "_a";
    let broker = LocalBroker::default();
    let tokens = lex(i, broker);
    assert_eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("_a", &tokens[0..1]),
        "Identifier: {}",
        i
    );

    let i = "1a";
    let broker = LocalBroker::default();
    let tokens = lex(i, broker);
    assert!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens()).is_err(),
        "Identifier: {}",
        i
    );
}

#[test]
fn keywords() {
    let kw = "array";
    let broker = LocalBroker::default();
    let tokens = lex(kw, broker);
    assert!(
        all_consuming(terminated(keywords::array, eof))(tokens.to_tokens()).is_ok(),
        "Keyword: {}",
        kw
    );

    let kw = "array_";
    let broker = LocalBroker::default();
    let tokens = lex(kw, broker);
    assert!(
        all_consuming(terminated(keywords::array, eof))(tokens.to_tokens()).is_err(),
        "Keyword: {}",
        kw
    );

    let kw = "type a=int;";
    let broker = LocalBroker::default();
    let tokens = lex(kw, broker);
    assert!(
        all_consuming(terminated(TypeDeclaration::parse, eof))(tokens.to_tokens()).is_ok(),
        "Keyword: {}",
        kw
    );

    let kw = "typea=int;";
    let broker = LocalBroker::default();
    let tokens = lex(kw, broker);
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
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        *int_lit(1, &tokens[0..1]),
        "Expression: {}",
        expr
    );

    let expr = "1 + 2";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: int_lit(1, &tokens[0..1]),
            rhs: int_lit(2, &tokens[2..3]),
            info: AstInfo::new(&tokens[0..3]),
        }),
        "Expression: {}",
        expr
    );

    let expr = "1 + 2 * 3";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: int_lit(1, &tokens[0..1]),
            rhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Mul,
                lhs: int_lit(2, &tokens[2..3]),
                rhs: int_lit(3, &tokens[4..5]),
                info: AstInfo::new(&tokens[2..5]),
            })),
            info: AstInfo::new(&tokens[..5]),
        }),
        "Expression: {}",
        expr
    );

    let expr = "1 / 2 + 3";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    assert_eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Div,
                lhs: int_lit(1, &tokens[0..1]),
                rhs: int_lit(2, &tokens[2..3]),
                info: AstInfo::new(&tokens[0..3]),
            })),
            rhs: int_lit(3, &tokens[4..5]),
            info: AstInfo::new(&tokens[..5]),
        }),
        "Expression: {}",
        expr
    );

    let expr = "1 * 2 / 3 * 4";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
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
                    lhs: int_lit(1, &tokens[0..1]),
                    rhs: int_lit(2, &tokens[2..3]),
                    info: AstInfo::new(&tokens[0..3]),
                })),
                rhs: int_lit(3, &tokens[4..5]),
                info: AstInfo::new(&tokens[0..5]),
            })),
            rhs: int_lit(4, &tokens[6..7]),
            info: AstInfo::new(&tokens[..7]),
        }),
        "Expression: {}",
        expr
    );

    let expr = "1 - 2 + 3 - 4";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
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
                    lhs: int_lit(1, &tokens[0..1]),
                    rhs: int_lit(2, &tokens[2..3]),
                    info: AstInfo::new(&tokens[0..3]),
                })),
                rhs: int_lit(3, &tokens[4..5]),
                info: AstInfo::new(&tokens[0..5]),
            })),
            rhs: int_lit(4, &tokens[6..7]),
            info: AstInfo::new(&tokens[..7]),
        }),
        "Expression: {}",
        expr
    );

    let expr = "(1 + 2) * 3 = 4 + 5 * 6 / 6";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
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
                    lhs: int_lit(1, &tokens[1..2]),
                    rhs: int_lit(2, &tokens[3..4]),
                    info: AstInfo::new(&tokens[1..4]),
                })),
                rhs: int_lit(3, &tokens[6..7]),
                info: AstInfo::new(&tokens[0..7]),
            })),
            rhs: Box::new(Expression::Binary(BinaryExpression {
                operator: Operator::Add,
                lhs: int_lit(4, &tokens[8..9]),
                rhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Div,
                    lhs: Box::new(E::Binary(BinaryExpression {
                        operator: Operator::Mul,
                        lhs: int_lit(5, &tokens[10..11]),
                        rhs: int_lit(6, &tokens[12..13]),
                        info: AstInfo::new(&tokens[10..13]),
                    })),
                    rhs: int_lit(6, &tokens[14..15]),
                    info: AstInfo::new(&tokens[10..15]),
                })),
                info: AstInfo::new(&tokens[8..15]),
            })),
            info: AstInfo::new(&tokens[..15]),
        }),
        "Expression: {}",
        expr
    );
    let expr = "a < b > c";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
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
    let broker = LocalBroker::default();
    let tokens = lex(dec, broker);
    assert_eq!(
        all_consuming(terminated(TD::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        TD {
            name: Some(Identifier::new("a", &tokens[1..2])),
            type_expr: Some(TE::NamedType(Identifier::new("int", &tokens[3..4]))),
            info: AstInfo::new(&tokens[..5]),
        },
        "Declaration: {}",
        dec
    );

    let dec = "type a = array [2] of array [3] of int;";
    let broker = LocalBroker::default();
    let tokens = lex(dec, broker);
    assert_eq!(
        all_consuming(terminated(TD::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        TD {
            name: Some(Identifier::new("a", &tokens[1..2])),
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
                        "int",
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

    let dec = "type = array [] of array [] of;";
    let broker = LocalBroker::default();
    let tokens = lex(dec, broker);
    let (input, td) = all_consuming(terminated(TD::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
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
    assert_eq!(
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

#[test]
fn assignments() {
    let broker = LocalBroker::default();
    let asgn = "a := 1;";
    let tokens = lex(asgn, broker);
    let (input, assignment) =
        all_consuming(terminated(Assignment::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        assignment,
        Assignment {
            variable: Variable::NamedVariable(Identifier::new("a", &tokens[0..1])),
            expr: Some(*int_lit(1, &tokens[2..3])),
            info: AstInfo::new(&tokens[..4]),
        },
        "Assignment: {}",
        asgn
    );
    assert!(input.broker.errors().is_empty(), "Assignment: {}", asgn);

    let asgn = "a = 1;";
    let broker = LocalBroker::default();
    let tokens = lex(asgn, broker);
    assert!(
        Assignment::parse(tokens.to_tokens()).is_err(),
        "Assignment: {}",
        asgn
    );
}

#[test]
fn call_statements() {
    let broker = LocalBroker::default();
    let stmt = "a();";
    let tokens = lex(stmt, broker);
    let (input, cs) =
        all_consuming(terminated(CallStatement::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a", &tokens[0..1]),
            arguments: Vec::new(),
            info: AstInfo::new(&tokens[0..4]),
        },
        "CallStatement: {}",
        stmt
    );
    assert!(input.broker.errors().is_empty(), "CallStatement: {}", stmt);

    let stmt = "a(1, 2, 3);";
    let broker = LocalBroker::default();
    let tokens = lex(stmt, broker);
    let (input, cs) =
        all_consuming(terminated(CallStatement::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a", &tokens[0..1]),
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

    let stmt = "a(1,)";
    let broker = LocalBroker::default();
    let tokens = lex(stmt, broker);
    let (input, cs) =
        all_consuming(terminated(CallStatement::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a", &tokens[0..1]),
            arguments: vec![*int_lit(1, &tokens[2..3])],
            info: AstInfo::new(&tokens[..5]),
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
    let broker = LocalBroker::default();
    let tokens = lex(stmt, broker);
    let (input, is) =
        all_consuming(terminated(IfStatement::parse, eof))(tokens.to_tokens()).unwrap();
    assert_eq!(
        is,
        IfStatement {
            condition: Some(Expression::Binary(BinaryExpression {
                operator: Operator::Equ,
                lhs: int_lit(1, &tokens[2..3]),
                rhs: int_lit(2, &tokens[4..5]),
                info: AstInfo::new(&tokens[2..5]),
            })),
            if_branch: Some(Box::new(Statement::Block(BlockStatement {
                statements: Vec::new(),
                info: AstInfo::new(&tokens[6..8]),
            }))),
            else_branch: None,
            info: AstInfo::new(&tokens[..8]),
        },
        "IfStatement: {}",
        stmt
    );
    assert!(input.broker.errors().is_empty(), "IfStatement: {}", stmt);
}

#[test]
fn acker() {
    let acker = std::fs::read_to_string("/Users/alex/dev/compiler/programs/acker.spl").unwrap();
    let broker = LocalBroker::default();
    let tokens = lex(&acker, broker);
    let (input, program) = all_consuming(Program::parse)(tokens.to_tokens()).unwrap();

    // variables for use in assertion
    let int_type = |tokens| Some(TypeExpression::NamedType(Identifier::new("int", tokens)));
    let a = |tokens| Some(Identifier::new("a", tokens));
    let i = |tokens| Some(Identifier::new("i", tokens));
    let j = |tokens| Some(Identifier::new("j", tokens));
    let k = |tokens| Some(Identifier::new("k", tokens));
    let var_i = |tokens| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("i", tokens),
        )))
    };
    let var_a = |tokens| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("a", tokens),
        )))
    };
    let var_j = |tokens| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("j", tokens),
        )))
    };
    let var_k = |tokens| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("k", tokens),
        )))
    };
    fn call_ackermann(
        ident_tokens: &[Token],
        tokens: &[Token],
        arg0: Expression,
        arg1: Expression,
        arg2: Expression,
    ) -> Statement {
        Statement::Call(CallStatement {
            name: Identifier::new("ackermann", ident_tokens),
            arguments: vec![arg0, arg1, arg2],
            info: AstInfo::new(tokens),
        })
    }

    assert_eq!(
        program,
        Program {
            global_declarations: vec![
                GlobalDeclaration::Procedure(ProcedureDeclaration {
                    name: Some(Identifier::new("ackermann", &tokens[4..5])),
                    parameters: vec![
                        ParameterDeclaration {
                            is_ref: false,
                            name: i(&tokens[6..7]),
                            type_expr: int_type(&tokens[8..9]),
                            info: AstInfo::new(&tokens[6..9]),
                        },
                        ParameterDeclaration {
                            is_ref: false,
                            name: j(&tokens[10..11]),
                            type_expr: int_type(&tokens[12..13]),
                            info: AstInfo::new(&tokens[10..13]),
                        },
                        ParameterDeclaration {
                            is_ref: true,
                            name: k(&tokens[15..16]),
                            type_expr: int_type(&tokens[17..18]),
                            info: AstInfo::new(&tokens[14..18]),
                        },
                    ],
                    variable_declarations: vec![VariableDeclaration {
                        name: a(&tokens[21..22]),
                        type_expr: int_type(&tokens[23..24]),
                        info: AstInfo::new(&tokens[20..25]),
                    }],
                    statements: vec![Statement::If(IfStatement {
                        condition: Some(Expression::Binary(BinaryExpression {
                            operator: Operator::Equ,
                            lhs: var_i(&tokens[27..28]),
                            rhs: int_lit(0, &tokens[29..30]),
                            info: AstInfo::new(&tokens[27..30]),
                        })),
                        if_branch: Some(Box::new(Statement::Block(BlockStatement {
                            statements: vec![Statement::Assignment(Assignment {
                                variable: Variable::NamedVariable(Identifier::new(
                                    "k",
                                    &tokens[32..33]
                                )),
                                expr: Some(Expression::Binary(BinaryExpression {
                                    operator: Operator::Add,
                                    lhs: var_j(&tokens[34..35]),
                                    rhs: int_lit(1, &tokens[36..37]),
                                    info: AstInfo::new(&tokens[34..37]),
                                })),
                                info: AstInfo::new(&tokens[32..38]),
                            })],
                            info: AstInfo::new(&tokens[31..39]),
                        }))),
                        else_branch: Some(Box::new(Statement::Block(BlockStatement {
                            statements: vec![Statement::If(IfStatement {
                                condition: Some(Expression::Binary(BinaryExpression {
                                    operator: Operator::Equ,
                                    lhs: var_j(&tokens[43..44]),
                                    rhs: int_lit(0, &tokens[45..46]),
                                    info: AstInfo::new(&tokens[43..46]),
                                })),
                                if_branch: Some(Box::new(Statement::Block(BlockStatement {
                                    statements: vec![call_ackermann(
                                        &tokens[48..49],
                                        &tokens[48..59],
                                        Expression::Binary(BinaryExpression {
                                            operator: Operator::Sub,
                                            lhs: var_i(&tokens[50..51]),
                                            rhs: int_lit(1, &tokens[52..53]),
                                            info: AstInfo::new(&tokens[50..53]),
                                        }),
                                        Expression::IntLiteral(IntLiteral::new(
                                            1,
                                            AstInfo::new(&tokens[54..55])
                                        )),
                                        *var_k(&tokens[56..57])
                                    )],
                                    info: AstInfo::new(&tokens[47..60]),
                                }))),
                                else_branch: Some(Box::new(Statement::Block(BlockStatement {
                                    statements: vec![
                                        call_ackermann(
                                            &tokens[62..63],
                                            &tokens[62..73],
                                            *var_i(&tokens[64..65]),
                                            Expression::Binary(BinaryExpression {
                                                operator: Operator::Sub,
                                                lhs: var_j(&tokens[66..67]),
                                                rhs: int_lit(1, &tokens[68..69]),
                                                info: AstInfo::new(&tokens[66..69]),
                                            }),
                                            *var_a(&tokens[70..71])
                                        ),
                                        call_ackermann(
                                            &tokens[73..74],
                                            &tokens[73..84],
                                            Expression::Binary(BinaryExpression {
                                                operator: Operator::Sub,
                                                lhs: var_i(&tokens[75..76]),
                                                rhs: int_lit(1, &tokens[77..78]),
                                                info: AstInfo::new(&tokens[75..78]),
                                            }),
                                            *var_a(&tokens[79..80]),
                                            *var_k(&tokens[81..82])
                                        )
                                    ],
                                    info: AstInfo::new(&tokens[61..85]),
                                }))),

                                info: AstInfo::new(&tokens[41..85]),
                            })],
                            info: AstInfo::new(&tokens[40..86]),
                        }))),
                        info: AstInfo::new(&tokens[25..86]),
                    })],
                    info: AstInfo::new(&tokens[0..87]),
                }),
                GlobalDeclaration::Procedure(ProcedureDeclaration {
                    name: Some(Identifier::new("main", &tokens[88..89])),
                    parameters: Vec::new(),
                    variable_declarations: vec![
                        VariableDeclaration {
                            name: i(&tokens[93..94]),
                            type_expr: int_type(&tokens[95..96]),
                            info: AstInfo::new(&tokens[92..97]),
                        },
                        VariableDeclaration {
                            name: j(&tokens[98..99]),
                            type_expr: int_type(&tokens[100..101]),
                            info: AstInfo::new(&tokens[97..102]),
                        },
                        VariableDeclaration {
                            name: k(&tokens[103..104]),
                            type_expr: int_type(&tokens[105..106]),
                            info: AstInfo::new(&tokens[102..107]),
                        }
                    ],
                    statements: vec![
                        Statement::Assignment(Assignment {
                            variable: Variable::NamedVariable(Identifier::new(
                                "i",
                                &tokens[107..108]
                            )),
                            expr: Some(*int_lit(0, &tokens[109..110]).clone()),
                            info: AstInfo::new(&tokens[107..111]),
                        }),
                        Statement::While(WhileStatement {
                            condition: Some(Expression::Binary(BinaryExpression {
                                operator: Operator::Lse,
                                lhs: var_i(&tokens[113..114]),
                                rhs: int_lit(3, &tokens[115..116]),
                                info: AstInfo::new(&tokens[113..116]),
                            })),
                            statement: Some(Box::new(Statement::Block(BlockStatement {
                                statements: vec![
                                    Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new(
                                            "j",
                                            &tokens[118..119],
                                        )),
                                        expr: Some(*int_lit(0, &tokens[120..121])),
                                        info: AstInfo::new(&tokens[118..122]),
                                    }),
                                    Statement::While(WhileStatement {
                                        condition: Some(Expression::Binary(BinaryExpression {
                                            operator: Operator::Lse,
                                            lhs: var_j(&tokens[124..125]),
                                            rhs: int_lit(6, &tokens[126..127]),
                                            info: AstInfo::new(&tokens[124..127]),
                                        })),
                                        statement: Some(Box::new(Statement::Block(
                                            BlockStatement {
                                                statements: vec![
                                                    call_ackermann(
                                                        &tokens[129..130],
                                                        &tokens[129..138],
                                                        *var_i(&tokens[131..132]),
                                                        *var_j(&tokens[133..134]),
                                                        *var_k(&tokens[135..136])
                                                    ),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printi",
                                                            &tokens[138..139]
                                                        ),
                                                        arguments: vec![*var_i(&tokens[140..141])],
                                                        info: AstInfo::new(&tokens[138..143]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc",
                                                            &tokens[143..144]
                                                        ),
                                                        arguments: vec![*int_lit(
                                                            32,
                                                            &tokens[145..146]
                                                        )],
                                                        info: AstInfo::new(&tokens[143..148]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printi",
                                                            &tokens[148..149]
                                                        ),
                                                        arguments: vec![*var_j(&tokens[150..151])],
                                                        info: AstInfo::new(&tokens[148..153]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc",
                                                            &tokens[153..154]
                                                        ),
                                                        arguments: vec![*int_lit(
                                                            32,
                                                            &tokens[155..156]
                                                        )],
                                                        info: AstInfo::new(&tokens[153..158]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printi",
                                                            &tokens[158..159]
                                                        ),
                                                        arguments: vec![*var_k(&tokens[160..161])],
                                                        info: AstInfo::new(&tokens[158..163]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc",
                                                            &tokens[163..164]
                                                        ),
                                                        arguments: vec![*int_lit(
                                                            10,
                                                            &tokens[165..166]
                                                        )],
                                                        info: AstInfo::new(&tokens[163..168]),
                                                    }),
                                                    Statement::Assignment(Assignment {
                                                        variable: Variable::NamedVariable(
                                                            Identifier::new("j", &tokens[168..169])
                                                        ),
                                                        expr: Some(Expression::Binary(
                                                            BinaryExpression {
                                                                operator: Operator::Add,
                                                                lhs: var_j(&tokens[170..171]),
                                                                rhs: int_lit(1, &tokens[172..173]),
                                                                info: AstInfo::new(
                                                                    &tokens[170..173]
                                                                ),
                                                            }
                                                        )),
                                                        info: AstInfo::new(&tokens[168..174]),
                                                    })
                                                ],
                                                info: AstInfo::new(&tokens[128..175]),
                                            }
                                        ))),
                                        info: AstInfo::new(&tokens[122..175]),
                                    }),
                                    Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new(
                                            "i",
                                            &tokens[175..176],
                                        )),
                                        expr: Some(Expression::Binary(BinaryExpression {
                                            operator: Operator::Add,
                                            lhs: var_i(&tokens[177..178]),
                                            rhs: int_lit(1, &tokens[179..180]),
                                            info: AstInfo::new(&tokens[177..180]),
                                        })),
                                        info: AstInfo::new(&tokens[175..181]),
                                    })
                                ],
                                info: AstInfo::new(&tokens[117..182]),
                            }))),
                            info: AstInfo::new(&tokens[111..182]),
                        })
                    ],
                    info: AstInfo::new(&tokens[87..183]),
                }),
            ],
        },
        // "Acker: {}",
        // acker
    );
    assert!(input.broker.errors().is_empty(), "Acker: {}", acker);
}
