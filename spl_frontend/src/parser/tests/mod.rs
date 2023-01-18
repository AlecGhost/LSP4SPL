use super::*;
use nom::combinator::all_consuming;

fn int_lit(value: u32) -> Box<Expression> {
    Box::new(Expression::IntLiteral(IntLiteral::new(value)))
}

#[test]
fn expressions() {
    type E = Expression;

    let expr = "1";
    assert_eq!(
        all_consuming(E::parse)(expr.to_span()).unwrap().1,
        E::IntLiteral(IntLiteral::new(1)),
        "Expression: {}",
        expr
    );
    let expr = "1 + 2";
    assert_eq!(
        all_consuming(E::parse)(expr.to_span()).unwrap().1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: int_lit(1),
            rhs: int_lit(2),
        }),
        "Expression: {}",
        expr
    );
    let expr = "1 + 2 * 3";
    assert_eq!(
        all_consuming(E::parse)(expr.to_span()).unwrap().1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: int_lit(1),
            rhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Mul,
                lhs: int_lit(2),
                rhs: int_lit(3),
            })),
        }),
        "Expression: {}",
        expr
    );
    let expr = "1 / 2 + 3";
    assert_eq!(
        all_consuming(E::parse)(expr.to_span()).unwrap().1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Div,
                lhs: int_lit(1),
                rhs: int_lit(2)
            })),
            rhs: int_lit(3),
        }),
        "Expression: {}",
        expr
    );
    let expr = "1 * 2 / 3 * 4";
    assert_eq!(
        all_consuming(E::parse)(expr.to_span()).unwrap().1,
        E::Binary(BinaryExpression {
            operator: Operator::Mul,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Div,
                lhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Mul,
                    lhs: int_lit(1),
                    rhs: int_lit(2),
                })),
                rhs: int_lit(3),
            })),
            rhs: int_lit(4),
        }),
        "Expression: {}",
        expr
    );
    let expr = "1 - 2 + 3 - 4";
    assert_eq!(
        all_consuming(E::parse)(expr.to_span()).unwrap().1,
        E::Binary(BinaryExpression {
            operator: Operator::Sub,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Add,
                lhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Sub,
                    lhs: int_lit(1),
                    rhs: int_lit(2),
                })),
                rhs: int_lit(3),
            })),
            rhs: int_lit(4),
        }),
        "Expression: {}",
        expr
    );
    let expr = "(1 + 2) * 3 = 4 + 5 * 6 / 6";
    assert_eq!(
        all_consuming(E::parse)(expr.to_span()).unwrap().1,
        E::Binary(BinaryExpression {
            operator: Operator::Equ,
            lhs: Box::new(Expression::Binary(BinaryExpression {
                operator: Operator::Mul,
                lhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Add,
                    lhs: int_lit(1),
                    rhs: int_lit(2)
                })),
                rhs: int_lit(3),
            })),
            rhs: Box::new(Expression::Binary(BinaryExpression {
                operator: Operator::Add,
                lhs: int_lit(4),
                rhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Div,
                    lhs: Box::new(E::Binary(BinaryExpression {
                        operator: Operator::Mul,
                        lhs: int_lit(5),
                        rhs: int_lit(6)
                    })),
                    rhs: int_lit(6),
                }))
            }))
        }),
        "Expression: {}",
        expr
    );
    let expr = "a < b > c";
    assert!(
        all_consuming(E::parse)(expr.to_span()).is_err(),
        "Expression: {}",
        expr
    );
}

#[test]
fn type_declarations() {
    type TD = TypeDeclaration;
    type TE = TypeExpression;

    let dec = "type a = int;";
    assert_eq!(
        all_consuming(TD::parse)(dec.to_span()).unwrap().1,
        TD {
            name: Some(Identifier {
                value: "a".to_string()
            }),
            type_expr: Some(TE::Type(Identifier {
                value: "int".to_string()
            }))
        },
        "Declaration: {}",
        dec
    );

    let dec = "type a = array [2] of array [3] of int;";
    assert_eq!(
        all_consuming(TD::parse)(dec.to_span()).unwrap().1,
        TD {
            name: Some(Identifier {
                value: "a".to_string()
            }),
            type_expr: Some(TE::ArrayType(
                Some(2),
                Some(Box::new(TE::ArrayType(
                    Some(3),
                    Some(Box::new(TE::Type(Identifier {
                        value: "int".to_string()
                    })))
                )))
            ))
        },
        "Declaration: {}",
        dec
    );

    let dec = "type = array [] of array [] of;";
    let (input, td) = all_consuming(TD::parse)(dec.to_span()).unwrap();
    assert_eq!(
        td,
        TD {
            name: None,
            type_expr: Some(TE::ArrayType(
                None,
                Some(Box::new(TE::ArrayType(None, None)))
            ))
        },
        "Declaration: {}",
        dec
    );
    let vec: &[ParseError] = &input.extra.borrow();
    assert_eq!(
        vec,
        vec![
            ParseError(5..5, ErrorMessage::ExpectedToken("identifier".to_string())),
            ParseError(14..14, ErrorMessage::ExpectedToken("integer".to_string())),
            ParseError(26..26, ErrorMessage::ExpectedToken("integer".to_string())),
            ParseError(
                30..30,
                ErrorMessage::ExpectedToken("type expression".to_string())
            ),
        ],
        "Declaration: {}",
        dec
    );
}

#[test]
fn assignments() {
    let asgn = "a := 1;";
    let (input, assignment) = all_consuming(Assignment::parse)(asgn.to_span()).unwrap();
    assert_eq!(
        assignment,
        Assignment {
            variable: Variable::NamedVariable(Identifier::new("a")),
            expr: Some(Expression::IntLiteral(IntLiteral::new(1)))
        },
        "Assignment: {}",
        asgn
    );
    let vec: &[ParseError] = &input.extra.borrow();
    assert!(vec.is_empty(), "Assignment: {}", asgn);

    let asgn = "a = 1;";
    assert!(
        Assignment::parse(asgn.to_span()).is_err(),
        "Assignment: {}",
        asgn
    );
}

#[test]
fn call_statements() {
    let stmt = "a();";
    let (input, cs) = all_consuming(CallStatement::parse)(stmt.to_span()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a"),
            arguments: Vec::new(),
        },
        "CallStatement: {}",
        stmt
    );
    let vec: &[ParseError] = &input.extra.borrow();
    assert!(vec.is_empty(), "CallStatement: {}", stmt);

    let stmt = "a(1, 2, 3);";
    let (input, cs) = all_consuming(CallStatement::parse)(stmt.to_span()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a"),
            arguments: vec![
                Expression::IntLiteral(IntLiteral::new(1)),
                Expression::IntLiteral(IntLiteral::new(2)),
                Expression::IntLiteral(IntLiteral::new(3)),
            ],
        },
        "CallStatement: {}",
        stmt
    );
    let vec: &[ParseError] = &input.extra.borrow();
    assert!(vec.is_empty(), "CallStatement: {}", stmt);

    let stmt = "a(1,)";
    let (input, cs) = all_consuming(CallStatement::parse)(stmt.to_span()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a"),
            arguments: vec![Expression::IntLiteral(IntLiteral::new(1)),],
        },
        "CallStatement: {}",
        stmt
    );
    let vec: &[ParseError] = &input.extra.borrow();
    assert_eq!(
        vec,
        vec![
            ParseError(4..4, ErrorMessage::ExpectedToken("expression".to_string())),
            ParseError(5..5, ErrorMessage::MissingTrailingSemic),
        ],
        "CallStatement: {}",
        stmt
    );
}

#[test]
fn if_statements() {
    let stmt = "if (1 = 2) {}";
    let (input, is) = all_consuming(IfStatement::parse)(stmt.to_span()).unwrap();
    assert_eq!(
        is,
        IfStatement {
            condition: Some(Expression::Binary(BinaryExpression {
                operator: Operator::Equ,
                lhs: int_lit(1),
                rhs: int_lit(2)
            })),
            if_branch: Some(Box::new(Statement::Block(BlockStatement {
                statements: Vec::new()
            }))),
            else_branch: None
        },
        "IfStatement: {}",
        stmt
    );
    let vec: &[ParseError] = &input.extra.borrow();
    assert!(vec.is_empty(), "IfStatement: {}", stmt);
}

#[test]
fn acker() {
    let acker = std::fs::read_to_string("/Users/alex/dev/compiler/programs/acker.spl").unwrap();
    let (input, program) = all_consuming(Program::parse)(acker.to_span()).unwrap();
    let int_type = Some(TypeExpression::Type(Identifier::new("int")));
    let a = Some(Identifier::new("a"));
    let i = Some(Identifier::new("i"));
    let j = Some(Identifier::new("j"));
    let k = Some(Identifier::new("k"));
    let var_i = Box::new(Expression::Variable(Variable::NamedVariable(
        Identifier::new("i"),
    )));
    let var_a = Box::new(Expression::Variable(Variable::NamedVariable(
        Identifier::new("a"),
    )));
    let var_j = Box::new(Expression::Variable(Variable::NamedVariable(
        Identifier::new("j"),
    )));
    let var_k = Box::new(Expression::Variable(Variable::NamedVariable(
        Identifier::new("k"),
    )));
    fn call_ackermann(arg0: Expression, arg1: Expression, arg2: Expression) -> Statement {
        Statement::Call(CallStatement {
            name: Identifier::new("ackermann"),
            arguments: vec![arg0, arg1, arg2],
        })
    }
    assert_eq!(
        program,
        Program {
            type_declarations: Vec::new(),
            procedure_declarations: vec![
                ProcedureDeclaration {
                    name: Some(Identifier::new("ackermann")),
                    parameters: vec![
                        ParameterDeclaration {
                            is_ref: false,
                            name: i.clone(),
                            type_expr: int_type.clone()
                        },
                        ParameterDeclaration {
                            is_ref: false,
                            name: j.clone(),
                            type_expr: int_type.clone()
                        },
                        ParameterDeclaration {
                            is_ref: true,
                            name: k.clone(),
                            type_expr: int_type.clone()
                        },
                    ],
                    variable_declarations: vec![VariableDeclaration {
                        name: a.clone(),
                        type_expr: int_type.clone(),
                    }],
                    statements: vec![Statement::If(IfStatement {
                        condition: Some(Expression::Binary(BinaryExpression {
                            operator: Operator::Equ,
                            lhs: var_i.clone(),
                            rhs: int_lit(0)
                        })),
                        if_branch: Some(Box::new(Statement::Block(BlockStatement {
                            statements: vec![Statement::Assignment(Assignment {
                                variable: Variable::NamedVariable(Identifier::new("k")),
                                expr: Some(Expression::Binary(BinaryExpression {
                                    operator: Operator::Add,
                                    lhs: var_j.clone(),
                                    rhs: int_lit(1)
                                }))
                            })]
                        }))),
                        else_branch: Some(Box::new(Statement::Block(BlockStatement {
                            statements: vec![Statement::If(IfStatement {
                                condition: Some(Expression::Binary(BinaryExpression {
                                    operator: Operator::Equ,
                                    lhs: var_j.clone(),
                                    rhs: int_lit(0)
                                })),
                                if_branch: Some(Box::new(Statement::Block(BlockStatement {
                                    statements: vec![call_ackermann(
                                        Expression::Binary(BinaryExpression {
                                            operator: Operator::Sub,
                                            lhs: var_i.clone(),
                                            rhs: int_lit(1)
                                        }),
                                        Expression::IntLiteral(IntLiteral::new(1)),
                                        *var_k.clone()
                                    )]
                                }))),
                                else_branch: Some(Box::new(Statement::Block(BlockStatement {
                                    statements: vec![
                                        call_ackermann(
                                            *var_i.clone(),
                                            Expression::Binary(BinaryExpression {
                                                operator: Operator::Sub,
                                                lhs: var_j.clone(),
                                                rhs: int_lit(1)
                                            }),
                                            *var_a.clone()
                                        ),
                                        call_ackermann(
                                            Expression::Binary(BinaryExpression {
                                                operator: Operator::Sub,
                                                lhs: var_i.clone(),
                                                rhs: int_lit(1)
                                            }),
                                            *var_a.clone(),
                                            *var_k.clone()
                                        )
                                    ]
                                })))
                            })]
                        })))
                    })],
                },
                ProcedureDeclaration {
                    name: Some(Identifier::new("main")),
                    parameters: Vec::new(),
                    variable_declarations: vec![
                        VariableDeclaration {
                            name: i.clone(),
                            type_expr: int_type.clone(),
                        },
                        VariableDeclaration {
                            name: j.clone(),
                            type_expr: int_type.clone(),
                        },
                        VariableDeclaration {
                            name: k.clone(),
                            type_expr: int_type.clone(),
                        }
                    ],
                    statements: vec![
                        Statement::Assignment(Assignment {
                            variable: Variable::NamedVariable(Identifier::new("i")),
                            expr: Some(*int_lit(0).clone())
                        }),
                        Statement::While(WhileStatement {
                            condition: Some(Expression::Binary(BinaryExpression {
                                operator: Operator::Lse,
                                lhs: var_i.clone(),
                                rhs: int_lit(3)
                            })),
                            statement: Some(Box::new(Statement::Block(BlockStatement {
                                statements: vec![
                                    Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new("j")),
                                        expr: Some(*int_lit(0))
                                    }),
                                    Statement::While(WhileStatement {
                                        condition: Some(Expression::Binary(BinaryExpression {
                                            operator: Operator::Lse,
                                            lhs: var_j.clone(),
                                            rhs: int_lit(6)
                                        })),
                                        statement: Some(Box::new(Statement::Block(
                                            BlockStatement {
                                                statements: vec![
                                                    call_ackermann(
                                                        *var_i.clone(),
                                                        *var_j.clone(),
                                                        *var_k.clone()
                                                    ),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printi"),
                                                        arguments: vec![*var_i.clone()]
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printc"),
                                                        arguments: vec![*int_lit(32)]
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printi"),
                                                        arguments: vec![*var_j.clone()]
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printc"),
                                                        arguments: vec![*int_lit(32)]
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printi"),
                                                        arguments: vec![*var_k.clone()]
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printc"),
                                                        arguments: vec![*int_lit(10)]
                                                    }),
                                                    Statement::Assignment(Assignment {
                                                        variable: Variable::NamedVariable(
                                                            Identifier::new("j")
                                                        ),
                                                        expr: Some(Expression::Binary(
                                                            BinaryExpression {
                                                                operator: Operator::Add,
                                                                lhs: var_j.clone(),
                                                                rhs: int_lit(1)
                                                            }
                                                        ))
                                                    })
                                                ]
                                            }
                                        )))
                                    }),
                                    Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new("i")),
                                        expr: Some(Expression::Binary(BinaryExpression {
                                            operator: Operator::Add,
                                            lhs: var_i.clone(),
                                            rhs: int_lit(1)
                                        }))
                                    })
                                ]
                            })))
                        })
                    ]
                },
            ],
        },
        "Acker: {}",
        acker
    );
    let vec: &[ParseError] = &input.extra.borrow();
    assert!(vec.is_empty(), "Acker: {}", acker);
}
