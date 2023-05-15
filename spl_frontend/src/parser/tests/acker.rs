use super::*;

#[test]
fn acker() {
    let acker = std::fs::read_to_string("tests/parser/acker.spl").unwrap();
    let broker = LocalBroker::default();
    let tokens = lex(&acker, broker);
    let (input, program) = all_consuming(Program::parse)(tokens.to_tokens()).unwrap();

    // variables for use in assertion
    let int_type = |tokens, offset| {
        Some(Reference {
            reference: TypeExpression::NamedType(Identifier::new("int".to_string(), tokens)),
            offset,
        })
    };
    let a = |tokens| Some(Identifier::new("a".to_string(), tokens));
    let i = |tokens| Some(Identifier::new("i".to_string(), tokens));
    let j = |tokens| Some(Identifier::new("j".to_string(), tokens));
    let k = |tokens| Some(Identifier::new("k".to_string(), tokens));
    let var_i = |tokens| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("i".to_string(), tokens),
        )))
    };
    let var_a = |tokens| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("a".to_string(), tokens),
        )))
    };
    let var_j = |tokens| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("j".to_string(), tokens),
        )))
    };
    let var_k = |tokens| {
        Box::new(Expression::Variable(Variable::NamedVariable(
            Identifier::new("k".to_string(), tokens),
        )))
    };
    fn call_ackermann(
        ident_range: Range<usize>,
        range: Range<usize>,
        offset: usize,
        arg0: Reference<Expression>,
        arg1: Reference<Expression>,
        arg2: Reference<Expression>,
    ) -> Reference<Statement> {
        Reference {
            reference: Statement::Call(CallStatement {
                name: Identifier::new("ackermann".to_string(), ident_range),
                arguments: vec![arg0, arg1, arg2],
                info: AstInfo::new(range),
            }),
            offset,
        }
    }

    let correct_program = Program {
            global_declarations: vec![
                    Reference::new(GlobalDeclaration::Procedure(ProcedureDeclaration {
                        doc: vec![
                            "".to_string(),
                            " acker.spl -- Ackermann's function".to_string(),
                            "".to_string()
                        ],
                        name: Some(Identifier::new("ackermann".to_string(), 4..5)),
                        parameters: vec![
                                Reference::new(ParameterDeclaration::Valid {
                                    doc: Vec::new(),
                                    is_ref: false,
                                    name: i(0..1),
                                    type_expr: int_type(0..1, 2),
                                    info: AstInfo::new(0..3),
                                }, 6),
                                Reference::new(ParameterDeclaration::Valid {
                                    doc: Vec::new(),
                                    is_ref: false,
                                    name: j(0..1),
                                    type_expr: int_type(0..1, 2),
                                    info: AstInfo::new(0..3),
                                }, 10),
                                Reference::new(ParameterDeclaration::Valid {
                                    doc: Vec::new(),
                                    is_ref: true,
                                    name: k(1..2),
                                    type_expr: int_type(0..1, 3),
                                    info: AstInfo::new(0..4),
                                }, 14),
                        ],
                        variable_declarations: vec![
                            Reference::new(VariableDeclaration::Valid {
                                doc: Vec::new(),
                                name: a(1..2),
                                type_expr: int_type(0..1, 3),
                                info: AstInfo::new(0..5),
                            }, 20),
                        ],
                        statements: vec![
                            Reference::new(Statement::If(IfStatement {
                                condition: Some(
                                    Reference::new(Expression::Binary(BinaryExpression {
                                        operator: Operator::Equ,
                                        lhs: var_i(0..1),
                                        rhs: int_lit(0, 2..3),
                                        info: AstInfo::new(0..3),
                                    }), 2),
                                ),
                                if_branch: Some(Box::new(
                                    Reference::new(Statement::Block(BlockStatement {
                                        statements: vec![
                                            Reference::new(Statement::Assignment(Assignment {
                                                variable: Variable::NamedVariable(Identifier::new(
                                                    "k".to_string(),
                                                    0..1
                                                )),
                                                expr: Some(
                                                    Reference::new(Expression::Binary(
                                                        BinaryExpression {
                                                            operator: Operator::Add,
                                                            lhs: var_j(0..1),
                                                            rhs: int_lit(1, 2..3),
                                                            info: AstInfo::new(0..3),
                                                        }
                                                    ), 2),
                                                ),
                                                info: AstInfo::new(0..6),
                                            }), 1),
                                        ],
                                        info: AstInfo::new(0..8),
                                    }), 6),
                                )
                                ),
                                else_branch: Some(Box::new(
                                    Reference::new(Statement::Block(BlockStatement {
                                        statements: vec![
                                            Reference::new(Statement::If(IfStatement {
                                                condition: Some(
                                                    Reference::new(Expression::Binary(
                                                        BinaryExpression {
                                                            operator: Operator::Equ,
                                                            lhs: var_j(0..1),
                                                            rhs: int_lit(0, 2..3),
                                                            info: AstInfo::new(0..3),
                                                        }
                                                    ), 2)
                                                ),
                                                if_branch: Some(Box::new(
                                                    Reference::new(Statement::Block(BlockStatement {
                                                        statements: vec![call_ackermann(
                                                            0..1,
                                                            0..11,
                                                            1,
                                                            Reference::new(Expression::Binary(
                                                                    BinaryExpression {
                                                                        operator: Operator::Sub,
                                                                        lhs: var_i(0..1),
                                                                        rhs: int_lit(1, 2..3),
                                                                        info: AstInfo::new(0..3),
                                                                    }
                                                                ), 2),
                                                            Reference::new(Expression::IntLiteral(
                                                                    IntLiteral::new(
                                                                        1,
                                                                        AstInfo::new(0..1)
                                                                    )
                                                                ), 6),
                                                            Reference {
                                                                reference: *var_k(0..1),
                                                                offset: 8,
                                                            }
                                                        )],
                                                        info: AstInfo::new(0..13),
                                                    }), 6)
                                                )),
                                                else_branch: Some(Box::new(
                                                    Reference::new(Statement::Block(BlockStatement {
                                                        statements: vec![
                                                            call_ackermann(
                                                                0..1,
                                                                0..11,
                                                                1,
                                                                Reference {
                                                                    reference: *var_i(0..1),
                                                                    offset: 2
                                                                },
                                                                    Reference::new(Expression::Binary(
                                                                        BinaryExpression {
                                                                            operator: Operator::Sub,
                                                                            lhs: var_j(0..1),
                                                                            rhs: int_lit(1, 2..3),
                                                                            info: AstInfo::new(
                                                                                0..3
                                                                            ),
                                                                        }
                                                                    ), 4),
                                                                Reference {
                                                                    reference: *var_a(0..1),
                                                                    offset: 8,
                                                                }
                                                            ),
                                                            call_ackermann(
                                                                0..1,
                                                                0..11,
                                                                12,
                                                                Reference::new(Expression::Binary(
                                                                        BinaryExpression {
                                                                            operator: Operator::Sub,
                                                                            lhs: var_i(0..1),
                                                                            rhs: int_lit(1, 2..3),
                                                                            info: AstInfo::new(
                                                                                0..3
                                                                            ),
                                                                        }
                                                                    ), 2),
                                                                Reference::new(*var_a(0..1), 6),
                                                                Reference::new(*var_k(0..1), 8)                                                             ,)
                                                        ],
                                                        info: AstInfo::new(0..24),
                                                    }), 20)
                                                ),),

                                                info: AstInfo::new(0..44),
                                            }), 1),
                                        ],
                                        info: AstInfo::new(0..46),
                                    }), 15),
                                )),
                                info: AstInfo::new(0..61),
                            }), 25),
                        ],
                        info: AstInfo::new(0..87),
                    }), 0),
                Reference::new(GlobalDeclaration::Procedure(ProcedureDeclaration {
                    doc: Vec::new(),
                    name: Some(Identifier::new("main".to_string(), 1..2)),
                    parameters: Vec::new(),
                    variable_declarations: vec![
                        Reference::new(VariableDeclaration::Valid {
                            doc: Vec::new(),
                            name: i(1..2),
                            type_expr: int_type(0..1, 3),
                            info: AstInfo::new(0..5),
                        }, 5),
                        Reference::new(VariableDeclaration::Valid {
                            doc: Vec::new(),
                            name: j(1..2),
                            type_expr: int_type(0..1, 3),
                            info: AstInfo::new(0..5),
                        }, 10),
                        Reference::new(VariableDeclaration::Valid {
                            doc: Vec::new(),
                            name: k(1..2),
                            type_expr: int_type(0..1, 3),
                            info: AstInfo::new(0..5),
                        }, 15)
                    ],
                    statements: vec![
                        Reference::new(Statement::Assignment(Assignment {
                            variable: Variable::NamedVariable(Identifier::new(
                                "i".to_string(),
                                0..1
                            )),
                            expr: Some(Reference::new(*int_lit(0, 0..1), 2)),
                            info: AstInfo::new(0..4),
                        }), 20),
                        Reference::new(Statement::While(WhileStatement {
                            condition: Some(Reference::new(Expression::Binary(BinaryExpression {
                                operator: Operator::Lse,
                                lhs: var_i(0..1),
                                rhs: int_lit(3, 2..3),
                                info: AstInfo::new(0..3),
                            }), 2)),
                            statement: Some(Box::new(Reference::new(Statement::Block(BlockStatement {
                                statements: vec![
                                    Reference::new(Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new(
                                            "j".to_string(),
                                            0..1,
                                        )),
                                        expr: Some(Reference::new(*int_lit(0, 0..1), 2)),
                                        info: AstInfo::new(0..4),
                                    }), 1),
                                    Reference::new(Statement::While(WhileStatement {
                                        condition: Some(Reference::new(Expression::Binary(BinaryExpression {
                                            operator: Operator::Lse,
                                            lhs: var_j(0..1),
                                            rhs: int_lit(6, 2..3),
                                            info: AstInfo::new(0..3),
                                        }), 2)),
                                        statement: Some(Box::new(Reference::new(Statement::Block(
                                            BlockStatement {
                                                statements: vec![
                                                    call_ackermann(
                                                        0..1,
                                                        0..9,
                                                        1,
                                                        Reference::new(*var_i(0..1), 2),
                                                        Reference::new(*var_j(0..1), 4),
                                                        Reference::new(*var_k(0..1), 6),
                                                    ),
                                                    Reference::new(Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printi".to_string(),
                                                            0..1
                                                        ),
                                                        arguments: vec![Reference::new(*var_i(0..1), 2)],
                                                        info: AstInfo::new(0..5),
                                                    }), 10),
                                                    Reference::new(Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc".to_string(),
                                                            0..1
                                                        ),
                                                        arguments: vec![Reference::new(*int_lit(32, 0..1), 2)],
                                                        info: AstInfo::new(0..5),
                                                    }), 15),
                                                    Reference::new(Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printi".to_string(),
                                                            0..1
                                                        ),
                                                        arguments: vec![Reference::new(*var_j(0..1), 2)],
                                                        info: AstInfo::new(0..5),
                                                    }), 20),
                                                    Reference::new(Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc".to_string(),
                                                            0..1
                                                        ),
                                                        arguments: vec![Reference::new(*int_lit(32, 0..1), 2)],
                                                        info: AstInfo::new(0..5),
                                                    }), 25),
                                                    Reference::new(Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printi".to_string(),
                                                            0..1
                                                        ),
                                                        arguments: vec![Reference::new(*var_k(0..1), 2)],
                                                        info: AstInfo::new(0..5),
                                                    }), 30),
                                                    Reference::new(Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc".to_string(),
                                                            0..1
                                                        ),
                                                        arguments: vec![Reference::new(*int_lit(10, 0..1), 2)],
                                                        info: AstInfo::new(0..5),
                                                    }), 35),
                                                    Reference::new(Statement::Assignment(Assignment {
                                                        variable: Variable::NamedVariable(
                                                            Identifier::new(
                                                                "j".to_string(),
                                                                0..1
                                                            )
                                                        ),
                                                        expr: Some(Reference::new(Expression::Binary(
                                                            BinaryExpression {
                                                                operator: Operator::Add,
                                                                lhs: var_j(0..1),
                                                                rhs: int_lit(1, 2..3),
                                                                info: AstInfo::new(0..3),
                                                            }
                                                        ), 2)),
                                                        info: AstInfo::new(0..6),
                                                    }), 40)
                                                ],
                                                info: AstInfo::new(0..47),
                                            }
                                        ), 6))),
                                        info: AstInfo::new(0..53),
                                    }), 5),
                                    Reference::new(Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new(
                                            "i".to_string(),
                                            0..1,
                                        )),
                                        expr: Some(Reference::new(Expression::Binary(BinaryExpression {
                                            operator: Operator::Add,
                                            lhs: var_i(0..1),
                                            rhs: int_lit(1, 2..3),
                                            info: AstInfo::new(0..3),
                                        }), 2)),
                                        info: AstInfo::new(0..6),
                                    }), 58)
                                ],
                                info: AstInfo::new(0..65),
                            }), 6))),
                            info: AstInfo::new(0..71),
                        }), 24)
                    ],
                    info: AstInfo::new(0..96),
                }), 87),
            ],
            info: AstInfo::new(0..183),
        };

    eq!(
        program,
        correct_program,
        // "Acker: {}",
        // acker
    );
    assert!(input.broker.errors().is_empty(), "Acker: {}", acker);
}
