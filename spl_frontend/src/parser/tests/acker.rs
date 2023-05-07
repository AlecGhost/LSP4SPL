use super::*;

#[test]
fn acker() {
    let acker = std::fs::read_to_string("/Users/alex/dev/compiler/programs/acker.spl").unwrap();
    let broker = LocalBroker::default();
    let tokens = lex(&acker, broker);
    let (input, program) = all_consuming(Program::parse)(tokens.to_tokens()).unwrap();

    // variables for use in assertion
    let int_type = |tokens| {
        Some(TypeExpression::NamedType(Identifier::new(
            "int".to_string(),
            tokens,
        )))
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
        ident_tokens: &[Token],
        tokens: &[Token],
        arg0: Expression,
        arg1: Expression,
        arg2: Expression,
    ) -> Statement {
        Statement::Call(CallStatement {
            name: Identifier::new("ackermann".to_string(), ident_tokens),
            arguments: vec![arg0, arg1, arg2],
            info: AstInfo::new(tokens),
        })
    }

    eq!(
        program,
        Program {
            global_declarations: vec![
                GlobalDeclaration::Procedure(ProcedureDeclaration {
                    name: Some(Identifier::new("ackermann".to_string(), &tokens[4..5])),
                    parameters: vec![
                        ParameterDeclaration::Valid {
                            is_ref: false,
                            name: i(&tokens[6..7]),
                            type_expr: int_type(&tokens[8..9]),
                            info: AstInfo::new(&tokens[6..9]),
                        },
                        ParameterDeclaration::Valid {
                            is_ref: false,
                            name: j(&tokens[10..11]),
                            type_expr: int_type(&tokens[12..13]),
                            info: AstInfo::new(&tokens[10..13]),
                        },
                        ParameterDeclaration::Valid {
                            is_ref: true,
                            name: k(&tokens[15..16]),
                            type_expr: int_type(&tokens[17..18]),
                            info: AstInfo::new(&tokens[14..18]),
                        },
                    ],
                    variable_declarations: vec![VariableDeclaration::Valid {
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
                                    "k".to_string(),
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
                    name: Some(Identifier::new("main".to_string(), &tokens[88..89])),
                    parameters: Vec::new(),
                    variable_declarations: vec![
                        VariableDeclaration::Valid {
                            name: i(&tokens[93..94]),
                            type_expr: int_type(&tokens[95..96]),
                            info: AstInfo::new(&tokens[92..97]),
                        },
                        VariableDeclaration::Valid {
                            name: j(&tokens[98..99]),
                            type_expr: int_type(&tokens[100..101]),
                            info: AstInfo::new(&tokens[97..102]),
                        },
                        VariableDeclaration::Valid {
                            name: k(&tokens[103..104]),
                            type_expr: int_type(&tokens[105..106]),
                            info: AstInfo::new(&tokens[102..107]),
                        }
                    ],
                    statements: vec![
                        Statement::Assignment(Assignment {
                            variable: Variable::NamedVariable(Identifier::new(
                                "i".to_string(),
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
                                            "j".to_string(),
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
                                                            "printi".to_string(),
                                                            &tokens[138..139]
                                                        ),
                                                        arguments: vec![*var_i(&tokens[140..141])],
                                                        info: AstInfo::new(&tokens[138..143]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc".to_string(),
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
                                                            "printi".to_string(),
                                                            &tokens[148..149]
                                                        ),
                                                        arguments: vec![*var_j(&tokens[150..151])],
                                                        info: AstInfo::new(&tokens[148..153]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc".to_string(),
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
                                                            "printi".to_string(),
                                                            &tokens[158..159]
                                                        ),
                                                        arguments: vec![*var_k(&tokens[160..161])],
                                                        info: AstInfo::new(&tokens[158..163]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc".to_string(),
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
                                                            Identifier::new(
                                                                "j".to_string(),
                                                                &tokens[168..169]
                                                            )
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
                                            "i".to_string(),
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
