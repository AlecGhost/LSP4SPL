use super::*;
use nom::combinator::all_consuming;
use std::{cell::RefCell, rc::Rc};

#[derive(Debug)]
struct LocalBroker<E>(Rc<RefCell<Vec<E>>>);

impl<E> Clone for LocalBroker<E> {
    fn clone(&self) -> Self {
        LocalBroker(Rc::clone(&self.0))
    }
}

impl<E: Clone> LocalBroker<E> {
    fn new() -> Self {
        Self(Rc::new(RefCell::new(Vec::new())))
    }

    fn errors<'a>(&self) -> Vec<E> {
        self.0.borrow().clone()
    }
}

impl<E> DiagnosticsBroker<E> for LocalBroker<E> {
    fn report_error(&self, error: E) {
        self.0.borrow_mut().push(error);
    }
}

trait ToSpan<B> {
    fn to_span(&self) -> Span<B>;
}

impl ToSpan<LocalBroker<ParseError>> for &str {
    fn to_span(&self) -> Span<LocalBroker<ParseError>> {
        Span::new_extra(self, LocalBroker::new())
    }
}

impl ToSpan<LocalBroker<ParseError>> for String {
    fn to_span(&self) -> Span<LocalBroker<ParseError>> {
        Span::new_extra(self, LocalBroker::new())
    }
}

impl Identifier {
    fn new<T: ToString>(value: T, range: Range<usize>) -> Self {
        Self {
            value: value.to_string(),
            range,
        }
    }
}

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
            name: Some(Identifier::new("a", 5..6)),
            type_expr: Some(TE::IntType)
        },
        "Declaration: {}",
        dec
    );

    let dec = "type a = array [2] of array [3] of int;";
    assert_eq!(
        all_consuming(TD::parse)(dec.to_span()).unwrap().1,
        TD {
            name: Some(Identifier::new("a", 5..6)),
            type_expr: Some(TE::ArrayType {
                size: Some(2),
                base_type: Some(Box::new(TE::ArrayType {
                    size: Some(3),
                    base_type: Some(Box::new(TE::IntType))
                }))
            })
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
            type_expr: Some(TE::ArrayType {
                size: None,
                base_type: Some(Box::new(TE::ArrayType {
                    size: None,
                    base_type: None
                }))
            })
        },
        "Declaration: {}",
        dec
    );
    assert_eq!(
        input.extra.errors(),
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
            variable: Variable::NamedVariable(Identifier::new("a", 0..1)),
            expr: Some(Expression::IntLiteral(IntLiteral::new(1)))
        },
        "Assignment: {}",
        asgn
    );
    assert!(input.extra.errors().is_empty(), "Assignment: {}", asgn);

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
            name: Identifier::new("a", 0..1),
            arguments: Vec::new(),
        },
        "CallStatement: {}",
        stmt
    );
    assert!(input.extra.errors().is_empty(), "CallStatement: {}", stmt);

    let stmt = "a(1, 2, 3);";
    let (input, cs) = all_consuming(CallStatement::parse)(stmt.to_span()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a", 0..1),
            arguments: vec![
                Expression::IntLiteral(IntLiteral::new(1)),
                Expression::IntLiteral(IntLiteral::new(2)),
                Expression::IntLiteral(IntLiteral::new(3)),
            ],
        },
        "CallStatement: {}",
        stmt
    );
    assert!(input.extra.errors().is_empty(), "CallStatement: {}", stmt);

    let stmt = "a(1,)";
    let (input, cs) = all_consuming(CallStatement::parse)(stmt.to_span()).unwrap();
    assert_eq!(
        cs,
        CallStatement {
            name: Identifier::new("a", 0..1),
            arguments: vec![Expression::IntLiteral(IntLiteral::new(1)),],
        },
        "CallStatement: {}",
        stmt
    );
    assert_eq!(
        input.extra.errors(),
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
    assert!(input.extra.errors().is_empty(), "IfStatement: {}", stmt);
}

#[test]
fn acker() {
    let acker = std::fs::read_to_string("/Users/alex/dev/compiler/programs/acker.spl").unwrap();
    let (input, program) = all_consuming(Program::parse)(acker.to_span()).unwrap();

    // variables for use in assertion
    let int_type = Some(TypeExpression::IntType);
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
        range: Range<usize>,
        arg0: Expression,
        arg1: Expression,
        arg2: Expression,
    ) -> Statement {
        Statement::Call(CallStatement {
            name: Identifier::new("ackermann", range),
            arguments: vec![arg0, arg1, arg2],
        })
    }

    assert_eq!(
        program,
        Program {
            global_declarations: vec![
                GlobalDeclaration::Procedure(ProcedureDeclaration {
                    name: Some(Identifier::new("ackermann", 50..59)),
                    parameters: vec![
                        ParameterDeclaration {
                            is_ref: false,
                            name: i(60..61),
                            type_expr: int_type.clone()
                        },
                        ParameterDeclaration {
                            is_ref: false,
                            name: j(68..69),
                            type_expr: int_type.clone()
                        },
                        ParameterDeclaration {
                            is_ref: true,
                            name: k(80..81),
                            type_expr: int_type.clone()
                        },
                    ],
                    variable_declarations: vec![VariableDeclaration {
                        name: a(96..97),
                        type_expr: int_type.clone(),
                    }],
                    statements: vec![Statement::If(IfStatement {
                        condition: Some(Expression::Binary(BinaryExpression {
                            operator: Operator::Equ,
                            lhs: var_i(111..112),
                            rhs: int_lit(0)
                        })),
                        if_branch: Some(Box::new(Statement::Block(BlockStatement {
                            statements: vec![Statement::Assignment(Assignment {
                                variable: Variable::NamedVariable(Identifier::new("k", 124..125)),
                                expr: Some(Expression::Binary(BinaryExpression {
                                    operator: Operator::Add,
                                    lhs: var_j(129..130),
                                    rhs: int_lit(1)
                                }))
                            })]
                        }))),
                        else_branch: Some(Box::new(Statement::Block(BlockStatement {
                            statements: vec![Statement::If(IfStatement {
                                condition: Some(Expression::Binary(BinaryExpression {
                                    operator: Operator::Equ,
                                    lhs: var_j(155..156),
                                    rhs: int_lit(0)
                                })),
                                if_branch: Some(Box::new(Statement::Block(BlockStatement {
                                    statements: vec![call_ackermann(
                                        170..179,
                                        Expression::Binary(BinaryExpression {
                                            operator: Operator::Sub,
                                            lhs: var_i(180..181),
                                            rhs: int_lit(1)
                                        }),
                                        Expression::IntLiteral(IntLiteral::new(1)),
                                        *var_k(190..191)
                                    )]
                                }))),
                                else_branch: Some(Box::new(Statement::Block(BlockStatement {
                                    statements: vec![
                                        call_ackermann(
                                            213..222,
                                            *var_i(223..224),
                                            Expression::Binary(BinaryExpression {
                                                operator: Operator::Sub,
                                                lhs: var_j(226..227),
                                                rhs: int_lit(1)
                                            }),
                                            *var_a(233..234)
                                        ),
                                        call_ackermann(
                                            243..252,
                                            Expression::Binary(BinaryExpression {
                                                operator: Operator::Sub,
                                                lhs: var_i(253..254),
                                                rhs: int_lit(1)
                                            }),
                                            *var_a(260..261),
                                            *var_k(263..264)
                                        )
                                    ]
                                })))
                            })]
                        })))
                    })],
                }),
                GlobalDeclaration::Procedure(ProcedureDeclaration {
                    name: Some(Identifier::new("main", 286..290)),
                    parameters: Vec::new(),
                    variable_declarations: vec![
                        VariableDeclaration {
                            name: i(301..302),
                            type_expr: int_type.clone(),
                        },
                        VariableDeclaration {
                            name: j(315..316),
                            type_expr: int_type.clone(),
                        },
                        VariableDeclaration {
                            name: k(329..330),
                            type_expr: int_type.clone(),
                        }
                    ],
                    statements: vec![
                        Statement::Assignment(Assignment {
                            variable: Variable::NamedVariable(Identifier::new("i", 340..341)),
                            expr: Some(*int_lit(0).clone())
                        }),
                        Statement::While(WhileStatement {
                            condition: Some(Expression::Binary(BinaryExpression {
                                operator: Operator::Lse,
                                lhs: var_i(357..358),
                                rhs: int_lit(3)
                            })),
                            statement: Some(Box::new(Statement::Block(BlockStatement {
                                statements: vec![
                                    Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new(
                                            "j",
                                            371..372
                                        )),
                                        expr: Some(*int_lit(0))
                                    }),
                                    Statement::While(WhileStatement {
                                        condition: Some(Expression::Binary(BinaryExpression {
                                            operator: Operator::Lse,
                                            lhs: var_j(390..391),
                                            rhs: int_lit(6)
                                        })),
                                        statement: Some(Box::new(Statement::Block(
                                            BlockStatement {
                                                statements: vec![
                                                    call_ackermann(
                                                        406..415,
                                                        *var_i(416..417),
                                                        *var_j(419..420),
                                                        *var_k(422..423)
                                                    ),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printi", 432..438),
                                                        arguments: vec![*var_i(439..440)]
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printc", 449..455),
                                                        arguments: vec![*int_lit(32)]
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printi", 468..474),
                                                        arguments: vec![*var_j(475..476)]
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printc", 485..491),
                                                        arguments: vec![*int_lit(32)]
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printi", 504..510),
                                                        arguments: vec![*var_k(511..512)]
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new("printc", 521..527),
                                                        arguments: vec![*int_lit(10)]
                                                    }),
                                                    Statement::Assignment(Assignment {
                                                        variable: Variable::NamedVariable(
                                                            Identifier::new("j", 541..542)
                                                        ),
                                                        expr: Some(Expression::Binary(
                                                            BinaryExpression {
                                                                operator: Operator::Add,
                                                                lhs: var_j(546..547),
                                                                rhs: int_lit(1)
                                                            }
                                                        ))
                                                    })
                                                ]
                                            }
                                        )))
                                    }),
                                    Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new(
                                            "i",
                                            563..564
                                        )),
                                        expr: Some(Expression::Binary(BinaryExpression {
                                            operator: Operator::Add,
                                            lhs: var_i(568..569),
                                            rhs: int_lit(1)
                                        }))
                                    })
                                ]
                            })))
                        })
                    ]
                }),
            ],
        },
        "Acker: {}",
        acker
    );
    let broker = LocalBroker::new();
    let table = crate::table::build(&program, broker.clone());
    eprintln!("Table: {:#?}", table);
    eprintln!("Errors: {:#?}", broker);
    assert!(input.extra.errors().is_empty(), "Acker: {}", acker);
}
