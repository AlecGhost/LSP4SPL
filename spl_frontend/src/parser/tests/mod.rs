use std::ops::Range;

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
    let tokens = lex(i);
    assert_eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("ab1", &tokens[0..1]),
        "Identifier: {}",
        i
    );

    let i = "test_ident";
    let tokens = lex(i);
    assert_eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("test_ident", &tokens[0..1]),
        "Identifier: {}",
        i
    );

    let i = "_a";
    let tokens = lex(i);
    assert_eq!(
        all_consuming(terminated(Identifier::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        Identifier::new("_a", &tokens[0..1]),
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
        *int_lit(1, &tokens[0..1]),
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
            lhs: int_lit(1, &tokens[0..1]),
            rhs: int_lit(2, &tokens[2..3]),
            info: AstInfo::new(&tokens[0..3]),
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
    let tokens = lex(expr);
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
            name: Some(Identifier::new("a", &tokens[1..2])),
            type_expr: Some(TE::NamedType(Identifier::new("int", &tokens[3..4]))),
            info: AstInfo::new(&tokens[..5]),
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
            name: Some(Identifier::new("a", &tokens[1..2])),
            type_expr: Some(TE::ArrayType {
                size: Some(2),
                base_type: Some(Box::new(TE::ArrayType {
                    size: Some(3),
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
    let tokens = lex(dec);
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
            variable: Variable::NamedVariable(Identifier::new("a", &tokens[0..1])),
            expr: Some(*int_lit(1, &tokens[2..3])),
            info: AstInfo::new(&tokens[..4]),
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
            name: Identifier::new("a", &tokens[0..1]),
            arguments: Vec::new(),
            info: AstInfo::new(&tokens[0..4]),
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
    let tokens = lex(stmt);
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
    let tokens = lex(stmt);
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
    let tokens = lex(&acker);
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
                    name: Some(Identifier::new("ackermann", &tokens[0..0])),
                    parameters: vec![
                        ParameterDeclaration {
                            is_ref: false,
                            name: i(&tokens[0..0]),
                            type_expr: int_type(&tokens[0..0]),
                            info: AstInfo::new(&tokens[0..0]),
                        },
                        ParameterDeclaration {
                            is_ref: false,
                            name: j(&tokens[0..0]),
                            type_expr: int_type(&tokens[0..0]),
                            info: AstInfo::new(&tokens[0..0]),
                        },
                        ParameterDeclaration {
                            is_ref: false,
                            name: k(&tokens[0..0]),
                            type_expr: int_type(&tokens[0..0]),
                            info: AstInfo::new(&tokens[0..0]),
                        },
                    ],
                    variable_declarations: vec![VariableDeclaration {
                        name: a(&tokens[0..0]),
                        type_expr: int_type(&tokens[0..0]),
                        info: AstInfo::new(&tokens[0..0]),
                    }],
                    statements: vec![Statement::If(IfStatement {
                        condition: Some(Expression::Binary(BinaryExpression {
                            operator: Operator::Equ,
                            lhs: var_i(&tokens[0..0]),
                            rhs: int_lit(0, &tokens[0..0]),
                            info: AstInfo::new(&tokens[0..0]),
                        })),
                        if_branch: Some(Box::new(Statement::Block(BlockStatement {
                            statements: vec![Statement::Assignment(Assignment {
                                variable: Variable::NamedVariable(Identifier::new(
                                    "k",
                                    &tokens[0..0]
                                )),
                                expr: Some(Expression::Binary(BinaryExpression {
                                    operator: Operator::Add,
                                    lhs: var_j(&tokens[0..0]),
                                    rhs: int_lit(1, &tokens[0..0]),
                                    info: AstInfo::new(&tokens[0..0]),
                                })),
                                info: AstInfo::new(&tokens[0..0]),
                            })],
                            info: AstInfo::new(&tokens[0..0]),
                        }))),
                        else_branch: Some(Box::new(Statement::Block(BlockStatement {
                            statements: vec![Statement::If(IfStatement {
                                condition: Some(Expression::Binary(BinaryExpression {
                                    operator: Operator::Equ,
                                    lhs: var_j(&tokens[0..0]),
                                    rhs: int_lit(0, &tokens[0..0]),
                                    info: AstInfo::new(&tokens[0..0]),
                                })),
                                if_branch: Some(Box::new(Statement::Block(BlockStatement {
                                    statements: vec![call_ackermann(
                                        &tokens[0..0],
                                        &tokens[0..0],
                                        Expression::Binary(BinaryExpression {
                                            operator: Operator::Sub,
                                            lhs: var_i(&tokens[0..0]),
                                            rhs: int_lit(1, &tokens[0..0]),
                                            info: AstInfo::new(&tokens[0..0]),
                                        }),
                                        Expression::IntLiteral(IntLiteral::new(
                                            1,
                                            AstInfo::new(&tokens[0..0])
                                        )),
                                        *var_k(&tokens[0..0])
                                    )],
                                    info: AstInfo::new(&tokens[0..0]),
                                }))),
                                else_branch: Some(Box::new(Statement::Block(BlockStatement {
                                    statements: vec![
                                        call_ackermann(
                                            &tokens[0..0],
                                            &tokens[0..0],
                                            *var_i(&tokens[0..0]),
                                            Expression::Binary(BinaryExpression {
                                                operator: Operator::Sub,
                                                lhs: var_j(&tokens[0..0]),
                                                rhs: int_lit(1, &tokens[0..0]),
                                                info: AstInfo::new(&tokens[0..0]),
                                            }),
                                            *var_a(&tokens[0..0])
                                        ),
                                        call_ackermann(
                                            &tokens[0..0],
                                            &tokens[0..0],
                                            Expression::Binary(BinaryExpression {
                                                operator: Operator::Sub,
                                                lhs: var_i(&tokens[0..0]),
                                                rhs: int_lit(1, &tokens[0..0]),
                                                info: AstInfo::new(&tokens[0..0]),
                                            }),
                                            *var_a(&tokens[0..0]),
                                            *var_k(&tokens[0..0])
                                        )
                                    ],
                                    info: AstInfo::new(&tokens[0..0]),
                                }))),

                                info: AstInfo::new(&tokens[0..0]),
                            })],
                            info: AstInfo::new(&tokens[0..0]),
                        }))),
                        info: AstInfo::new(&tokens[0..0]),
                    })],
                    info: AstInfo::new(&tokens[0..0]),
                }),
                GlobalDeclaration::Procedure(ProcedureDeclaration {
                    name: Some(Identifier::new("main", &tokens[0..0])),
                    parameters: Vec::new(),
                    variable_declarations: vec![
                        VariableDeclaration {
                            name: i(&tokens[0..0]),
                            type_expr: int_type(&tokens[0..0]),
                            info: AstInfo::new(&tokens[0..0]),
                        },
                        VariableDeclaration {
                            name: j(&tokens[0..0]),
                            type_expr: int_type(&tokens[0..0]),
                            info: AstInfo::new(&tokens[0..0]),
                        },
                        VariableDeclaration {
                            name: k(&tokens[0..0]),
                            type_expr: int_type(&tokens[0..0]),
                            info: AstInfo::new(&tokens[0..0]),
                        }
                    ],
                    statements: vec![
                        Statement::Assignment(Assignment {
                            variable: Variable::NamedVariable(Identifier::new("i", &tokens[0..0])),
                            expr: Some(*int_lit(0, &tokens[0..0]).clone()),
                            info: AstInfo::new(&tokens[0..0]),
                        }),
                        Statement::While(WhileStatement {
                            condition: Some(Expression::Binary(BinaryExpression {
                                operator: Operator::Lse,
                                lhs: var_i(&tokens[0..0]),
                                rhs: int_lit(3, &tokens[0..0]),
                                info: AstInfo::new(&tokens[0..0]),
                            })),
                            statement: Some(Box::new(Statement::Block(BlockStatement {
                                statements: vec![
                                    Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new(
                                            "j",
                                            &tokens[0..0],
                                        )),
                                        expr: Some(*int_lit(0, &tokens[0..0])),
                                        info: AstInfo::new(&tokens[0..0]),
                                    }),
                                    Statement::While(WhileStatement {
                                        condition: Some(Expression::Binary(BinaryExpression {
                                            operator: Operator::Lse,
                                            lhs: var_j(&tokens[0..0]),
                                            rhs: int_lit(6, &tokens[0..0]),
                                            info: AstInfo::new(&tokens[0..0]),
                                        })),
                                        statement: Some(Box::new(Statement::Block(
                                            BlockStatement {
                                                statements: vec![
                                                    call_ackermann(
                                                        &tokens[0..0],
                                                        &tokens[0..0],
                                                        *var_i(&tokens[0..0]),
                                                        *var_j(&tokens[0..0]),
                                                        *var_k(&tokens[0..0])
                                                    ),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printi",
                                                            &tokens[0..0]
                                                        ),
                                                        arguments: vec![*var_i(&tokens[0..0])],
                                                        info: AstInfo::new(&tokens[0..0]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc",
                                                            &tokens[0..0]
                                                        ),
                                                        arguments: vec![*int_lit(
                                                            32,
                                                            &tokens[0..0]
                                                        )],
                                                        info: AstInfo::new(&tokens[0..0]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printi",
                                                            &tokens[0..0]
                                                        ),
                                                        arguments: vec![*var_j(&tokens[0..0])],
                                                        info: AstInfo::new(&tokens[0..0]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc",
                                                            &tokens[0..0]
                                                        ),
                                                        arguments: vec![*int_lit(
                                                            32,
                                                            &tokens[0..0]
                                                        )],
                                                        info: AstInfo::new(&tokens[0..0]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printi",
                                                            &tokens[0..0]
                                                        ),
                                                        arguments: vec![*var_k(&tokens[0..0])],
                                                        info: AstInfo::new(&tokens[0..0]),
                                                    }),
                                                    Statement::Call(CallStatement {
                                                        name: Identifier::new(
                                                            "printc",
                                                            &tokens[0..0]
                                                        ),
                                                        arguments: vec![*int_lit(
                                                            10,
                                                            &tokens[0..0]
                                                        )],
                                                        info: AstInfo::new(&tokens[0..0]),
                                                    }),
                                                    Statement::Assignment(Assignment {
                                                        variable: Variable::NamedVariable(
                                                            Identifier::new("j", &tokens[0..0])
                                                        ),
                                                        expr: Some(Expression::Binary(
                                                            BinaryExpression {
                                                                operator: Operator::Add,
                                                                lhs: var_j(&tokens[0..0]),
                                                                rhs: int_lit(1, &tokens[0..0]),
                                                                info: AstInfo::new(&tokens[0..0]),
                                                            }
                                                        )),
                                                        info: AstInfo::new(&tokens[0..0]),
                                                    })
                                                ],
                                                info: AstInfo::new(&tokens[0..0]),
                                            }
                                        ))),
                                        info: AstInfo::new(&tokens[0..0]),
                                    }),
                                    Statement::Assignment(Assignment {
                                        variable: Variable::NamedVariable(Identifier::new(
                                            "i",
                                            &tokens[0..0],
                                        )),
                                        expr: Some(Expression::Binary(BinaryExpression {
                                            operator: Operator::Add,
                                            lhs: var_i(&tokens[0..0]),
                                            rhs: int_lit(1, &tokens[0..0]),
                                            info: AstInfo::new(&tokens[0..0]),
                                        })),
                                        info: AstInfo::new(&tokens[0..0]),
                                    })
                                ],
                                info: AstInfo::new(&tokens[0..0]),
                            }))),
                            info: AstInfo::new(&tokens[0..0]),
                        })
                    ],
                    info: AstInfo::new(&tokens[0..0]),
                }),
            ],
        },
        "Acker: {}",
        acker
    );
    assert!(input.broker.errors().is_empty(), "Acker: {}", acker);
}
