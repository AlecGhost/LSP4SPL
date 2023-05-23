use crate::parser::inc;
use super::*;

type E = Expression;

#[test]
fn one() {
    let expr = "1";
    let tokens = lex(expr);
    eq!(
        all_consuming(terminated(inc::<E>(None), eof))(tokens.to_tokens())
            .unwrap()
            .1,
        *int_lit(1, 0..1),
        "Expression: {}",
        expr
    );
}

#[test]
fn addition() {
    let expr = "1 + 2";
    let tokens = lex(expr);
    eq!(
        all_consuming(terminated(inc::<E>(None), eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: int_lit(1, 0..1),
            rhs: int_lit(2, 2..3),
            info: AstInfo::new(0..3),
        }),
        "Expression: {}",
        expr
    );
}

#[test]
fn multiplication_before_addition() {
    let expr = "1 + 2 * 3";
    let tokens = lex(expr);
    eq!(
        all_consuming(terminated(inc::<E>(None), eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: int_lit(1, 0..1),
            rhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Mul,
                lhs: int_lit(2, 2..3),
                rhs: int_lit(3, 4..5),
                info: AstInfo::new(2..5),
            })),
            info: AstInfo::new(0..5),
        }),
        "Expression: {}",
        expr
    );
}

#[test]
fn division_before_addition() {
    let expr = "1 / 2 + 3";
    let tokens = lex(expr);
    eq!(
        all_consuming(terminated(inc::<E>(None), eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Add,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Div,
                lhs: int_lit(1, 0..1),
                rhs: int_lit(2, 2..3),
                info: AstInfo::new(0..3),
            })),
            rhs: int_lit(3, 4..5),
            info: AstInfo::new(0..5),
        }),
        "Expression: {}",
        expr
    );
}

#[test]
fn left_associativity_in_multiplication() {
    let expr = "1 * 2 / 3 * 4";
    let tokens = lex(expr);
    eq!(
        all_consuming(terminated(inc::<E>(None), eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Mul,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Div,
                lhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Mul,
                    lhs: int_lit(1, 0..1),
                    rhs: int_lit(2, 2..3),
                    info: AstInfo::new(0..3),
                })),
                rhs: int_lit(3, 4..5),
                info: AstInfo::new(0..5),
            })),
            rhs: int_lit(4, 6..7),
            info: AstInfo::new(0..7),
        }),
        "Expression: {}",
        expr
    );
}

#[test]
fn left_associativity_in_addition() {
    let expr = "1 - 2 + 3 - 4";
    let tokens = lex(expr);
    eq!(
        all_consuming(terminated(inc::<E>(None), eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Sub,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Add,
                lhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Sub,
                    lhs: int_lit(1, 0..1),
                    rhs: int_lit(2, 2..3),
                    info: AstInfo::new(0..3),
                })),
                rhs: int_lit(3, 4..5),
                info: AstInfo::new(0..5),
            })),
            rhs: int_lit(4, 6..7),
            info: AstInfo::new(0..7),
        }),
        "Expression: {}",
        expr
    );
}

#[test]
fn complex_comparison() {
    let expr = "(1 + 2) * 3 = 4 + 5 * 6 / 6";
    let tokens = lex(expr);
    eq!(
        all_consuming(terminated(inc::<E>(None), eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Equ,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Mul,
                lhs: Box::new(E::Bracketed(BracketedExpression {
                    expr: Box::new(E::Binary(BinaryExpression {
                        operator: Operator::Add,
                        lhs: int_lit(1, 1..2),
                        rhs: int_lit(2, 3..4),
                        info: AstInfo::new(1..4),
                    })),
                    info: AstInfo::new(0..5)
                })),
                rhs: int_lit(3, 6..7),
                info: AstInfo::new(0..7),
            })),
            rhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Add,
                lhs: int_lit(4, 8..9),
                rhs: Box::new(E::Binary(BinaryExpression {
                    operator: Operator::Div,
                    lhs: Box::new(E::Binary(BinaryExpression {
                        operator: Operator::Mul,
                        lhs: int_lit(5, 10..11),
                        rhs: int_lit(6, 12..13),
                        info: AstInfo::new(10..13),
                    })),
                    rhs: int_lit(6, 14..15),
                    info: AstInfo::new(10..15),
                })),
                info: AstInfo::new(8..15),
            })),
            info: AstInfo::new(0..15),
        }),
        "Expression: {}",
        expr
    );
}

#[test]
fn no_double_comparison() {
    let expr = "a < b > c";
    let tokens = lex(expr);
    assert!(
        all_consuming(terminated(inc::<E>(None), eof))(tokens.to_tokens()).is_err(),
        "Expression: {}",
        expr
    );
}
