use super::*;

type E = Expression;

#[test]
fn one() {
    let expr = "1";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        *int_lit(1, &tokens[0..1]),
        "Expression: {}",
        expr
    );
}

#[test]
fn addition() {
    let expr = "1 + 2";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    eq!(
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
}

#[test]
fn multiplication_before_addition() {
    let expr = "1 + 2 * 3";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    eq!(
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
}

#[test]
fn division_before_addition() {
    let expr = "1 / 2 + 3";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    eq!(
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
}

#[test]
fn left_associativity_in_multiplication() {
    let expr = "1 * 2 / 3 * 4";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    eq!(
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
}

#[test]
fn left_associativity_in_addition() {
    let expr = "1 - 2 + 3 - 4";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    eq!(
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
}

#[test]
fn complex_comparison() {
    let expr = "(1 + 2) * 3 = 4 + 5 * 6 / 6";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    eq!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens())
            .unwrap()
            .1,
        E::Binary(BinaryExpression {
            operator: Operator::Equ,
            lhs: Box::new(E::Binary(BinaryExpression {
                operator: Operator::Mul,
                lhs: Box::new(E::Bracketed(BracketedExpression {
                    expr: Box::new(E::Binary(BinaryExpression {
                        operator: Operator::Add,
                        lhs: int_lit(1, &tokens[1..2]),
                        rhs: int_lit(2, &tokens[3..4]),
                        info: AstInfo::new(&tokens[1..4]),
                    })),
                    info: AstInfo::new(&tokens[0..5])
                })),
                rhs: int_lit(3, &tokens[6..7]),
                info: AstInfo::new(&tokens[0..7]),
            })),
            rhs: Box::new(E::Binary(BinaryExpression {
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
}

#[test]
fn no_double_comparison() {
    let expr = "a < b > c";
    let broker = LocalBroker::default();
    let tokens = lex(expr, broker);
    assert!(
        all_consuming(terminated(E::parse, eof))(tokens.to_tokens()).is_err(),
        "Expression: {}",
        expr
    );
}
