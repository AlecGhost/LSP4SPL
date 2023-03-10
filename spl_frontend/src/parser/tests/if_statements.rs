use super::*;

#[test]
fn simple() {
    let stmt = "if (1 = 2) {}";
    let broker = LocalBroker::default();
    let tokens = lex(stmt, broker);
    let (input, is) =
        all_consuming(terminated(IfStatement::parse, eof))(tokens.to_tokens()).unwrap();
    eq!(
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
