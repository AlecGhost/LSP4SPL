use super::*;

#[test]
fn simple() {
    let stmt = "if (1 = 2) {}";
    let tokens = lex(stmt);
    let (_, is) =
        all_consuming(terminated(|input| IfStatement::parse(None, input), eof))(tokens.to_tokens())
            .unwrap();
    eq!(
        is,
        IfStatement {
            condition: Some(Reference::new(
                Expression::Binary(BinaryExpression {
                    operator: Operator::Equ,
                    lhs: int_lit(1, 0..1),
                    rhs: int_lit(2, 2..3),
                    info: AstInfo::new(0..3),
                }),
                2,
            )),
            if_branch: Some(Box::new(Reference::new(
                Statement::Block(BlockStatement {
                    statements: Vec::new(),
                    info: AstInfo::new(0..2),
                }),
                6,
            ))),
            else_branch: None,
            info: AstInfo::new(0..8),
        },
        "IfStatement: {}",
        stmt
    );
}
