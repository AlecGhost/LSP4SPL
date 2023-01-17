use self::util_parsers::{expect, ws};
use crate::ast::*;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric0, anychar, digit1, hex_digit1},
    combinator::map,
    multi::many0,
    sequence::{delimited, pair, preceded},
};
use std::{cell::RefCell, ops::Range, sync::Arc};

mod util_parsers;

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError(Range<usize>, ErrorMessage);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorMessage {
    MissingClosing(char),
    UnexpectedCharacters(String),
    ExpectedToken(String),
}

impl ToString for ErrorMessage {
    fn to_string(&self) -> String {
        match self {
            Self::MissingClosing(c) => format!("missing closing `{}`", c),
            Self::UnexpectedCharacters(s) => format!("unexpected `{}`", s),
            Self::ExpectedToken(t) => format!("expected `{}`", t),
        }
    }
}

pub trait DiagnosticsBroker {
    fn report_error(&self, error: ParseError);
}

impl DiagnosticsBroker for Arc<RefCell<Vec<ParseError>>> {
    fn report_error(&self, error: ParseError) {
        self.borrow_mut().push(error);
    }
}
// source: https://github.com/ebkalderon/example-fault-tolerant-parser/blob/master/src/main.rs
// see also: https://eyalkalderon.com/blog/nom-error-recovery/
trait ToRange {
    fn to_range(&self) -> Range<usize>;
}

impl ToRange for Span<'_> {
    fn to_range(&self) -> Range<usize> {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        start..end
    }
}

trait ToSpan {
    fn to_span(&self) -> Span;
}

impl ToSpan for &str {
    fn to_span(&self) -> Span {
        Span::new_extra(self, Arc::new(RefCell::new(Vec::new())))
    }
}

pub type Span<'a> = nom_locate::LocatedSpan<&'a str, Arc<RefCell<Vec<ParseError>>>>;

type IResult<'a, T> = nom::IResult<Span<'a>, T>;

trait Parser: Sized {
    fn parse(input: Span) -> IResult<Self>;
}

impl Parser for Option<char> {
    fn parse(input: Span) -> IResult<Self> {
        let (input, _) = tag("'")(input)?;
        let (input, c) = alt((map(tag("\\n"), |_| '\n'), anychar))(input)?;
        let (input, _) = tag("'")(input)?;
        Ok((input, Some(c)))
    }
}

impl Parser for u32 {
    fn parse(input: Span) -> IResult<Self> {
        ws(alt((map(digit1, |span: Span| {
            span.parse().expect("Parsing digit failed")
        }),)))(input)
    }
}

impl Parser for IntLiteral {
    fn parse(input: Span) -> IResult<Self> {
        map(
            ws(alt((
                map(
                    preceded(
                        tag("0x"),
                        expect(
                            hex_digit1,
                            ErrorMessage::ExpectedToken("Hexadecimal digit".to_string()),
                        ),
                    ),
                    |opt: Option<Span>| {
                        opt.map(|span| {
                            u32::from_str_radix(&span, 16).expect("Parsing hex digit failed")
                        })
                    },
                ),
                map(Option::parse, |opt: Option<char>| opt.map(|c| c as u32)),
                map(u32::parse, Some),
            ))),
            |opt| Self { value: opt },
        )(input)
    }
}

impl Parser for Identifier {
    fn parse(input: Span) -> IResult<Self> {
        map(
            pair(alpha1, alt((alphanumeric0, tag("_")))),
            |pair: (Span, Span)| Self {
                value: String::new() + *pair.0 + *pair.1,
            },
        )(input)
    }
}

impl Parser for Variable {
    fn parse(input: Span) -> IResult<Self> {
        let (input, mut name) = map(Identifier::parse, Self::NamedVariable)(input)?;
        let (input, accesses) =
            many0(delimited(ws(tag("[")), Expression::parse, ws(tag("]"))))(input)?;
        for access in accesses {
            name = Self::ArrayAccess(ArrayAccess {
                array: Box::new(name),
                index: Box::new(access),
            })
        }
        Ok((input, name))
    }
}

impl Parser for Expression {
    fn parse(input: Span) -> IResult<Self> {
        fn parse_primary(input: Span) -> IResult<Expression> {
            // Primary := IntLit | Variable | "(" Expr ")"
            alt((
                map(IntLiteral::parse, Expression::IntLiteral),
                map(Variable::parse, Expression::Variable),
                map(
                    delimited(
                        tag("("),
                        expect(
                            Expression::parse,
                            ErrorMessage::ExpectedToken("expression".to_string()),
                        ),
                        expect(tag(")"), ErrorMessage::MissingClosing(')')),
                    ),
                    |opt| opt.unwrap_or(Expression::Error),
                ),
            ))(input)
        }

        fn parse_unary(input: Span) -> IResult<Expression> {
            // Unary := Primary | "-" Primary
            alt((
                parse_primary,
                map(preceded(tag("-"), parse_unary), |exp| {
                    Expression::Binary(BinaryExpression {
                        operator: Operator::Sub,
                        lhs: Box::new(Expression::IntLiteral(IntLiteral { value: Some(0) })),
                        rhs: Box::new(exp),
                    })
                }),
            ))(input)
        }

        fn parse_rhs<'a, P>(
            input: Span<'a>,
            lhs: Expression,
            op: &str,
            parser: P,
        ) -> IResult<'a, Expression>
        where
            P: Fn(Span) -> IResult<Expression>,
        {
            let (input, rhs) = expect(
                parser,
                ErrorMessage::ExpectedToken("expression".to_string()),
            )(input)?;
            let rhs = rhs.unwrap_or(Expression::Error);
            let exp = Expression::Binary(BinaryExpression {
                operator: Operator::new(op).expect("Operator conversion failed"),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
            Ok((input, exp))
        }

        fn parse_mul(input: Span) -> IResult<Expression> {
            // Mul := Unary (("*" | "/") Unary)*
            let (mut input, mut exp) = parse_unary(input)?;
            while let Ok((i, op)) = ws(alt((tag("*"), tag("/"))))(input.clone()) {
                (input, exp) = parse_rhs(i, exp, &op, parse_unary)?;
            }
            Ok((input, exp))
        }

        fn parse_add(input: Span) -> IResult<Expression> {
            // Add := Mul (("+" | "-") Mul)*
            let (mut input, mut exp) = parse_mul(input)?;
            while let Ok((i, op)) = ws(alt((tag("+"), tag("-"))))(input.clone()) {
                (input, exp) = parse_rhs(i, exp, &op, parse_mul)?;
            }
            Ok((input, exp))
        }

        fn parse_comparison(input: Span) -> IResult<Expression> {
            // Comp := Add (("=" | "#" | "<" | "<=" | ">" | ">=") Add)?
            let (mut input, mut exp) = parse_add(input)?;
            if let Ok((i, op)) = ws(alt((
                tag("="),
                tag("#"),
                tag("<"),
                tag("<="),
                tag(">"),
                tag(">="),
            )))(input.clone())
            {
                (input, exp) = parse_rhs(i, exp, &op, parse_add)?;
            }
            Ok((input, exp))
        }

        // Expr := Comp
        parse_comparison(input)
    }
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    #[test]
    fn expressions() {
        type E = Expression;

        fn int_lit(value: u32) -> Box<E> {
            Box::new(E::IntLiteral(IntLiteral { value: Some(value) }))
        }

        let expr = "1";
        assert_eq!(
            all_consuming(E::parse)(expr.to_span()).unwrap().1,
            E::IntLiteral(IntLiteral { value: Some(1) }),
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
}
