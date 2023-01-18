use self::util_parsers::{expect, keywords, ws};
use crate::{ast::*, parser::util_parsers::symbols};
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
            pair(alt((alpha1, tag("_"))), alt((alphanumeric0, tag("_")))),
            |pair: (Span, Span)| Self {
                value: String::new() + *pair.0 + *pair.1,
            },
        )(input)
    }
}

impl Parser for Variable {
    fn parse(input: Span) -> IResult<Self> {
        let (input, mut name) = map(Identifier::parse, Self::NamedVariable)(input)?;
        let (input, accesses) = many0(delimited(
            symbols::lbracket,
            Expression::parse,
            symbols::rbracket,
        ))(input)?;
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
            while let Ok((i, op)) = alt((symbols::times, symbols::divide))(input.clone()) {
                (input, exp) = parse_rhs(i, exp, &op, parse_unary)?;
            }
            Ok((input, exp))
        }

        fn parse_add(input: Span) -> IResult<Expression> {
            // Add := Mul (("+" | "-") Mul)*
            let (mut input, mut exp) = parse_mul(input)?;
            while let Ok((i, op)) = alt((symbols::plus, symbols::minus))(input.clone()) {
                (input, exp) = parse_rhs(i, exp, &op, parse_mul)?;
            }
            Ok((input, exp))
        }

        fn parse_comparison(input: Span) -> IResult<Expression> {
            // Comp := Add (("=" | "#" | "<" | "<=" | ">" | ">=") Add)?
            let (mut input, mut exp) = parse_add(input)?;
            if let Ok((i, op)) = alt((
                symbols::eq,
                symbols::neq,
                symbols::le,
                symbols::lt,
                symbols::ge,
                symbols::gt,
            ))(input.clone())
            {
                (input, exp) = parse_rhs(i, exp, &op, parse_add)?;
            }
            Ok((input, exp))
        }

        // Expr := Comp
        parse_comparison(input)
    }
}

impl Parser for TypeExpression {
    fn parse(input: Span) -> IResult<Self> {
        fn parse_array_type(input: Span) -> IResult<TypeExpression> {
            let (input, _) = keywords::array(input)?;
            let (input, _) = expect(
                symbols::lbracket,
                ErrorMessage::ExpectedToken("[".to_string()),
            )(input)?;
            let (input, dim) = expect(
                ws(u32::parse),
                ErrorMessage::ExpectedToken("integer".to_string()),
            )(input)?;
            let (input, _) = expect(symbols::rbracket, ErrorMessage::MissingClosing(']'))(input)?;
            let (input, _) =
                expect(keywords::of, ErrorMessage::ExpectedToken("of".to_string()))(input)?;
            let (input, type_expr) = expect(
                TypeExpression::parse,
                ErrorMessage::ExpectedToken("type expression".to_string()),
            )(input)?;
            Ok((
                input,
                TypeExpression::ArrayType(dim, type_expr.map(Box::new)),
            ))
        }

        alt((parse_array_type, map(Identifier::parse, Self::Type)))(input)
    }
}

impl Parser for TypeDeclaration {
    fn parse(input: Span) -> IResult<Self> {
        let (input, _) = keywords::r#type(input)?;
        let (input, name) = expect(
            Identifier::parse,
            ErrorMessage::ExpectedToken("identifier".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::eq, ErrorMessage::ExpectedToken("=".to_string()))(input)?;
        let (input, type_expr) = expect(
            TypeExpression::parse,
            ErrorMessage::ExpectedToken("type expression".to_string()),
        )(input)?;
        let (input, _) =
            expect(symbols::semic, ErrorMessage::ExpectedToken(";".to_string()))(input)?;
        Ok((input, Self { name, type_expr }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::combinator::all_consuming;

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
            ]
        )
    }
}
