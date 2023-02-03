use std::ops::Range;

use crate::{
    ast::*,
    error::{ParseErrorMessage, SplError},
    DiagnosticsBroker, ToRange,
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, anychar, digit1, hex_digit1},
    combinator::{all_consuming, eof, map, opt, peek},
    multi::many0,
    sequence::{pair, preceded, terminated, tuple},
};
use utility::{
    alpha_numeric0, expect, ignore_until, ignore_until1, keywords, symbols, ws,
};

#[cfg(test)]
mod tests;
mod utility;

pub type Span<'a, B> = nom_locate::LocatedSpan<&'a str, B>;

impl<B> ToRange for Span<'_, B> {
    fn to_range(&self) -> Range<usize> {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        start..end
    }
}

type IResult<'a, T, B> = nom::IResult<Span<'a, B>, T>;

pub fn parse<B: DiagnosticsBroker>(src: &str, broker: B) -> Program {
    let input = Span::new_extra(src, broker);
    let (_, program) = all_consuming(Program::parse)(input).expect("Parser cannot fail");
    program
}

trait Parser<B>: Sized {
    fn parse(input: Span<B>) -> IResult<Self, B>;
}

impl<B: DiagnosticsBroker> Parser<B> for Char {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let start = input.location_offset();
        let (input, _) = tag("'")(input)?;
        let (input, c) = alt((map(tag("\\n"), |_| '\n'), anychar))(input)?;
        let (input, _) = expect(tag("'"), ParseErrorMessage::MissingClosing('\''))(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                value: c,
                range: start..end,
            },
        ))
    }
}

impl<B: Clone> Parser<B> for Digit {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        ws(map(digit1, |span: Span<B>| Self {
            value: span.parse().expect("Parsing digit failed"),
            range: span.to_range(),
        }))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for IntLiteral {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        ws(alt((
            map(
                pair(
                    tag("0x"),
                    expect(
                        hex_digit1,
                        ParseErrorMessage::ExpectedToken("Hexadecimal digit".to_string()),
                    ),
                ),
                |pair| {
                    let end = if let Some(span) = &pair.1 {
                        span.to_range().end
                    } else {
                        pair.0.to_range().end
                    };
                    let value = pair.1.map(|span| {
                        u32::from_str_radix(&span, 16).expect("Parsing hex digit failed")
                    });
                    Self {
                        value,
                        range: pair.0.to_range().start..end,
                    }
                },
            ),
            map(Char::parse, |char| Self {
                value: Some(char.value as u32),
                range: char.range,
            }),
            map(Digit::parse, |digit| Self {
                value: Some(digit.value),
                range: digit.range,
            }),
        )))(input)
    }
}

impl<B: Clone> Parser<B> for Identifier {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        map(
            ws(pair(alt((alpha1, tag("_"))), alpha_numeric0)),
            |pair: (Span<B>, Span<B>)| Self {
                value: String::new() + *pair.0 + *pair.1,
                range: pair.0.location_offset()..pair.1.to_range().end,
            },
        )(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Variable {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, mut name) = map(Identifier::parse, Self::NamedVariable)(input)?;
        let (input, accesses) = many0(tuple((
            symbols::lbracket,
            Expression::parse,
            symbols::rbracket,
        )))(input)?;
        for access in accesses {
            let expression = access.1;
            let range = name.to_range().start..access.2.to_range().end;
            name = Self::ArrayAccess(ArrayAccess {
                array: Box::new(name),
                index: Box::new(expression),
                range,
            });
        }
        Ok((input, name))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Expression {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        fn parse_primary<B: DiagnosticsBroker>(input: Span<B>) -> IResult<Expression, B> {
            // Primary := IntLit | Variable | "(" Expr ")"
            alt((
                map(IntLiteral::parse, Expression::IntLiteral),
                map(Variable::parse, Expression::Variable),
                map(
                    tuple((
                        symbols::lparen,
                        expect(
                            Expression::parse,
                            ParseErrorMessage::ExpectedToken("expression".to_string()),
                        ),
                        expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
                    )),
                    |tuple| {
                        let paren = tuple.0;
                        let expr = tuple.1;
                        expr.unwrap_or_else(|| {
                            let after_paren = paren.to_range().end;
                            Expression::Error(after_paren..after_paren)
                        })
                    },
                ),
            ))(input)
        }

        fn parse_unary<B: DiagnosticsBroker>(input: Span<B>) -> IResult<Expression, B> {
            // Unary := Primary | "-" Primary
            alt((
                parse_primary,
                map(pair(tag("-"), parse_unary), |pair| {
                    let minus = pair.0;
                    let primary = pair.1;
                    let range = minus.to_range().start..primary.to_range().end;
                    Expression::Binary(BinaryExpression {
                        operator: Operator::Sub,
                        lhs: Box::new(Expression::IntLiteral(IntLiteral::new(0, minus.to_range()))),
                        rhs: Box::new(primary),
                        range,
                    })
                }),
            ))(input)
        }

        fn parse_rhs<'a, P, B: DiagnosticsBroker>(
            input: Span<'a, B>,
            lhs: Expression,
            op: &str,
            parser: P,
        ) -> IResult<'a, Expression, B>
        where
            P: Fn(Span<B>) -> IResult<Expression, B>,
        {
            let (input, rhs) = expect(
                parser,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            )(input)?;
            let rhs = rhs.unwrap_or_else(|| {
                let pos = input.location_offset();
                Expression::Error(pos..pos)
            });
            let range = lhs.to_range().start..rhs.to_range().end;
            let exp = Expression::Binary(BinaryExpression {
                operator: Operator::new(op).expect("Operator conversion failed"),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                range,
            });
            Ok((input, exp))
        }

        fn parse_mul<B: DiagnosticsBroker>(input: Span<B>) -> IResult<Expression, B> {
            // Mul := Unary (("*" | "/") Unary)*
            let (mut input, mut exp) = parse_unary(input)?;
            while let Ok((i, op)) = alt((symbols::times, symbols::divide))(input.clone()) {
                (input, exp) = parse_rhs(i, exp, &op, parse_unary)?;
            }
            Ok((input, exp))
        }

        fn parse_add<B: DiagnosticsBroker>(input: Span<B>) -> IResult<Expression, B> {
            // Add := Mul (("+" | "-") Mul)*
            let (mut input, mut exp) = parse_mul(input)?;
            while let Ok((i, op)) = alt((symbols::plus, symbols::minus))(input.clone()) {
                (input, exp) = parse_rhs(i, exp, &op, parse_mul)?;
            }
            Ok((input, exp))
        }

        fn parse_comparison<B: DiagnosticsBroker>(input: Span<B>) -> IResult<Expression, B> {
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

impl<B: DiagnosticsBroker> Parser<B> for TypeExpression {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        fn parse_array_type<B: DiagnosticsBroker>(input: Span<B>) -> IResult<TypeExpression, B> {
            let (input, array) = keywords::array(input)?;
            let start = array.location_offset();
            let (input, _) = expect(
                symbols::lbracket,
                ParseErrorMessage::ExpectedToken("[".to_string()),
            )(input)?;
            let (input, size) = expect(
                map(ws(IntLiteral::parse), |int_lit| int_lit.value),
                ParseErrorMessage::ExpectedToken("int literal".to_string()),
            )(input)?;
            let (input, _) =
                expect(symbols::rbracket, ParseErrorMessage::MissingClosing(']'))(input)?;
            let (input, _) = expect(
                keywords::of,
                ParseErrorMessage::ExpectedToken("of".to_string()),
            )(input)?;
            let (input, type_expr) = expect(
                TypeExpression::parse,
                ParseErrorMessage::ExpectedToken("type expression".to_string()),
            )(input)?;
            let end = input.location_offset();
            Ok((
                input,
                TypeExpression::ArrayType {
                    size: size.flatten(),
                    base_type: type_expr.map(Box::new),
                    range: start..end,
                },
            ))
        }

        alt((
            parse_array_type,
            map(Identifier::parse, Self::NamedType),
        ))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for TypeDeclaration {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, r#type) = keywords::r#type(input)?;
        let start = r#type.location_offset();
        let (input, name) = expect(
            Identifier::parse,
            ParseErrorMessage::ExpectedToken("identifier".to_string()),
        )(input)?;
        let (input, _) = expect(
            symbols::eq,
            ParseErrorMessage::ExpectedToken("=".to_string()),
        )(input)?;
        let (input, type_expr) = expect(
            TypeExpression::parse,
            ParseErrorMessage::ExpectedToken("type expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic)(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                name,
                type_expr,
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for VariableDeclaration {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, var) = keywords::var(input)?;
        let start = var.location_offset();
        let (input, name) = expect(
            Identifier::parse,
            ParseErrorMessage::ExpectedToken("identifier".to_string()),
        )(input)?;
        let (input, _) = expect(
            symbols::colon,
            ParseErrorMessage::ExpectedToken(":".to_string()),
        )(input)?;
        let (input, type_expr) = expect(
            TypeExpression::parse,
            ParseErrorMessage::ExpectedToken("type expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic)(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                name,
                type_expr,
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for ParameterDeclaration {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, (start, is_ref, name)) = alt((
            map(
                pair(
                    keywords::r#ref,
                    expect(
                        Identifier::parse,
                        ParseErrorMessage::ExpectedToken("identifier".to_string()),
                    ),
                ),
                |pair| (pair.0.location_offset(), true, pair.1),
            ),
            map(Identifier::parse, |ident| {
                (ident.range.start, false, Some(ident))
            }),
        ))(input)?;
        let (input, _) = expect(
            symbols::colon,
            ParseErrorMessage::ExpectedToken(":".to_string()),
        )(input)?;
        let (input, type_expr) = expect(
            TypeExpression::parse,
            ParseErrorMessage::ExpectedToken("type expression".to_string()),
        )(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                is_ref,
                name,
                type_expr,
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for CallStatement {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, name) = terminated(Identifier::parse, symbols::lparen)(input)?;
        let start = name.range.start;
        let (input, mut arguments) = many0(terminated(Expression::parse, symbols::comma))(input)?;
        let (input, opt_argument) = if arguments.is_empty() {
            opt(Expression::parse)(input)?
        } else {
            expect(
                Expression::parse,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            )(input)?
        };
        if let Some(argument) = opt_argument {
            arguments.push(argument);
        };
        let (input, _) = expect(symbols::rparen, ParseErrorMessage::MissingClosing(')'))(input)?;
        let (input, _) = expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic)(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                name,
                arguments,
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Assignment {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, variable) = terminated(Variable::parse, symbols::assign)(input)?;
        let start = variable.to_range().start;
        let (input, expr) = expect(
            Expression::parse,
            ParseErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic)(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                variable,
                expr,
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for IfStatement {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, r#if) = keywords::r#if(input)?;
        let start = r#if.location_offset();
        let (input, _) = expect(symbols::lparen, ParseErrorMessage::MissingOpening('('))(input)?;
        let (input, condition) = expect(
            Expression::parse,
            ParseErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::rparen, ParseErrorMessage::MissingClosing(')'))(input)?;
        let (input, if_branch) = expect(
            Statement::parse,
            ParseErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let (input, else_branch) = opt(preceded(
            keywords::r#else,
            expect(
                Statement::parse,
                ParseErrorMessage::ExpectedToken("statement".to_string()),
            ),
        ))(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                condition,
                if_branch: if_branch.map(Box::new),
                else_branch: else_branch.flatten().map(Box::new),
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for WhileStatement {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, r#while) = keywords::r#while(input)?;
        let start = r#while.location_offset();
        let (input, _) = expect(symbols::lparen, ParseErrorMessage::MissingOpening('('))(input)?;
        let (input, condition) = expect(
            Expression::parse,
            ParseErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::rparen, ParseErrorMessage::MissingClosing(')'))(input)?;
        let (input, stmt) = expect(
            Statement::parse,
            ParseErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                condition,
                statement: stmt.map(Box::new),
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for BlockStatement {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, curly) = symbols::lcurly(input)?;
        let start = curly.location_offset();
        let (input, statements) = many0(Statement::parse)(input)?;
        let (input, _) = expect(symbols::rcurly, ParseErrorMessage::MissingClosing('}'))(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                statements,
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Statement {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        alt((
            map(symbols::semic, |semic| Self::Empty(semic.to_range())),
            map(IfStatement::parse, Self::If),
            map(WhileStatement::parse, Self::While),
            map(BlockStatement::parse, Self::Block),
            map(Assignment::parse, Self::Assignment),
            map(CallStatement::parse, Self::Call),
            map(
                pair(
                    Variable::parse,
                    ignore_until::<B, _>(peek(alt((
                        symbols::lcurly,
                        symbols::rcurly,
                        symbols::semic,
                        eof,
                    )))),
                ),
                |pair| {
                    let mut var = pair.0;
                    loop {
                        match var {
                            Variable::NamedVariable(ident) => {
                                let span = pair.1;
                                let range = ident.range.start..span.to_range().end;
                                let err = SplError(
                                    range.clone(),
                                    ParseErrorMessage::UnexpectedCharacters(
                                        ident.value + span.fragment(),
                                    )
                                    .to_string(),
                                );
                                span.extra.report_error(err);
                                return Self::Error(range);
                            }
                            Variable::ArrayAccess(a) => var = *a.array,
                        }
                    }
                },
            ),
        ))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for ProcedureDeclaration {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, proc) = keywords::proc(input)?;
        let start = proc.location_offset();
        let (input, name) = expect(
            Identifier::parse,
            ParseErrorMessage::ExpectedToken("identifier".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::lparen, ParseErrorMessage::MissingOpening('('))(input)?;
        let (input, mut parameters) =
            many0(terminated(ParameterDeclaration::parse, symbols::comma))(input)?;
        let (input, opt_parameter) = if parameters.is_empty() {
            opt(ParameterDeclaration::parse)(input)?
        } else {
            expect(
                ParameterDeclaration::parse,
                ParseErrorMessage::ExpectedToken("parameter declaration".to_string()),
            )(input)?
        };
        if let Some(parameter) = opt_parameter {
            parameters.push(parameter);
        };
        let (input, _) = expect(symbols::rparen, ParseErrorMessage::MissingClosing(')'))(input)?;
        let (input, _) = expect(symbols::lcurly, ParseErrorMessage::MissingOpening('{'))(input)?;
        let (input, variable_declarations) = many0(VariableDeclaration::parse)(input)?;
        let (input, statements) = many0(Statement::parse)(input)?;
        let (input, _) = expect(symbols::rcurly, ParseErrorMessage::MissingClosing('}'))(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                name,
                parameters,
                variable_declarations,
                statements,
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for GlobalDeclaration {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        alt((
            map(TypeDeclaration::parse, Self::Type),
            map(ProcedureDeclaration::parse, Self::Procedure),
            map(
                ignore_until1(peek(alt((keywords::r#type::<B>, keywords::proc, ws(eof))))),
                |span| {
                    let err = SplError(
                        span.to_range(),
                        ParseErrorMessage::UnexpectedCharacters(span.fragment().to_string())
                            .to_string(),
                    );
                    span.extra.report_error(err);
                    Self::Error(span.to_range())
                },
            ),
        ))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Program {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, global_declarations) = many0(GlobalDeclaration::parse)(input)?;
        let (input, _) = ws(eof)(input)?;
        Ok((
            input,
            Self {
                global_declarations,
            },
        ))
    }
}
