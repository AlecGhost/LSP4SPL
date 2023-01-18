use crate::{ast::*, parser::util_parsers::symbols};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric0, anychar, digit1, hex_digit1},
    combinator::{all_consuming, eof, map, opt, peek},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated},
};
use std::ops::Range;
use util_parsers::{expect, ignore_until1, keywords, ws};

#[cfg(test)]
mod tests;
mod util_parsers;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError(Range<usize>, ErrorMessage);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorMessage {
    MissingOpening(char),
    MissingClosing(char),
    MissingTrailingSemic,
    UnexpectedCharacters(String),
    ExpectedToken(String),
}

impl ToString for ErrorMessage {
    fn to_string(&self) -> String {
        match self {
            Self::MissingOpening(c) => format!("missing opening `{}`", c),
            Self::MissingClosing(c) => format!("missing closing `{}`", c),
            Self::MissingTrailingSemic => "missing trailing `;`".to_string(),
            Self::UnexpectedCharacters(s) => format!("unexpected `{}`", s),
            Self::ExpectedToken(t) => format!("expected `{}`", t),
        }
    }
}

pub trait DiagnosticsBroker {
    fn report_error(&self, error: ParseError);
}

// source: https://github.com/ebkalderon/example-fault-tolerant-parser/blob/master/src/main.rs
// see also: https://eyalkalderon.com/blog/nom-error-recovery/
trait ToRange {
    fn to_range(&self) -> Range<usize>;
}

impl<B> ToRange for Span<'_, B> {
    fn to_range(&self) -> Range<usize> {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        start..end
    }
}

pub type Span<'a, B> = nom_locate::LocatedSpan<&'a str, B>;

type IResult<'a, T, B> = nom::IResult<Span<'a, B>, T>;

pub fn parse<B>(src: &str, broker: B) -> Program
where
    B: Clone + std::fmt::Debug + DiagnosticsBroker,
{
    let input = Span::new_extra(src, broker);
    let (_, program) = all_consuming(Program::parse)(input).expect("Parser cannot fail");
    program
}

trait Parser<B>: Sized {
    fn parse(input: Span<B>) -> IResult<Self, B>;
}

impl<B: Clone> Parser<B> for Option<char> {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, _) = tag("'")(input)?;
        let (input, c) = alt((map(tag("\\n"), |_| '\n'), anychar))(input)?;
        let (input, _) = tag("'")(input)?;
        Ok((input, Some(c)))
    }
}

impl<B: Clone> Parser<B> for u32 {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        ws(alt((map(digit1, |span: Span<B>| {
            span.parse().expect("Parsing digit failed")
        }),)))(input)
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for IntLiteral {
    fn parse(input: Span<B>) -> IResult<Self, B> {
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
                    |opt: Option<Span<B>>| {
                        opt.map(|span: Span<B>| {
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

impl<B: Clone> Parser<B> for Identifier {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        map(
            pair(alt((alpha1, tag("_"))), alt((alphanumeric0, tag("_")))),
            |pair: (Span<B>, Span<B>)| Self {
                value: String::new() + *pair.0 + *pair.1,
            },
        )(input)
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for Variable {
    fn parse(input: Span<B>) -> IResult<Self, B> {
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
            });
        }
        Ok((input, name))
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for Expression {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        fn parse_primary<B: Clone + DiagnosticsBroker>(input: Span<B>) -> IResult<Expression, B> {
            // Primary := IntLit | Variable | "(" Expr ")"
            alt((
                map(IntLiteral::parse, Expression::IntLiteral),
                map(Variable::parse, Expression::Variable),
                map(
                    delimited(
                        symbols::lparen,
                        expect(
                            Expression::parse,
                            ErrorMessage::ExpectedToken("expression".to_string()),
                        ),
                        expect(symbols::rparen, ErrorMessage::MissingClosing(')')),
                    ),
                    |opt| opt.unwrap_or(Expression::Error),
                ),
            ))(input)
        }

        fn parse_unary<B: Clone + DiagnosticsBroker>(input: Span<B>) -> IResult<Expression, B> {
            // Unary := Primary | "-" Primary
            alt((
                parse_primary,
                map(preceded(tag("-"), parse_unary), |exp| {
                    Expression::Binary(BinaryExpression {
                        operator: Operator::Sub,
                        lhs: Box::new(Expression::IntLiteral(IntLiteral::new(0))),
                        rhs: Box::new(exp),
                    })
                }),
            ))(input)
        }

        fn parse_rhs<'a, P, B: Clone + DiagnosticsBroker>(
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

        fn parse_mul<B: Clone + DiagnosticsBroker>(input: Span<B>) -> IResult<Expression, B> {
            // Mul := Unary (("*" | "/") Unary)*
            let (mut input, mut exp) = parse_unary(input)?;
            while let Ok((i, op)) = alt((symbols::times, symbols::divide))(input.clone()) {
                (input, exp) = parse_rhs(i, exp, &op, parse_unary)?;
            }
            Ok((input, exp))
        }

        fn parse_add<B: Clone + DiagnosticsBroker>(input: Span<B>) -> IResult<Expression, B> {
            // Add := Mul (("+" | "-") Mul)*
            let (mut input, mut exp) = parse_mul(input)?;
            while let Ok((i, op)) = alt((symbols::plus, symbols::minus))(input.clone()) {
                (input, exp) = parse_rhs(i, exp, &op, parse_mul)?;
            }
            Ok((input, exp))
        }

        fn parse_comparison<B: Clone + DiagnosticsBroker>(
            input: Span<B>,
        ) -> IResult<Expression, B> {
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

impl<B: Clone + DiagnosticsBroker> Parser<B> for TypeExpression {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        fn parse_array_type<B: Clone + DiagnosticsBroker>(
            input: Span<B>,
        ) -> IResult<TypeExpression, B> {
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

impl<B: Clone + DiagnosticsBroker> Parser<B> for TypeDeclaration {
    fn parse(input: Span<B>) -> IResult<Self, B> {
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
        let (input, _) = expect(symbols::semic, ErrorMessage::MissingTrailingSemic)(input)?;
        Ok((input, Self { name, type_expr }))
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for VariableDeclaration {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, _) = keywords::var(input)?;
        let (input, name) = expect(
            Identifier::parse,
            ErrorMessage::ExpectedToken("identifier".to_string()),
        )(input)?;
        let (input, _) =
            expect(symbols::colon, ErrorMessage::ExpectedToken(":".to_string()))(input)?;
        let (input, type_expr) = expect(
            TypeExpression::parse,
            ErrorMessage::ExpectedToken("type expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::semic, ErrorMessage::MissingTrailingSemic)(input)?;
        Ok((input, Self { name, type_expr }))
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for ParameterDeclaration {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, (is_ref, name)) = alt((
            map(
                preceded(
                    keywords::r#ref,
                    expect(
                        Identifier::parse,
                        ErrorMessage::ExpectedToken("identifier".to_string()),
                    ),
                ),
                |ident| (true, ident),
            ),
            map(Identifier::parse, |ident| (false, Some(ident))),
        ))(input)?;
        let (input, _) =
            expect(symbols::colon, ErrorMessage::ExpectedToken(":".to_string()))(input)?;
        let (input, type_expr) = expect(
            TypeExpression::parse,
            ErrorMessage::ExpectedToken("type expression".to_string()),
        )(input)?;
        Ok((
            input,
            Self {
                is_ref,
                name,
                type_expr,
            },
        ))
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for CallStatement {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, name) = terminated(Identifier::parse, symbols::lparen)(input)?;
        let (input, mut arguments) = many0(terminated(Expression::parse, symbols::comma))(input)?;
        let (input, opt_argument) = if arguments.is_empty() {
            opt(Expression::parse)(input)?
        } else {
            expect(
                Expression::parse,
                ErrorMessage::ExpectedToken("expression".to_string()),
            )(input)?
        };
        if let Some(argument) = opt_argument {
            arguments.push(argument);
        };
        let (input, _) = expect(symbols::rparen, ErrorMessage::MissingClosing(')'))(input)?;
        let (input, _) = expect(symbols::semic, ErrorMessage::MissingTrailingSemic)(input)?;
        Ok((input, Self { name, arguments }))
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for Assignment {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, variable) = terminated(Variable::parse, symbols::assign)(input)?;
        let (input, expr) = expect(
            Expression::parse,
            ErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::semic, ErrorMessage::MissingTrailingSemic)(input)?;
        Ok((input, Self { variable, expr }))
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for IfStatement {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, _) = keywords::r#if(input)?;
        let (input, _) = expect(symbols::lparen, ErrorMessage::MissingOpening('('))(input)?;
        let (input, condition) = expect(
            Expression::parse,
            ErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::rparen, ErrorMessage::MissingClosing(')'))(input)?;
        let (input, if_branch) = expect(
            Statement::parse,
            ErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let (input, else_branch) = opt(preceded(
            keywords::r#else,
            expect(
                Statement::parse,
                ErrorMessage::ExpectedToken("statement".to_string()),
            ),
        ))(input)?;
        Ok((
            input,
            Self {
                condition,
                if_branch: if_branch.map(Box::new),
                else_branch: else_branch.flatten().map(Box::new),
            },
        ))
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for WhileStatement {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, _) = keywords::r#while(input)?;
        let (input, _) = expect(symbols::lparen, ErrorMessage::MissingOpening('('))(input)?;
        let (input, condition) = expect(
            Expression::parse,
            ErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::rparen, ErrorMessage::MissingClosing(')'))(input)?;
        let (input, stmt) = expect(
            Statement::parse,
            ErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        Ok((
            input,
            Self {
                condition,
                statement: stmt.map(Box::new),
            },
        ))
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for BlockStatement {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, _) = symbols::lcurly(input)?;
        let (input, statements) = many0(Statement::parse)(input)?;
        let (input, _) = expect(symbols::rcurly, ErrorMessage::MissingClosing('}'))(input)?;
        Ok((input, Self { statements }))
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for Statement {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        alt((
            map(symbols::semic, |_| Self::Empty),
            map(IfStatement::parse, Self::If),
            map(WhileStatement::parse, Self::While),
            map(BlockStatement::parse, Self::Block),
            map(Assignment::parse, Self::Assignment),
            map(CallStatement::parse, Self::Call),
        ))(input)
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for ProcedureDeclaration {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let (input, _) = keywords::proc(input)?;
        let (input, name) = expect(
            Identifier::parse,
            ErrorMessage::ExpectedToken("identifier".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::lparen, ErrorMessage::MissingOpening('('))(input)?;
        let (input, mut parameters) =
            many0(terminated(ParameterDeclaration::parse, symbols::comma))(input)?;
        let (input, opt_parameter) = if parameters.is_empty() {
            opt(ParameterDeclaration::parse)(input)?
        } else {
            expect(
                ParameterDeclaration::parse,
                ErrorMessage::ExpectedToken("parameter declaration".to_string()),
            )(input)?
        };
        if let Some(parameter) = opt_parameter {
            parameters.push(parameter);
        };
        let (input, _) = expect(symbols::rparen, ErrorMessage::MissingClosing(')'))(input)?;
        let (input, _) = expect(symbols::lcurly, ErrorMessage::MissingOpening('{'))(input)?;
        let (input, variable_declarations) = many0(VariableDeclaration::parse)(input)?;
        let (input, statements) = many0(Statement::parse)(input)?;
        let (input, _) = expect(symbols::rcurly, ErrorMessage::MissingClosing('}'))(input)?;
        Ok((
            input,
            Self {
                name,
                parameters,
                variable_declarations,
                statements,
            },
        ))
    }
}

impl<B: Clone + DiagnosticsBroker> Parser<B> for Program {
    fn parse(input: Span<B>) -> IResult<Self, B> {
        let mut type_declarations = Vec::new();
        let mut procedure_declarations = Vec::new();
        let (input, _) = many0(alt((
            map(TypeDeclaration::parse, |td| type_declarations.push(td)),
            map(ProcedureDeclaration::parse, |pd| {
                procedure_declarations.push(pd);
            }),
            map(
                ignore_until1(peek(alt((keywords::r#type::<B>, keywords::proc, eof)))),
                |span| {
                    let err = ParseError(
                        span.to_range(),
                        ErrorMessage::UnexpectedCharacters(span.fragment().to_string()),
                    );
                    span.extra.report_error(err);
                },
            ),
        )))(input)?;
        Ok((
            input,
            Self {
                type_declarations,
                procedure_declarations,
            },
        ))
    }
}
