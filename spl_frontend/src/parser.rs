use self::util_parsers::{expect, ignore_until1, keywords, ws};
use crate::{ast::*, parser::util_parsers::symbols};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric0, anychar, digit1, hex_digit1},
    combinator::{eof, map, opt, peek},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated},
};
use std::{cell::RefCell, ops::Range, sync::Arc};

#[cfg(test)]
mod tests;
mod util_parsers;

#[derive(Debug, PartialEq, Eq)]
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

impl ToSpan for String {
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
            });
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

        fn parse_unary(input: Span) -> IResult<Expression> {
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
        let (input, _) = expect(symbols::semic, ErrorMessage::MissingTrailingSemic)(input)?;
        Ok((input, Self { name, type_expr }))
    }
}

impl Parser for VariableDeclaration {
    fn parse(input: Span) -> IResult<Self> {
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

impl Parser for ParameterDeclaration {
    fn parse(input: Span) -> IResult<Self> {
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

impl Parser for CallStatement {
    fn parse(input: Span) -> IResult<Self> {
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

impl Parser for Assignment {
    fn parse(input: Span) -> IResult<Self> {
        let (input, variable) = terminated(Variable::parse, symbols::assign)(input)?;
        let (input, expr) = expect(
            Expression::parse,
            ErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::semic, ErrorMessage::MissingTrailingSemic)(input)?;
        Ok((input, Self { variable, expr }))
    }
}

impl Parser for IfStatement {
    fn parse(input: Span) -> IResult<Self> {
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

impl Parser for WhileStatement {
    fn parse(input: Span) -> IResult<Self> {
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

impl Parser for BlockStatement {
    fn parse(input: Span) -> IResult<Self> {
        let (input, _) = symbols::lcurly(input)?;
        let (input, statements) = many0(Statement::parse)(input)?;
        let (input, _) = expect(symbols::rcurly, ErrorMessage::MissingClosing('}'))(input)?;
        Ok((input, Self { statements }))
    }
}

impl Parser for Statement {
    fn parse(input: Span) -> IResult<Self> {
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

impl Parser for ProcedureDeclaration {
    fn parse(input: Span) -> IResult<Self> {
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

impl Parser for Program {
    fn parse(input: Span) -> IResult<Self> {
        let mut type_declarations = Vec::new();
        let mut procedure_declarations = Vec::new();
        let (input, _) = many0(alt((
            map(TypeDeclaration::parse, |td| type_declarations.push(td)),
            map(ProcedureDeclaration::parse, |pd| {
                procedure_declarations.push(pd);
            }),
            map(
                ignore_until1(peek(alt((keywords::r#type, keywords::proc, eof)))),
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
