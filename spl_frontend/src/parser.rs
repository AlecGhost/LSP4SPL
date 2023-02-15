use crate::{
    ast::*,
    error::{ParseErrorMessage, SplError},
    lexer::token::{Token, TokenType, Tokens},
    DiagnosticsBroker, ToRange,
};
use nom::{
    branch::alt,
    combinator::{all_consuming, map, opt, peek, recognize},
    multi::many0,
    sequence::{pair, terminated, tuple},
};
use utility::{expect, ignore_until, ignore_until1};

#[cfg(test)]
mod tests;
mod utility;

type IResult<'a, T, B> = nom::IResult<Tokens<'a, B>, T>;

/// Parses the given source code and returns an AST.
/// Errors are reported by the specified broker.
/// Panics if parsing fails.
///
/// # Examples
///
/// ```
/// use spl_frontend::parser::parse;
/// use spl_frontend::ast::Program;
/// # use spl_frontend::LocalBroker;
///
/// # let broker = LocalBroker::default();
/// let program = parse("", broker);
///
/// assert_eq!(program, Program {
///     global_declarations: Vec::new()
/// });
/// ```
pub fn parse<B: DiagnosticsBroker>(input: &[Token], broker: B) -> Program {
    let input = Tokens::new(input, broker);
    let (_, program) = all_consuming(Program::parse)(input).expect("Parser cannot fail");
    program
}

/// Implemented by all AST nodes.
trait Parser<B>: Sized {
    fn parse(input: Tokens<B>) -> IResult<Self, B>;
}

impl<B: DiagnosticsBroker> Parser<B> for IntLiteral {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        alt((
            map(hex, |token| Self {
                value: Some(if let TokenType::Hex(value) = token.fragment().token_type {
                    value
                } else {
                    panic!("Invalid hex parse")
                }),
                range: token.to_range(),
            }),
            map(char, |token| Self {
                value: Some(if let TokenType::Char(c) = token.fragment().token_type {
                    (c as u8).into()
                } else {
                    panic!("Invalid char parse")
                }),
                range: token.to_range(),
            }),
            map(int, |token| Self {
                value: Some(if let TokenType::Int(value) = token.fragment().token_type {
                    value
                } else {
                    panic!("Invalid int parse")
                }),
                range: token.to_range(),
            }),
        ))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Identifier {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        map(ident, |token| Self {
            value: token.fragment().to_string(),
            range: token.to_range(),
        })(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Variable {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
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
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        fn parse_primary<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Expression, B> {
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

        fn parse_unary<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Expression, B> {
            // Unary := Primary | "-" Primary
            alt((
                parse_primary,
                map(pair(symbols::minus, parse_unary), |pair| {
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

        fn parse_rhs<P, B: DiagnosticsBroker>(
            input: Tokens<B>,
            lhs: Expression,
            operator: Operator,
            parser: P,
        ) -> IResult<Expression, B>
        where
            P: Fn(Tokens<B>) -> IResult<Expression, B>,
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
                operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                range,
            });
            Ok((input, exp))
        }

        fn parse_mul<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Expression, B> {
            // Mul := Unary (("*" | "/") Unary)*
            let (mut input, mut exp) = parse_unary(input)?;
            while let Ok((i, op)) = alt((symbols::times, symbols::divide))(input.clone()) {
                (input, exp) =
                    parse_rhs(i, exp, op.fragment().token_type.clone().into(), parse_unary)?;
            }
            Ok((input, exp))
        }

        fn parse_add<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Expression, B> {
            // Add := Mul (("+" | "-") Mul)*
            let (mut input, mut exp) = parse_mul(input)?;
            while let Ok((i, op)) = alt((symbols::plus, symbols::minus))(input.clone()) {
                (input, exp) =
                    parse_rhs(i, exp, op.fragment().token_type.clone().into(), parse_mul)?;
            }
            Ok((input, exp))
        }

        fn parse_comparison<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Expression, B> {
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
                (input, exp) =
                    parse_rhs(i, exp, op.fragment().token_type.clone().into(), parse_add)?;
            }
            Ok((input, exp))
        }

        // Expr := Comp
        parse_comparison(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for TypeExpression {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        fn parse_array_type<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<TypeExpression, B> {
            let (input, array_kw) = keywords::array(input)?;
            let start = array_kw.to_range().start;
            let (input, _) = expect(
                symbols::lbracket,
                ParseErrorMessage::ExpectedToken("[".to_string()),
            )(input)?;
            let (input, size) = expect(
                map(IntLiteral::parse, |int_lit| int_lit.value),
                ParseErrorMessage::ExpectedToken("int literal".to_string()),
            )(input)?;
            let (input, _) =
                expect(symbols::rbracket, ParseErrorMessage::MissingClosing(']'))(input)?;
            let (input, of_kw) = expect(
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
                    array_kw: array_kw.to_range(),
                    of_kw: of_kw.map(|token| token.to_range()),
                    size: size.flatten(),
                    base_type: type_expr.map(Box::new),
                    range: start..end,
                },
            ))
        }

        alt((parse_array_type, map(Identifier::parse, Self::NamedType)))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for TypeDeclaration {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let (input, type_kw) = keywords::r#type(input)?;
        let start = type_kw.to_range().start;
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
                type_kw: type_kw.to_range(),
                name,
                type_expr,
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for VariableDeclaration {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let (input, var_kw) = keywords::var(input)?;
        let start = var_kw.to_range().start;
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
                var_kw: var_kw.to_range(),
                name,
                type_expr,
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for ParameterDeclaration {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let (input, (start, ref_kw, name)) = alt((
            map(
                pair(
                    keywords::r#ref,
                    expect(
                        Identifier::parse,
                        ParseErrorMessage::ExpectedToken("identifier".to_string()),
                    ),
                ),
                |pair| (pair.0.to_range().start, Some(pair.0.to_range()), pair.1),
            ),
            map(Identifier::parse, |ident| {
                (ident.range.start, None, Some(ident))
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
                ref_kw,
                name,
                type_expr,
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for CallStatement {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
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
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
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
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let (input, if_kw) = keywords::r#if(input)?;
        let start = if_kw.to_range().start;
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
        let (input, (else_kw, else_branch)) = map(
            opt(pair(
                keywords::r#else,
                expect(
                    Statement::parse,
                    ParseErrorMessage::ExpectedToken("statement".to_string()),
                ),
            )),
            |opt| match opt {
                Some(pair) => (Some(pair.0.to_range()), pair.1),
                None => (None, None),
            },
        )(input)?;
        let end = input.location_offset();
        Ok((
            input,
            Self {
                if_kw: if_kw.to_range(),
                condition,
                if_branch: if_branch.map(Box::new),
                else_kw,
                else_branch: else_branch.map(Box::new),
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for WhileStatement {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let (input, while_kw) = keywords::r#while(input)?;
        let start = while_kw.to_range().start;
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
                while_kw: while_kw.to_range(),
                condition,
                statement: stmt.map(Box::new),
                range: start..end,
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for BlockStatement {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let (input, curly) = symbols::lcurly(input)?;
        let start = curly.to_range().start;
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
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
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
                        recognize(symbols::lcurly),
                        recognize(symbols::rcurly),
                        recognize(symbols::semic),
                        eof,
                    )))),
                ),
                |pair| {
                    let mut var = pair.0;
                    loop {
                        match var {
                            Variable::NamedVariable(ident) => {
                                let tokens = pair.1;
                                let range = ident.range.start..tokens.to_range().end;
                                let err = SplError(
                                    range.clone(),
                                    ParseErrorMessage::UnexpectedCharacters(
                                        ident.value + &tokens.fragment().to_string(),
                                    )
                                    .to_string(),
                                );
                                tokens.broker.report_error(err);
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
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let (input, proc_kw) = keywords::proc(input)?;
        let start = proc_kw.to_range().start;
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
                proc_kw: proc_kw.to_range(),
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
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        alt((
            map(TypeDeclaration::parse, Self::Type),
            map(ProcedureDeclaration::parse, Self::Procedure),
            map(
                ignore_until1(peek(alt((keywords::r#type::<B>, keywords::proc, eof)))),
                |token| {
                    let err = SplError(
                        token.to_range(),
                        ParseErrorMessage::UnexpectedCharacters(token.fragment().to_string())
                            .to_string(),
                    );
                    token.broker.report_error(err);
                    Self::Error(token.to_range())
                },
            ),
        ))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Program {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let (input, global_declarations) = many0(GlobalDeclaration::parse)(input)?;
        let (input, _) = eof(input)?;
        Ok((
            input,
            Self {
                global_declarations,
            },
        ))
    }
}

macro_rules! tag_parser {
    ($name:ident, $token_type:pat) => {
        pub(super) fn $name<B: crate::DiagnosticsBroker>(
            input: crate::lexer::token::Tokens<B>,
        ) -> crate::parser::IResult<crate::lexer::token::Tokens<B>, B> {
            use crate::lexer::token::TokenType;
            use nom::bytes::complete::take;
            let original_input = input.clone();
            let (input, tokens) = take(1usize)(input)?;
            if matches!(tokens.fragment().token_type, $token_type) {
                Ok((input, tokens))
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    original_input,
                    nom::error::ErrorKind::Tag,
                )))
            }
        }
    };
}

tag_parser!(ident, TokenType::Ident(_));
tag_parser!(char, TokenType::Char(_));
tag_parser!(int, TokenType::Int(_));
tag_parser!(hex, TokenType::Hex(_));
tag_parser!(comment, TokenType::Comment(_));
tag_parser!(eof, TokenType::Eof);

mod keywords {
    tag_parser!(array, TokenType::Array);
    tag_parser!(of, TokenType::Of);
    tag_parser!(r#if, TokenType::If);
    tag_parser!(r#else, TokenType::Else);
    tag_parser!(r#while, TokenType::While);
    tag_parser!(proc, TokenType::Proc);
    tag_parser!(r#ref, TokenType::Ref);
    tag_parser!(r#type, TokenType::Type);
    tag_parser!(var, TokenType::Var);
}

mod symbols {
    tag_parser!(lparen, TokenType::LParen);
    tag_parser!(rparen, TokenType::RParen);
    tag_parser!(lbracket, TokenType::LBracket);
    tag_parser!(rbracket, TokenType::RBracket);
    tag_parser!(lcurly, TokenType::LCurly);
    tag_parser!(rcurly, TokenType::RCurly);
    tag_parser!(eq, TokenType::Eq);
    tag_parser!(neq, TokenType::Neq);
    tag_parser!(lt, TokenType::Lt);
    tag_parser!(le, TokenType::Le);
    tag_parser!(gt, TokenType::Gt);
    tag_parser!(ge, TokenType::Ge);
    tag_parser!(assign, TokenType::Assign);
    tag_parser!(colon, TokenType::Colon);
    tag_parser!(comma, TokenType::Comma);
    tag_parser!(semic, TokenType::Semic);
    tag_parser!(plus, TokenType::Plus);
    tag_parser!(minus, TokenType::Minus);
    tag_parser!(times, TokenType::Times);
    tag_parser!(divide, TokenType::Divide);
}
