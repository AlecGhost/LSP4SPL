use crate::{
    ast::*,
    error::{ParseErrorMessage, SplError},
    lexer::token::{Token, TokenType, Tokens},
    DiagnosticsBroker, ToRange,
};
use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::{all_consuming, map, opt, peek, recognize},
    multi::many0,
    sequence::{pair, preceded, terminated, tuple},
    Offset,
};
use utility::{expect, ignore_until, ignore_until1};

#[cfg(test)]
mod tests;
mod utility;

type IResult<'a, T, B> = nom::IResult<Tokens<'a, B>, T>;

/// Parses the given tokens and returns an AST.
/// Errors are reported by the specified broker.
/// Panics if parsing fails.
///
/// # Examples
///
/// ```
/// use spl_frontend::parser::parse;
/// use spl_frontend::ast::Program;
/// use spl_frontend::lexer::lex;
/// # use spl_frontend::LocalBroker;
///
/// let tokens = lex("");
/// # let broker = LocalBroker::default();
/// let program = parse(&tokens, broker);
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
        let tokens = input.clone();
        let (input, value) = alt((
            map(hex, |token| {
                if let TokenType::Hex(value) = token.fragment().token_type {
                    value
                } else {
                    panic!("Invalid hex parse")
                }
            }),
            map(char, |token| {
                if let TokenType::Char(c) = token.fragment().token_type {
                    (c as u8).into()
                } else {
                    panic!("Invalid char parse")
                }
            }),
            map(int, |token| {
                if let TokenType::Int(value) = token.fragment().token_type {
                    value
                } else {
                    panic!("Invalid int parse")
                }
            }),
        ))(input)?;
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                value: Some(value),
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Identifier {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, ident) = ident(input)?;
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                value: ident.fragment().to_string(),
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Variable {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, mut name) = map(Identifier::parse, Self::NamedVariable)(input)?;
        let (input, accesses) = many0(tuple((
            symbols::lbracket,
            Expression::parse,
            symbols::rbracket,
        )))(input)?;
        for access in accesses {
            let expression = access.1;
            let offset = tokens.offset(&input);
            name = Self::ArrayAccess(ArrayAccess {
                array: Box::new(name),
                index: Box::new(expression),
                info: AstInfo::new(&tokens[..offset]),
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

        fn parse_negated<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Expression, B> {
            let tokens = input.clone();
            let (input, _) = symbols::minus(input)?;
            let (input, primary) = parse_unary(input)?;
            let offset = tokens.offset(&input);
            let expr = Expression::Binary(BinaryExpression {
                operator: Operator::Sub,
                lhs: Box::new(Expression::IntLiteral(IntLiteral::new(
                    0,
                    AstInfo::new(&Vec::new()),
                ))),
                rhs: Box::new(primary),
                info: AstInfo::new(&tokens[..offset]),
            });
            Ok((input, expr))
        }

        fn parse_unary<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Expression, B> {
            // Unary := Primary | "-" Primary
            alt((parse_primary, parse_negated))(input)
        }

        fn parse_rhs<'a, P, B: DiagnosticsBroker>(
            input: Tokens<'a, B>,
            tokens: Tokens<B>,
            lhs: Expression,
            operator: Operator,
            parser: P,
        ) -> IResult<'a, Expression, B>
        where
            P: Fn(Tokens<B>) -> IResult<Expression, B>,
        {
            let (input, rhs) = expect(
                parser,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            )(input)?;
            let rhs = rhs.unwrap_or_else(|| {
                let pos = input.to_range().start;
                Expression::Error(pos..pos)
            });
            let offset = tokens.offset(&input);
            let exp = Expression::Binary(BinaryExpression {
                operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                info: AstInfo::new(&tokens[..offset]),
            });
            Ok((input, exp))
        }

        fn parse_mul<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Expression, B> {
            // Mul := Unary (("*" | "/") Unary)*
            let tokens = input.clone();
            let (mut input, mut exp) = parse_unary(input)?;
            while let Ok((i, op)) = alt((symbols::times, symbols::divide))(input.clone()) {
                (input, exp) = parse_rhs(
                    i,
                    tokens.clone(),
                    exp,
                    op.fragment().token_type.clone().into(),
                    parse_unary,
                )?;
            }
            Ok((input, exp))
        }

        fn parse_add<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Expression, B> {
            // Add := Mul (("+" | "-") Mul)*
            let tokens = input.clone();
            let (mut input, mut exp) = parse_mul(input)?;
            while let Ok((i, op)) = alt((symbols::plus, symbols::minus))(input.clone()) {
                (input, exp) = parse_rhs(
                    i,
                    tokens.clone(),
                    exp,
                    op.fragment().token_type.clone().into(),
                    parse_mul,
                )?;
            }
            Ok((input, exp))
        }

        fn parse_comparison<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Expression, B> {
            // Comp := Add (("=" | "#" | "<" | "<=" | ">" | ">=") Add)?
            let tokens = input.clone();
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
                (input, exp) = parse_rhs(
                    i,
                    tokens,
                    exp,
                    op.fragment().token_type.clone().into(),
                    parse_add,
                )?;
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
            let tokens = input.clone();
            let (input, _) = keywords::array(input)?;
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
            let (input, _) = expect(
                keywords::of,
                ParseErrorMessage::ExpectedToken("of".to_string()),
            )(input)?;
            let (input, type_expr) = expect(
                TypeExpression::parse,
                ParseErrorMessage::ExpectedToken("type expression".to_string()),
            )(input)?;
            let offset = tokens.offset(&input);
            Ok((
                input,
                TypeExpression::ArrayType {
                    size: size.flatten(),
                    base_type: type_expr.map(Box::new),
                    info: AstInfo::new(&tokens[..offset]),
                },
            ))
        }

        alt((parse_array_type, map(Identifier::parse, Self::NamedType)))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for TypeDeclaration {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, _) = keywords::r#type(input)?;
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
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                name,
                type_expr,
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for VariableDeclaration {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, _) = keywords::var(input)?;
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
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                name,
                type_expr,
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for ParameterDeclaration {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, (ref_kw, name)) = alt((
            map(
                pair(
                    keywords::r#ref,
                    expect(
                        Identifier::parse,
                        ParseErrorMessage::ExpectedToken("identifier".to_string()),
                    ),
                ),
                |pair| (Some(pair.0), pair.1),
            ),
            map(Identifier::parse, |ident| (None, Some(ident))),
        ))(input)?;
        let is_ref = ref_kw.is_some();
        let (input, _) = expect(
            symbols::colon,
            ParseErrorMessage::ExpectedToken(":".to_string()),
        )(input)?;
        let (input, type_expr) = expect(
            TypeExpression::parse,
            ParseErrorMessage::ExpectedToken("type expression".to_string()),
        )(input)?;
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                is_ref,
                name,
                type_expr,
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for CallStatement {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, name) = terminated(Identifier::parse, symbols::lparen)(input)?;
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
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                name,
                arguments,
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Assignment {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, variable) = terminated(Variable::parse, symbols::assign)(input)?;
        let (input, expr) = expect(
            Expression::parse,
            ParseErrorMessage::ExpectedToken("expression".to_string()),
        )(input)?;
        let (input, _) = expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic)(input)?;
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                variable,
                expr,
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for IfStatement {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, _) = keywords::r#if(input)?;
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
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                condition,
                if_branch: if_branch.map(Box::new),
                else_branch: else_branch.flatten().map(Box::new),
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for WhileStatement {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, _) = keywords::r#while(input)?;
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
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                condition,
                statement: stmt.map(Box::new),
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for BlockStatement {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, _) = symbols::lcurly(input)?;
        let (input, statements) = many0(Statement::parse)(input)?;
        let (input, _) = expect(symbols::rcurly, ParseErrorMessage::MissingClosing('}'))(input)?;
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                statements,
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Statement {
    fn parse(input: Tokens<B>) -> IResult<Self, B> {
        alt((
            map(symbols::semic, |semic| {
                Self::Empty(AstInfo::new(&semic[0..0]))
            }),
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
                                let range = ident.to_range().start..tokens.to_range().end;
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
        let tokens = input.clone();
        let (input, _) = keywords::proc(input)?;
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
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                name,
                parameters,
                variable_declarations,
                statements,
                info: AstInfo::new(&tokens[..offset]),
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
                    Self::Error(AstInfo::new(&token[0..0]))
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

// Comment parser is separated from the other token parsers,
// because it is used in the `tag_parser` macro
fn comment<B: DiagnosticsBroker>(input: Tokens<B>) -> IResult<Tokens<B>, B> {
    let original_input = input.clone();
    let (input, tokens) = take(1usize)(input)?;
    if matches!(tokens.fragment().token_type, TokenType::Comment(_)) {
        Ok((input, tokens))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            original_input,
            nom::error::ErrorKind::Tag,
        )))
    }
}

macro_rules! tag_parser {
    ($name:ident, $token_type:pat) => {
        pub(super) fn $name<B: crate::DiagnosticsBroker>(
            input: crate::lexer::token::Tokens<B>,
        ) -> crate::parser::IResult<crate::lexer::token::Tokens<B>, B> {
            use crate::{lexer::token::TokenType, parser::comment};
            use nom::{bytes::complete::take, multi::many0};
            let original_input = input.clone();
            // Consume comments in front of any `tag_parser`
            let (input, _) = many0(comment)(input)?;
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
