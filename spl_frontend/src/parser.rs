use crate::{
    ast::*,
    error::{ParseErrorMessage, SplError},
    lexer::token::{IntResult, Token, TokenStream, TokenType},
    parser::utility::{expect, ignore_until, parse_list},
    DiagnosticsBroker, ToRange,
};
use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::{all_consuming, map, opt, peek, recognize},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    Offset,
};

#[cfg(test)]
mod tests;
mod utility;

type IResult<'a, T, B> = nom::IResult<TokenStream<'a, B>, T>;

/// Parses the given tokens and returns an AST.
/// Errors are reported by the specified broker.
///
/// # Panics
///
/// Panics if parsing fails.
pub(crate) fn parse<B: DiagnosticsBroker>(input: &[Token], broker: B) -> Program {
    let input = TokenStream::new(input, broker);
    let (_, program) = all_consuming(Program::parse)(input).expect("Parser cannot fail");
    program
}

/// Try to parse token stream.
/// Implemented by all AST nodes.
trait Parser<B>: Sized {
    fn parse(input: TokenStream<B>) -> IResult<Self, B>;
}

impl<B: DiagnosticsBroker> Parser<B> for IntLiteral {
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, value) = alt((
            map(hex, |token| {
                if let TokenType::Hex(hex_result) = token.token_type {
                    match hex_result {
                        IntResult::Int(i) => Some(i),
                        IntResult::Err(_) => None,
                    }
                } else {
                    panic!("Invalid hex parse")
                }
            }),
            map(char, |token| {
                if let TokenType::Char(c) = token.token_type {
                    Some((c as u8).into())
                } else {
                    panic!("Invalid char parse")
                }
            }),
            map(int, |token| {
                if let TokenType::Int(int_result) = token.token_type {
                    match int_result {
                        IntResult::Int(i) => Some(i),
                        IntResult::Err(_) => None,
                    }
                } else {
                    panic!("Invalid int parse")
                }
            }),
        ))(input)?;
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                value,
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Identifier {
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, ident) = ident(input)?;
        let offset = tokens.offset(&input);
        Ok((
            input,
            Self {
                value: ident.to_string(),
                info: AstInfo::new(&tokens[..offset]),
            },
        ))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Variable {
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, (mut variable, accesses)) = pair(
            map(Identifier::parse, Self::NamedVariable),
            many0(delimited(
                symbols::lbracket,
                expect(
                    Expression::parse,
                    ParseErrorMessage::ExpectedToken("expression".to_string()),
                ),
                expect(symbols::rbracket, ParseErrorMessage::MissingClosing(']')),
            )),
        )(input)?;
        for access in accesses {
            let offset = tokens.offset(&input);
            variable = Self::ArrayAccess(ArrayAccess {
                array: Box::new(variable),
                index: access.map(Box::new),
                info: AstInfo::new(&tokens[..offset]),
            });
        }
        Ok((input, variable))
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Expression {
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        fn parse_bracketed<B: DiagnosticsBroker>(input: TokenStream<B>) -> IResult<Expression, B> {
            // Bracketed := "(" Expr ")"
            let tokens = input.clone();
            let (input, (lparen, expr, _)) = tuple((
                symbols::lparen,
                expect(
                    Expression::parse,
                    ParseErrorMessage::ExpectedToken("expression".to_string()),
                ),
                expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
            ))(input)?;
            let offset = tokens.offset(&input);
            let after_paren = lparen.to_range().end;
            let expr = expr.unwrap_or_else(|| Expression::Error(after_paren..after_paren));
            let bracketed = Expression::Bracketed(BracketedExpression {
                expr: Box::new(expr),
                info: AstInfo::new(&tokens[..offset]),
            });
            Ok((input, bracketed))
        }

        fn parse_primary<B: DiagnosticsBroker>(input: TokenStream<B>) -> IResult<Expression, B> {
            // Primary := IntLit | Variable | Bracketed
            alt((
                map(IntLiteral::parse, Expression::IntLiteral),
                map(Variable::parse, Expression::Variable),
                parse_bracketed,
            ))(input)
        }

        fn parse_unary<B: DiagnosticsBroker>(input: TokenStream<B>) -> IResult<Expression, B> {
            // Unary := "-" Primary
            let tokens = input.clone();
            let (input, primary) = preceded(symbols::minus, parse_factor)(input)?;
            let offset = tokens.offset(&input);
            let expr = Expression::Unary(UnaryExpression {
                operator: Operator::Sub,
                expr: Box::new(primary),
                info: AstInfo::new(&tokens[..offset]),
            });
            Ok((input, expr))
        }

        fn parse_factor<B: DiagnosticsBroker>(input: TokenStream<B>) -> IResult<Expression, B> {
            // Factor := Primary | Unary
            alt((parse_primary, parse_unary))(input)
        }

        fn parse_rhs<'a, P, B: DiagnosticsBroker>(
            input: TokenStream<'a, B>,
            tokens: &TokenStream<B>,
            lhs: Expression,
            operator: Operator,
            parser: P,
        ) -> IResult<'a, Expression, B>
        where
            P: Fn(TokenStream<B>) -> IResult<Expression, B>,
        {
            let (input, rhs) = expect(
                parser,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            )(input)?;
            let pos = input.to_range().start;
            let rhs = rhs.unwrap_or_else(|| Expression::Error(pos..pos));
            let offset = tokens.offset(&input);
            let exp = Expression::Binary(BinaryExpression {
                operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                info: AstInfo::new(&tokens[..offset]),
            });
            Ok((input, exp))
        }

        fn parse_mul<B: DiagnosticsBroker>(input: TokenStream<B>) -> IResult<Expression, B> {
            // Mul := Factor (("*" | "/") Factor)*
            let tokens = input.clone();
            let (mut input, mut exp) = parse_factor(input)?;
            while let Ok((i, op)) = alt((symbols::times, symbols::divide))(input.clone()) {
                (input, exp) = parse_rhs(
                    i,
                    &tokens,
                    exp,
                    op.token_type
                        .try_into()
                        .expect("Operator conversion failed"),
                    parse_factor,
                )?;
            }
            Ok((input, exp))
        }

        fn parse_add<B: DiagnosticsBroker>(input: TokenStream<B>) -> IResult<Expression, B> {
            // Add := Mul (("+" | "-") Mul)*
            let tokens = input.clone();
            let (mut input, mut exp) = parse_mul(input)?;
            while let Ok((i, op)) = alt((symbols::plus, symbols::minus))(input.clone()) {
                (input, exp) = parse_rhs(
                    i,
                    &tokens,
                    exp,
                    op.token_type
                        .try_into()
                        .expect("Operator conversion failed"),
                    parse_mul,
                )?;
            }
            Ok((input, exp))
        }

        fn parse_comparison<B: DiagnosticsBroker>(input: TokenStream<B>) -> IResult<Expression, B> {
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
                    &tokens,
                    exp,
                    op.token_type
                        .try_into()
                        .expect("Operator conversion failed"),
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
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        fn parse_array_type<B: DiagnosticsBroker>(
            input: TokenStream<B>,
        ) -> IResult<TypeExpression, B> {
            let tokens = input.clone();
            let (input, (_, _, size, _, _, type_expr)) = tuple((
                keywords::array,
                expect(
                    symbols::lbracket,
                    ParseErrorMessage::ExpectedToken("[".to_string()),
                ),
                expect(
                    IntLiteral::parse,
                    ParseErrorMessage::ExpectedToken("int literal".to_string()),
                ),
                expect(symbols::rbracket, ParseErrorMessage::MissingClosing(']')),
                expect(
                    keywords::of,
                    ParseErrorMessage::ExpectedToken("of".to_string()),
                ),
                expect(
                    TypeExpression::parse,
                    ParseErrorMessage::ExpectedToken("type expression".to_string()),
                ),
            ))(input)?;
            let offset = tokens.offset(&input);
            Ok((
                input,
                TypeExpression::ArrayType {
                    size,
                    base_type: type_expr.map(Box::new),
                    info: AstInfo::new(&tokens[..offset]),
                },
            ))
        }

        alt((parse_array_type, map(Identifier::parse, Self::NamedType)))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for TypeDeclaration {
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, (_, name, _, type_expr, _)) = tuple((
            keywords::r#type,
            expect(
                Identifier::parse,
                ParseErrorMessage::ExpectedToken("identifier".to_string()),
            ),
            expect(
                symbols::eq,
                ParseErrorMessage::ExpectedToken("=".to_string()),
            ),
            expect(
                TypeExpression::parse,
                ParseErrorMessage::ExpectedToken("type expression".to_string()),
            ),
            expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic),
        ))(input)?;
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
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, (_, name, _, type_expr, _)) = tuple((
            keywords::var,
            expect(
                Identifier::parse,
                ParseErrorMessage::ExpectedToken("identifier".to_string()),
            ),
            expect(
                symbols::colon,
                ParseErrorMessage::ExpectedToken(":".to_string()),
            ),
            expect(
                TypeExpression::parse,
                ParseErrorMessage::ExpectedToken("type expression".to_string()),
            ),
            expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic),
        ))(input)?;
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
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, ((ref_kw, name), _, type_expr)) = tuple((
            alt((
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
            )),
            expect(
                symbols::colon,
                ParseErrorMessage::ExpectedToken(":".to_string()),
            ),
            expect(
                TypeExpression::parse,
                ParseErrorMessage::ExpectedToken("type expression".to_string()),
            ),
        ))(input)?;
        let offset = tokens.offset(&input);
        let is_ref = ref_kw.is_some();
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
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, (name, arguments, _, _)) = tuple((
            terminated(Identifier::parse, symbols::lparen),
            parse_list(
                Expression::parse,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
            expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
            expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic),
        ))(input)?;
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
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, (variable, expr, _)) = tuple((
            terminated(Variable::parse, symbols::assign),
            expect(
                Expression::parse,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
            expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic),
        ))(input)?;
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
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, (_, _, condition, _, if_branch, else_branch)) = tuple((
            keywords::r#if,
            expect(symbols::lparen, ParseErrorMessage::MissingOpening('(')),
            expect(
                Expression::parse,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
            expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
            expect(
                Statement::parse,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
            opt(preceded(
                keywords::r#else,
                expect(
                    Statement::parse,
                    ParseErrorMessage::ExpectedToken("statement".to_string()),
                ),
            )),
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
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, (_, _, condition, _, stmt)) = tuple((
            keywords::r#while,
            expect(symbols::lparen, ParseErrorMessage::MissingOpening('(')),
            expect(
                Expression::parse,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
            expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
            expect(
                Statement::parse,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
        ))(input)?;
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
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, statements) = delimited(
            symbols::lcurly,
            many0(Statement::parse),
            expect(symbols::rcurly, ParseErrorMessage::MissingClosing('}')),
        )(input)?;
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
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        fn parse_error<B: DiagnosticsBroker>(input: TokenStream<B>) -> IResult<Statement, B> {
            let tokens = input.clone();
            let (input, (_, ignored)) = tuple((
                many0(comment),
                ignore_until(peek(alt((
                    recognize(symbols::lcurly),
                    recognize(symbols::rcurly),
                    recognize(symbols::semic),
                    recognize(keywords::r#if),
                    recognize(keywords::r#while),
                    recognize(Assignment::parse),
                    recognize(CallStatement::parse),
                    recognize(eof),
                )))),
            ))(input)?;
            // convert into AstInfo, so that `to_range()` and `to_string()` are available
            let ignored_info = AstInfo::new(&ignored[..]);
            let err = SplError(
                ignored_info.to_range(),
                ParseErrorMessage::UnexpectedCharacters(ignored_info.to_string()).to_string(),
            );
            input.broker.report_error(err);
            let offset = tokens.offset(&input);
            let stmt = Statement::Error(AstInfo::new(&tokens[..offset]));
            Ok((input, stmt))
        }

        alt((
            map(symbols::semic, |semic| Self::Empty(AstInfo::new(&[semic]))),
            map(IfStatement::parse, Self::If),
            map(WhileStatement::parse, Self::While),
            map(BlockStatement::parse, Self::Block),
            map(Assignment::parse, Self::Assignment),
            map(CallStatement::parse, Self::Call),
            parse_error,
        ))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for ProcedureDeclaration {
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        let tokens = input.clone();
        let (input, (_, name, _, parameters, _, _, variable_declarations, statements, _)) =
            tuple((
                keywords::proc,
                expect(
                    Identifier::parse,
                    ParseErrorMessage::ExpectedToken("identifier".to_string()),
                ),
                expect(symbols::lparen, ParseErrorMessage::MissingOpening('(')),
                parse_list(
                    ParameterDeclaration::parse,
                    ParseErrorMessage::ExpectedToken("parameter declaration".to_string()),
                ),
                expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
                expect(symbols::lcurly, ParseErrorMessage::MissingOpening('{')),
                many0(VariableDeclaration::parse),
                many0(Statement::parse),
                expect(symbols::rcurly, ParseErrorMessage::MissingClosing('}')),
            ))(input)?;
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
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
        fn parse_error<B: DiagnosticsBroker>(
            input: TokenStream<B>,
        ) -> IResult<GlobalDeclaration, B> {
            let tokens = input.clone();
            let (input, ignored) = ignore_until(peek(alt((
                recognize(keywords::r#type),
                recognize(keywords::proc),
                recognize(eof),
            ))))(input)?;
            // convert into AstInfo, so that `to_range()` and `to_string()` are available
            let ignored_info = AstInfo::new(&ignored[..]);
            let err = SplError(
                ignored_info.to_range(),
                ParseErrorMessage::UnexpectedCharacters(ignored_info.to_string()).to_string(),
            );
            input.broker.report_error(err);
            let offset = tokens.offset(&input);
            let gd = GlobalDeclaration::Error(AstInfo::new(&tokens[..offset]));
            Ok((input, gd))
        }

        alt((
            map(TypeDeclaration::parse, Self::Type),
            map(ProcedureDeclaration::parse, Self::Procedure),
            parse_error,
        ))(input)
    }
}

impl<B: DiagnosticsBroker> Parser<B> for Program {
    fn parse(input: TokenStream<B>) -> IResult<Self, B> {
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
fn comment<B: DiagnosticsBroker>(input: TokenStream<B>) -> IResult<Token, B> {
    let original_input = input.clone();
    let (input, tokens) = take(1usize)(input)?;
    let token = tokens.fragment().clone();
    if matches!(token.token_type, TokenType::Comment(_)) {
        Ok((input, token))
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
            input: crate::lexer::token::TokenStream<B>,
        ) -> crate::parser::IResult<crate::lexer::token::Token, B> {
            use crate::{lexer::token::TokenType, parser::comment};
            use nom::{bytes::complete::take, multi::many0};
            let original_input = input.clone();
            // Consume comments in front of any `tag_parser`
            let (input, _) = many0(comment)(input)?;
            let (input, tokens) = take(1usize)(input)?;
            let token = tokens.fragment().clone();
            if matches!(token.token_type, $token_type) {
                Ok((input, token))
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
