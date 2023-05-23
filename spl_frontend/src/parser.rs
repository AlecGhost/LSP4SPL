use crate::{
    ast::*,
    error::{ParseErrorMessage, ParserError, SplError},
    lexer::token::{IntResult, Token, TokenStream, TokenType},
    parser::utility::{
        affected, alt, confusable, expect, ignore_until0, ignore_until1, info, many, parse_list,
        reference,
    },
    token, ToRange,
};
use nom::{
    bytes::complete::take,
    combinator::{all_consuming, map, opt, peek, recognize},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
};

#[cfg(test)]
mod tests;
mod utility;

type IResult<'a, T> = nom::IResult<TokenStream<'a>, T, ParserError<'a>>;

/// Parses the given tokens and returns an AST.
///
/// # Panics
///
/// Panics if parsing fails.
pub fn parse(input: &[Token]) -> Program {
    let input = TokenStream::new(input);
    let (_, program) = Program::parse(None, input).expect("Parser cannot fail");
    program
}

pub fn update(program: Program, input: TokenStream) -> Program {
    let (_, program) = Program::parse(Some(program), input).expect("Parser cannot fail");
    program
}

/// Try to parse token stream.
/// Implemented by all AST nodes.
trait Parser: Sized {
    fn parse(this: Option<Self>, input: TokenStream) -> IResult<Self>;
}

fn inc<'a, P: Parser + Clone>(node: Option<P>) -> impl FnMut(TokenStream<'a>) -> IResult<P> {
    move |input| P::parse(node.clone(), input)
}

impl Parser for IntLiteral {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        map(
            info(alt((
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
            ))),
            |(value, info)| Self { value, info },
        )(input)
    }
}

impl Parser for Identifier {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        map(info(ident), |(ident, info)| Self {
            value: ident.to_string(),
            info,
        })(input)
    }
}

impl Parser for Variable {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        let (input, ((mut variable, variable_info), accesses)) = pair(
            info(map(inc(None), Self::NamedVariable)),
            many0(info(delimited(
                symbols::lbracket,
                expect(
                    reference(inc::<Expression>(None)),
                    ParseErrorMessage::ExpectedToken("expression".to_string()),
                ),
                expect(symbols::rbracket, ParseErrorMessage::MissingClosing(']')),
            ))),
        )(input)?;
        for access in accesses {
            let (index, mut index_info) = access;
            index_info.extend_range(&variable_info);
            variable = Self::ArrayAccess(ArrayAccess {
                array: Box::new(variable),
                index: index.map(Box::new),
                info: index_info,
            });
        }
        Ok((input, variable))
    }
}

impl Parser for Expression {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        fn parse_bracketed(input: TokenStream) -> IResult<Expression> {
            // Bracketed := "(" Expr ")"
            let (input, (((_, lparen_info), expr, _), info)) = info(tuple((
                info(symbols::lparen),
                expect(
                    parse_comparison, // directly go into comparison to prevent re-referencing
                    ParseErrorMessage::ExpectedToken("expression".to_string()),
                ),
                expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
            )))(input)?;
            let error_pos = lparen_info.to_range().end;
            let expr =
                expr.unwrap_or_else(|| Expression::Error(AstInfo::new(error_pos..error_pos)));
            let bracketed = Expression::Bracketed(BracketedExpression {
                expr: Box::new(expr),
                info,
            });
            Ok((input, bracketed))
        }

        fn parse_primary(input: TokenStream) -> IResult<Expression> {
            // Primary := IntLit | Variable | Bracketed
            alt((
                map(inc::<IntLiteral>(None), Expression::IntLiteral),
                map(inc::<Variable>(None), Expression::Variable),
                parse_bracketed,
            ))(input)
        }

        fn parse_unary(input: TokenStream) -> IResult<Expression> {
            // Unary := "-" Primary
            let (input, (primary, info)) = info(preceded(symbols::minus, parse_factor))(input)?;
            let expr = Expression::Unary(UnaryExpression {
                operator: Operator::Sub,
                expr: Box::new(primary),
                info,
            });
            Ok((input, expr))
        }

        fn parse_factor(input: TokenStream) -> IResult<Expression> {
            // Factor := Primary | Unary
            alt((parse_primary, parse_unary))(input)
        }

        fn parse_rhs<P>(
            input: TokenStream,
            lhs: Expression,
            operator: Operator,
            parser: P,
        ) -> IResult<Expression>
        where
            P: Fn(TokenStream) -> IResult<Expression>,
        {
            let (input, rhs) = expect(
                parser,
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            )(input)?;
            let error_pos = lhs.to_range().end;
            let rhs = rhs.unwrap_or_else(|| Expression::Error(AstInfo::new(error_pos..error_pos)));
            // all errors are stored in lhs and rhs expressions.
            // Operators cannot lead to errors.
            let expr_start = lhs.to_range().start;
            let expr_end = rhs.to_range().end;
            let info = AstInfo::new(expr_start..expr_end);
            let exp = Expression::Binary(BinaryExpression {
                operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                info,
            });
            Ok((input, exp))
        }

        fn parse_mul(input: TokenStream) -> IResult<Expression> {
            // Mul := Factor (("*" | "/") Factor)*
            let (mut input, mut exp) = parse_factor(input)?;
            while let Ok((i, op)) = alt((symbols::times, symbols::divide))(input.clone()) {
                (input, exp) = parse_rhs(
                    i,
                    exp,
                    op.token_type
                        .try_into()
                        .expect("Operator conversion failed"),
                    parse_factor,
                )?;
            }
            Ok((input, exp))
        }

        fn parse_add(input: TokenStream) -> IResult<Expression> {
            // Add := Mul (("+" | "-") Mul)*
            let (mut input, mut exp) = parse_mul(input)?;
            while let Ok((i, op)) = alt((symbols::plus, symbols::minus))(input.clone()) {
                (input, exp) = parse_rhs(
                    i,
                    exp,
                    op.token_type
                        .try_into()
                        .expect("Operator conversion failed"),
                    parse_mul,
                )?;
            }
            Ok((input, exp))
        }

        fn parse_comparison(input: TokenStream) -> IResult<Expression> {
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
                (input, exp) = parse_rhs(
                    i,
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

impl Parser for TypeExpression {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        fn parse_array_type(input: TokenStream) -> IResult<TypeExpression> {
            let (input, ((_, _, size, _, _, type_expr), info)) = info(tuple((
                keywords::array,
                expect(
                    symbols::lbracket,
                    ParseErrorMessage::ExpectedToken("[".to_string()),
                ),
                expect(
                    inc::<IntLiteral>(None),
                    ParseErrorMessage::ExpectedToken("int literal".to_string()),
                ),
                expect(symbols::rbracket, ParseErrorMessage::MissingClosing(']')),
                expect(
                    keywords::of,
                    ParseErrorMessage::ExpectedToken("of".to_string()),
                ),
                expect(
                    reference(inc::<TypeExpression>(None)),
                    ParseErrorMessage::ExpectedToken("type expression".to_string()),
                ),
            )))(input)?;
            Ok((
                input,
                TypeExpression::ArrayType {
                    size,
                    base_type: type_expr.map(Box::new),
                    info,
                },
            ))
        }

        alt((
            parse_array_type,
            map(inc::<Identifier>(None), Self::NamedType),
        ))(input)
    }
}

impl Parser for TypeDeclaration {
    fn parse(this: Option<Self>, input: TokenStream) -> IResult<Self> {
        affected(map(
            info(tuple((
                many0(comment),
                keywords::r#type,
                expect(
                    inc::<Identifier>(None),
                    ParseErrorMessage::ExpectedToken("identifier".to_string()),
                ),
                expect(
                    alt((
                        symbols::eq,
                        confusable(
                            symbols::assign,
                            ParseErrorMessage::ConfusedToken(
                                token::EQ.to_string(),
                                token::ASSIGN.to_string(),
                            ),
                        ),
                        confusable(
                            symbols::colon,
                            ParseErrorMessage::ConfusedToken(
                                token::EQ.to_string(),
                                token::COLON.to_string(),
                            ),
                        ),
                    )),
                    ParseErrorMessage::ExpectedToken(token::EQ.to_string()),
                ),
                expect(
                    reference(inc::<TypeExpression>(None)),
                    ParseErrorMessage::ExpectedToken("type expression".to_string()),
                ),
                expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic),
            ))),
            |((doc, _, name, _, type_expr, _), info)| Self {
                doc,
                name,
                type_expr,
                info,
            },
        ))(this, input)
    }
}

impl Parser for VariableDeclaration {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        fn parse_valid(input: TokenStream) -> IResult<VariableDeclaration> {
            let (input, ((doc, _, name, _, type_expr, _), info)) = info(tuple((
                many0(comment),
                keywords::var,
                expect(
                    inc::<Identifier>(None),
                    ParseErrorMessage::ExpectedToken("identifier".to_string()),
                ),
                expect(
                    alt((
                        symbols::colon,
                        confusable(
                            symbols::assign,
                            ParseErrorMessage::ConfusedToken(
                                token::COLON.to_string(),
                                token::ASSIGN.to_string(),
                            ),
                        ),
                        confusable(
                            symbols::eq,
                            ParseErrorMessage::ConfusedToken(
                                token::COLON.to_string(),
                                token::EQ.to_string(),
                            ),
                        ),
                    )),
                    ParseErrorMessage::ExpectedToken(token::COLON.to_string()),
                ),
                expect(
                    reference(inc::<TypeExpression>(None)),
                    ParseErrorMessage::ExpectedToken("type expression".to_string()),
                ),
                expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic),
            )))(input)?;
            Ok((
                input,
                VariableDeclaration::Valid {
                    doc,
                    name,
                    type_expr,
                    info,
                },
            ))
        }

        fn parse_error(input: TokenStream) -> IResult<VariableDeclaration> {
            let (input, (_, mut info)) = info(ignore_until1(peek(look_ahead::var_dec)))(input)?;
            let err = SplError(
                info.to_range(),
                ParseErrorMessage::ExpectedToken("variable declaration".to_string()).to_string(),
            );
            info.append_error(err);
            Ok((input, VariableDeclaration::Error(info)))
        }

        alt((parse_valid, parse_error))(input)
    }
}

impl Parser for ParameterDeclaration {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        fn parse_valid(input: TokenStream) -> IResult<ParameterDeclaration> {
            let (input, ((doc, (ref_kw, name), _, type_expr, _), info)) = info(tuple((
                many0(comment),
                alt((
                    map(
                        pair(
                            keywords::r#ref,
                            expect(
                                inc::<Identifier>(None),
                                ParseErrorMessage::ExpectedToken("identifier".to_string()),
                            ),
                        ),
                        |pair| (Some(pair.0), pair.1),
                    ),
                    map(inc::<Identifier>(None), |ident| (None, Some(ident))),
                )),
                expect(
                    symbols::colon,
                    ParseErrorMessage::ExpectedToken(token::COLON.to_string()),
                ),
                expect(
                    reference(inc::<TypeExpression>(None)),
                    ParseErrorMessage::ExpectedToken("type expression".to_string()),
                ),
                peek(alt((
                    recognize(symbols::rparen),
                    recognize(symbols::lcurly),
                    recognize(symbols::comma),
                    recognize(eof),
                ))),
            )))(input)?;
            let is_ref = ref_kw.is_some();
            Ok((
                input,
                ParameterDeclaration::Valid {
                    doc,
                    is_ref,
                    name,
                    type_expr,
                    info,
                },
            ))
        }

        fn parse_error(input: TokenStream) -> IResult<ParameterDeclaration> {
            let (input, (_, mut info)) = info(ignore_until0(peek(alt((
                recognize(symbols::rparen),
                recognize(symbols::lcurly),
                recognize(symbols::comma),
                recognize(eof),
            )))))(input)?;
            let err = SplError(
                info.to_range(),
                ParseErrorMessage::ExpectedToken("parameter declaration".to_string()).to_string(),
            );
            info.append_error(err);
            Ok((input, ParameterDeclaration::Error(info)))
        }

        alt((parse_valid, parse_error))(input)
    }
}

impl Parser for CallStatement {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        fn parse_call_expression(input: TokenStream) -> IResult<Expression> {
            alt((
                terminated(
                    inc::<Expression>(None),
                    peek(alt((
                        recognize(symbols::rparen),
                        recognize(symbols::semic),
                        recognize(symbols::comma),
                        recognize(eof),
                    ))),
                ),
                map(
                    info(ignore_until0(peek(alt((
                        recognize(symbols::rparen),
                        recognize(symbols::semic),
                        recognize(symbols::comma),
                        recognize(eof),
                    ))))),
                    |(_, mut info)| {
                        let err = SplError(
                            info.to_range(),
                            ParseErrorMessage::ExpectedToken("expression".to_string()).to_string(),
                        );
                        info.append_error(err);
                        Expression::Error(info)
                    },
                ),
            ))(input)
        }

        map(
            info(tuple((
                terminated(inc::<Identifier>(None), symbols::lparen),
                alt((
                    map(
                        peek(alt((
                            recognize(symbols::rparen),
                            recognize(symbols::semic),
                            recognize(eof),
                        ))),
                        |_| Vec::new(),
                    ),
                    parse_list(reference(parse_call_expression)),
                )),
                expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
                expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic),
            ))),
            |((name, arguments, _, _), info)| Self {
                name,
                arguments,
                info,
            },
        )(input)
    }
}

impl Parser for Assignment {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        let (input, ((variable, expr, _), info)) = info(tuple((
            terminated(
                inc::<Variable>(None),
                alt((
                    symbols::assign,
                    confusable(
                        symbols::eq,
                        ParseErrorMessage::ConfusedToken(
                            token::ASSIGN.to_string(),
                            token::EQ.to_string(),
                        ),
                    ),
                )),
            ),
            expect(
                reference(inc::<Expression>(None)),
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
            expect(symbols::semic, ParseErrorMessage::MissingTrailingSemic),
        )))(input)?;
        Ok((
            input,
            Self {
                variable,
                expr,
                info,
            },
        ))
    }
}

impl Parser for IfStatement {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        let (input, ((_, _, condition, _, if_branch, else_branch), info)) = info(tuple((
            keywords::r#if,
            expect(symbols::lparen, ParseErrorMessage::MissingOpening('(')),
            expect(
                reference(inc::<Expression>(None)),
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
            expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
            expect(
                reference(inc::<Statement>(None)),
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
            opt(preceded(
                keywords::r#else,
                expect(
                    reference(inc::<Statement>(None)),
                    ParseErrorMessage::ExpectedToken("statement".to_string()),
                ),
            )),
        )))(input)?;
        Ok((
            input,
            Self {
                condition,
                if_branch: if_branch.map(Box::new),
                else_branch: else_branch.flatten().map(Box::new),
                info,
            },
        ))
    }
}

impl Parser for WhileStatement {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        let (input, ((_, _, condition, _, stmt), info)) = info(tuple((
            keywords::r#while,
            expect(symbols::lparen, ParseErrorMessage::MissingOpening('(')),
            expect(
                reference(inc::<Expression>(None)),
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
            expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
            expect(
                reference(inc::<Statement>(None)),
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            ),
        )))(input)?;
        Ok((
            input,
            Self {
                condition,
                statement: stmt.map(Box::new),
                info,
            },
        ))
    }
}

impl Parser for BlockStatement {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        let (input, (statements, info)) = info(delimited(
            symbols::lcurly,
            many0(reference(inc::<Statement>(None))),
            expect(symbols::rcurly, ParseErrorMessage::MissingClosing('}')),
        ))(input)?;
        Ok((input, Self { statements, info }))
    }
}

impl Parser for Statement {
    fn parse(_this: Option<Self>, input: TokenStream) -> IResult<Self> {
        fn parse_error(input: TokenStream) -> IResult<Statement> {
            let (input, ((_, ignored), mut info)) = info(tuple((
                many0(comment),
                ignore_until1(peek(look_ahead::stmt)),
            )))(input)?;
            let err = SplError(
                info.to_range(),
                ParseErrorMessage::UnexpectedCharacters(
                    ignored.iter().map(|token| token.to_string()).collect(),
                )
                .to_string(),
            );
            info.append_error(err);
            let stmt = Statement::Error(info);
            Ok((input, stmt))
        }

        alt((
            map(info(symbols::semic), |(_, info)| Self::Empty(info)),
            map(inc::<IfStatement>(None), Self::If),
            map(inc::<WhileStatement>(None), Self::While),
            map(inc::<BlockStatement>(None), Self::Block),
            map(inc::<CallStatement>(None), Self::Call),
            map(inc::<Assignment>(None), Self::Assignment),
            parse_error,
        ))(input)
    }
}

impl Parser for ProcedureDeclaration {
    fn parse(this: Option<Self>, input: TokenStream) -> IResult<Self> {
        affected(map(
            info(tuple((
                many0(comment),
                keywords::proc,
                expect(
                    inc::<Identifier>(None),
                    ParseErrorMessage::ExpectedToken("identifier".to_string()),
                ),
                expect(symbols::lparen, ParseErrorMessage::MissingOpening('(')),
                alt((
                    map(
                        peek(alt((
                            recognize(symbols::rparen),
                            recognize(symbols::lcurly),
                            recognize(eof),
                        ))),
                        |_| Vec::new(),
                    ),
                    parse_list(reference(inc::<ParameterDeclaration>(None))),
                )),
                expect(symbols::rparen, ParseErrorMessage::MissingClosing(')')),
                expect(symbols::lcurly, ParseErrorMessage::MissingOpening('{')),
                many0(reference(inc::<VariableDeclaration>(None))),
                many0(reference(inc::<Statement>(None))),
                expect(symbols::rcurly, ParseErrorMessage::MissingClosing('}')),
            ))),
            |((doc, _, name, _, parameters, _, _, variable_declarations, statements, _), info)| {
                Self {
                    doc,
                    name,
                    parameters,
                    variable_declarations,
                    statements,
                    info,
                }
            },
        ))(this, input)
    }
}

impl Parser for GlobalDeclaration {
    fn parse(this: Option<Self>, input: TokenStream) -> IResult<Self> {
        fn parse_error(input: TokenStream) -> IResult<GlobalDeclaration> {
            let (input, (ignored, mut info)) = info(ignore_until1(peek(alt((
                recognize(keywords::r#type),
                recognize(keywords::proc),
                recognize(eof),
            )))))(input)?;
            let err = SplError(
                info.to_range(),
                ParseErrorMessage::UnexpectedCharacters(
                    ignored.iter().map(|token| token.to_string()).collect(),
                )
                .to_string(),
            );
            info.append_error(err);
            let gd = GlobalDeclaration::Error(info);
            Ok((input, gd))
        }

        match this {
            Some(GlobalDeclaration::Type(td)) => {
                map(inc::<TypeDeclaration>(Some(td)), Self::Type)(input)
            }
            Some(GlobalDeclaration::Procedure(pd)) => {
                map(inc::<ProcedureDeclaration>(Some(pd)), Self::Procedure)(input)
            }
            _ => affected(alt((
                map(inc::<TypeDeclaration>(None), Self::Type),
                map(inc::<ProcedureDeclaration>(None), Self::Procedure),
                parse_error,
            )))(this, input),
        }
    }
}

impl Parser for Program {
    fn parse(this: Option<Self>, input: TokenStream) -> IResult<Self> {
        // early return on no change
        if input.token_change.insertion_len == 0 && input.token_change.deletion_range.is_empty() {
            return Ok((
                input,
                this.unwrap_or_else(|| Program {
                    global_declarations: Vec::new(),
                    info: AstInfo::new(0..0),
                }),
            ));
        }

        let result = map(
            terminated(
                info(many(
                    this.clone().map(|program| program.global_declarations),
                )),
                all_consuming(eof),
            ),
            |(global_declarations, info)| Self {
                global_declarations,
                info,
            },
        )(input);
        result
    }
}

impl<T: Parser> Parser for Reference<T> {
    fn parse(this: Option<Self>, mut input: TokenStream) -> IResult<Self> {
        let reference_backup = input.reference_pos;
        if let Some(this) = &this {
            input.old_reference_pos = if this.offset < input.old_reference_pos {
                input.old_reference_pos + this.offset
            } else {
                this.offset
            };
        }
        input.reference_pos = input.location_offset();
        let offset = input.reference_pos - reference_backup;
        let inner_this = this.map(|r| r.reference);
        match T::parse(inner_this, input) {
            Ok((mut input, out)) => {
                // recover backup
                input.reference_pos = reference_backup;
                Ok((
                    input,
                    Reference {
                        reference: out,
                        offset,
                    },
                ))
            }
            Err(nom::Err::Failure(mut err) | nom::Err::Error(mut err)) => {
                // recover backup
                err.input.reference_pos = reference_backup;
                Err(nom::Err::Error(err))
            }
            Err(_) => panic!("Incomplete data"),
        }
    }
}

// Comment parser is separated from the other token parsers,
// because it is used in the `tag_parser` macro
fn comment(input: TokenStream) -> IResult<String> {
    let err = Err(nom::Err::Error(crate::error::ParserError {
        input: input.clone(),
        kind: crate::error::ParserErrorKind::Token,
    }));
    let (input, tokens) = take(1usize)(input)?;
    let token = match tokens.fragment() {
        Some(token) => token.clone(),
        None => return err,
    };
    if let TokenType::Comment(comment) = token.token_type {
        Ok((input, comment))
    } else {
        err
    }
}

macro_rules! tag_parser {
    ($name:ident, $token_type:pat) => {
        pub fn $name(
            input: crate::lexer::token::TokenStream,
        ) -> crate::parser::IResult<crate::lexer::token::Token> {
            use crate::{lexer::token::TokenType, parser::comment};
            use nom::{bytes::complete::take, multi::many0};
            let err = Err(nom::Err::Error(crate::error::ParserError {
                input: input.clone(),
                kind: crate::error::ParserErrorKind::Token,
            }));
            // Consume comments in front of any `tag_parser`
            let (input, _) = many0(comment)(input)?;
            let (input, tokens) = take(1usize)(input)?;
            let token = match tokens.fragment() {
                Some(token) => token.clone(),
                None => return err,
            };
            if matches!(token.token_type, $token_type) {
                Ok((input, token))
            } else {
                err
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

mod look_ahead {
    use super::{eof, inc, keywords, symbols};
    use crate::ast::Identifier;
    use nom::{branch::alt, sequence::pair};

    macro_rules! look_ahead_parser {
        ($name:ident, $($parser:expr, )+) => {
        pub fn $name(
            input: crate::token::TokenStream,
        ) -> crate::parser::IResult<crate::token::TokenStream> {
            nom::branch::alt((
                $(
                    nom::combinator::recognize($parser),
                )+
            ))(input)
        }
        };
    }

    look_ahead_parser!(
        stmt,
        symbols::lcurly,
        symbols::rcurly,
        symbols::semic,
        keywords::r#if,
        keywords::r#while,
        pair(
            inc::<Identifier>(None),
            alt((symbols::assign, symbols::lparen, symbols::lbracket))
        ),
        keywords::proc,
        keywords::r#type,
        eof,
    );
    look_ahead_parser!(var_dec, keywords::var, stmt, eof,);
}
