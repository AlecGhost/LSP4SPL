use crate::{
    ast::*,
    error::{ParseErrorMessage, ParserError, SplError},
    lexer::token::{IntResult, Token, TokenStream, TokenType},
    parser::utility::{
        affected, confusable, expect, ignore_until0, ignore_until1, info, many, parse_list,
    },
    token, ToRange,
};
use nom::{
    branch::alt,
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

/// Incrementally updates the AST based on the changed tokens.
///
/// # Panics
///
/// Panics if updating fails.
pub fn update(program: Program, input: TokenStream) -> Program {
    let (_, program) = Program::parse(Some(&program), input).expect("Parser cannot fail");
    program
}

/// Try to parse token stream.
/// Implemented by all AST nodes.
trait Parser: Sized {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self>;
}

fn inc<'a, F, O>(mut f: F) -> impl FnMut(Option<&O>, TokenStream<'a>) -> IResult<'a, O>
where
    F: FnMut(TokenStream<'a>) -> IResult<'a, O>,
{
    move |_, input| f(input)
}

impl Parser for IntLiteral {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        affected(
            this,
            map(
                info(alt((
                    map(literals::hex, |token| {
                        if let TokenType::Hex(hex_result) = token.token_type {
                            match hex_result {
                                IntResult::Int(i) => Some(i),
                                IntResult::Err(_) => None,
                            }
                        } else {
                            panic!("Invalid hex parse")
                        }
                    }),
                    map(literals::char, |token| {
                        if let TokenType::Char(c) = token.token_type {
                            Some((c as u8).into())
                        } else {
                            panic!("Invalid char parse")
                        }
                    }),
                    map(literals::int, |token| {
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
            ),
        )(input)
    }
}

impl Parser for Identifier {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        affected(
            this,
            map(info(literals::ident), |(ident, info)| Self {
                value: ident.to_string(),
                info,
            }),
        )(input)
    }
}

impl Parser for Variable {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        affected(this, |input| {
            let (input, ((mut variable, variable_info), accesses)) = pair(
                info(map(
                    |input| Identifier::parse(None, input),
                    Self::NamedVariable,
                )),
                many0(info(delimited(
                    symbols::lbracket,
                    expect(
                        None,
                        Reference::<Expression>::parse,
                        ParseErrorMessage::ExpectedToken("expression".to_string()),
                    ),
                    expect(
                        None,
                        inc(symbols::rbracket),
                        ParseErrorMessage::MissingClosing(']'),
                    ),
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
        })(input)
    }
}

impl Parser for Expression {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        fn parse_bracketed(input: TokenStream) -> IResult<Expression> {
            // Bracketed := "(" Expr ")"
            let (input, (((_, lparen_info), expr, _), info)) = info(tuple((
                info(symbols::lparen),
                expect(
                    None,
                    inc(parse_comparison), // directly go into comparison to prevent re-referencing
                    ParseErrorMessage::ExpectedToken("expression".to_string()),
                ),
                expect(
                    None,
                    inc(symbols::rparen),
                    ParseErrorMessage::MissingClosing(')'),
                ),
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
                map(
                    |input| IntLiteral::parse(None, input),
                    Expression::IntLiteral,
                ),
                map(|input| Variable::parse(None, input), Expression::Variable),
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
                None,
                inc(parser),
                ParseErrorMessage::ExpectedToken("expression".to_string()),
            )(input)?;
            let pos = input.location_offset() - input.reference_pos;
            let error_pos = if pos > 0 { pos - 1 } else { 0 };
            let rhs = rhs.unwrap_or_else(|| Expression::Error(AstInfo::new(error_pos..error_pos)));
            // all errors are stored in lhs and rhs expressions.
            // Operators cannot lead to errors.
            let expr_start = lhs.to_range().start;
            let expr_end = pos;
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
        affected(this, parse_comparison)(input)
    }
}

impl Parser for TypeExpression {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        fn parse_array_type<'a>(
            this: Option<&TypeExpression>,
            input: TokenStream<'a>,
        ) -> IResult<'a, TypeExpression> {
            let (size, base_type) = match this {
                Some(TypeExpression::ArrayType {
                    size, base_type, ..
                }) => (size.as_ref(), base_type.as_ref().map(|r| r.as_ref())),
                None => (None, None),
                _ => panic!("Must be array type"),
            };

            affected(
                this,
                map(
                    info(tuple((
                        keywords::array,
                        expect(
                            None,
                            inc(symbols::lbracket),
                            ParseErrorMessage::ExpectedToken("[".to_string()),
                        ),
                        expect(
                            size,
                            IntLiteral::parse,
                            ParseErrorMessage::ExpectedToken("int literal".to_string()),
                        ),
                        expect(
                            None,
                            inc(symbols::rbracket),
                            ParseErrorMessage::MissingClosing(']'),
                        ),
                        expect(
                            None,
                            inc(keywords::of),
                            ParseErrorMessage::ExpectedToken("of".to_string()),
                        ),
                        expect(
                            base_type,
                            Reference::<TypeExpression>::parse,
                            ParseErrorMessage::ExpectedToken("type expression".to_string()),
                        ),
                    ))),
                    |((_, _, size, _, _, type_expr), info)| TypeExpression::ArrayType {
                        size,
                        base_type: type_expr.map(Box::new),
                        info,
                    },
                ),
            )(input)
        }

        match this {
            Some(Self::NamedType(name)) => map(
                |input| Identifier::parse(Some(name), input),
                Self::NamedType,
            )(input),
            Some(Self::ArrayType { .. }) => parse_array_type(this, input),
            None => alt((
                |input| parse_array_type(None, input),
                map(|input| Identifier::parse(None, input), Self::NamedType),
            ))(input),
        }
    }
}

impl Parser for TypeDeclaration {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        affected(
            this,
            map(
                info(tuple((
                    many0(comment),
                    keywords::r#type,
                    expect(
                        this.and_then(|td| td.name.as_ref()),
                        Identifier::parse,
                        ParseErrorMessage::ExpectedToken("identifier".to_string()),
                    ),
                    expect(
                        None,
                        inc(alt((
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
                        ))),
                        ParseErrorMessage::ExpectedToken(token::EQ.to_string()),
                    ),
                    expect(
                        this.and_then(|td| td.type_expr.as_ref()),
                        Reference::<TypeExpression>::parse,
                        ParseErrorMessage::ExpectedToken("type expression".to_string()),
                    ),
                    expect(
                        None,
                        inc(symbols::semic),
                        ParseErrorMessage::MissingTrailingSemic,
                    ),
                ))),
                |((doc, _, name, _, type_expr, _), info)| Self {
                    doc,
                    name,
                    type_expr,
                    info,
                },
            ),
        )(input)
    }
}

impl Parser for VariableDeclaration {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        fn parse_valid<'a>(
            this: Option<&VariableDeclaration>,
            input: TokenStream<'a>,
        ) -> IResult<'a, VariableDeclaration> {
            let (name, type_expr) = match this {
                Some(VariableDeclaration::Valid {
                    name, type_expr, ..
                }) => (name.as_ref(), type_expr.as_ref()),
                None => (None, None),
                _ => panic!("Must be valid variable declaration"),
            };

            affected(
                this,
                map(
                    info(tuple((
                        many0(comment),
                        keywords::var,
                        expect(
                            name,
                            Identifier::parse,
                            ParseErrorMessage::ExpectedToken("identifier".to_string()),
                        ),
                        expect(
                            None,
                            inc(alt((
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
                            ))),
                            ParseErrorMessage::ExpectedToken(token::COLON.to_string()),
                        ),
                        expect(
                            type_expr,
                            Reference::<TypeExpression>::parse,
                            ParseErrorMessage::ExpectedToken("type expression".to_string()),
                        ),
                        expect(
                            None,
                            inc(symbols::semic),
                            ParseErrorMessage::MissingTrailingSemic,
                        ),
                    ))),
                    |((doc, _, name, _, type_expr, _), info)| VariableDeclaration::Valid {
                        doc,
                        name,
                        type_expr,
                        info,
                    },
                ),
            )(input)
        }

        fn parse_error(input: TokenStream) -> IResult<VariableDeclaration> {
            let (input, (_, mut info)) = info(ignore_until1(peek(look_ahead::var_dec)))(input)?;
            let err = SplError(
                info.to_range(),
                ParseErrorMessage::ExpectedToken("variable declaration".to_string()).into(),
            );
            info.append_error(err);
            Ok((input, VariableDeclaration::Error(info)))
        }

        match this {
            Some(Self::Valid { .. }) => parse_valid(this, input),
            _ => alt((|input| parse_valid(None, input), parse_error))(input),
        }
    }
}

impl Parser for ParameterDeclaration {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        fn parse_valid<'a>(
            this: Option<&ParameterDeclaration>,
            input: TokenStream<'a>,
        ) -> IResult<'a, ParameterDeclaration> {
            let (name, type_expr) = match this {
                Some(ParameterDeclaration::Valid {
                    name, type_expr, ..
                }) => (name.as_ref(), type_expr.as_ref()),
                None => (None, None),
                _ => panic!("Must be a valid parameter"),
            };

            let parser = map(
                info(tuple((
                    many0(comment),
                    alt((
                        map(
                            pair(
                                keywords::r#ref,
                                expect(
                                    name,
                                    Identifier::parse,
                                    ParseErrorMessage::ExpectedToken("identifier".to_string()),
                                ),
                            ),
                            |pair| (Some(pair.0), pair.1),
                        ),
                        map(
                            |input| Identifier::parse(name, input),
                            |ident| (None, Some(ident)),
                        ),
                    )),
                    expect(
                        None,
                        inc(symbols::colon),
                        ParseErrorMessage::ExpectedToken(token::COLON.to_string()),
                    ),
                    expect(
                        type_expr,
                        Reference::<TypeExpression>::parse,
                        ParseErrorMessage::ExpectedToken("type expression".to_string()),
                    ),
                    peek(look_ahead::param_dec),
                ))),
                |((doc, (ref_kw, name), _, type_expr, _), info)| {
                    let is_ref = ref_kw.is_some();
                    ParameterDeclaration::Valid {
                        doc,
                        is_ref,
                        name,
                        type_expr,
                        info,
                    }
                },
            )(input);
            parser
        }

        fn parse_error(input: TokenStream) -> IResult<ParameterDeclaration> {
            let (input, (_, mut info)) = info(ignore_until0(peek(look_ahead::param_dec)))(input)?;
            let err = SplError(
                info.to_range(),
                ParseErrorMessage::ExpectedToken("parameter declaration".to_string()).into(),
            );
            info.append_error(err);
            Ok((input, ParameterDeclaration::Error(info)))
        }

        match this {
            Some(Self::Valid { .. }) => {
                affected(this, alt((|input| parse_valid(this, input), parse_error)))(input)
            }
            _ => alt((|input| parse_valid(None, input), parse_error))(input),
        }
    }
}

#[derive(Clone, Debug)]
enum Argument {
    Valid(Expression),
    Error(Expression),
}

impl ToRange for Argument {
    fn to_range(&self) -> std::ops::Range<usize> {
        match self {
            Self::Valid(expr) => expr.to_range(),
            Self::Error(expr) => expr.to_range(),
        }
    }
}

impl Parser for Argument {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        fn parse_valid<'a>(
            this: Option<&Expression>,
            input: TokenStream<'a>,
        ) -> IResult<'a, Expression> {
            terminated(
                |input| Expression::parse(this, input),
                peek(look_ahead::arg),
            )(input)
        }

        fn parse_error(input: TokenStream) -> IResult<Expression> {
            map(
                info(ignore_until0(peek(look_ahead::arg))),
                |(_, mut info)| {
                    let err = SplError(
                        info.to_range(),
                        ParseErrorMessage::ExpectedToken("expression".to_string()).into(),
                    );
                    info.append_error(err);
                    Expression::Error(info)
                },
            )(input)
        }

        let (input, expr) = match this {
            Some(Self::Valid(expr)) => affected(
                Some(expr),
                alt((|input| parse_valid(Some(expr), input), parse_error)),
            )(input)?,
            _ => alt((|input| parse_valid(None, input), parse_error))(input)?,
        };
        Ok((input, expr.into()))
    }
}

impl From<Expression> for Argument {
    fn from(value: Expression) -> Self {
        if matches!(value, Expression::Error(_)) {
            Self::Error(value)
        } else {
            Self::Valid(value)
        }
    }
}

impl From<Argument> for Expression {
    fn from(value: Argument) -> Self {
        match value {
            Argument::Valid(expr) | Argument::Error(expr) => expr,
        }
    }
}

impl Parser for CallStatement {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        affected(
            this,
            map(
                info(tuple((
                    terminated(
                        |input| Identifier::parse(this.map(|c| &c.name), input),
                        symbols::lparen,
                    ),
                    alt((
                        map(
                            peek(alt((
                                recognize(symbols::rparen),
                                recognize(symbols::semic),
                                recognize(markers::eof),
                            ))),
                            |_| Vec::new(),
                        ),
                        map(
                            parse_list::<Argument>(
                                this.map(|c| {
                                    c.arguments
                                        .iter()
                                        .map(|arg| Reference {
                                            reference: arg.reference.clone().into(),
                                            offset: arg.offset,
                                        })
                                        .collect::<Vec<_>>()
                                })
                                .as_deref(),
                            ),
                            |args| {
                                args.into_iter()
                                    .map(|arg| Reference {
                                        reference: arg.reference.into(),
                                        offset: arg.offset,
                                    })
                                    .collect()
                            },
                        ),
                    )),
                    expect(
                        None,
                        inc(symbols::rparen),
                        ParseErrorMessage::MissingClosing(')'),
                    ),
                    expect(
                        None,
                        inc(symbols::semic),
                        ParseErrorMessage::MissingTrailingSemic,
                    ),
                ))),
                |((name, arguments, _, _), info)| Self {
                    name,
                    arguments,
                    info,
                },
            ),
        )(input)
    }
}

impl Parser for Assignment {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        affected(
            this,
            map(
                info(tuple((
                    terminated(
                        |input| Variable::parse(this.map(|assignment| &assignment.variable), input),
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
                        this.and_then(|assignment| assignment.expr.as_ref()),
                        Reference::<Expression>::parse,
                        ParseErrorMessage::ExpectedToken("expression".to_string()),
                    ),
                    expect(
                        None,
                        inc(symbols::semic),
                        ParseErrorMessage::MissingTrailingSemic,
                    ),
                ))),
                |((variable, expr, _), info)| Self {
                    variable,
                    expr,
                    info,
                },
            ),
        )(input)
    }
}

impl Parser for IfStatement {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        affected(
            this,
            map(
                info(tuple((
                    keywords::r#if,
                    expect(
                        None,
                        inc(symbols::lparen),
                        ParseErrorMessage::MissingOpening('('),
                    ),
                    expect(
                        this.and_then(|if_stmt| if_stmt.condition.as_ref()),
                        Reference::<Expression>::parse,
                        ParseErrorMessage::ExpectedToken("expression".to_string()),
                    ),
                    expect(
                        None,
                        inc(symbols::rparen),
                        ParseErrorMessage::MissingClosing(')'),
                    ),
                    expect(
                        this.and_then(|if_stmt| if_stmt.if_branch.as_ref().map(|r| r.as_ref())),
                        Reference::<Statement>::parse,
                        ParseErrorMessage::ExpectedToken("expression".to_string()),
                    ),
                    opt(preceded(
                        keywords::r#else,
                        expect(
                            this.and_then(|if_stmt| {
                                if_stmt.else_branch.as_ref().map(|r| r.as_ref())
                            }),
                            Reference::<Statement>::parse,
                            ParseErrorMessage::ExpectedToken("statement".to_string()),
                        ),
                    )),
                ))),
                |((_, _, condition, _, if_branch, else_branch), info)| Self {
                    condition,
                    if_branch: if_branch.map(Box::new),
                    else_branch: else_branch.flatten().map(Box::new),
                    info,
                },
            ),
        )(input)
    }
}

impl Parser for WhileStatement {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        affected(
            this,
            map(
                info(tuple((
                    keywords::r#while,
                    expect(
                        None,
                        inc(symbols::lparen),
                        ParseErrorMessage::MissingOpening('('),
                    ),
                    expect(
                        this.and_then(|while_stmt| while_stmt.condition.as_ref()),
                        Reference::<Expression>::parse,
                        ParseErrorMessage::ExpectedToken("expression".to_string()),
                    ),
                    expect(
                        None,
                        inc(symbols::rparen),
                        ParseErrorMessage::MissingClosing(')'),
                    ),
                    expect(
                        this.and_then(|while_stmt| {
                            while_stmt.statement.as_ref().map(|r| r.as_ref())
                        }),
                        Reference::<Statement>::parse,
                        ParseErrorMessage::ExpectedToken("expression".to_string()),
                    ),
                ))),
                |((_, _, condition, _, stmt), info)| Self {
                    condition,
                    statement: stmt.map(Box::new),
                    info,
                },
            ),
        )(input)
    }
}

impl Parser for BlockStatement {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        affected(
            this,
            map(
                info(delimited(
                    symbols::lcurly,
                    many(this.map(|block_stmt| block_stmt.statements.as_slice())),
                    expect(
                        None,
                        inc(symbols::rcurly),
                        ParseErrorMessage::MissingClosing('}'),
                    ),
                )),
                |(statements, info)| Self { statements, info },
            ),
        )(input)
    }
}

impl Parser for Statement {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
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
                .into(),
            );
            info.append_error(err);
            let stmt = Statement::Error(info);
            Ok((input, stmt))
        }

        match this {
            Some(Self::If(stmt)) => {
                map(|input| IfStatement::parse(Some(stmt), input), Self::If)(input)
            }
            Some(Self::While(stmt)) => map(
                |input| WhileStatement::parse(Some(stmt), input),
                Self::While,
            )(input),
            Some(Self::Assignment(stmt)) => map(
                |input| Assignment::parse(Some(stmt), input),
                Self::Assignment,
            )(input),
            Some(Self::Call(stmt)) => {
                map(|input| CallStatement::parse(Some(stmt), input), Self::Call)(input)
            }
            Some(Self::Block(stmt)) => map(
                |input| BlockStatement::parse(Some(stmt), input),
                Self::Block,
            )(input),
            _ => alt((
                map(info(symbols::semic), |(_, info)| Self::Empty(info)),
                map(|input| IfStatement::parse(None, input), Self::If),
                map(|input| WhileStatement::parse(None, input), Self::While),
                map(|input| BlockStatement::parse(None, input), Self::Block),
                map(|input| CallStatement::parse(None, input), Self::Call),
                map(|input| Assignment::parse(None, input), Self::Assignment),
                parse_error,
            ))(input),
        }
    }
}

impl Parser for ProcedureDeclaration {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        affected(
            this,
            map(
                info(tuple((
                    many0(comment),
                    keywords::proc,
                    expect(
                        this.and_then(|pd| pd.name.as_ref()),
                        Identifier::parse,
                        ParseErrorMessage::ExpectedToken("identifier".to_string()),
                    ),
                    expect(
                        None,
                        inc(symbols::lparen),
                        ParseErrorMessage::MissingOpening('('),
                    ),
                    alt((
                        map(
                            peek(alt((
                                recognize(symbols::rparen),
                                recognize(symbols::lcurly),
                                recognize(markers::eof),
                            ))),
                            |_| Vec::new(),
                        ),
                        parse_list::<ParameterDeclaration>(this.map(|pd| pd.parameters.as_slice())),
                    )),
                    expect(
                        None,
                        inc(symbols::rparen),
                        ParseErrorMessage::MissingClosing(')'),
                    ),
                    expect(
                        None,
                        inc(symbols::lcurly),
                        ParseErrorMessage::MissingOpening('{'),
                    ),
                    many(this.map(|pd| pd.variable_declarations.as_slice())),
                    many(this.map(|pd| pd.statements.as_slice())),
                    expect(
                        None,
                        inc(symbols::rcurly),
                        ParseErrorMessage::MissingClosing('}'),
                    ),
                ))),
                |(
                    (doc, _, name, _, parameters, _, _, variable_declarations, statements, _),
                    info,
                )| {
                    Self {
                        doc,
                        name,
                        parameters,
                        variable_declarations,
                        statements,
                        info,
                    }
                },
            ),
        )(input)
    }
}

impl Parser for GlobalDeclaration {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        fn parse_error(input: TokenStream) -> IResult<GlobalDeclaration> {
            let (input, (ignored, mut info)) =
                info(ignore_until1(peek(look_ahead::global_dec)))(input)?;
            let err = SplError(
                info.to_range(),
                ParseErrorMessage::UnexpectedCharacters(
                    ignored.iter().map(|token| token.to_string()).collect(),
                )
                .into(),
            );
            info.append_error(err);
            let gd = GlobalDeclaration::Error(info);
            Ok((input, gd))
        }

        match this {
            Some(Self::Type(td)) => {
                map(|input| TypeDeclaration::parse(Some(td), input), Self::Type)(input)
            }
            Some(Self::Procedure(pd)) => map(
                |input| ProcedureDeclaration::parse(Some(pd), input),
                Self::Procedure,
            )(input),
            _ => alt((
                map(|input| TypeDeclaration::parse(None, input), Self::Type),
                map(
                    |input| ProcedureDeclaration::parse(None, input),
                    Self::Procedure,
                ),
                parse_error,
            ))(input),
        }
    }
}

impl Parser for Program {
    fn parse<'a>(this: Option<&Self>, input: TokenStream<'a>) -> IResult<'a, Self> {
        map(
            terminated(
                info(many(
                    this.map(|program| program.global_declarations.as_slice()),
                )),
                all_consuming(markers::eof),
            ),
            |(global_declarations, info)| Self {
                global_declarations,
                info,
            },
        )(input)
    }
}

impl<T: Parser> Parser for Reference<T> {
    fn parse<'a>(this: Option<&Self>, mut input: TokenStream<'a>) -> IResult<'a, Self> {
        let reference_backup = input.reference_pos;
        let some_this = this.is_some();
        if let Some(this) = &this {
            input.inc_references.push(this.offset);
        }
        input.reference_pos = input.location_offset();
        let offset = input.reference_pos - reference_backup;
        let inner_this = this.map(|r| r.as_ref());
        match T::parse(inner_this, input) {
            Ok((mut input, out)) => {
                // recover backup
                input.reference_pos = reference_backup;
                if some_this {
                    input.inc_references.pop();
                }
                Ok((
                    input,
                    Self {
                        reference: out,
                        offset,
                    },
                ))
            }
            Err(nom::Err::Error(mut err)) => {
                // recover backup
                err.input.reference_pos = reference_backup;
                err.input.inc_references.pop();
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

mod literals {
    tag_parser!(ident, TokenType::Ident(_));
    tag_parser!(char, TokenType::Char(_));
    tag_parser!(int, TokenType::Int(_));
    tag_parser!(hex, TokenType::Hex(_));
}

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

mod markers {
    tag_parser!(eof, TokenType::Eof);
}

mod look_ahead {
    use super::{keywords, markers::eof, symbols, Parser};
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

    look_ahead_parser!(global_dec, keywords::proc, keywords::r#type, eof,);
    look_ahead_parser!(
        stmt,
        symbols::lcurly,
        symbols::rcurly,
        symbols::semic,
        keywords::r#if,
        keywords::r#while,
        pair(
            |input| Identifier::parse(None, input),
            alt((symbols::assign, symbols::lparen))
        ),
        global_dec,
    );
    look_ahead_parser!(
        var_dec,
        keywords::var,
        stmt,
        pair(
            |input| Identifier::parse(None, input),
            alt((symbols::lbracket, symbols::eq, symbols::colon))
        ),
    );
    look_ahead_parser!(param_dec, symbols::rparen, symbols::comma, var_dec,);
    look_ahead_parser!(arg, param_dec,);
}
