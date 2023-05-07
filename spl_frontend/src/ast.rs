//! Contains structs and enums for all AST nodes
use crate::{
    error::OperatorConversionError,
    lexer::token::{Token, TokenType},
    ToRange,
};
use spl_frontend_macros::ToRange;
use std::{fmt::Display, ops::Range};

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct AstInfo {
    pub tokens: Vec<Token>,
}

impl ToRange for AstInfo {
    fn to_range(&self) -> Range<usize> {
        self.tokens.to_range()
    }
}

impl ToRange for Range<usize> {
    fn to_range(&self) -> Range<usize> {
        self.clone()
    }
}

impl AstInfo {
    pub fn new(tokens: &[Token]) -> Self {
        Self {
            tokens: tokens.to_vec(),
        }
    }

    pub fn empty() -> Self {
        Self { tokens: Vec::new() }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, ToRange)]
pub struct IntLiteral {
    pub value: Option<u32>,
    pub info: AstInfo,
}

impl IntLiteral {
    pub const fn new(value: u32, info: AstInfo) -> Self {
        Self {
            value: Some(value),
            info,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, ToRange)]
pub struct Identifier {
    pub value: String,
    pub info: AstInfo,
}

impl Identifier {
    pub fn new(value: String, tokens: &[Token]) -> Self {
        Self {
            value,
            info: AstInfo::new(tokens),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct ArrayAccess {
    pub array: Box<Variable>,
    pub index: Option<Box<Expression>>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub enum Variable {
    NamedVariable(Identifier),
    ArrayAccess(ArrayAccess),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Equ, // =
    Neq, // #
    Lst, // <
    Lse, // <=
    Grt, // >
    Gre, // >=
}

impl Operator {
    pub fn new(symbol: &str) -> Option<Self> {
        match symbol {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "=" => Some(Self::Equ),
            "#" => Some(Self::Neq),
            "<" => Some(Self::Lst),
            "<=" => Some(Self::Lse),
            ">" => Some(Self::Grt),
            ">=" => Some(Self::Gre),
            _ => None,
        }
    }

    pub const fn is_arithmetic(&self) -> bool {
        matches!(self, Self::Add | Self::Sub | Self::Mul | Self::Div)
    }
}

impl TryFrom<TokenType> for Operator {
    type Error = OperatorConversionError<TokenType>;
    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        use TokenType::*;
        match value {
            Plus => Ok(Self::Add),
            Minus => Ok(Self::Sub),
            Times => Ok(Self::Mul),
            Divide => Ok(Self::Div),
            Eq => Ok(Self::Equ),
            Neq => Ok(Self::Neq),
            Lt => Ok(Self::Lst),
            Le => Ok(Self::Lse),
            Gt => Ok(Self::Grt),
            Ge => Ok(Self::Gre),
            token => Err(OperatorConversionError::new(token)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct BinaryExpression {
    pub operator: Operator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct BracketedExpression {
    pub expr: Box<Expression>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct UnaryExpression {
    pub operator: Operator,
    pub expr: Box<Expression>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub enum Expression {
    Binary(BinaryExpression),
    Bracketed(BracketedExpression),
    IntLiteral(IntLiteral),
    Unary(UnaryExpression),
    Variable(Variable),
    Error(AstInfo),
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct TypeDeclaration {
    pub name: Option<Identifier>,
    pub type_expr: Option<TypeExpression>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub enum TypeExpression {
    NamedType(Identifier),
    ArrayType {
        size: Option<IntLiteral>,
        base_type: Option<Box<TypeExpression>>,
        info: AstInfo,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub enum VariableDeclaration {
    Valid {
        name: Option<Identifier>,
        type_expr: Option<TypeExpression>,
        info: AstInfo,
    },
    Error(AstInfo),
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub enum ParameterDeclaration {
    Valid {
        is_ref: bool,
        name: Option<Identifier>,
        type_expr: Option<TypeExpression>,
        info: AstInfo,
    },
    Error(AstInfo),
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct CallStatement {
    pub name: Identifier,
    pub arguments: Vec<Expression>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct Assignment {
    pub variable: Variable,
    pub expr: Option<Expression>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct IfStatement {
    pub condition: Option<Expression>,
    pub if_branch: Option<Box<Statement>>,
    pub else_branch: Option<Box<Statement>>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct WhileStatement {
    pub condition: Option<Expression>,
    pub statement: Option<Box<Statement>>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub enum Statement {
    Empty(AstInfo),
    Assignment(Assignment),
    Call(CallStatement),
    If(IfStatement),
    While(WhileStatement),
    Block(BlockStatement),
    Error(AstInfo),
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct ProcedureDeclaration {
    pub name: Option<Identifier>,
    pub parameters: Vec<ParameterDeclaration>,
    pub variable_declarations: Vec<VariableDeclaration>,
    pub statements: Vec<Statement>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub enum GlobalDeclaration {
    Type(TypeDeclaration),
    Procedure(ProcedureDeclaration),
    Error(AstInfo),
}

/// Contains entire AST
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub global_declarations: Vec<GlobalDeclaration>,
}

impl Display for AstInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = self
            .tokens
            .iter()
            .map(|token| token.to_string())
            .reduce(|acc, token| {
                if acc.ends_with('\n') {
                    acc + &token
                } else {
                    acc + " " + &token
                }
            })
            .unwrap_or_default();
        write!(f, "{}", display)
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Display for IntLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = self
            .info
            .tokens
            .iter()
            .find_map(|token| match &token.token_type {
                TokenType::Int(_) | TokenType::Hex(_) | TokenType::Char(_) => {
                    Some(token.to_string())
                }
                _ => None,
            })
            .expect("IntLiteral must contain a `Int`, `Hex` or `Char` token");
        write!(f, "{}", display)
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operator::*;
        write!(
            f,
            "{}",
            match self {
                Add => "+",
                Sub => "-",
                Mul => "*",
                Div => "/",
                Equ => "=",
                Neq => "#",
                Lst => "<",
                Lse => "<=",
                Grt => ">",
                Gre => ">=",
            }
        )
    }
}

impl Display for ArrayAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let index = fmt_or_empty(&self.index);
        write!(f, "{}[{}]", self.array, index)
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::NamedVariable(ident) => ident.to_string(),
            Self::ArrayAccess(access) => access.to_string(),
        };
        write!(f, "{}", display)
    }
}

impl Display for BinaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.operator, self.rhs)
    }
}

impl Display for BracketedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.expr)
    }
}

impl Display for UnaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.operator, self.expr)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::Binary(binary) => binary.to_string(),
            Self::Bracketed(bracketed) => bracketed.to_string(),
            Self::IntLiteral(int_lit) => int_lit.to_string(),
            Self::Unary(unary) => unary.to_string(),
            Self::Variable(var) => var.to_string(),
            Self::Error(_) => String::new(),
        };
        write!(f, "{}", display)
    }
}

impl Display for TypeExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Self::NamedType(ident) => ident.to_string(),
            Self::ArrayType {
                size, base_type, ..
            } => {
                let size = fmt_or_empty(size);
                base_type.as_ref().map_or_else(
                    || format!("array [{}] of", size),
                    |type_expr| format!("array [{}] of {}", size, type_expr),
                )
            }
        };
        write!(f, "{}", display)
    }
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Valid {
                name, type_expr, ..
            } => {
                let name = fmt_or_empty(&name);
                let type_expr = fmt_or_empty(&type_expr);
                writeln!(f, "var {}: {};", name, type_expr)
            }
            Self::Error(info) => write!(f, "{}", info),
        }
    }
}

impl Display for ParameterDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Valid {
                is_ref,
                name,
                type_expr,
                ..
            } => {
                let r#ref = if *is_ref { "ref " } else { "" };
                let name = fmt_or_empty(&name);
                let type_expr = fmt_or_empty(&type_expr);
                write!(f, "{}{}: {}", r#ref, name, type_expr)
            }
            Self::Error(info) => write!(f, "{}", info),
        }
    }
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expr = fmt_or_empty(&self.expr);
        writeln!(f, "{} := {};", self.variable, expr)
    }
}

impl Display for CallStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .arguments
            .iter()
            .map(|arg| arg.to_string())
            .reduce(|acc, arg| acc + ", " + arg.as_str())
            .unwrap_or_default();
        writeln!(f, "{}({});", self.name, args)
    }
}

fn fmt_or_empty<T: Display>(opt: &Option<T>) -> String {
    opt.as_ref()
        .map_or_else(String::new, |inner| inner.to_string())
}
