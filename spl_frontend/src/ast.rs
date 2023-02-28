//! Contains structs and enums for all AST nodes
use spl_frontend_macros::ToRange;

use crate::{
    error::OperatorConversionError,
    lexer::token::{Token, TokenType},
    ToRange,
};
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
    pub fn new<T: ToString>(value: T, tokens: &[Token]) -> Self {
        Self {
            value: value.to_string(),
            info: AstInfo::new(tokens),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct ArrayAccess {
    pub array: Box<Variable>,
    pub index: Box<Expression>,
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
pub enum Expression {
    Binary(BinaryExpression),
    IntLiteral(IntLiteral),
    Variable(Variable),
    Error(Range<usize>),
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
pub struct VariableDeclaration {
    pub name: Option<Identifier>,
    pub type_expr: Option<TypeExpression>,
    pub info: AstInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange)]
pub struct ParameterDeclaration {
    pub is_ref: bool,
    pub name: Option<Identifier>,
    pub type_expr: Option<TypeExpression>,
    pub info: AstInfo,
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
    Error(Range<usize>),
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
