use std::ops::Range;
use crate::parser::ToRange;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Digit {
    pub value: u32,
    pub range: Range<usize>,
}

pub struct Char {
    pub value: char,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntLiteral {
    pub value: Option<u32>,
    pub range: Range<usize>,
}

impl IntLiteral {
    pub(crate) fn new(value: u32, range: Range<usize>) -> Self {
        Self { value: Some(value), range }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Identifier {
    pub value: String,
    pub range: Range<usize>,
}

impl Identifier {
    pub fn new<T: ToString>(value: T, range: Range<usize>) -> Self {
        Self {
            value: value.to_string(),
            range,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayAccess {
    pub array: Box<Variable>,
    pub index: Box<Expression>,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Variable {
    NamedVariable(Identifier),
    ArrayAccess(ArrayAccess),
}

impl ToRange for Variable {
    fn to_range(&self) -> Range<usize> {
        use Variable::*;
        match self {
            NamedVariable(i) => i.range.to_owned(),
            ArrayAccess(a) => a.range.to_owned(),
        } 
    }
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

    pub fn is_arithmetic(&self) -> bool {
        matches!(self, Self::Add | Self::Sub | Self::Mul | Self::Div)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryExpression {
    pub operator: Operator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Binary(BinaryExpression),
    IntLiteral(IntLiteral),
    Variable(Variable),
    Error(Range<usize>),
}

impl ToRange for Expression {
    fn to_range(&self) -> Range<usize> {
        use Expression::*;
        match self {
            Binary(b) => b.range.to_owned(),
            IntLiteral(i) => i.range.to_owned(),
            Variable(v) => v.to_range(),
            Error(range) => range.to_owned(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeDeclaration {
    pub name: Option<Identifier>,
    pub type_expr: Option<TypeExpression>,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeExpression {
    IntType,
    NamedType(Identifier),
    ArrayType {
        size: Option<u32>,
        base_type: Option<Box<TypeExpression>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclaration {
    pub name: Option<Identifier>,
    pub type_expr: Option<TypeExpression>,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParameterDeclaration {
    pub is_ref: bool,
    pub name: Option<Identifier>,
    pub type_expr: Option<TypeExpression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallStatement {
    pub name: Identifier,
    pub arguments: Vec<Expression>,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Assignment {
    pub variable: Variable,
    pub expr: Option<Expression>,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfStatement {
    pub condition: Option<Expression>,
    pub if_branch: Option<Box<Statement>>,
    pub else_branch: Option<Box<Statement>>,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WhileStatement {
    pub condition: Option<Expression>,
    pub statement: Option<Box<Statement>>,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Empty,
    Assignment(Assignment),
    Call(CallStatement),
    If(IfStatement),
    While(WhileStatement),
    Block(BlockStatement),
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcedureDeclaration {
    pub name: Option<Identifier>,
    pub parameters: Vec<ParameterDeclaration>,
    pub variable_declarations: Vec<VariableDeclaration>,
    pub statements: Vec<Statement>,
    pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GlobalDeclaration {
    Type(TypeDeclaration),
    Procedure(ProcedureDeclaration),
    Error(Range<usize>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub global_declarations: Vec<GlobalDeclaration>,
}
