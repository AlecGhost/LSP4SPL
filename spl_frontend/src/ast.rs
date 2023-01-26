use std::ops::Range;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntLiteral {
    pub value: Option<u32>,
}

impl IntLiteral {
    pub(crate) fn new(value: u32) -> Self {
        Self { value: Some(value) }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Identifier {
    pub value: String,
    pub range: Range<usize>,
}

impl Identifier {
    pub(crate) fn new<T: ToString>(value: T, range: Range<usize>) -> Self {
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    pub(crate) fn new(symbol: &str) -> Option<Self> {
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

    pub(crate) fn is_arithmatic(&self) -> bool {
        matches!(self, Self::Add | Self::Sub | Self::Mul | Self::Div)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryExpression {
    pub operator: Operator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Binary(BinaryExpression),
    IntLiteral(IntLiteral),
    Variable(Variable),
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeDeclaration {
    pub name: Option<Identifier>,
    pub type_expr: Option<TypeExpression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeExpression {
    IntType,
    BoolType,
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Assignment {
    pub variable: Variable,
    pub expr: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfStatement {
    pub condition: Option<Expression>,
    pub if_branch: Option<Box<Statement>>,
    pub else_branch: Option<Box<Statement>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WhileStatement {
    pub condition: Option<Expression>,
    pub statement: Option<Box<Statement>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Empty,
    Assignment(Assignment),
    Call(CallStatement),
    If(IfStatement),
    While(WhileStatement),
    Block(BlockStatement),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProcedureDeclaration {
    pub name: Option<Identifier>,
    pub parameters: Vec<ParameterDeclaration>,
    pub variable_declarations: Vec<VariableDeclaration>,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GlobalDeclaration {
    Type(TypeDeclaration),
    Procedure(ProcedureDeclaration),
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub global_declarations: Vec<GlobalDeclaration>,
}
