#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntLiteral {
    pub value: Option<u32>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Identifier {
    pub value: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArrayAccess {
    pub array: Box<Variable>,
    pub index: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Variable {
    NamedVariable(Identifier),
    ArrayAccess(ArrayAccess),
}

#[derive(Debug, PartialEq, Eq)]
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
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinaryExpression {
    pub operator: Operator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Binary(BinaryExpression),
    IntLiteral(IntLiteral),
    Variable(Variable),
    Error,
}
