use crate::ToRange;
use std::{fmt::Display, ops::Range};

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
        Self {
            value: Some(value),
            range,
        }
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

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
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
            NamedVariable(i) => i.range.clone(),
            ArrayAccess(a) => a.range.clone(),
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
            Binary(b) => b.range.clone(),
            IntLiteral(i) => i.range.clone(),
            Variable(v) => v.to_range(),
            Error(range) => range.clone(),
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
    NamedType(Identifier),
    ArrayType {
        size: Option<u32>,
        base_type: Option<Box<TypeExpression>>,
        range: Range<usize>,
    },
}

impl ToRange for TypeExpression {
    fn to_range(&self) -> Range<usize> {
        use TypeExpression::*;
        match self {
            NamedType(ident) => ident.range.clone(),
            ArrayType {
                size: _,
                base_type: _,
                range,
            } => range.clone(),
        }
    }
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
    pub range: Range<usize>,
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
    Empty(Range<usize>),
    Assignment(Assignment),
    Call(CallStatement),
    If(IfStatement),
    While(WhileStatement),
    Block(BlockStatement),
    Error(Range<usize>),
}

impl ToRange for Statement {
    fn to_range(&self) -> Range<usize> {
        use Statement::*;
        match self {
            Empty(range) => range.clone(),
            Assignment(a) => a.range.clone(),
            Call(c) => c.range.clone(),
            If(i) => i.range.clone(),
            While(w) => w.range.clone(),
            Block(b) => b.range.clone(),
            Error(range) => range.clone(),
        }
    }
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

impl ToRange for GlobalDeclaration {
    fn to_range(&self) -> Range<usize> {
        use GlobalDeclaration::*;
        match self {
            Type(t) => t.range.clone(),
            Procedure(p) => p.range.clone(),
            Error(range) => range.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub global_declarations: Vec<GlobalDeclaration>,
}

impl Program {
    pub fn ident_at(&self, index: usize) -> Option<&Identifier> {
        fn search_type_expr(type_expr: &TypeExpression, index: usize) -> Option<&Identifier> {
            if !type_expr.to_range().contains(&index) {
                return None;
            }
            match type_expr {
                TypeExpression::NamedType(name) => {
                    if name.range.contains(&index) {
                        Some(name)
                    } else {
                        None
                    }
                }
                TypeExpression::ArrayType {
                    size: _,
                    base_type,
                    range,
                } => {
                    if range.contains(&index) {
                        base_type.as_ref().and_then(|t| search_type_expr(t, index))
                    } else {
                        None
                    }
                }
            }
        }

        fn search_statement(stmt: &Statement, index: usize) -> Option<&Identifier> {
            if !stmt.to_range().contains(&index) {
                return None;
            }
            match stmt {
                Statement::If(i) => {
                    if let Some(condition) = &i.condition {
                        if let Some(ident) = search_expression(condition, index) {
                            return Some(ident);
                        }
                    }
                    if let Some(stmt) = &i.if_branch {
                        if let Some(ident) = search_statement(stmt, index) {
                            return Some(ident);
                        }
                    }
                    if let Some(stmt) = &i.else_branch {
                        if let Some(ident) = search_statement(stmt, index) {
                            return Some(ident);
                        }
                    }
                    None
                }
                Statement::While(w) => {
                    if let Some(condition) = &w.condition {
                        if let Some(ident) = search_expression(condition, index) {
                            return Some(ident);
                        }
                    }
                    if let Some(stmt) = &w.statement {
                        if let Some(ident) = search_statement(stmt, index) {
                            return Some(ident);
                        }
                    }
                    None
                }
                Statement::Call(c) => {
                    if c.name.range.contains(&index) {
                        Some(&c.name)
                    } else {
                        c.arguments
                            .iter()
                            .map(|arg| search_expression(arg, index))
                            .find(|opt| opt.is_some())
                            .flatten()
                    }
                }
                Statement::Assignment(a) => {
                    if let Some(ident) = search_variable(&a.variable, index) {
                        return Some(ident);
                    }
                    if let Some(expr) = &a.expr {
                        return search_expression(expr, index);
                    }
                    None
                }
                Statement::Block(b) => b
                    .statements
                    .iter()
                    .map(|stmt| search_statement(stmt, index))
                    .find(|opt| opt.is_some())
                    .flatten(),
                Statement::Empty(_) => None,
                Statement::Error(_) => None,
            }
        }

        fn search_expression(expr: &Expression, index: usize) -> Option<&Identifier> {
            if !expr.to_range().contains(&index) {
                return None;
            }
            match expr {
                Expression::Binary(b) => {
                    if let Some(ident) = search_expression(&b.lhs, index) {
                        return Some(ident);
                    }
                    search_expression(&b.rhs, index)
                }
                Expression::Variable(v) => search_variable(v, index),
                Expression::IntLiteral(_) => None,
                Expression::Error(_) => None,
            }
        }

        fn search_variable(var: &Variable, index: usize) -> Option<&Identifier> {
            if !var.to_range().contains(&index) {
                return None;
            }
            match var {
                Variable::NamedVariable(name) => {
                    if name.range.contains(&index) {
                        Some(name)
                    } else {
                        None
                    }
                }
                Variable::ArrayAccess(a) => {
                    if let Some(ident) = search_variable(&a.array, index) {
                        return Some(ident);
                    }
                    search_expression(&a.index, index)
                }
            }
        }

        let gd = self
            .global_declarations
            .iter()
            .find(|gd| gd.to_range().contains(&index));
        if let Some(gd) = gd {
            match gd {
                GlobalDeclaration::Type(t) => {
                    if !t.range.contains(&index) {
                        return None;
                    }
                    if let Some(name) = &t.name {
                        if name.range.contains(&index) {
                            return Some(name);
                        }
                    }
                    if let Some(type_expr) = &t.type_expr {
                        return search_type_expr(type_expr, index);
                    }
                }
                GlobalDeclaration::Procedure(p) => {
                    if !p.range.contains(&index) {
                        return None;
                    }
                    if let Some(name) = &p.name {
                        if name.range.contains(&index) {
                            return Some(name);
                        }
                    }
                    for param in p.parameters.iter() {
                        if let Some(name) = &param.name {
                            if name.range.contains(&index) {
                                return Some(name);
                            }
                        }
                        if let Some(type_expr) = &param.type_expr {
                            if let Some(ident) = search_type_expr(type_expr, index) {
                                return Some(ident);
                            }
                        }
                    }
                    for var in &p.variable_declarations {
                        if let Some(name) = &var.name {
                            if name.range.contains(&index) {
                                return Some(name);
                            }
                        }
                        if let Some(type_expr) = &var.type_expr {
                            if let Some(ident) = search_type_expr(type_expr, index) {
                                return Some(ident);
                            }
                        }
                    }
                    for stmt in &p.statements {
                        if let Some(ident) = search_statement(stmt, index) {
                            return Some(ident);
                        }
                    }
                }
                GlobalDeclaration::Error(_) => {}
            };
        }
        None
    }
}
