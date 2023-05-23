//! Contains structs and enums for all AST nodes
use crate::{
    error::{OperatorConversionError, SplError},
    lexer::token::TokenType,
    token::Token,
    ErrorContainer, Shiftable, ToRange, ToTextRange,
};
use spl_frontend_macros::{ToRange, ToTextRange};
use std::{
    fmt::Display,
    ops::{Deref, DerefMut, Range},
};

// TODO: Remove Hash
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct AstInfo {
    range: Range<usize>,
    errors: Vec<SplError>,
}

impl AstInfo {
    pub const fn new(range: Range<usize>) -> Self {
        Self {
            range,
            errors: Vec::new(),
        }
    }

    pub fn new_with_errors(range: Range<usize>, errors: Vec<SplError>) -> Self {
        Self { range, errors }
    }

    pub fn append_error(&mut self, error: SplError) {
        self.errors.push(error);
    }

    pub(crate) fn extend_range(&mut self, other: &Self) {
        let start = self.range.start.min(other.range.start);
        let end = self.range.end.max(other.range.end);
        self.range = start..end;
    }

    pub fn slice<'a>(&self, tokens: &'a [Token]) -> &'a [Token] {
        &tokens[self.range.clone()]
    }
}

impl ToRange for AstInfo {
    fn to_range(&self) -> Range<usize> {
        self.range.clone()
    }
}

impl ToTextRange for AstInfo {
    fn to_text_range(&self, tokens: &[Token]) -> Range<usize> {
        match self.to_range() {
            range if range.is_empty() => {
                let token = &tokens[range.end];
                let end_pos = token.range.end;
                end_pos..end_pos
            }
            range => {
                let tokens = &tokens[range];
                let start_pos = tokens.first().expect("Token slice is empty").range.start;
                let end_pos = tokens.last().expect("Token slice is empty").range.end;
                start_pos..end_pos
            }
        }
    }
}

impl ErrorContainer for AstInfo {
    fn errors(&self) -> Vec<SplError> {
        self.errors.clone()
    }
}

impl Shiftable for AstInfo {
    fn shift(self, offset: usize) -> Self {
        Self {
            range: self.range.shift(offset),
            errors: self.errors,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Reference<T> {
    pub reference: T,
    pub offset: usize,
}

impl<T> Reference<T> {
    pub const fn new(reference: T, offset: usize) -> Self {
        Self { reference, offset }
    }
}

impl<T: ToRange> ToRange for Reference<T> {
    fn to_range(&self) -> Range<usize> {
        self.reference.to_range().shift(self.offset)
    }
}

impl<T> Deref for Reference<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.reference
    }
}

impl<T> DerefMut for Reference<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.reference
    }
}

impl<T> AsRef<T> for Reference<T> {
    fn as_ref(&self) -> &T {
        &self.reference
    }
}

impl<T> AsMut<T> for Reference<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.reference
    }
}

#[derive(Clone, Debug, Eq, PartialEq, ToRange, ToTextRange)]
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

impl ErrorContainer for IntLiteral {
    fn errors(&self) -> Vec<SplError> {
        self.info.errors()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, ToRange, ToTextRange)]
pub struct Identifier {
    pub value: String,
    pub info: AstInfo,
}

impl Identifier {
    pub const fn new(value: String, range: Range<usize>) -> Self {
        Self {
            value,
            info: AstInfo::new(range),
        }
    }
}

impl ErrorContainer for Identifier {
    fn errors(&self) -> Vec<SplError> {
        self.info.errors()
    }
}

impl Shiftable for Identifier {
    fn shift(self, offset: usize) -> Self {
        Self {
            value: self.value,
            info: self.info.shift(offset),
        }
    }
}

impl Shiftable for Vec<Identifier> {
    fn shift(self, offset: usize) -> Self {
        self.into_iter().map(|ident| ident.shift(offset)).collect()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct ArrayAccess {
    pub array: Box<Variable>,
    pub index: Option<Box<Reference<Expression>>>,
    pub info: AstInfo,
}

impl ErrorContainer for ArrayAccess {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(self.array.errors());
        if let Some(index) = &self.index {
            errors.extend(index.errors().shift(index.offset))
        }
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub enum Variable {
    NamedVariable(Identifier),
    ArrayAccess(ArrayAccess),
}

impl ErrorContainer for Variable {
    fn errors(&self) -> Vec<SplError> {
        match self {
            Self::ArrayAccess(a) => a.errors(),
            Self::NamedVariable(n) => n.errors(),
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

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct BinaryExpression {
    pub operator: Operator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub info: AstInfo,
}

impl ErrorContainer for BinaryExpression {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(self.lhs.errors());
        errors.extend(self.rhs.errors());
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct BracketedExpression {
    pub expr: Box<Expression>,
    pub info: AstInfo,
}

impl ErrorContainer for BracketedExpression {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(self.expr.errors());
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct UnaryExpression {
    pub operator: Operator,
    pub expr: Box<Expression>,
    pub info: AstInfo,
}

impl ErrorContainer for UnaryExpression {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(self.expr.errors());
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub enum Expression {
    Binary(BinaryExpression),
    Bracketed(BracketedExpression),
    IntLiteral(IntLiteral),
    Unary(UnaryExpression),
    Variable(Variable),
    Error(AstInfo),
}

impl Expression {
    pub fn info_mut(&mut self) -> &mut AstInfo {
        match self {
            Self::Binary(b) => &mut b.info,
            Self::Bracketed(b) => &mut b.info,
            Self::Error(info) => info,
            Self::Unary(u) => &mut u.info,
            Self::IntLiteral(i) => &mut i.info,
            Self::Variable(v) => match v {
                Variable::ArrayAccess(a) => &mut a.info,
                Variable::NamedVariable(n) => &mut n.info,
            },
        }
    }
}

impl ErrorContainer for Expression {
    fn errors(&self) -> Vec<SplError> {
        match self {
            Self::Binary(b) => b.errors(),
            Self::Bracketed(b) => b.errors(),
            Self::Error(info) => info.errors(),
            Self::IntLiteral(i) => i.errors(),
            Self::Variable(v) => v.errors(),
            Self::Unary(u) => u.errors(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct TypeDeclaration {
    pub doc: Vec<String>,
    pub name: Option<Identifier>,
    pub type_expr: Option<Reference<TypeExpression>>,
    pub info: AstInfo,
}

impl ErrorContainer for TypeDeclaration {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        if let Some(name) = &self.name {
            errors.extend(name.errors());
        }
        if let Some(type_expr) = &self.type_expr {
            errors.extend(type_expr.errors().shift(type_expr.offset));
        }
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub enum TypeExpression {
    NamedType(Identifier),
    ArrayType {
        size: Option<IntLiteral>,
        base_type: Option<Box<Reference<TypeExpression>>>,
        info: AstInfo,
    },
}

impl ErrorContainer for TypeExpression {
    fn errors(&self) -> Vec<SplError> {
        match self {
            Self::NamedType(n) => n.errors(),
            Self::ArrayType {
                base_type, info, ..
            } => {
                let mut errors = info.errors();
                if let Some(base_type) = base_type {
                    errors.extend(base_type.errors());
                }
                errors
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub enum VariableDeclaration {
    Valid {
        doc: Vec<String>,
        name: Option<Identifier>,
        type_expr: Option<Reference<TypeExpression>>,
        info: AstInfo,
    },
    Error(AstInfo),
}

impl ErrorContainer for VariableDeclaration {
    fn errors(&self) -> Vec<SplError> {
        match self {
            Self::Error(info) => info.errors(),
            Self::Valid {
                name,
                type_expr,
                info,
                ..
            } => {
                let mut errors = info.errors();
                if let Some(name) = name {
                    errors.extend(name.errors());
                }
                if let Some(type_expr) = type_expr {
                    errors.extend(type_expr.errors().shift(type_expr.offset));
                }
                errors
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub enum ParameterDeclaration {
    Valid {
        doc: Vec<String>,
        is_ref: bool,
        name: Option<Identifier>,
        type_expr: Option<Reference<TypeExpression>>,
        info: AstInfo,
    },
    Error(AstInfo),
}

impl ErrorContainer for ParameterDeclaration {
    fn errors(&self) -> Vec<SplError> {
        match self {
            Self::Error(info) => info.errors(),
            Self::Valid {
                name,
                type_expr,
                info,
                ..
            } => {
                let mut errors = info.errors();
                if let Some(name) = name {
                    errors.extend(name.errors());
                }
                if let Some(type_expr) = type_expr {
                    errors.extend(type_expr.errors().shift(type_expr.offset));
                }
                errors
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct CallStatement {
    pub name: Identifier,
    pub arguments: Vec<Reference<Expression>>,
    pub info: AstInfo,
}

impl ErrorContainer for CallStatement {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(self.name.errors());
        errors.extend(
            self.arguments
                .iter()
                .flat_map(|arg| arg.errors().shift(arg.offset)),
        );
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct Assignment {
    pub variable: Variable,
    pub expr: Option<Reference<Expression>>,
    pub info: AstInfo,
}

impl ErrorContainer for Assignment {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(self.variable.errors());
        if let Some(expr) = &self.expr {
            errors.extend(expr.errors().shift(expr.offset));
        }
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct IfStatement {
    pub condition: Option<Reference<Expression>>,
    pub if_branch: Option<Box<Reference<Statement>>>,
    pub else_branch: Option<Box<Reference<Statement>>>,
    pub info: AstInfo,
}

impl ErrorContainer for IfStatement {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        if let Some(expr) = &self.condition {
            errors.extend(expr.errors().shift(expr.offset));
        }
        if let Some(stmt) = &self.if_branch {
            errors.extend(stmt.errors().shift(stmt.offset));
        }
        if let Some(stmt) = &self.else_branch {
            errors.extend(stmt.errors().shift(stmt.offset));
        }
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct WhileStatement {
    pub condition: Option<Reference<Expression>>,
    pub statement: Option<Box<Reference<Statement>>>,
    pub info: AstInfo,
}

impl ErrorContainer for WhileStatement {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        if let Some(expr) = &self.condition {
            errors.extend(expr.errors().shift(expr.offset));
        }
        if let Some(stmt) = &self.statement {
            errors.extend(stmt.errors().shift(stmt.offset));
        }
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct BlockStatement {
    pub statements: Vec<Reference<Statement>>,
    pub info: AstInfo,
}

impl ErrorContainer for BlockStatement {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(
            self.statements
                .iter()
                .flat_map(|stmt| stmt.errors().shift(stmt.offset)),
        );
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub enum Statement {
    Empty(AstInfo),
    Assignment(Assignment),
    Call(CallStatement),
    If(IfStatement),
    While(WhileStatement),
    Block(BlockStatement),
    Error(AstInfo),
}

impl Statement {
    pub const fn info(&self) -> &AstInfo {
        match self {
            Self::Empty(info) => info,
            Self::If(i) => &i.info,
            Self::Call(c) => &c.info,
            Self::While(w) => &w.info,
            Self::Block(b) => &b.info,
            Self::Assignment(a) => &a.info,
            Self::Error(info) => info,
        }
    }
}

impl ErrorContainer for Statement {
    fn errors(&self) -> Vec<SplError> {
        match self {
            Self::Empty(info) => info.errors(),
            Self::Error(info) => info.errors(),
            Self::Assignment(a) => a.errors(),
            Self::Block(b) => b.errors(),
            Self::Call(c) => c.errors(),
            Self::If(i) => i.errors(),
            Self::While(w) => w.errors(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct ProcedureDeclaration {
    pub doc: Vec<String>,
    pub name: Option<Identifier>,
    pub parameters: Vec<Reference<ParameterDeclaration>>,
    pub variable_declarations: Vec<Reference<VariableDeclaration>>,
    pub statements: Vec<Reference<Statement>>,
    pub info: AstInfo,
}

impl ErrorContainer for ProcedureDeclaration {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        if let Some(name) = &self.name {
            errors.extend(name.errors());
        }
        errors.extend(
            self.parameters
                .iter()
                .flat_map(|stmt| stmt.errors().shift(stmt.offset)),
        );
        errors.extend(
            self.variable_declarations
                .iter()
                .flat_map(|stmt| stmt.errors().shift(stmt.offset)),
        );
        errors.extend(
            self.statements
                .iter()
                .flat_map(|stmt| stmt.errors().shift(stmt.offset)),
        );
        errors
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub enum GlobalDeclaration {
    Type(TypeDeclaration),
    Procedure(ProcedureDeclaration),
    Error(AstInfo),
}

impl ErrorContainer for GlobalDeclaration {
    fn errors(&self) -> Vec<SplError> {
        match self {
            Self::Type(t) => t.errors(),
            Self::Procedure(p) => p.errors(),
            Self::Error(info) => info.errors(),
        }
    }
}

/// Contains entire AST
#[derive(Clone, Debug, PartialEq, Eq, ToRange, ToTextRange)]
pub struct Program {
    pub global_declarations: Vec<Reference<GlobalDeclaration>>,
    pub info: AstInfo,
}

impl ErrorContainer for Program {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(
            self.global_declarations
                .iter()
                .flat_map(|gd| gd.errors().shift(gd.offset)),
        );
        errors
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
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
