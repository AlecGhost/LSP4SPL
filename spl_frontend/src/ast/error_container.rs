use super::*;
use crate::ErrorContainer;
use crate::SplError;

impl ErrorContainer for AstInfo {
    fn errors(&self) -> Vec<SplError> {
        self.errors.clone()
    }
}

impl ErrorContainer for IntLiteral {
    fn errors(&self) -> Vec<SplError> {
        self.info.errors()
    }
}

impl ErrorContainer for Identifier {
    fn errors(&self) -> Vec<SplError> {
        self.info.errors()
    }
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

impl ErrorContainer for Variable {
    fn errors(&self) -> Vec<SplError> {
        match self {
            Self::ArrayAccess(a) => a.errors(),
            Self::NamedVariable(n) => n.errors(),
        }
    }
}

impl ErrorContainer for BinaryExpression {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(self.lhs.errors());
        errors.extend(self.rhs.errors());
        errors
    }
}

impl ErrorContainer for BracketedExpression {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(self.expr.errors());
        errors
    }
}

impl ErrorContainer for UnaryExpression {
    fn errors(&self) -> Vec<SplError> {
        let mut errors = self.info.errors();
        errors.extend(self.expr.errors());
        errors
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

impl ErrorContainer for GlobalDeclaration {
    fn errors(&self) -> Vec<SplError> {
        match self {
            Self::Type(t) => t.errors(),
            Self::Procedure(p) => p.errors(),
            Self::Error(info) => info.errors(),
        }
    }
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
