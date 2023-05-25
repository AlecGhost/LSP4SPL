use super::*;

impl AstInfoTraverser for Program {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.global_declarations
            .iter()
            .for_each(|gd| gd.traverse(f));
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.global_declarations
            .iter_mut()
            .for_each(|gd| gd.traverse_mut(f));
    }
}

impl AstInfoTraverser for GlobalDeclaration {
    fn traverse(&self, f: fn(&AstInfo)) {
        use GlobalDeclaration::*;
        match self {
            Procedure(pd) => pd.traverse(f),
            Type(td) => td.traverse(f),
            Error(err) => f(err),
        }
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        use GlobalDeclaration::*;
        match self {
            Procedure(pd) => pd.traverse_mut(f),
            Type(td) => td.traverse_mut(f),
            Error(err) => f(err),
        }
    }
}

impl AstInfoTraverser for TypeDeclaration {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.name.iter().for_each(|name| name.traverse(f));
        self.type_expr
            .iter()
            .for_each(|type_expr| type_expr.traverse(f));
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.name.iter_mut().for_each(|name| name.traverse_mut(f));
        self.type_expr
            .iter_mut()
            .for_each(|type_expr| type_expr.traverse_mut(f));
    }
}

impl AstInfoTraverser for TypeExpression {
    fn traverse(&self, f: fn(&AstInfo)) {
        use TypeExpression::*;
        match self {
            NamedType(name) => name.traverse(f),
            ArrayType {
                size,
                base_type,
                info,
            } => {
                f(info);
                size.iter().for_each(|size| size.traverse(f));
                base_type.iter().for_each(|base_type| base_type.traverse(f));
            }
        }
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        use TypeExpression::*;
        match self {
            NamedType(name) => name.traverse_mut(f),
            ArrayType {
                size,
                base_type,
                info,
            } => {
                f(info);
                size.iter_mut().for_each(|size| size.traverse_mut(f));
                base_type
                    .iter_mut()
                    .for_each(|base_type| base_type.traverse_mut(f));
            }
        }
    }
}

impl AstInfoTraverser for ProcedureDeclaration {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.name.iter().for_each(|name| name.traverse(f));
        self.parameters.iter().for_each(|param| param.traverse(f));
        self.variable_declarations
            .iter()
            .for_each(|var| var.traverse(f));
        self.statements.iter().for_each(|stmt| stmt.traverse(f));
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.name.iter_mut().for_each(|name| name.traverse_mut(f));
        self.parameters
            .iter_mut()
            .for_each(|param| param.traverse_mut(f));
        self.variable_declarations
            .iter_mut()
            .for_each(|var| var.traverse_mut(f));
        self.statements
            .iter_mut()
            .for_each(|stmt| stmt.traverse_mut(f));
    }
}

impl AstInfoTraverser for ParameterDeclaration {
    fn traverse(&self, f: fn(&AstInfo)) {
        use ParameterDeclaration::*;
        match self {
            Valid {
                name,
                type_expr,
                info,
                ..
            } => {
                f(info);
                name.iter().for_each(|name| name.traverse(f));
                type_expr.iter().for_each(|type_expr| type_expr.traverse(f));
            }
            Error(err) => f(err),
        }
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        use ParameterDeclaration::*;
        match self {
            Valid {
                name,
                type_expr,
                info,
                ..
            } => {
                f(info);
                name.iter_mut().for_each(|name| name.traverse_mut(f));
                type_expr
                    .iter_mut()
                    .for_each(|type_expr| type_expr.traverse_mut(f));
            }
            Error(err) => f(err),
        }
    }
}

impl AstInfoTraverser for VariableDeclaration {
    fn traverse(&self, f: fn(&AstInfo)) {
        use VariableDeclaration::*;
        match self {
            Valid {
                name,
                type_expr,
                info,
                ..
            } => {
                f(info);
                name.iter().for_each(|name| name.traverse(f));
                type_expr.iter().for_each(|type_expr| type_expr.traverse(f));
            }
            Error(err) => f(err),
        }
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        use VariableDeclaration::*;
        match self {
            Valid {
                name,
                type_expr,
                info,
                ..
            } => {
                f(info);
                name.iter_mut().for_each(|name| name.traverse_mut(f));
                type_expr
                    .iter_mut()
                    .for_each(|type_expr| type_expr.traverse_mut(f));
            }
            Error(err) => f(err),
        }
    }
}

impl AstInfoTraverser for Statement {
    fn traverse(&self, f: fn(&AstInfo)) {
        use Statement::*;
        match self {
            If(stmt) => stmt.traverse(f),
            While(stmt) => stmt.traverse(f),
            Call(stmt) => stmt.traverse(f),
            Assignment(stmt) => stmt.traverse(f),
            Block(stmt) => stmt.traverse(f),
            Empty(info) => f(info),
            Error(info) => f(info),
        }
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        use Statement::*;
        match self {
            If(stmt) => stmt.traverse_mut(f),
            While(stmt) => stmt.traverse_mut(f),
            Call(stmt) => stmt.traverse_mut(f),
            Assignment(stmt) => stmt.traverse_mut(f),
            Block(stmt) => stmt.traverse_mut(f),
            Empty(info) => f(info),
            Error(info) => f(info),
        }
    }
}

impl AstInfoTraverser for IfStatement {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.if_branch.iter().for_each(|stmt| stmt.traverse(f));
        self.else_branch.iter().for_each(|stmt| stmt.traverse(f));
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.if_branch.iter_mut().for_each(|stmt| stmt.traverse_mut(f));
        self.else_branch.iter_mut().for_each(|stmt| stmt.traverse_mut(f));
    }
}

impl AstInfoTraverser for WhileStatement {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.statement.iter().for_each(|stmt| stmt.traverse(f));
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.statement.iter_mut().for_each(|stmt| stmt.traverse_mut(f));
    }
}

impl AstInfoTraverser for BlockStatement {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.statements.iter().for_each(|stmt| stmt.traverse(f));
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.statements
            .iter_mut()
            .for_each(|stmt| stmt.traverse_mut(f));
    }
}

impl AstInfoTraverser for Assignment {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.variable.traverse(f);
        self.expr.iter().for_each(|expr| expr.traverse(f));
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.variable.traverse_mut(f);
        self.expr.iter_mut().for_each(|expr| expr.traverse_mut(f));
    }
}

impl AstInfoTraverser for CallStatement {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.name.traverse(f);
        self.arguments.iter().for_each(|arg| arg.traverse(f));
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.name.traverse_mut(f);
        self.arguments
            .iter_mut()
            .for_each(|arg| arg.traverse_mut(f));
    }
}

impl AstInfoTraverser for Expression {
    fn traverse(&self, f: fn(&AstInfo)) {
        use Expression::*;
        match self {
            Unary(expr) => expr.traverse(f),
            Binary(expr) => expr.traverse(f),
            Bracketed(expr) => expr.traverse(f),
            Variable(expr) => expr.traverse(f),
            IntLiteral(expr) => expr.traverse(f),
            Error(info) => f(info),
        }
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        use Expression::*;
        match self {
            Unary(expr) => expr.traverse_mut(f),
            Binary(expr) => expr.traverse_mut(f),
            Bracketed(expr) => expr.traverse_mut(f),
            Variable(expr) => expr.traverse_mut(f),
            IntLiteral(expr) => expr.traverse_mut(f),
            Error(info) => f(info),
        }
    }
}

impl AstInfoTraverser for BinaryExpression {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.lhs.traverse(f);
        self.rhs.traverse(f);
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.lhs.traverse_mut(f);
        self.rhs.traverse_mut(f);
    }
}

impl AstInfoTraverser for UnaryExpression {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.expr.traverse(f);
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.expr.traverse_mut(f);
    }
}

impl AstInfoTraverser for BracketedExpression {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.expr.traverse(f);
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.expr.traverse_mut(f);
    }
}

impl AstInfoTraverser for Variable {
    fn traverse(&self, f: fn(&AstInfo)) {
        use Variable::*;
        match self {
            NamedVariable(name) => name.traverse(f),
            ArrayAccess(a) => a.traverse(f),
        }
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        use Variable::*;
        match self {
            NamedVariable(name) => name.traverse_mut(f),
            ArrayAccess(a) => a.traverse_mut(f),
        }
    }
}

impl AstInfoTraverser for ArrayAccess {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
        self.array.traverse(f);
        self.index.iter().for_each(|index| index.traverse(f));
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
        self.array.traverse_mut(f);
        self.index.iter_mut().for_each(|index| index.traverse_mut(f));
    }
}

impl AstInfoTraverser for IntLiteral {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
    }
}

impl AstInfoTraverser for Identifier {
    fn traverse(&self, f: fn(&AstInfo)) {
        f(&self.info);
    }

    fn traverse_mut(&mut self, f: fn(&mut AstInfo)) {
        f(&mut self.info);
    }
}
