use super::{Entry, SymbolTable};
use crate::{
    ast::{
        ArrayAccess, Assignment, BinaryExpression, BlockStatement, CallStatement, Expression,
        GlobalDeclaration, Identifier, IfStatement, Program, Statement, TypeExpression, Variable,
        WhileStatement,
    },
    error::{SemanticError, SemanticErrorMessage},
    DiagnosticsBroker,
};
use std::cmp::Ordering;

#[cfg(test)]
mod tests;

trait SemanticErrorBroker: Clone + std::fmt::Debug + DiagnosticsBroker<SemanticError> {}

impl<T> SemanticErrorBroker for T where T: Clone + std::fmt::Debug + DiagnosticsBroker<SemanticError>
{}

#[derive(Debug)]
struct LookupTable<'a> {
    local_table: &'a SymbolTable,
    global_table: &'a SymbolTable,
}

impl<'a> LookupTable<'a> {
    fn lookup(&self, key: &Identifier) -> Option<&Entry> {
        let mut value = self.local_table.lookup(key);
        if value.is_none() {
            value = self.global_table.lookup(key);
        }
        value
    }
}

pub fn analyze<B: Clone + std::fmt::Debug + DiagnosticsBroker<SemanticError>>(
    program: &Program,
    table: &SymbolTable,
    broker: B,
) {
    program
        .global_declarations
        .iter()
        .filter_map(|dec| match dec {
            GlobalDeclaration::Procedure(proc) => Some(proc),
            _ => None,
        })
        .for_each(|proc| {
            if let Some(name) = &proc.name {
                let entry = table.lookup(name).expect("Named declaration without entry");
                if let Entry::Procedure(proc_entry) = entry {
                    let lookup_table = &LookupTable {
                        local_table: &proc_entry.local_table,
                        global_table: table,
                    };
                    proc.statements
                        .iter()
                        .for_each(|stmt| stmt.analyze(lookup_table, broker.clone()));
                }
            }
        });
}

trait AnalyzeStatement<B> {
    fn analyze(&self, table: &LookupTable, broker: B);
}

trait AnalyzeExpression<B> {
    fn analyze(&self, table: &LookupTable, broker: B) -> Option<TypeExpression>;
}

impl<B: SemanticErrorBroker> AnalyzeStatement<B> for Statement {
    fn analyze(&self, table: &LookupTable, broker: B) {
        match self {
            Self::Assignment(stmt) => stmt.analyze(table, broker),
            Self::Block(stmt) => stmt.analyze(table, broker),
            Self::Call(stmt) => stmt.analyze(table, broker),
            Self::If(stmt) => stmt.analyze(table, broker),
            Self::While(stmt) => stmt.analyze(table, broker),
            Self::Empty => {}
        };
    }
}

impl<B: SemanticErrorBroker> AnalyzeStatement<B> for Assignment {
    fn analyze(&self, table: &LookupTable, broker: B) {
        if let Some(expr) = &self.expr {
            let left = self.variable.analyze(table, broker.clone());
            let right = expr.analyze(table, broker.clone());
            // only analyze further if type information for both sides is available
            if let (Some(left), Some(right)) = (left, right) {
                if left != right {
                    broker.report_error(SemanticError(
                        0..0,
                        SemanticErrorMessage::AssignmentHasDifferentTypes,
                    ));
                } else if !matches!(left, TypeExpression::IntType) {
                    broker.report_error(SemanticError(
                        0..0,
                        SemanticErrorMessage::AssignmentRequiresIntegers,
                    ));
                }
            }
        }
    }
}

impl<B: SemanticErrorBroker> AnalyzeStatement<B> for BlockStatement {
    fn analyze(&self, table: &LookupTable, broker: B) {
        self.statements
            .iter()
            .for_each(|stmt| stmt.analyze(table, broker.clone()));
    }
}

impl<B: SemanticErrorBroker> AnalyzeStatement<B> for CallStatement {
    fn analyze(&self, table: &LookupTable, broker: B) {
        if let Some(entry) = table.lookup(&self.name) {
            if let Entry::Procedure(proc_entry) = entry {
                let arg_len = self.arguments.len();
                let param_len = proc_entry.parameters.len();
                match arg_len.cmp(&param_len) {
                    Ordering::Less => {
                        broker.report_error(
                            self.name
                                .to_semantic_error(SemanticErrorMessage::TooFewArguments),
                        );
                    }
                    Ordering::Greater => {
                        broker.report_error(
                            self.name
                                .to_semantic_error(SemanticErrorMessage::TooManyArguments),
                        );
                    }
                    Ordering::Equal => {}
                };
                for (i, (arg, param)) in
                    std::iter::zip(&self.arguments, &proc_entry.parameters).enumerate()
                {
                    if param.is_ref && !matches!(arg, Expression::Variable(_)) {
                        broker.report_error(SemanticError(
                            self.name.range.clone(),
                            SemanticErrorMessage::ArgumentMustBeAVariable(
                                self.name.value.clone(),
                                // enumeration starts with 1
                                i + 1,
                            ),
                        ));
                    }
                    if arg.analyze(table, broker.clone()) != param.type_expr {
                        broker.report_error(SemanticError(
                            self.name.range.clone(),
                            SemanticErrorMessage::ArgumentsTypeMismatch(
                                self.name.value.clone(),
                                // enumeration starts with 1
                                i + 1,
                            ),
                        ));
                    }
                }
            } else {
                broker.report_error(
                    self.name
                        .to_semantic_error(SemanticErrorMessage::CallOfNoneProcedure),
                );
            }
        } else {
            broker.report_error(
                self.name
                    .to_semantic_error(SemanticErrorMessage::UndefinedProcedure),
            );
        }
    }
}

impl<B: SemanticErrorBroker> AnalyzeStatement<B> for IfStatement {
    fn analyze(&self, table: &LookupTable, broker: B) {
        let condition_type = self
            .condition
            .as_ref()
            .and_then(|expr| expr.analyze(table, broker.clone()));
        if let Some(condition_type) = condition_type {
            if condition_type != TypeExpression::BoolType {
                broker.report_error(SemanticError(
                    0..0,
                    SemanticErrorMessage::IfConditionMustBeBoolean,
                ));
            }
        }
        if let Some(stmt) = &self.if_branch {
            stmt.analyze(table, broker.clone());
        }
        if let Some(stmt) = &self.else_branch {
            stmt.analyze(table, broker);
        };
    }
}

impl<B: SemanticErrorBroker> AnalyzeStatement<B> for WhileStatement {
    fn analyze(&self, table: &LookupTable, broker: B) {
        let condition_type = self
            .condition
            .as_ref()
            .and_then(|expr| expr.analyze(table, broker.clone()));
        if let Some(condition_type) = condition_type {
            if condition_type != TypeExpression::BoolType {
                broker.report_error(SemanticError(
                    0..0,
                    SemanticErrorMessage::WhileConditionMustBeBoolean,
                ));
            }
        }
        if let Some(stmt) = &self.statement {
            stmt.analyze(table, broker);
        }
    }
}

impl<B: SemanticErrorBroker> AnalyzeExpression<B> for Variable {
    fn analyze(&self, table: &LookupTable, broker: B) -> Option<TypeExpression> {
        match self {
            Self::ArrayAccess(a) => a.analyze(table, broker),
            Self::NamedVariable(named) => {
                if let Some(entry) = table.lookup(named) {
                    match entry {
                        Entry::Variable(v) => {
                            if let Some(TypeExpression::NamedType(mut named_type)) =
                                v.type_expr.clone()
                            {
                                while let Some(entry) = table.global_table.lookup(&named_type) {
                                    if let Entry::Type(t) = entry {
                                        match t.clone() {
                                            Some(TypeExpression::NamedType(ident)) => {
                                                named_type = ident
                                            }
                                            type_expr => return type_expr,
                                        };
                                    } else {
                                        // already reported at build stage
                                        return None;
                                    }
                                }
                                // already reported at build stage
                                None
                            } else {
                                v.type_expr.clone()
                            }
                        }
                        _ => {
                            broker.report_error(
                                named.to_semantic_error(SemanticErrorMessage::NotAVariable),
                            );
                            None
                        }
                    }
                } else {
                    broker.report_error(
                        named.to_semantic_error(SemanticErrorMessage::UndefinedVariable),
                    );
                    None
                }
            }
        }
    }
}

impl<B: SemanticErrorBroker> AnalyzeExpression<B> for ArrayAccess {
    fn analyze(&self, table: &LookupTable, broker: B) -> Option<TypeExpression> {
        let index_type = self.index.analyze(table, broker.clone());
        if index_type != Some(TypeExpression::IntType) {
            broker.report_error(SemanticError(
                0..0,
                SemanticErrorMessage::IndexingWithNonInteger,
            ));
        };
        if let Some(array_type) = self.array.analyze(table, broker.clone()) {
            match array_type {
                TypeExpression::ArrayType { size: _, base_type } => base_type.map(|boxed| *boxed),
                _ => {
                    broker
                        .report_error(SemanticError(0..0, SemanticErrorMessage::IndexingNonArray));
                    None
                }
            }
        } else {
            None
        }
    }
}

impl<B: SemanticErrorBroker> AnalyzeExpression<B> for Expression {
    fn analyze(&self, table: &LookupTable, broker: B) -> Option<TypeExpression> {
        match self {
            Self::IntLiteral(_) => Some(TypeExpression::IntType),
            Self::Variable(v) => v.analyze(table, broker),
            Self::Binary(b) => b.analyze(table, broker),
            Self::Error => None,
        }
    }
}

impl<B: SemanticErrorBroker> AnalyzeExpression<B> for BinaryExpression {
    fn analyze(&self, table: &LookupTable, broker: B) -> Option<TypeExpression> {
        let lhs = self.lhs.analyze(table, broker.clone());
        let rhs = self.rhs.analyze(table, broker.clone());
        if lhs != rhs {
            broker.report_error(SemanticError(
                0..0,
                SemanticErrorMessage::OperatorDifferentTypes,
            ));
            // no immediate return with None
            // because operator still states the type intention
        }
        if lhs != Some(TypeExpression::IntType) {
            if self.operator.is_arithmetic() {
                broker.report_error(SemanticError(
                    0..0,
                    SemanticErrorMessage::ArithmeticOperatorNonInteger,
                ));
            } else {
                broker.report_error(SemanticError(
                    0..0,
                    SemanticErrorMessage::ComparisonNonInteger,
                ));
            }
            None
        } else if self.operator.is_arithmetic() {
            Some(TypeExpression::IntType)
        } else {
            // non arithmetic means comparison
            Some(TypeExpression::BoolType)
        }
    }
}
