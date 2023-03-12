use super::{DataType, Entry, GlobalEntry, GlobalTable, LookupTable, SymbolTable};
use crate::{
    ast::{
        ArrayAccess, Assignment, BinaryExpression, BlockStatement, CallStatement, Expression,
        GlobalDeclaration, IfStatement, Program, Statement, Variable, WhileStatement,
    },
    error::{SemanticErrorMessage, SplError},
    DiagnosticsBroker, ToRange,
};
use std::cmp::Ordering;

#[cfg(test)]
mod tests;

/// Analyzes the given program for semantic errors.
/// Errors are reported by the specified broker.
pub(crate) fn analyze<B: Clone + std::fmt::Debug + DiagnosticsBroker>(
    program: &Program,
    table: &GlobalTable,
    broker: &B,
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
                let entry = table
                    .lookup(&name.value)
                    .expect("Named declaration without entry");
                if let GlobalEntry::Procedure(proc_entry) = &entry {
                    let lookup_table = &LookupTable {
                        local_table: Some(&proc_entry.local_table),
                        global_table: Some(table),
                    };
                    proc.statements
                        .iter()
                        .for_each(|stmt| stmt.analyze(lookup_table, broker));
                }
            }
        });
}

trait AnalyzeStatement<B> {
    fn analyze(&self, table: &LookupTable, broker: &B);
}

trait AnalyzeExpression<B> {
    fn analyze(&self, table: &LookupTable, broker: &B) -> Option<DataType>;
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for Statement {
    fn analyze(&self, table: &LookupTable, broker: &B) {
        match self {
            Self::Assignment(stmt) => stmt.analyze(table, broker),
            Self::Block(stmt) => stmt.analyze(table, broker),
            Self::Call(stmt) => stmt.analyze(table, broker),
            Self::If(stmt) => stmt.analyze(table, broker),
            Self::While(stmt) => stmt.analyze(table, broker),
            Self::Empty(_) | Self::Error(_) => {}
        };
    }
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for Assignment {
    fn analyze(&self, table: &LookupTable, broker: &B) {
        if let Some(expr) = &self.expr {
            let left = self.variable.analyze(table, broker);
            let right = expr.analyze(table, broker);
            // only analyze further if type information for both sides is available
            if let (Some(left), Some(right)) = (left, right) {
                if left != right {
                    broker.report_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::AssignmentHasDifferentTypes.to_string(),
                    ));
                } else if !matches!(left, DataType::Int) {
                    broker.report_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::AssignmentRequiresIntegers.to_string(),
                    ));
                }
            }
        }
    }
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for BlockStatement {
    fn analyze(&self, table: &LookupTable, broker: &B) {
        self.statements
            .iter()
            .for_each(|stmt| stmt.analyze(table, broker));
    }
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for CallStatement {
    fn analyze(&self, table: &LookupTable, broker: &B) {
        table.lookup(&self.name.value).map_or_else(
            || {
                broker.report_error(SplError(
                    self.to_range(),
                    SemanticErrorMessage::UndefinedProcedure(self.name.value.clone()).to_string(),
                ));
            },
            |entry| {
                if let Entry::Procedure(proc_entry) = &entry {
                    let arg_len = self.arguments.len();
                    let params = proc_entry.parameters();
                    let param_len = params.len();
                    match arg_len.cmp(&param_len) {
                        Ordering::Less => {
                            broker.report_error(SplError(
                                self.to_range(),
                                SemanticErrorMessage::TooFewArguments(self.name.value.clone())
                                    .to_string(),
                            ));
                        }
                        Ordering::Greater => {
                            broker.report_error(SplError(
                                self.to_range(),
                                SemanticErrorMessage::TooManyArguments(self.name.value.clone())
                                    .to_string(),
                            ));
                        }
                        Ordering::Equal => {}
                    };
                    for (i, (arg, param)) in
                        std::iter::zip(&self.arguments, params).enumerate()
                    {
                        if param.is_ref && !matches!(arg, Expression::Variable(_)) {
                            broker.report_error(SplError(
                                self.name.to_range(),
                                SemanticErrorMessage::ArgumentMustBeAVariable(
                                    self.name.value.clone(),
                                    // enumeration starts with 1
                                    i + 1,
                                )
                                .to_string(),
                            ));
                        }
                        if arg.analyze(table, broker) != param.data_type {
                            broker.report_error(SplError(
                                self.name.to_range(),
                                SemanticErrorMessage::ArgumentsTypeMismatch(
                                    self.name.value.clone(),
                                    // enumeration starts with 1
                                    i + 1,
                                )
                                .to_string(),
                            ));
                        }
                    }
                } else {
                    broker.report_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::CallOfNoneProcedure(self.name.value.clone())
                            .to_string(),
                    ));
                }
            },
        );
    }
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for IfStatement {
    fn analyze(&self, table: &LookupTable, broker: &B) {
        if let Some(condition) = &self.condition {
            if let Some(condition_type) = condition.analyze(table, broker) {
                if condition_type != DataType::Bool {
                    broker.report_error(SplError(
                        condition.to_range(),
                        SemanticErrorMessage::IfConditionMustBeBoolean.to_string(),
                    ));
                }
            }
        }
        if let Some(stmt) = &self.if_branch {
            stmt.analyze(table, broker);
        }
        if let Some(stmt) = &self.else_branch {
            stmt.analyze(table, broker);
        };
    }
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for WhileStatement {
    fn analyze(&self, table: &LookupTable, broker: &B) {
        if let Some(condition) = &self.condition {
            if let Some(condition_type) = condition.analyze(table, broker) {
                if condition_type != DataType::Bool {
                    broker.report_error(SplError(
                        condition.to_range(),
                        SemanticErrorMessage::WhileConditionMustBeBoolean.to_string(),
                    ));
                }
            }
        }
        if let Some(stmt) = &self.statement {
            stmt.analyze(table, broker);
        }
    }
}

impl<B: DiagnosticsBroker> AnalyzeExpression<B> for Variable {
    fn analyze(&self, table: &LookupTable, broker: &B) -> Option<DataType> {
        match self {
            Self::ArrayAccess(a) => a.analyze(table, broker),
            Self::NamedVariable(named) => table.lookup(&named.value).map_or_else(
                || {
                    broker.report_error(named.to_error(SemanticErrorMessage::UndefinedVariable));
                    None
                },
                |entry| match &entry {
                    Entry::Variable(v) | Entry::Parameter(v) => v.data_type.clone(),
                    _ => {
                        broker.report_error(named.to_error(SemanticErrorMessage::NotAVariable));
                        None
                    }
                },
            ),
        }
    }
}

impl<B: DiagnosticsBroker> AnalyzeExpression<B> for ArrayAccess {
    fn analyze(&self, table: &LookupTable, broker: &B) -> Option<DataType> {
        if let Some(index) = &self.index {
            let index_type = index.analyze(table, broker);
            if index_type != Some(DataType::Int) {
                broker.report_error(SplError(
                    index.to_range(),
                    SemanticErrorMessage::IndexingWithNonInteger.to_string(),
                ));
            };
        }
        self.array
            .analyze(table, broker)
            .and_then(|array_type| match array_type {
                DataType::Array { base_type, .. } => Some(*base_type),
                _ => {
                    broker.report_error(SplError(
                        self.array.to_range(),
                        SemanticErrorMessage::IndexingNonArray.to_string(),
                    ));
                    None
                }
            })
    }
}

impl<B: DiagnosticsBroker> AnalyzeExpression<B> for Expression {
    fn analyze(&self, table: &LookupTable, broker: &B) -> Option<DataType> {
        match self {
            Self::IntLiteral(_) => Some(DataType::Int),
            Self::Variable(v) => v.analyze(table, broker),
            Self::Binary(b) => b.analyze(table, broker),
            Self::Unary(u) => u.expr.analyze(table, broker),
            Self::Bracketed(b) => b.expr.analyze(table, broker),
            Self::Error(_) => None,
        }
    }
}

impl<B: DiagnosticsBroker> AnalyzeExpression<B> for BinaryExpression {
    fn analyze(&self, table: &LookupTable, broker: &B) -> Option<DataType> {
        let lhs = self.lhs.analyze(table, broker);
        let rhs = self.rhs.analyze(table, broker);
        if lhs != rhs {
            broker.report_error(SplError(
                self.to_range(),
                SemanticErrorMessage::OperatorDifferentTypes.to_string(),
            ));
            // no immediate return with None
            // because operator still states the type intention
        }
        if lhs != Some(DataType::Int) {
            if self.operator.is_arithmetic() {
                broker.report_error(SplError(
                    self.to_range(),
                    SemanticErrorMessage::ArithmeticOperatorNonInteger.to_string(),
                ));
            } else {
                broker.report_error(SplError(
                    self.to_range(),
                    SemanticErrorMessage::ComparisonNonInteger.to_string(),
                ));
            }
            None
        } else if self.operator.is_arithmetic() {
            Some(DataType::Int)
        } else {
            // non arithmetic means comparison
            Some(DataType::Bool)
        }
    }
}
