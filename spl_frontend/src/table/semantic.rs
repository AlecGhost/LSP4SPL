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
    program: &mut Program,
    table: &GlobalTable,
    broker: &B,
) {
    program
        .global_declarations
        .iter_mut()
        .map(|r| r.as_mut())
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
                        .iter_mut()
                        .for_each(|stmt| stmt.analyze(lookup_table, broker));
                }
            }
        });
}

trait AnalyzeStatement<B> {
    fn analyze(&mut self, table: &LookupTable, broker: &B);
}

trait AnalyzeExpression<B> {
    fn analyze(&mut self, table: &LookupTable, broker: &B) -> Option<DataType>;
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for Statement {
    fn analyze(&mut self, table: &LookupTable, broker: &B) {
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
    fn analyze(&mut self, table: &LookupTable, broker: &B) {
        if let Some(expr) = &mut self.expr {
            let left = self.variable.analyze(table, broker);
            let right = expr.analyze(table, broker);
            // only analyze further if type information for both sides is available
            if let (Some(left), Some(right)) = (left, right) {
                if left != right {
                    self.info.append_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::AssignmentHasDifferentTypes.to_string(),
                    ));
                } else if !matches!(left, DataType::Int) {
                    self.info.append_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::AssignmentRequiresIntegers.to_string(),
                    ));
                }
            }
        }
    }
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for BlockStatement {
    fn analyze(&mut self, table: &LookupTable, broker: &B) {
        self.statements
            .iter_mut()
            .for_each(|stmt| stmt.analyze(table, broker));
    }
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for CallStatement {
    fn analyze(&mut self, table: &LookupTable, broker: &B) {
        if let Some(entry) = table.lookup(&self.name.value) {
            if let Entry::Procedure(proc_entry) = &entry {
                let arg_len = self.arguments.len();
                let param_len = proc_entry.parameters.len();
                match arg_len.cmp(&param_len) {
                    Ordering::Equal => { /* happy path */ }
                    Ordering::Less => {
                        self.info.append_error(SplError(
                            self.to_range(),
                            SemanticErrorMessage::TooFewArguments(self.name.value.clone())
                                .to_string(),
                        ));
                    }
                    Ordering::Greater => {
                        self.info.append_error(SplError(
                            self.to_range(),
                            SemanticErrorMessage::TooManyArguments(self.name.value.clone())
                                .to_string(),
                        ));
                    }
                };
                for (i, (arg, param)) in std::iter::zip(
                    self.arguments.iter_mut().map(|r| r.as_mut()),
                    &proc_entry.parameters,
                )
                .enumerate()
                {
                    let range = arg.to_range();
                    let is_variable = matches!(arg, Expression::Variable(_));
                    if param.is_ref && !is_variable {
                        arg.info_mut().append_error(SplError(
                            range.clone(),
                            SemanticErrorMessage::ArgumentMustBeAVariable(
                                self.name.value.clone(),
                                // enumeration starts with 1
                                i + 1,
                            )
                            .to_string(),
                        ));
                    }
                    let arg_type = arg.analyze(table, broker);
                    // only analyze further if type information for both sides is available
                    if let (Some(arg_type), Some(param_type)) = (&arg_type, &param.data_type) {
                        if arg_type != param_type {
                            arg.info_mut().append_error(SplError(
                                range,
                                SemanticErrorMessage::ArgumentsTypeMismatch(
                                    self.name.value.clone(),
                                    // enumeration starts with 1
                                    i + 1,
                                )
                                .to_string(),
                            ));
                        }
                    }
                }
            } else {
                self.info.append_error(SplError(
                    self.to_range(),
                    SemanticErrorMessage::CallOfNoneProcedure(self.name.value.clone()).to_string(),
                ));
            }
        } else {
            self.info.append_error(SplError(
                self.to_range(),
                SemanticErrorMessage::UndefinedProcedure(self.name.value.clone()).to_string(),
            ));
        }
    }
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for IfStatement {
    fn analyze(&mut self, table: &LookupTable, broker: &B) {
        if let Some(condition) = &mut self.condition {
            if let Some(condition_type) = condition.analyze(table, broker) {
                if condition_type != DataType::Bool {
                    let range = condition.to_range();
                    condition.info_mut().append_error(SplError(
                        range,
                        SemanticErrorMessage::IfConditionMustBeBoolean.to_string(),
                    ));
                }
            }
        }
        if let Some(stmt) = &mut self.if_branch {
            stmt.analyze(table, broker);
        }
        if let Some(stmt) = &mut self.else_branch {
            stmt.analyze(table, broker);
        };
    }
}

impl<B: DiagnosticsBroker> AnalyzeStatement<B> for WhileStatement {
    fn analyze(&mut self, table: &LookupTable, broker: &B) {
        if let Some(condition) = &mut self.condition {
            if let Some(condition_type) = condition.analyze(table, broker) {
                if condition_type != DataType::Bool {
                    let range = condition.to_range();
                    condition.info_mut().append_error(SplError(
                        range,
                        SemanticErrorMessage::WhileConditionMustBeBoolean.to_string(),
                    ));
                }
            }
        }
        if let Some(stmt) = &mut self.statement {
            stmt.analyze(table, broker);
        }
    }
}

impl<B: DiagnosticsBroker> AnalyzeExpression<B> for Variable {
    fn analyze(&mut self, table: &LookupTable, broker: &B) -> Option<DataType> {
        match self {
            Self::ArrayAccess(a) => a.analyze(table, broker),
            Self::NamedVariable(named) => {
                if let Some(entry) = table.lookup(&named.value) {
                    match &entry {
                        Entry::Variable(v) | Entry::Parameter(v) => v.data_type.clone(),
                        _ => {
                            named
                                .info
                                .append_error(named.to_error(SemanticErrorMessage::NotAVariable));
                            None
                        }
                    }
                } else {
                    named
                        .info
                        .append_error(named.to_error(SemanticErrorMessage::UndefinedVariable));
                    None
                }
            }
        }
    }
}

impl<B: DiagnosticsBroker> AnalyzeExpression<B> for ArrayAccess {
    fn analyze(&mut self, table: &LookupTable, broker: &B) -> Option<DataType> {
        if let Some(index) = &mut self.index {
            let index_type = index.analyze(table, broker);
            match index_type {
                Some(DataType::Int) => { /* happy path */ }
                Some(_) => {
                    let range = index.to_range();
                    index.info_mut().append_error(SplError(
                        range,
                        SemanticErrorMessage::IndexingWithNonInteger.to_string(),
                    ));
                }
                None => {
                    // Index has no type information.
                    // Therefore an error already occurred
                    // and nothing is reported here to prevent spurious errors.
                }
            }
        }
        self.array
            .analyze(table, broker)
            .and_then(|array_type| match array_type {
                DataType::Array { base_type, .. } => Some(*base_type),
                _ => {
                    self.info.append_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::IndexingNonArray.to_string(),
                    ));
                    None
                }
            })
    }
}

impl<B: DiagnosticsBroker> AnalyzeExpression<B> for Expression {
    fn analyze(&mut self, table: &LookupTable, broker: &B) -> Option<DataType> {
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
    fn analyze(&mut self, table: &LookupTable, broker: &B) -> Option<DataType> {
        let lhs_type = self.lhs.analyze(table, broker);
        let rhs_type = self.rhs.analyze(table, broker);

        match (lhs_type, rhs_type) {
            (Some(DataType::Int), Some(DataType::Int)) => { /* happy path */ }
            (Some(DataType::Int), Some(_)) | (Some(_), Some(DataType::Int)) => {
                self.info.append_error(SplError(
                    self.to_range(),
                    SemanticErrorMessage::OperatorDifferentTypes.to_string(),
                ));
            }
            (Some(_), Some(_)) => {
                if self.operator.is_arithmetic() {
                    self.info.append_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::ArithmeticOperatorNonInteger.to_string(),
                    ));
                } else {
                    self.info.append_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::ComparisonNonInteger.to_string(),
                    ));
                }
            }
            _ => {
                // At least one expression has no type information.
                // Therefore an error already occurred
                // and nothing is reported here to prevent spurious errors.
            }
        }

        // Type is always inferable from operator.
        if self.operator.is_arithmetic() {
            Some(DataType::Int)
        } else {
            // non arithmetic means comparison
            Some(DataType::Bool)
        }
    }
}
