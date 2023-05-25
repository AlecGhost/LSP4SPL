use super::{DataType, Entry, GlobalEntry, GlobalTable, LookupTable, SymbolTable};
use crate::{
    ast::{
        ArrayAccess, Assignment, BinaryExpression, BlockStatement, CallStatement, Expression,
        GlobalDeclaration, IfStatement, Program, Statement, Variable, WhileStatement,
    },
    error::{SemanticErrorMessage, SplError},
    ToRange,
};
use std::cmp::Ordering;

#[cfg(test)]
mod tests;

/// Analyzes the given program for semantic errors.
/// Errors are reported by the specifie.
pub fn analyze(program: &mut Program, table: &GlobalTable) {
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
                        .for_each(|stmt| stmt.analyze(lookup_table));
                }
            }
        });
}

trait AnalyzeStatement {
    fn analyze(&mut self, table: &LookupTable);
}

trait AnalyzeExpression {
    fn analyze(&mut self, table: &LookupTable) -> Option<DataType>;
}

impl AnalyzeStatement for Statement {
    fn analyze(&mut self, table: &LookupTable) {
        match self {
            Self::Assignment(stmt) => stmt.analyze(table),
            Self::Block(stmt) => stmt.analyze(table),
            Self::Call(stmt) => stmt.analyze(table),
            Self::If(stmt) => stmt.analyze(table),
            Self::While(stmt) => stmt.analyze(table),
            Self::Empty(_) | Self::Error(_) => {}
        };
    }
}

impl AnalyzeStatement for Assignment {
    fn analyze(&mut self, table: &LookupTable) {
        if let Some(expr) = &mut self.expr {
            let left = self.variable.analyze(table);
            let right = expr.analyze(table);
            // only analyze further if type information for both sides is available
            if let (Some(left), Some(right)) = (left, right) {
                if left != right {
                    self.info.append_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::AssignmentHasDifferentTypes.into(),
                    ));
                } else if !matches!(left, DataType::Int) {
                    self.info.append_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::AssignmentRequiresIntegers.into(),
                    ));
                }
            }
        }
    }
}

impl AnalyzeStatement for BlockStatement {
    fn analyze(&mut self, table: &LookupTable) {
        self.statements
            .iter_mut()
            .for_each(|stmt| stmt.analyze(table));
    }
}

impl AnalyzeStatement for CallStatement {
    fn analyze(&mut self, table: &LookupTable) {
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
                                .into(),
                        ));
                    }
                    Ordering::Greater => {
                        self.info.append_error(SplError(
                            self.to_range(),
                            SemanticErrorMessage::TooManyArguments(self.name.value.clone())
                                .into(),
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
                            .into(),
                        ));
                    }
                    let arg_type = arg.analyze(table);
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
                                .into(),
                            ));
                        }
                    }
                }
            } else {
                self.info.append_error(SplError(
                    self.to_range(),
                    SemanticErrorMessage::CallOfNoneProcedure(self.name.value.clone()).into(),
                ));
            }
        } else {
            self.info.append_error(SplError(
                self.to_range(),
                SemanticErrorMessage::UndefinedProcedure(self.name.value.clone()).into(),
            ));
        }
    }
}

impl AnalyzeStatement for IfStatement {
    fn analyze(&mut self, table: &LookupTable) {
        if let Some(condition) = &mut self.condition {
            if let Some(condition_type) = condition.analyze(table) {
                if condition_type != DataType::Bool {
                    let range = condition.as_ref().to_range();
                    condition.info_mut().append_error(SplError(
                        range,
                        SemanticErrorMessage::IfConditionMustBeBoolean.into(),
                    ));
                }
            }
        }
        if let Some(stmt) = &mut self.if_branch {
            stmt.analyze(table);
        }
        if let Some(stmt) = &mut self.else_branch {
            stmt.analyze(table);
        };
    }
}

impl AnalyzeStatement for WhileStatement {
    fn analyze(&mut self, table: &LookupTable) {
        if let Some(condition) = &mut self.condition {
            if let Some(condition_type) = condition.analyze(table) {
                if condition_type != DataType::Bool {
                    let range = condition.as_ref().to_range();
                    condition.info_mut().append_error(SplError(
                        range,
                        SemanticErrorMessage::WhileConditionMustBeBoolean.into(),
                    ));
                }
            }
        }
        if let Some(stmt) = &mut self.statement {
            stmt.analyze(table);
        }
    }
}

impl AnalyzeExpression for Variable {
    fn analyze(&mut self, table: &LookupTable) -> Option<DataType> {
        match self {
            Self::ArrayAccess(a) => a.analyze(table),
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

impl AnalyzeExpression for ArrayAccess {
    fn analyze(&mut self, table: &LookupTable) -> Option<DataType> {
        if let Some(index) = &mut self.index {
            let index_type = index.analyze(table);
            match index_type {
                Some(DataType::Int) => { /* happy path */ }
                Some(_) => {
                    let range = index.as_ref().as_ref().to_range();
                    index.info_mut().append_error(SplError(
                        range,
                        SemanticErrorMessage::IndexingWithNonInteger.into(),
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
            .analyze(table)
            .and_then(|array_type| match array_type {
                DataType::Array { base_type, .. } => Some(*base_type),
                _ => {
                    self.info.append_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::IndexingNonArray.into(),
                    ));
                    None
                }
            })
    }
}

impl AnalyzeExpression for Expression {
    fn analyze(&mut self, table: &LookupTable) -> Option<DataType> {
        match self {
            Self::IntLiteral(_) => Some(DataType::Int),
            Self::Variable(v) => v.analyze(table),
            Self::Binary(b) => b.analyze(table),
            Self::Unary(u) => u.expr.analyze(table),
            Self::Bracketed(b) => b.expr.analyze(table),
            Self::Error(_) => None,
        }
    }
}

impl AnalyzeExpression for BinaryExpression {
    fn analyze(&mut self, table: &LookupTable) -> Option<DataType> {
        let lhs_type = self.lhs.analyze(table);
        let rhs_type = self.rhs.analyze(table);

        match (lhs_type, rhs_type) {
            (Some(DataType::Int), Some(DataType::Int)) => { /* happy path */ }
            (Some(DataType::Int), Some(_)) | (Some(_), Some(DataType::Int)) => {
                self.info.append_error(SplError(
                    self.to_range(),
                    SemanticErrorMessage::OperatorDifferentTypes.into(),
                ));
            }
            (Some(_), Some(_)) => {
                if self.operator.is_arithmetic() {
                    self.info.append_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::ArithmeticOperatorNonInteger.into(),
                    ));
                } else {
                    self.info.append_error(SplError(
                        self.to_range(),
                        SemanticErrorMessage::ComparisonNonInteger.into(),
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
