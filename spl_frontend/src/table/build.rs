use super::*;
use crate::ast::*;

#[cfg(test)]
mod tests;

trait BuildErrorBroker: Clone + std::fmt::Debug + DiagnosticsBroker<BuildError> {}

impl<T> BuildErrorBroker for T where T: Clone + std::fmt::Debug + DiagnosticsBroker<BuildError> {}

pub fn build<B>(program: &Program, broker: B) -> SymbolTable
where
    B: Clone + std::fmt::Debug + DiagnosticsBroker<BuildError>,
{
    let mut table = SymbolTable::initialized();
    program.build(&mut table, broker);
    table
}

trait TableBuilder<B> {
    fn build(&self, table: &mut SymbolTable, broker: B);
}

impl<B: BuildErrorBroker> TableBuilder<B> for Program {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        self.global_declarations
            .iter()
            .for_each(|dec| dec.build(table, broker.clone()));
        match &table.entries.iter().find(|(key, _)| key.value == "main") {
            Some((_, entry)) => {
                if let Entry::Procedure(main) = entry {
                    if !main.parameters.is_empty() {
                        broker.report_error(BuildError(
                            0..0,
                            BuildErrorMessage::MainMustNotHaveParameters,
                        ));
                    }
                } else {
                    panic!("'main' must be a procedure");
                }
            }
            None => {
                broker.report_error(BuildError(0..0, BuildErrorMessage::MainIsMissing));
            }
        };
    }
}

impl<B: BuildErrorBroker> TableBuilder<B> for GlobalDeclaration {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        match self {
            Self::Type(t) => t.build(table, broker),
            Self::Procedure(p) => p.build(table, broker),
            Self::Error => {}
        }
    }
}

impl<B: BuildErrorBroker> TableBuilder<B> for TypeDeclaration {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        if let Some(name) = &self.name {
            if name.value == "main" {
                broker.report_error(BuildError(
                    name.range.clone(),
                    BuildErrorMessage::MainIsNotAProcedure,
                ));
                return;
            }
            if let Some(t) = get_underlying_type(&self.type_expr) {
                if let Some(entry) = table.lookup(t) {
                    if !matches!(entry, Entry::Type(_)) {
                        broker.report_error(t.to_build_error(BuildErrorMessage::NotAType))
                    }
                } else {
                    broker.report_error(t.to_build_error(BuildErrorMessage::UndefinedType));
                }
            }
            table.enter(name.clone(), Entry::Type(self.type_expr.clone()), || {
                broker.report_error(name.to_build_error(BuildErrorMessage::RedeclarationAsType))
            });
        }
    }
}

impl<B: BuildErrorBroker> TableBuilder<B> for ProcedureDeclaration {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        if let Some(name) = &self.name {
            let mut local_table = SymbolTable::new();
            let parameters = self
                .parameters
                .iter()
                .map(|param| VariableEntry::from(param.clone()))
                .collect();
            self.parameters
                .iter()
                .for_each(|dec| dec.build(&mut local_table, broker.clone()));
            self.variable_declarations
                .iter()
                .for_each(|dec| dec.build(&mut local_table, broker.clone()));
            local_table.entries.values().for_each(|value| {
                if let Entry::Variable(entry) = value {
                    if let Some(t) = get_underlying_type(&entry.type_expr) {
                        if let Some(entry) = table.lookup(t) {
                            if !matches!(entry, Entry::Type(_)) {
                                broker.report_error(t.to_build_error(BuildErrorMessage::NotAType))
                            }
                        } else {
                            broker.report_error(t.to_build_error(BuildErrorMessage::UndefinedType));
                        }
                    }
                }
            });
            let entry = ProcedureEntry {
                local_table,
                parameters,
            };
            table.enter(name.clone(), Entry::Procedure(entry), || {
                broker
                    .report_error(name.to_build_error(BuildErrorMessage::RedeclarationAsProcedure));
            });
        }
    }
}

impl<B: BuildErrorBroker> TableBuilder<B> for ParameterDeclaration {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        if let Some(name) = &self.name {
            if let Some(type_expr) = &self.type_expr {
                let is_primitive = matches!(
                    type_expr,
                    TypeExpression::IntType | TypeExpression::BoolType
                );
                if !is_primitive && !self.is_ref {
                    broker.report_error(
                        name.to_build_error(BuildErrorMessage::MustBeAReferenceParameter),
                    );
                }
            }
            table.enter(
                name.clone(),
                Entry::Variable(VariableEntry::from(self.clone())),
                || {
                    broker.report_error(
                        name.to_build_error(BuildErrorMessage::RedeclarationAsParameter),
                    )
                },
            );
        }
    }
}

impl<B: BuildErrorBroker> TableBuilder<B> for VariableDeclaration {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        if let Some(name) = &self.name {
            table.enter(
                name.clone(),
                Entry::Variable(VariableEntry::from(self.clone())),
                || {
                    broker.report_error(
                        name.to_build_error(BuildErrorMessage::RedeclarationAsVariable),
                    )
                },
            );
        }
    }
}

impl From<ParameterDeclaration> for VariableEntry {
    fn from(value: ParameterDeclaration) -> Self {
        Self {
            is_ref: value.is_ref,
            type_expr: value.type_expr,
        }
    }
}

impl From<VariableDeclaration> for VariableEntry {
    fn from(value: VariableDeclaration) -> Self {
        Self {
            is_ref: false,
            type_expr: value.type_expr,
        }
    }
}

fn get_underlying_type(type_expr: &Option<TypeExpression>) -> Option<&Identifier> {
    if let Some(type_expr) = type_expr {
        let mut type_expr = type_expr;
        while let TypeExpression::ArrayType { size: _, base_type } = type_expr {
            if let Some(next_level) = base_type {
                type_expr = next_level;
            }
        }
        if let TypeExpression::NamedType(ident) = type_expr {
            return Some(ident);
        }
    }
    None
}
