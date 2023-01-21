use super::*;
use crate::ast::*;

trait TableErrorBroker: Clone + std::fmt::Debug + DiagnosticsBroker<TableError> {}

impl<T> TableErrorBroker for T where T: Clone + std::fmt::Debug + DiagnosticsBroker<TableError> {}

pub fn build<B>(program: &Program, broker: B) -> SymbolTable
where
    B: Clone + std::fmt::Debug + DiagnosticsBroker<TableError>,
{
    let mut table = SymbolTable::new();
    program.build(&mut table, broker);
    table
}

trait TableBuilder<B> {
    fn build(&self, table: &mut SymbolTable, broker: B);
}

impl<B: TableErrorBroker> TableBuilder<B> for Program {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        self.global_declarations
            .iter()
            .for_each(|dec| dec.build(table, broker.clone()));
        if table
            .entries
            .keys()
            .find(|key| key.value == "main")
            .is_none()
        {
            broker.report_error(TableError(0..0, ErrorMessage::MainIsMissing));
        }
    }
}

impl<B: TableErrorBroker> TableBuilder<B> for GlobalDeclaration {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        match self {
            Self::Type(t) => t.build(table, broker),
            Self::Procedure(p) => p.build(table, broker),
            Self::Error => {}
        }
    }
}

impl<B: TableErrorBroker> TableBuilder<B> for TypeDeclaration {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        if let Some(name) = &self.name {
            if name.value == "main" {
                broker.report_error(TableError(
                    name.range.clone(),
                    ErrorMessage::MainIsNotAProcedure,
                ));
                return;
            }
            if let Some(t) = get_underlying_type(&self.type_expr) {
                if table.lookup(t).is_none() {
                    broker.report_error(t.to_table_error(ErrorMessage::UndefinedType));
                }
            }
            table.enter(name.clone(), Entry::Type(self.type_expr.clone()), || {
                broker.report_error(name.to_table_error(ErrorMessage::RedeclarationAsType))
            });
        }
    }
}

impl<B: TableErrorBroker> TableBuilder<B> for ProcedureDeclaration {
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
                if let Entry::Parameter(entry) | Entry::Variable(entry) = value {
                    if let Some(t) = get_underlying_type(&entry.type_expr) {
                        if table.lookup(t).is_none() {
                            broker.report_error(t.to_table_error(ErrorMessage::UndefinedType));
                        }
                    }
                }
            });
            let entry = ProcedureEntry {
                local_table,
                parameters,
            };
            table.enter(name.clone(), Entry::Procedure(entry), || {
                broker.report_error(name.to_table_error(ErrorMessage::RedeclarationAsProcedure));
            });
        }
    }
}

impl<B: TableErrorBroker> TableBuilder<B> for ParameterDeclaration {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        if let Some(name) = &self.name {
            if let Some(type_expr) = &self.type_expr {
                let is_primitive = matches!(
                    type_expr,
                    TypeExpression::IntType | TypeExpression::BoolType
                );
                if !is_primitive && !self.is_ref {
                    broker
                        .report_error(name.to_table_error(ErrorMessage::MustBeAReferenceParameter));
                }
            }
            table.enter(
                name.clone(),
                Entry::Parameter(VariableEntry::from(self.clone())),
                || broker.report_error(name.to_table_error(ErrorMessage::RedeclarationAsParameter)),
            );
        }
    }
}

impl<B: TableErrorBroker> TableBuilder<B> for VariableDeclaration {
    fn build(&self, table: &mut SymbolTable, broker: B) {
        if let Some(name) = &self.name {
            table.enter(
                name.clone(),
                Entry::Variable(VariableEntry::from(self.clone())),
                || broker.report_error(name.to_table_error(ErrorMessage::RedeclarationAsVariable)),
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
