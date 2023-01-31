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
            Self::Error(_) => {}
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
            table.enter(
                name.clone(),
                Entry::Type(get_data_type(
                    &self.type_expr,
                    &self.name,
                    table,
                    broker.clone(),
                )),
                || broker.report_error(name.to_build_error(BuildErrorMessage::RedeclarationAsType)),
            );
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
                .map(|param| build_parameter(param, table, &mut local_table, broker.clone()))
                .collect();
            self.variable_declarations
                .iter()
                .for_each(|dec| build_variable(dec, table, &mut local_table, broker.clone()));
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

fn build_parameter<B: BuildErrorBroker>(
    param: &ParameterDeclaration,
    global_table: &SymbolTable,
    local_table: &mut SymbolTable,
    broker: B,
) -> VariableEntry {
    let param_entry = VariableEntry {
        is_ref: param.is_ref,
        data_type: get_data_type(&param.type_expr, &param.name, global_table, broker.clone()),
    };
    if let Some(name) = &param.name {
        if let Some(data_type) = &param_entry.data_type {
            if !data_type.is_primitive() && !param.is_ref {
                broker.report_error(
                    name.to_build_error(BuildErrorMessage::MustBeAReferenceParameter),
                );
            }
        }
        local_table.enter(name.clone(), Entry::Variable(param_entry.clone()), || {
            broker.report_error(name.to_build_error(BuildErrorMessage::RedeclarationAsParameter))
        });
    };
    param_entry
}

fn build_variable<B: BuildErrorBroker>(
    var: &VariableDeclaration,
    global_table: &SymbolTable,
    local_table: &mut SymbolTable,
    broker: B,
) {
    let entry = VariableEntry {
        is_ref: false,
        data_type: get_data_type(
            &var.type_expr,
            &var.name,
            &LookupTable {
                global_table,
                local_table,
            },
            broker.clone(),
        ),
    };
    if let Some(name) = &var.name {
        local_table.enter(name.clone(), Entry::Variable(entry), || {
            broker.report_error(name.to_build_error(BuildErrorMessage::RedeclarationAsVariable))
        });
    }
}

fn get_data_type<T: Table, B: BuildErrorBroker>(
    type_expr: &Option<TypeExpression>,
    caller: &Option<Identifier>,
    table: &T,
    broker: B,
) -> Option<DataType> {
    if let Some(type_expr) = type_expr {
        use TypeExpression::*;
        match type_expr {
            IntType => Some(DataType::Int),
            ArrayType { size, base_type } => {
                if let (Some(size), Some(base_type)) = (
                    size,
                    get_data_type(
                        &base_type.clone().map(|boxed| *boxed),
                        caller,
                        table,
                        broker,
                    ),
                ) {
                    caller.as_ref().map(|creator| DataType::Array {
                        size: *size,
                        base_type: Box::new(base_type),
                        creator: creator.clone(),
                    })
                } else {
                    None
                }
            }
            NamedType(name) => {
                if let Some(entry) = table.lookup(name) {
                    if let Entry::Type(t) = entry {
                        t.clone()
                    } else {
                        broker.report_error(name.to_build_error(BuildErrorMessage::NotAType));
                        None
                    }
                } else {
                    broker.report_error(name.to_build_error(BuildErrorMessage::UndefinedType));
                    None
                }
            }
        }
    } else {
        None
    }
}
