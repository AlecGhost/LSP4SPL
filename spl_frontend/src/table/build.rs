use super::{
    DataType, GlobalEntry, GlobalTable, LocalEntry, LocalTable, LookupTable, ProcedureEntry,
    SymbolTable, TypeEntry, VariableEntry,
};
use crate::{
    ast::*,
    error::{BuildErrorMessage, SplError},
    lexer::token::{Token, TokenType},
    table::Entry,
    DiagnosticsBroker, ToRange,
};

#[cfg(test)]
mod tests;

/// Builds a `GlobalTable` for the given program.
/// Errors are reported by the specified broker.
pub(crate) fn build<B>(program: &Program, broker: &B) -> GlobalTable
where
    B: Clone + std::fmt::Debug + DiagnosticsBroker,
{
    let mut table = GlobalTable::initialized();
    program.build(&mut table, broker);
    table
}

trait TableBuilder<B> {
    fn build(&self, table: &mut GlobalTable, broker: &B);
}

impl<B: DiagnosticsBroker> TableBuilder<B> for Program {
    fn build(&self, table: &mut GlobalTable, broker: &B) {
        self.global_declarations
            .iter()
            .for_each(|dec| dec.build(table, broker));
        table.lookup("main").as_ref().map_or_else(
            || broker.report_error(SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())),
            |entry| {
                if let GlobalEntry::Procedure(main) = &entry {
                    if !main.parameters.is_empty() {
                        broker.report_error(SplError(
                            main.name.to_range(),
                            BuildErrorMessage::MainMustNotHaveParameters.to_string(),
                        ));
                    }
                } else {
                    panic!("'main' must be a procedure");
                }
            },
        );
    }
}

impl<B: DiagnosticsBroker> TableBuilder<B> for GlobalDeclaration {
    fn build(&self, table: &mut GlobalTable, broker: &B) {
        match self {
            Self::Type(t) => t.build(table, broker),
            Self::Procedure(p) => p.build(table, broker),
            Self::Error(_) => {}
        }
    }
}

impl<B: DiagnosticsBroker> TableBuilder<B> for TypeDeclaration {
    fn build(&self, table: &mut GlobalTable, broker: &B) {
        if let Some(name) = &self.name {
            if name.value == "main" {
                broker.report_error(SplError(
                    name.to_range(),
                    BuildErrorMessage::MainIsNotAProcedure.to_string(),
                ));
                return;
            }
            let documentation = get_documentation(&self.info.tokens);
            let lookup_table = LookupTable {
                global_table: Some(table),
                local_table: None,
            };
            if table
                .enter(
                    name.to_string(),
                    GlobalEntry::Type(TypeEntry {
                        name: name.clone(),
                        data_type: get_data_type(
                            &self.type_expr,
                            &self.name,
                            &lookup_table,
                            broker,
                        ),
                        doc: documentation,
                    }),
                )
                .is_err()
            {
                broker.report_error(name.to_error(BuildErrorMessage::RedeclarationAsType));
            }
        }
    }
}

impl<B: DiagnosticsBroker> TableBuilder<B> for ProcedureDeclaration {
    fn build(&self, table: &mut GlobalTable, broker: &B) {
        if let Some(name) = &self.name {
            let documentation = get_documentation(&self.info.tokens);
            let mut local_table = LocalTable::default();
            let parameters = self
                .parameters
                .iter()
                .filter_map(|param| build_parameter(param, table, &mut local_table, broker))
                .collect();
            self.variable_declarations
                .iter()
                .for_each(|dec| build_variable(dec, table, &mut local_table, broker));
            let entry = ProcedureEntry {
                name: name.clone(),
                local_table,
                parameters,
                doc: documentation,
            };
            if table
                .enter(name.to_string(), GlobalEntry::Procedure(entry))
                .is_err()
            {
                broker.report_error(name.to_error(BuildErrorMessage::RedeclarationAsProcedure));
            }
        }
    }
}

fn build_parameter<B: DiagnosticsBroker>(
    param: &ParameterDeclaration,
    global_table: &GlobalTable,
    local_table: &mut LocalTable,
    broker: &B,
) -> Option<VariableEntry> {
    if let ParameterDeclaration::Valid {
        is_ref,
        name: opt_name,
        type_expr,
        info,
    } = param
    {
        opt_name.as_ref().map(|name| {
            let documentation = get_documentation(&info.tokens);
            let lookup_table = LookupTable {
                global_table: Some(global_table),
                local_table: None,
            };
            let param_entry = VariableEntry {
                name: name.clone(),
                is_ref: *is_ref,
                data_type: get_data_type(type_expr, opt_name, &lookup_table, broker),
                doc: documentation,
            };
            if let Some(data_type) = &param_entry.data_type {
                if !data_type.is_primitive() && !param_entry.is_ref {
                    broker
                        .report_error(name.to_error(BuildErrorMessage::MustBeAReferenceParameter));
                }
            }
            if local_table
                .enter(name.to_string(), LocalEntry::Parameter(param_entry.clone()))
                .is_err()
            {
                broker.report_error(name.to_error(BuildErrorMessage::RedeclarationAsParameter))
            }
            param_entry
        })
    } else {
        None
    }
}

fn build_variable<B: DiagnosticsBroker>(
    var: &VariableDeclaration,
    global_table: &GlobalTable,
    local_table: &mut LocalTable,
    broker: &B,
) {
    if let VariableDeclaration::Valid {
        name: Some(name),
        type_expr,
        info,
    } = &var
    {
        let documentation = get_documentation(&info.tokens);
        let entry = VariableEntry {
            name: name.clone(),
            is_ref: false,
            data_type: get_data_type(
                type_expr,
                &Some(name.clone()),
                &LookupTable {
                    global_table: Some(global_table),
                    local_table: Some(local_table),
                },
                broker,
            ),
            doc: documentation,
        };
        if local_table
            .enter(name.to_string(), LocalEntry::Variable(entry))
            .is_err()
        {
            broker.report_error(name.to_error(BuildErrorMessage::RedeclarationAsVariable));
        }
    }
}

fn get_data_type<B: DiagnosticsBroker>(
    type_expr: &Option<TypeExpression>,
    caller: &Option<Identifier>,
    table: &LookupTable,
    broker: &B,
) -> Option<DataType> {
    type_expr.as_ref().and_then(|type_expr| {
        use TypeExpression::*;
        match type_expr {
            ArrayType {
                size, base_type, ..
            } => {
                if let (
                    Some(IntLiteral {
                        value: Some(size), ..
                    }),
                    Some(base_type),
                ) = (
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
                if name.value == "int" {
                    Some(DataType::Int)
                } else if let Some(entry) = table.lookup(&name.value) {
                    if let Entry::Type(t) = &entry {
                        t.data_type.clone()
                    } else {
                        broker.report_error(name.to_error(BuildErrorMessage::NotAType));
                        None
                    }
                } else {
                    broker.report_error(name.to_error(BuildErrorMessage::UndefinedType));
                    None
                }
            }
        }
    })
}

fn get_documentation(tokens: &[Token]) -> Option<String> {
    let documentation: String = tokens
        .iter()
        .map_while(|token| match &token.token_type {
            TokenType::Comment(comment) => Some(String::new() + comment + "\n"),
            _ => None,
        })
        .collect();
    if documentation.is_empty() {
        None
    } else {
        Some(documentation)
    }
}
