use super::{
    DataType, GlobalEntry, GlobalTable, LocalEntry, LocalTable, LookupTable, ProcedureEntry,
    SymbolTable, TypeEntry, VariableEntry,
};
use crate::{
    ast::*,
    error::{BuildErrorMessage, SplError},
    table::Entry,
    Shiftable, ToRange,
};

#[cfg(test)]
mod tests;

/// Builds a `GlobalTable` for the given program.
pub fn build(program: &mut Program) -> GlobalTable {
    let mut table = GlobalTable::initialized();
    program.build(&mut table, 0);
    table
}

trait TableBuilder {
    fn build(&mut self, table: &mut GlobalTable, offset: usize);
}

impl TableBuilder for Program {
    fn build(&mut self, table: &mut GlobalTable, offset: usize) {
        self.global_declarations
            .iter_mut()
            .map(|dec| {
                let offset = offset + dec.offset;
                (dec, offset)
            })
            .for_each(|(dec, offset)| dec.build(table, offset));
        if let Some(entry) = table.lookup("main").as_ref() {
            if let GlobalEntry::Procedure(main) = &entry {
                if !main.parameters.is_empty() {
                    self.info.append_error(SplError(
                        main.name.to_range(),
                        BuildErrorMessage::MainMustNotHaveParameters.to_string(),
                    ));
                }
            } else {
                panic!("'main' must be a procedure");
            }
        } else {
            self.info
                .append_error(SplError(0..0, BuildErrorMessage::MainIsMissing.to_string()));
        }
    }
}

impl TableBuilder for GlobalDeclaration {
    fn build(&mut self, table: &mut GlobalTable, offset: usize) {
        match self {
            Self::Type(t) => t.build(table, offset),
            Self::Procedure(p) => p.build(table, offset),
            Self::Error(_) => {}
        }
    }
}

impl TableBuilder for TypeDeclaration {
    fn build(&mut self, table: &mut GlobalTable, offset: usize) {
        let range = self.to_range().shift(offset);
        if let Some(name) = &mut self.name {
            if name.value == "main" {
                name.info.append_error(SplError(
                    name.to_range(),
                    BuildErrorMessage::MainIsNotAProcedure.to_string(),
                ));
                return;
            }
            let documentation = get_documentation(&self.doc);
            let lookup_table = LookupTable {
                global_table: Some(table),
                local_table: None,
            };
            if table
                .enter(
                    name.to_string(),
                    GlobalEntry::Type(TypeEntry {
                        name: name.clone(),
                        data_type: get_data_type(&mut self.type_expr, Some(name), &lookup_table),
                        range,
                        doc: documentation,
                    }),
                )
                .is_err()
            {
                name.info
                    .append_error(name.to_error(BuildErrorMessage::RedeclarationAsType));
            }
        }
    }
}

impl TableBuilder for ProcedureDeclaration {
    fn build(&mut self, table: &mut GlobalTable, offset: usize) {
        let range = self.to_range().shift(offset);
        if let Some(name) = &mut self.name {
            let documentation = get_documentation(&self.doc);
            let mut local_table = LocalTable::default();
            let parameters = self
                .parameters
                .iter_mut()
                .filter_map(|param| build_parameter(param, table, &mut local_table))
                .collect();
            self.variable_declarations
                .iter_mut()
                .for_each(|dec| build_variable(dec, table, &mut local_table));
            let entry = ProcedureEntry {
                name: name.clone(),
                local_table,
                parameters,
                range,
                doc: documentation,
            };
            if table
                .enter(name.to_string(), GlobalEntry::Procedure(entry))
                .is_err()
            {
                name.info
                    .append_error(name.to_error(BuildErrorMessage::RedeclarationAsProcedure));
            }
        }
    }
}

fn build_parameter(
    param: &mut Reference<ParameterDeclaration>,
    global_table: &GlobalTable,
    local_table: &mut LocalTable,
) -> Option<VariableEntry> {
    let range = param.to_range();
    if let ParameterDeclaration::Valid {
        doc,
        is_ref,
        name: opt_name,
        type_expr,
        ..
    } = param.as_mut()
    {
        opt_name.as_mut().map(|name| {
            let documentation = get_documentation(doc);
            let lookup_table = LookupTable {
                global_table: Some(global_table),
                local_table: None,
            };
            let param_entry = VariableEntry {
                name: name.clone(),
                is_ref: *is_ref,
                data_type: get_data_type(type_expr, Some(name), &lookup_table),
                range,
                doc: documentation,
            };
            if let Some(data_type) = &param_entry.data_type {
                if !data_type.is_primitive() && !param_entry.is_ref {
                    name.info
                        .append_error(name.to_error(BuildErrorMessage::MustBeAReferenceParameter));
                }
            }
            if local_table
                .enter(name.to_string(), LocalEntry::Parameter(param_entry.clone()))
                .is_err()
            {
                name.info
                    .append_error(name.to_error(BuildErrorMessage::RedeclarationAsParameter))
            }
            param_entry
        })
    } else {
        None
    }
}

fn build_variable(
    var: &mut Reference<VariableDeclaration>,
    global_table: &GlobalTable,
    local_table: &mut LocalTable,
) {
    let range = var.to_range();
    if let VariableDeclaration::Valid {
        doc,
        name: Some(name),
        type_expr,
        ..
    } = var.as_mut()
    {
        let documentation = get_documentation(doc);
        let entry = VariableEntry {
            name: name.clone(),
            is_ref: false,
            data_type: get_data_type(
                type_expr,
                Some(name),
                &LookupTable {
                    global_table: Some(global_table),
                    local_table: Some(local_table),
                },
            ),
            range,
            doc: documentation,
        };
        if local_table
            .enter(name.to_string(), LocalEntry::Variable(entry))
            .is_err()
        {
            name.info
                .append_error(name.to_error(BuildErrorMessage::RedeclarationAsVariable));
        }
    }
}

fn get_data_type(
    type_expr: &mut Option<Reference<TypeExpression>>,
    caller: Option<&Identifier>,
    table: &LookupTable,
) -> Option<DataType> {
    type_expr.as_mut().and_then(|type_expr| {
        use TypeExpression::*;
        match type_expr.as_mut() {
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
                    get_data_type(&mut base_type.clone().map(|boxed| *boxed), caller, table),
                ) {
                    caller.map(|creator| DataType::Array {
                        size: *size,
                        base_type: Box::new(base_type),
                        creator: creator.to_string(),
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
                        name.info
                            .append_error(name.to_error(BuildErrorMessage::NotAType));
                        None
                    }
                } else {
                    name.info
                        .append_error(name.to_error(BuildErrorMessage::UndefinedType));
                    None
                }
            }
        }
    })
}

fn get_documentation(docs: &[String]) -> Option<String> {
    let documentation: String = docs.concat();
    if documentation.is_empty() {
        None
    } else {
        Some(documentation)
    }
}
