//! Distillation of the core language back into the surface language.
//!
//! This reverses some, but not all parts of elaboration, and is often used in
//! conjunction with [`crate::pass::surface_to_pretty`] to render core terms to
//! the user.

use crate::lang::core::{
    Item, ItemData, LocalIndex, LocalSize, Locals, Module, Primitive, Sort, Term, TermData,
};
use crate::lang::{surface, Located};

/// Distillation context.
pub struct Context {
    local_names: Locals<String>,
}

impl Context {
    /// Create a new context.
    pub fn new() -> Context {
        Context {
            local_names: Locals::new(),
        }
    }

    /// Get the number of local entries in the context.
    fn size(&self) -> LocalSize {
        self.local_names.size()
    }

    /// Get the surface name of a local binding.
    fn get_local(&self, index: LocalIndex) -> Option<&str> {
        self.local_names.get(index).map(String::as_str)
    }

    /// Push a local entry.
    pub fn push_local(&mut self, name: String) {
        self.local_names.push(name);
    }

    /// Pop a local entry.
    pub fn pop_local(&mut self) {
        // TODO: name/keyword avoidance!
        self.local_names.pop();
    }

    /// Pop the given number of local entries.
    pub fn truncate_locals(&mut self, local_size: LocalSize) {
        self.local_names.truncate(local_size);
    }

    pub fn from_module(&mut self, module: &Module) -> surface::Module {
        surface::Module {
            doc: module.doc.clone(),
            items: module
                .items
                .iter()
                .map(|item| self.from_item(item))
                .collect(),
        }
    }

    pub fn from_item(&mut self, item: &Item) -> surface::Item {
        let item_data = match &item.data {
            ItemData::Constant(constant) => {
                let (term, r#type) = match &constant.term.data {
                    TermData::Ann(term, r#type) => {
                        (self.from_term(term), Some(self.from_term(r#type)))
                    }
                    _ => (self.from_term(&constant.term), None),
                };

                surface::ItemData::Constant(surface::Constant {
                    doc: constant.doc.clone(),
                    name: Located::generated(constant.name.clone()),
                    type_: r#type,
                    term,
                })
            }
            ItemData::StructType(struct_type) => {
                // Remember the initial size, for later cleanup.
                let initial_size = self.size();

                let mut params = Vec::with_capacity(struct_type.params.len());
                for (param_name, param_type) in struct_type.params.iter() {
                    params.push((param_name.clone(), self.from_term(param_type)));
                }
                for (param_name, _) in &params {
                    self.push_local(param_name.data.clone());
                }

                let mut field_declarations = Vec::with_capacity(struct_type.fields.len());
                for field_declaration in struct_type.fields.iter() {
                    let r#type = self.from_term(&field_declaration.type_);
                    self.push_local(field_declaration.label.data.clone());
                    field_declarations.push(surface::FieldDeclaration {
                        doc: field_declaration.doc.clone(),
                        label: field_declaration.label.clone(),
                        type_: r#type,
                    });
                }

                // Clean up the distillation context
                self.truncate_locals(initial_size);

                surface::ItemData::StructType(surface::StructType {
                    doc: struct_type.doc.clone(),
                    name: Located::generated(struct_type.name.clone()),
                    params,
                    type_: Some(surface::Term::generated(surface::TermData::TypeType)),
                    fields: field_declarations,
                })
            }
            ItemData::StructFormat(struct_format) => {
                // Remember the initial size, for later cleanup.
                let initial_size = self.size();

                let mut params = Vec::with_capacity(struct_format.params.len());
                for (param_name, param_type) in struct_format.params.iter() {
                    params.push((param_name.clone(), self.from_term(param_type)));
                }
                for (param_name, _) in &params {
                    self.push_local(param_name.data.clone());
                }

                let mut field_declarations = Vec::with_capacity(struct_format.fields.len());
                for field_declaration in struct_format.fields.iter() {
                    let r#type = self.from_term(&field_declaration.type_);
                    self.push_local(field_declaration.label.data.clone());
                    field_declarations.push(surface::FieldDeclaration {
                        doc: field_declaration.doc.clone(),
                        label: field_declaration.label.clone(),
                        type_: r#type,
                    });
                }

                // Clean up the distillation context
                self.truncate_locals(initial_size);

                surface::ItemData::StructType(surface::StructType {
                    doc: struct_format.doc.clone(),
                    name: Located::generated(struct_format.name.clone()),
                    params,
                    type_: Some(surface::Term::generated(surface::TermData::FormatType)),
                    fields: field_declarations,
                })
            }
        };

        surface::Item::generated(item_data)
    }

    pub fn from_term(&mut self, term: &Term) -> surface::Term {
        let term_data = match &term.data {
            TermData::Global(global_name) => surface::TermData::Name(global_name.clone()),
            TermData::Item(item_name) => surface::TermData::Name(item_name.clone()),
            TermData::Local(local_index) => match self.get_local(*local_index) {
                Some(local_name) => surface::TermData::Name(local_name.to_owned()),
                None => surface::TermData::Error, // TODO: Add a warning?
            },

            TermData::Ann(term, r#type) => surface::TermData::Ann(
                Box::new(self.from_term(term)),
                Box::new(self.from_term(r#type)),
            ),
            TermData::Sort(Sort::Kind) => surface::TermData::KindType,
            TermData::Sort(Sort::Type) => surface::TermData::TypeType,

            TermData::FunctionType(param_type, body_type) => surface::TermData::FunctionType(
                Box::new(self.from_term(param_type)),
                Box::new(self.from_term(body_type)),
            ),
            TermData::FunctionElim(head, argument) => surface::TermData::FunctionElim(
                Box::new(self.from_term(head)),
                vec![self.from_term(argument)], // TODO: flatten arguments
            ),

            TermData::StructTerm(field_definitions) => surface::TermData::StructTerm(
                field_definitions
                    .iter()
                    .map(|field_definition| surface::FieldDefinition {
                        label: field_definition.label.clone(),
                        term: self.from_term(&field_definition.term),
                    })
                    .collect(),
            ),
            TermData::StructElim(head, field) => surface::TermData::StructElim(
                Box::new(self.from_term(head)),
                Located::generated(field.clone()),
            ),

            TermData::ArrayTerm(elem_terms) => surface::TermData::SequenceTerm(
                elem_terms
                    .iter()
                    .map(|elem_term| self.from_term(elem_term))
                    .collect(),
            ),

            TermData::Primitive(primitive) => match primitive {
                Primitive::Int(value) => surface::TermData::NumberLiteral(value.to_string()),
                Primitive::F32(value) => surface::TermData::NumberLiteral(value.to_string()),
                Primitive::F64(value) => surface::TermData::NumberLiteral(value.to_string()),
                Primitive::Pos(_) => surface::TermData::Error, // TODO: Warning?
            },
            TermData::BoolElim(head, if_true, if_false) => surface::TermData::If(
                Box::new(self.from_term(head)),
                Box::new(self.from_term(if_true)),
                Box::new(self.from_term(if_false)),
            ),
            TermData::IntElim(head, branches, default) => {
                let default = self.from_term(default);

                surface::TermData::Match(
                    Box::new(self.from_term(head)),
                    branches
                        .iter()
                        .map(|(value, term)| {
                            let pattern_data =
                                surface::PatternData::NumberLiteral(value.to_string());
                            (
                                surface::Pattern::generated(pattern_data),
                                self.from_term(term),
                            )
                        })
                        .chain(std::iter::once((
                            surface::Pattern::generated(surface::PatternData::Name("_".to_owned())),
                            default,
                        )))
                        .collect(),
                )
            }

            TermData::FormatType => surface::TermData::FormatType,

            TermData::Repr => surface::TermData::Repr,

            TermData::Error => surface::TermData::Error,
        };

        surface::Term::generated(term_data)
    }
}
