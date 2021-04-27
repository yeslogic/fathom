//! Elaboration from the surface syntax into the core syntax.
//!
//! Performs the following:
//!
//! - name resolution
//! - desugaring
//! - pattern compilation (TODO)
//! - bidirectional type checking (TODO)
//! - unification (TODO)

use contracts::debug_ensures;
use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::sync::Arc;

use crate::lang::core::semantics::{self, Elim, Value};
use crate::lang::core::{self, Primitive, Sort};
use crate::lang::surface::{ItemData, Module, Pattern, PatternData, StructType, Term, TermData};
use crate::lang::Location;
use crate::literal;
use crate::pass::core_to_surface;
use crate::reporting::{Message, SurfaceToCoreMessage};

/// Contextual information to be used during elaboration.
pub struct Context<'globals> {
    /// The global environment.
    globals: &'globals core::Globals,
    /// Top-level item declarations.
    item_declarations: HashMap<String, Arc<Value>>,
    /// Top-level item definitions.
    item_definitions: HashMap<String, semantics::Item>,
    /// Local variable declarations.
    local_declarations: Vec<(String, Arc<Value>)>,
    /// Local variable definitions.
    local_definitions: core::Locals<Arc<Value>>,
    /// Core-to-surface distillation context.
    core_to_surface: core_to_surface::Context,
    /// Diagnostic messages collected during elaboration.
    messages: Vec<Message>,
}

impl<'globals> Context<'globals> {
    /// Create a new context.
    pub fn new(globals: &'globals core::Globals) -> Context<'globals> {
        Context {
            globals,
            item_declarations: HashMap::new(),
            item_definitions: HashMap::new(),
            local_declarations: Vec::new(),
            local_definitions: core::Locals::new(),
            core_to_surface: core_to_surface::Context::new(),
            messages: Vec::new(),
        }
    }

    /// Get the number of local entries in the context.
    fn size(&self) -> core::LocalSize {
        self.local_definitions.size()
    }

    /// Get the most recently bound local variable of a given name.
    ///
    /// Returns the [`core::LocalIndex`] of the variable at the current binding
    /// depth and the type that the variable was bound with.
    fn get_local(&self, name: &str) -> Option<(&Arc<Value>, core::LocalIndex)> {
        Iterator::zip(core::local_indices(), self.local_declarations.iter().rev()).find_map(
            |(index, (decl_name, r#type))| match decl_name == name {
                true => Some((r#type, index)),
                false => None,
            },
        )
    }

    /// Push a local entry.
    fn push_local(&mut self, name: String, value: Arc<Value>, r#type: Arc<Value>) {
        self.local_declarations.push((name.clone(), r#type));
        self.local_definitions.push(value);
        self.core_to_surface.push_local(name);
    }

    /// Push a local parameter.
    fn push_local_param(&mut self, name: String, r#type: Arc<Value>) -> Arc<Value> {
        let value = Arc::new(Value::local(self.size().next_level(), Vec::new()));
        self.push_local(name, value.clone(), r#type);
        value
    }

    /// Pop a local entry.
    #[allow(dead_code)]
    fn pop_local(&mut self) {
        self.local_declarations.pop();
        self.local_definitions.pop();
        self.core_to_surface.pop_local();
    }

    /// Truncate number of local entries to the given size.
    fn truncate_locals(&mut self, local_size: core::LocalSize) {
        self.local_declarations.truncate(local_size.to_usize());
        self.local_definitions.truncate(local_size);
        self.core_to_surface.truncate_locals(local_size);
    }

    /// Store a diagnostic message in the context for later reporting.
    fn push_message(&mut self, message: impl Into<Message>) {
        self.messages.push(message.into());
    }

    /// Drain the collected diagnostic messages from the context.
    pub fn drain_messages<'a>(&'a mut self) -> impl 'a + Iterator<Item = Message> {
        self.messages.drain(..)
    }

    /// Force a value to resolve to an item, returning `None` if the value did
    /// not refer to an item.
    fn force_item<'context, 'value>(
        &'context self,
        value: &'value Value,
    ) -> Option<(Location, &'context semantics::ItemData, &'value [Elim])> {
        let (name, elims) = value.try_item()?;
        match self.item_definitions.get(name) {
            Some(item) => Some((item.location, &item.data, elims)),
            None => panic!("could not find an item called `{}` in the context", name),
        }
    }

    /// Force a value to resolve to some field declarations, returning `None`
    /// if the value did not refer to a valid struct type or struct format.
    fn force_field_declarations(&self, value: &Value) -> Option<semantics::FieldDeclarations> {
        let (_, item, elims) = self.force_item(value)?;
        item.try_field_declarations(&elims)
    }

    /// Evaluate a [`core::Term`] into a [`Value`] in the current elaboration context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`core::Term`]: crate::lang::core::Term
    pub fn eval(&mut self, term: &core::Term) -> Arc<Value> {
        semantics::eval(
            self.globals,
            &self.item_definitions,
            &mut self.local_definitions,
            term,
        )
    }

    /// Read back a [`Value`] to a [`core::Term`] using the current
    /// state of the elaborator.
    ///
    /// Unstuck eliminations are not unfolded, making this useful for printing
    /// terms and item_declarations in user-facing diagnostics.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`core::Term`]: crate::lang::core::Term
    pub fn read_back(&self, value: &Value) -> core::Term {
        semantics::read_back(
            self.globals,
            &self.item_definitions,
            self.local_definitions.size(),
            value,
        )
    }

    /// Check that one [`Value`] is [computationally equal]
    /// to another [`Value`] in the current elaboration context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
    pub fn is_equal(&self, value0: &Value, value1: &Value) -> bool {
        semantics::is_equal(self.globals, &self.item_definitions, value0, value1)
    }

    /// Distill a [`core::Term`] into a [`surface::Term`].
    ///
    /// [`core::Term`]: crate::lang::core::Term
    /// [`surface::Term`]: crate::lang::surface::Term
    pub fn core_to_surface(&mut self, core_term: &core::Term) -> Term {
        self.core_to_surface.from_term(&core_term)
    }

    /// Read back a [`Value`] into a [`surface::Term`] using the
    /// current state of the elaborator.
    ///
    /// Unstuck eliminations are not unfolded, making this useful for printing
    /// terms and item_declarations in user-facing diagnostics.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`surface::Term`]: crate::lang::surface::Term
    pub fn read_back_to_surface(&mut self, value: &Value) -> Term {
        self.core_to_surface(&self.read_back(value))
    }

    /// Translate a surface module into a core module,
    /// while validating that it is well-formed.
    #[debug_ensures(self.item_declarations.is_empty())]
    #[debug_ensures(self.item_definitions.is_empty())]
    #[debug_ensures(self.local_declarations.is_empty())]
    #[debug_ensures(self.local_definitions.is_empty())]
    pub fn from_module(&mut self, surface_module: &Module) -> core::Module {
        let mut core_items = Vec::new();

        for item in surface_module.items.iter() {
            use std::collections::hash_map::Entry;

            let (name, core_item_data, item_data, r#type) = match &item.data {
                ItemData::Constant(constant) => {
                    let (core_term, r#type) = match &constant.type_ {
                        Some(surface_type) => {
                            let (core_type, _) = self.is_type(surface_type);
                            match &core_type.data {
                                core::TermData::Error => (
                                    core::Term::new(constant.term.location, core::TermData::Error),
                                    Arc::new(Value::Error),
                                ),
                                _ => {
                                    let r#type = self.eval(&core_type);
                                    let term_data = core::TermData::Ann(
                                        Arc::new(self.check_type(&constant.term, &r#type)),
                                        Arc::new(core_type),
                                    );

                                    (core::Term::new(constant.term.location, term_data), r#type)
                                }
                            }
                        }
                        None => self.synth_type(&constant.term),
                    };

                    let item_data = semantics::ItemData::Constant(self.eval(&core_term));
                    let core_item_data = core::ItemData::Constant(core::Constant {
                        doc: constant.doc.clone(),
                        name: constant.name.data.clone(),
                        term: Arc::new(core_term),
                    });

                    (&constant.name, core_item_data, item_data, r#type)
                }
                ItemData::StructType(struct_type) => match &struct_type.type_ {
                    None => {
                        self.push_message(SurfaceToCoreMessage::MissingStructAnnotation {
                            name: struct_type.name.data.clone(),
                            name_location: struct_type.name.location,
                        });
                        continue;
                    }
                    Some(r#type) => {
                        // Elaborate the return type of the struct
                        let (core_type, _) = self.is_type(&r#type);
                        let r#type = self.eval(&core_type);

                        // Check the return type of the struct
                        let (core_item_data, item_data, r#type) = match r#type.as_ref() {
                            Value::Sort(Sort::Type) => self.is_struct_type(struct_type),
                            Value::FormatType => self.is_struct_format(struct_type),
                            Value::Error => continue,
                            r#type => {
                                let ann_type = self.read_back_to_surface(r#type);
                                self.push_message(SurfaceToCoreMessage::InvalidStructAnnotation {
                                    name: struct_type.name.data.clone(),
                                    ann_type,
                                    ann_location: core_type.location,
                                });
                                continue;
                            }
                        };

                        // TODO: Dependent function type??
                        (&struct_type.name, core_item_data, item_data, r#type)
                    }
                },
            };

            // FIXME: Avoid shadowing builtin definitions
            match self.item_definitions.entry(name.data.clone()) {
                Entry::Vacant(entry) => {
                    let core_item = core::Item::new(item.location, core_item_data);
                    core_items.push(core_item.clone());
                    self.item_declarations.insert(entry.key().clone(), r#type);
                    entry.insert(semantics::Item::new(item.location, item_data));
                }
                Entry::Occupied(entry) => {
                    let original_location = entry.get().location;
                    self.push_message(SurfaceToCoreMessage::ItemRedefinition {
                        name: name.data.clone(),
                        found_location: item.location,
                        original_location,
                    });
                }
            }
        }

        self.item_definitions.clear();
        self.item_declarations.clear();

        core::Module {
            doc: surface_module.doc.clone(),
            items: core_items,
        }
    }

    fn is_struct_type(
        &mut self,
        struct_type: &StructType,
    ) -> (core::ItemData, semantics::ItemData, Arc<Value>) {
        use std::collections::hash_map::Entry;

        // Remember the initial size, for later cleanup.
        let initial_size = self.size();

        // Elaborate the parameters into the core language
        let mut params = Vec::with_capacity(struct_type.params.len());
        for (param_name, param_type) in &struct_type.params {
            let (param_type, _) = self.is_type(&param_type);
            params.push((param_name.clone(), Arc::new(param_type)));
        }
        // Add the parameters to the context in preparation for
        // checking the body of the struct type.
        for (param_name, param_type) in &params {
            let param_type = self.eval(param_type);
            self.push_local_param(param_name.data.clone(), param_type);
        }

        // Field labels that have previously seen, along with the source
        // location where they were introduced (for diagnostic reporting).
        let mut seen_field_labels = HashMap::new();
        // Fields that have been elaborated into the core syntax.
        let mut core_field_declarations = Vec::with_capacity(struct_type.fields.len());
        let type_type = Arc::new(Value::Sort(Sort::Type));

        // Elaborate the field declarations
        for field in &struct_type.fields {
            let field_location = Location::merge(field.label.location, field.type_.location);
            let core_type = self.check_type(&field.type_, &type_type);

            match seen_field_labels.entry(field.label.data.clone()) {
                Entry::Vacant(entry) => {
                    let core_type = Arc::new(core_type);
                    let r#type = self.eval(&core_type);

                    core_field_declarations.push(core::FieldDeclaration {
                        doc: field.doc.clone(),
                        label: field.label.clone(),
                        type_: core_type,
                    });
                    self.push_local_param(field.label.data.clone(), r#type);
                    entry.insert(field_location);
                }
                Entry::Occupied(entry) => {
                    self.push_message(SurfaceToCoreMessage::FieldRedeclaration {
                        name: entry.key().clone(),
                        found_location: field_location,
                        original_location: *entry.get(),
                    });
                }
            }
        }

        // Clean up the elaboration context
        self.truncate_locals(initial_size);

        // Build up the return type
        let mut r#type = type_type;
        for (_, param_type) in params.iter().rev() {
            let param_type = self.eval(param_type);
            r#type = Arc::new(Value::FunctionType(param_type, r#type));
        }

        let arity = params.len();
        let core_field_declarations: Arc<[_]> = core_field_declarations.into();

        let core_item_data = core::ItemData::StructType(core::StructType {
            doc: struct_type.doc.clone(),
            params,
            name: struct_type.name.data.clone(),
            fields: core_field_declarations.clone(),
        });
        let item_data = semantics::ItemData::StructType(arity, core_field_declarations);

        (core_item_data, item_data, r#type)
    }

    pub fn is_struct_format(
        &mut self,
        struct_type: &StructType,
    ) -> (core::ItemData, semantics::ItemData, Arc<Value>) {
        use std::collections::hash_map::Entry;

        // Remember the initial size, for later cleanup.
        let initial_size = self.size();

        // Elaborate the parameters into the core language
        let mut params = Vec::with_capacity(struct_type.params.len());
        for (param_name, param_type) in &struct_type.params {
            let (param_type, _) = self.is_type(&param_type);
            params.push((param_name.clone(), Arc::new(param_type)));
        }
        // Add the parameters to the context in preparation for
        // checking the body of the struct type.
        for (param_name, param_type) in &params {
            let param_type = self.eval(param_type);
            self.push_local_param(param_name.data.clone(), param_type);
        }

        // Field names that have previously seen, along with the source
        // location where they were introduced (for diagnostic reporting).
        let mut seen_field_labels = HashMap::new();
        // Fields that have been elaborated into the core syntax.
        let mut core_field_declarations = Vec::with_capacity(struct_type.fields.len());
        let format_type = Arc::new(Value::FormatType);

        // Elaborate the field declarations
        for field in &struct_type.fields {
            let field_location = Location::merge(field.label.location, field.type_.location);
            let core_type = self.check_type(&field.type_, &format_type);

            match seen_field_labels.entry(field.label.data.clone()) {
                Entry::Vacant(entry) => {
                    let core_type = Arc::new(core_type);
                    let r#type = semantics::repr(self.eval(&core_type));

                    core_field_declarations.push(core::FieldDeclaration {
                        doc: field.doc.clone(),
                        label: field.label.clone(),
                        type_: core_type,
                    });
                    self.push_local_param(field.label.data.clone(), r#type);
                    entry.insert(field_location);
                }
                Entry::Occupied(entry) => {
                    self.push_message(SurfaceToCoreMessage::FieldRedeclaration {
                        name: entry.key().clone(),
                        found_location: field_location,
                        original_location: *entry.get(),
                    });
                }
            }
        }

        // Clean up the elaboration context
        self.truncate_locals(initial_size);

        // Build up the return type
        let mut r#type = format_type;
        for (_, param_type) in params.iter().rev() {
            let param_type = self.eval(param_type);
            r#type = Arc::new(Value::FunctionType(param_type, r#type));
        }

        let arity = params.len();
        let core_field_declarations: Arc<[_]> = core_field_declarations.into();

        let core_item_data = core::ItemData::StructFormat(core::StructFormat {
            doc: struct_type.doc.clone(),
            params,
            name: struct_type.name.data.clone(),
            fields: core_field_declarations.clone(),
        });
        let item_data = semantics::ItemData::StructFormat(arity, core_field_declarations);

        (core_item_data, item_data, r#type)
    }

    /// Validate that a surface term is a type, and translate it into the core syntax.
    #[debug_ensures(self.item_declarations.len() == old(self.item_declarations.len()))]
    #[debug_ensures(self.item_definitions.len() == old(self.item_definitions.len()))]
    #[debug_ensures(self.local_declarations.len() == old(self.local_declarations.len()))]
    #[debug_ensures(self.local_definitions.size() == old(self.local_definitions.size()))]
    pub fn is_type(&mut self, surface_term: &Term) -> (core::Term, Option<core::Sort>) {
        let (core_term, core_type) = self.synth_type(surface_term);
        match core_type.as_ref() {
            Value::Error => (
                core::Term::new(surface_term.location, core::TermData::Error),
                None,
            ),
            Value::Sort(sort) => (core_term, Some(*sort)),
            core_type => {
                let found_type = self.read_back_to_surface(core_type);
                self.push_message(SurfaceToCoreMessage::UniverseMismatch {
                    term_location: surface_term.location,
                    found_type,
                });
                (
                    core::Term::new(surface_term.location, core::TermData::Error),
                    None,
                )
            }
        }
    }

    /// Check that a surface term is an element of a type, and translate it into the
    /// core syntax.
    #[debug_ensures(self.item_declarations.len() == old(self.item_declarations.len()))]
    #[debug_ensures(self.item_definitions.len() == old(self.item_definitions.len()))]
    #[debug_ensures(self.local_declarations.len() == old(self.local_declarations.len()))]
    #[debug_ensures(self.local_definitions.size() == old(self.local_definitions.size()))]
    pub fn check_type(&mut self, surface_term: &Term, expected_type: &Arc<Value>) -> core::Term {
        match (&surface_term.data, expected_type.as_ref()) {
            (TermData::Error, _) => core::Term::new(surface_term.location, core::TermData::Error),
            (_, Value::Error) => core::Term::new(surface_term.location, core::TermData::Error),

            (TermData::StructTerm(surface_field_definitions), _) => {
                use std::collections::btree_map::Entry;

                // Resolve the struct type definition in the context.
                let field_declarations = match self.force_field_declarations(expected_type) {
                    Some(field_declarations) => field_declarations,
                    None => {
                        let expected_type = self.read_back_to_surface(expected_type);
                        self.push_message(SurfaceToCoreMessage::UnexpectedStructTerm {
                            term_location: surface_term.location,
                            expected_type,
                        });
                        return core::Term::new(surface_term.location, core::TermData::Error);
                    }
                };

                // Initial pass over the fields, looking for duplicate fields.
                let mut pending_field_definitions = BTreeMap::new();
                let mut duplicate_labels = Vec::new();
                for field_definition in surface_field_definitions {
                    match pending_field_definitions.entry(&field_definition.label.data) {
                        Entry::Vacant(entry) => drop(entry.insert(field_definition)),
                        Entry::Occupied(_) => duplicate_labels.push(field_definition.label.clone()),
                    }
                }

                // Check that the fields match the item_declarations from the type definition.
                let mut core_field_definitions =
                    Vec::with_capacity(surface_field_definitions.len());
                let mut missing_labels = Vec::new();

                field_declarations.for_each_field(
                    self.globals,
                    &self.item_definitions.clone(), // FIXME: avoid clone
                    |label, r#type| match (pending_field_definitions.remove(&label.data), r#type) {
                        (Some(field_definition), Some(r#type)) => {
                            let core_term = self.check_type(&field_definition.term, &r#type);
                            let value = self.eval(&core_term);

                            core_field_definitions.push(core::FieldDefinition {
                                label: field_definition.label.clone(),
                                term: Arc::new(core_term),
                            });

                            value
                        }
                        (Some(_), _) => Arc::new(Value::Error),
                        (None, _) => {
                            missing_labels.push(label.clone());
                            Arc::new(Value::Error)
                        }
                    },
                );

                // Collect unexpected fields that were defined in the term but
                // were not declared in the type.
                let unexpected_labels = pending_field_definitions
                    .into_iter()
                    .map(|(_, field)| field.label.clone())
                    .collect::<Vec<_>>();

                // Record any errors in the context.
                let mut has_problems = false;
                if !duplicate_labels.is_empty() {
                    has_problems = true;
                    self.push_message(SurfaceToCoreMessage::DuplicateStructFields {
                        duplicate_labels,
                    });
                }
                if !missing_labels.is_empty() {
                    has_problems = true;
                    self.push_message(SurfaceToCoreMessage::MissingStructFields {
                        term_location: surface_term.location,
                        missing_labels,
                    });
                }
                if !unexpected_labels.is_empty() {
                    has_problems = true;
                    self.push_message(SurfaceToCoreMessage::UnexpectedStructFields {
                        term_location: surface_term.location,
                        unexpected_labels,
                    });
                }
                if has_problems {
                    return core::Term::new(surface_term.location, core::TermData::Error);
                }

                core::Term::new(
                    surface_term.location,
                    core::TermData::StructTerm(core_field_definitions),
                )
            }

            (TermData::SequenceTerm(surface_elem_terms), _) => match expected_type.try_global() {
                Some(("Array", [Elim::Function(len), Elim::Function(elem_type)])) => {
                    let elem_terms = surface_elem_terms
                        .iter()
                        .map(|surface_elem_term| {
                            Arc::new(self.check_type(surface_elem_term, elem_type))
                        })
                        .collect();

                    match len.as_ref() {
                        Value::Primitive(Primitive::Int(len))
                            if *len == surface_elem_terms.len().into() =>
                        {
                            core::Term::new(
                                surface_term.location,
                                core::TermData::ArrayTerm(elem_terms),
                            )
                        }
                        len => {
                            let expected_len = self.read_back_to_surface(&len);
                            self.push_message(SurfaceToCoreMessage::MismatchedArrayLength {
                                term_location: surface_term.location,
                                found_len: elem_terms.len(),
                                expected_len,
                            });
                            core::Term::new(surface_term.location, core::TermData::Error)
                        }
                    }
                }
                Some(_) | None => {
                    let expected_type = self.read_back_to_surface(expected_type);
                    self.push_message(SurfaceToCoreMessage::UnexpectedSequenceTerm {
                        term_location: surface_term.location,
                        expected_type,
                    });
                    core::Term::new(surface_term.location, core::TermData::Error)
                }
            },

            (TermData::NumberLiteral(source), _) => {
                let parse_state =
                    literal::State::new(surface_term.location, source, &mut self.messages);
                let term_data = match expected_type.try_global() {
                    Some(("Int", [])) => parse_state
                        .number_to_big_int()
                        .map(Primitive::Int)
                        .map_or(core::TermData::Error, core::TermData::Primitive),
                    Some(("F32", [])) => parse_state
                        .number_to_float()
                        .map(Primitive::F32)
                        .map_or(core::TermData::Error, core::TermData::Primitive),
                    Some(("F64", [])) => parse_state
                        .number_to_float()
                        .map(Primitive::F64)
                        .map_or(core::TermData::Error, core::TermData::Primitive),
                    _ => {
                        let expected_type = self.read_back_to_surface(expected_type);
                        self.push_message(SurfaceToCoreMessage::NumericLiteralNotSupported {
                            literal_location: surface_term.location,
                            expected_type,
                        });
                        core::TermData::Error
                    }
                };

                core::Term::new(surface_term.location, term_data)
            }
            (TermData::If(surface_head, surface_if_true, surface_if_false), _) => {
                let bool_type = Arc::new(Value::global("Bool", Vec::new()));
                let term_data = core::TermData::BoolElim(
                    Arc::new(self.check_type(surface_head, &bool_type)),
                    Arc::new(self.check_type(surface_if_true, expected_type)),
                    Arc::new(self.check_type(surface_if_false, expected_type)),
                );

                core::Term::new(surface_term.location, term_data)
            }
            (TermData::Match(surface_head, surface_branches), _) => {
                let (head, head_type) = self.synth_type(surface_head);
                if let Value::Error = head_type.as_ref() {
                    return core::Term::new(surface_term.location, core::TermData::Error);
                }

                match head_type.try_global() {
                    Some(("Bool", [])) => {
                        self.push_message(Message::NotYetImplemented {
                            location: surface_term.location,
                            feature_name: "boolean patterns",
                        });
                        core::Term::new(surface_term.location, core::TermData::Error)
                    }
                    Some(("Int", [])) => {
                        let (branches, default) = self.from_int_branches(
                            surface_head.location,
                            surface_branches,
                            expected_type,
                        );

                        core::Term::new(
                            surface_term.location,
                            core::TermData::IntElim(Arc::new(head), branches, default),
                        )
                    }
                    _ => {
                        let found_type = self.read_back_to_surface(&head_type);
                        self.push_message(SurfaceToCoreMessage::UnsupportedPatternType {
                            scrutinee_location: surface_head.location,
                            found_type,
                        });
                        core::Term::new(surface_term.location, core::TermData::Error)
                    }
                }
            }

            (_, expected_type) => match self.synth_type(surface_term) {
                (core_term, found_type) if self.is_equal(&found_type, expected_type) => core_term,
                (_, found_type) => {
                    let expected_type = self.read_back_to_surface(expected_type);
                    let found_type = self.read_back_to_surface(&found_type);
                    self.push_message(SurfaceToCoreMessage::TypeMismatch {
                        term_location: surface_term.location,
                        expected_type,
                        found_type,
                    });
                    core::Term::new(surface_term.location, core::TermData::Error)
                }
            },
        }
    }

    /// Synthesize the type of a surface term, and elaborate it into the core syntax.
    #[debug_ensures(self.item_declarations.len() == old(self.item_declarations.len()))]
    #[debug_ensures(self.item_definitions.len() == old(self.item_definitions.len()))]
    #[debug_ensures(self.local_declarations.len() == old(self.local_declarations.len()))]
    #[debug_ensures(self.local_definitions.size() == old(self.local_definitions.size()))]
    pub fn synth_type(&mut self, surface_term: &Term) -> (core::Term, Arc<Value>) {
        match &surface_term.data {
            TermData::Name(name) => {
                if let Some((r#type, index)) = self.get_local(name) {
                    let term_data = core::TermData::Local(index);
                    let core_term = core::Term::new(surface_term.location, term_data);
                    return (core_term, r#type.clone());
                }
                if let Some(r#type) = self.item_declarations.get(name) {
                    let term_data = core::TermData::Item(name.to_owned());
                    let core_term = core::Term::new(surface_term.location, term_data);
                    return (core_term, r#type.clone());
                }
                if let Some((r#type, _)) = self.globals.get(name) {
                    let term_data = core::TermData::Global(name.to_owned());
                    let core_term = core::Term::new(surface_term.location, term_data);
                    return (core_term, self.eval(r#type));
                }

                self.push_message(SurfaceToCoreMessage::VarNameNotFound {
                    name: name.clone(),
                    name_location: surface_term.location,
                });
                (
                    core::Term::new(surface_term.location, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }

            TermData::Ann(surface_term, surface_type) => {
                let (core_type, _) = self.is_type(surface_type);
                match &core_type.data {
                    core::TermData::Error => (
                        core::Term::new(surface_term.location, core::TermData::Error),
                        Arc::new(Value::Error),
                    ),
                    _ => {
                        let r#type = self.eval(&core_type);
                        let term_data = core::TermData::Ann(
                            Arc::new(self.check_type(surface_term, &r#type)),
                            Arc::new(core_type),
                        );

                        (core::Term::new(surface_term.location, term_data), r#type)
                    }
                }
            }

            TermData::KindType => {
                self.push_message(SurfaceToCoreMessage::TermHasNoType {
                    term_location: surface_term.location,
                });
                (
                    core::Term::new(surface_term.location, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }
            TermData::TypeType => (
                core::Term::new(surface_term.location, core::TermData::Sort(Sort::Type)),
                Arc::new(Value::Sort(Sort::Kind)),
            ),

            TermData::FunctionType(param_type, body_type) => {
                let (core_param_type, param_sort) = self.is_type(param_type);
                let (core_body_type, body_sort) = self.is_type(body_type);

                match (param_sort, body_sort) {
                    (Some(param_sort), Some(body_sort)) => {
                        let term_data = core::TermData::FunctionType(
                            Arc::new(core_param_type),
                            Arc::new(core_body_type),
                        );
                        (
                            core::Term::new(surface_term.location, term_data),
                            Arc::new(Value::Sort(core::typing::rule(param_sort, body_sort))),
                        )
                    }
                    (_, _) => (
                        core::Term::new(surface_term.location, core::TermData::Error),
                        Arc::new(Value::Error),
                    ),
                }
            }
            TermData::FunctionElim(head, arguments) => {
                let (mut core_head, mut head_type) = self.synth_type(head);

                for argument in arguments {
                    match head_type.as_ref() {
                        Value::FunctionType(param_type, body_type) => {
                            let term_data = core::TermData::FunctionElim(
                                Arc::new(core_head),
                                Arc::new(self.check_type(argument, &param_type)),
                            );
                            core_head = core::Term::new(surface_term.location, term_data);
                            head_type = body_type.clone();
                        }
                        Value::Error => {
                            return (
                                core::Term::new(surface_term.location, core::TermData::Error),
                                Arc::new(Value::Error),
                            );
                        }
                        head_type => {
                            let head_type = self.read_back_to_surface(head_type);
                            self.push_message(SurfaceToCoreMessage::NotAFunction {
                                head_location: head.location,
                                head_type,
                                argument_location: argument.location,
                            });
                            return (
                                core::Term::new(surface_term.location, core::TermData::Error),
                                Arc::new(Value::Error),
                            );
                        }
                    }
                }

                (core_head, head_type)
            }

            TermData::StructTerm(_) => {
                self.push_message(SurfaceToCoreMessage::AmbiguousStructTerm {
                    term_location: surface_term.location,
                });
                (
                    core::Term::new(surface_term.location, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }
            TermData::StructElim(head, label) => {
                let (core_head, head_type) = self.synth_type(head);
                if let Value::Error = head_type.as_ref() {
                    return (
                        core::Term::new(surface_term.location, core::TermData::Error),
                        Arc::new(Value::Error),
                    );
                }

                if let Some(field_declarations) = self.force_field_declarations(&head_type) {
                    let head_value = self.eval(&core_head);

                    let field_type = field_declarations.get_field_type(
                        self.globals,
                        &self.item_definitions,
                        head_value,
                        &label.data,
                    );

                    if let Some(field_type) = field_type {
                        let core_term = core::Term::new(
                            surface_term.location,
                            core::TermData::StructElim(
                                Arc::new(core_head.clone()),
                                label.data.clone(),
                            ),
                        );
                        return (core_term, field_type);
                    }
                }

                // If we could not find a matching field, it's a type error.
                let head_type = self.read_back_to_surface(&head_type);
                self.push_message(SurfaceToCoreMessage::FieldNotFound {
                    head_location: head.location,
                    head_type,
                    label: label.clone(),
                });
                (
                    core::Term::new(surface_term.location, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }

            TermData::SequenceTerm(_) => {
                self.push_message(SurfaceToCoreMessage::AmbiguousSequenceTerm {
                    location: surface_term.location,
                });
                (
                    core::Term::new(surface_term.location, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }

            TermData::NumberLiteral(_) => {
                self.push_message(SurfaceToCoreMessage::AmbiguousNumericLiteral {
                    literal_location: surface_term.location,
                });
                (
                    core::Term::new(surface_term.location, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }
            TermData::If(surface_head, surface_if_true, surface_if_false) => {
                let bool_type = Arc::new(Value::global("Bool", Vec::new()));
                let head = self.check_type(surface_head, &bool_type);
                let (if_true, if_true_type) = self.synth_type(surface_if_true);
                let (if_false, if_false_type) = self.synth_type(surface_if_false);

                if self.is_equal(&if_true_type, &if_false_type) {
                    let term_data = core::TermData::BoolElim(
                        Arc::new(head),
                        Arc::new(if_true),
                        Arc::new(if_false),
                    );
                    (
                        core::Term::new(surface_term.location, term_data),
                        if_true_type,
                    )
                } else {
                    let expected_type = self.read_back_to_surface(&if_true_type);
                    let found_type = self.read_back_to_surface(&if_false_type);
                    self.push_message(SurfaceToCoreMessage::TypeMismatch {
                        term_location: surface_if_false.location,
                        expected_type,
                        found_type,
                    });
                    (
                        core::Term::new(surface_term.location, core::TermData::Error),
                        Arc::new(Value::Error),
                    )
                }
            }
            TermData::Match(_, _) => {
                self.push_message(SurfaceToCoreMessage::AmbiguousMatchExpression {
                    term_location: surface_term.location,
                });
                (
                    core::Term::new(surface_term.location, core::TermData::Error),
                    Arc::new(Value::Error),
                )
            }

            TermData::FormatType => (
                core::Term::new(surface_term.location, core::TermData::FormatType),
                Arc::new(Value::Sort(Sort::Kind)),
            ),

            TermData::Repr => (
                core::Term::new(surface_term.location, core::TermData::Repr),
                Arc::new(Value::FunctionType(
                    Arc::new(Value::FormatType),
                    Arc::new(Value::Sort(Sort::Type)),
                )),
            ),

            TermData::Error => (
                core::Term::new(surface_term.location, core::TermData::Error),
                Arc::new(Value::Error),
            ),
        }
    }

    fn from_int_branches(
        &mut self,
        location: Location,
        surface_branches: &[(Pattern, Term)],
        expected_type: &Arc<Value>,
    ) -> (BTreeMap<BigInt, Arc<core::Term>>, Arc<core::Term>) {
        use std::collections::btree_map::Entry;

        let mut branches = BTreeMap::new();
        let mut default = None;

        for (pattern, surface_term) in surface_branches {
            let unreachable_pattern = || SurfaceToCoreMessage::UnreachablePattern {
                pattern_location: pattern.location,
            };

            match &pattern.data {
                PatternData::NumberLiteral(source) => {
                    let core_term = self.check_type(surface_term, expected_type);
                    let parse_state = literal::State::new(location, source, &mut self.messages);
                    match parse_state.number_to_big_int() {
                        None => {} // Skipping - an error message should have already been recorded
                        Some(value) => match &default {
                            None => match branches.entry(value) {
                                Entry::Occupied(_) => self.push_message(unreachable_pattern()),
                                Entry::Vacant(entry) => {
                                    entry.insert(Arc::new(core_term));
                                }
                            },
                            Some(_) => self.push_message(unreachable_pattern()),
                        },
                    }
                }
                PatternData::Name(_name) => {
                    // TODO: check if name is bound
                    // - if so compare for equality
                    // - otherwise bind local variable
                    let core_term = self.check_type(surface_term, expected_type);
                    match &default {
                        None => default = Some(Arc::new(core_term)),
                        Some(_) => self.push_message(unreachable_pattern()),
                    }
                }
            }
        }

        let default = default.unwrap_or_else(|| {
            self.push_message(SurfaceToCoreMessage::NoDefaultPattern {
                match_location: location,
            });
            Arc::new(core::Term::new(location, core::TermData::Error))
        });

        (branches, default)
    }
}
