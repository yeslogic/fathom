//! Type-checking of Fathom's core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use contracts::debug_ensures;
use std::collections::HashMap;
use std::sync::Arc;

use crate::lang::core::semantics::{self, Elim, Value};
use crate::lang::core::{
    Globals, ItemData, LocalSize, Locals, Module, Primitive, Sort, Term, TermData,
};
use crate::lang::Location;
use crate::reporting::{CoreTypingMessage, Message};

/// Returns the sorts of sorts.
pub fn axiom(sort: Sort) -> Option<Sort> {
    match sort {
        Sort::Type => Some(Sort::Kind),
        Sort::Kind => None,
    }
}

/// Rule for finding the sort of a function type, given the sorts of its input an output types.
pub fn rule(sort0: Sort, sort1: Sort) -> Sort {
    match (sort0, sort1) {
        (Sort::Type, Sort::Type) => Sort::Type,
        (Sort::Type, Sort::Kind) => Sort::Kind,
        (Sort::Kind, Sort::Type) => Sort::Kind,
        (Sort::Kind, Sort::Kind) => Sort::Kind,
    }
}

/// Contextual information to be used during validation.
pub struct Context<'me> {
    /// The global environment.
    globals: &'me Globals,
    /// Top-level item declarations.
    item_declarations: HashMap<String, Arc<Value>>,
    /// Top-level item definitions.
    item_definitions: HashMap<String, semantics::Item>,
    /// Local variable declarations.
    local_declarations: Locals<Arc<Value>>,
    /// Local variable definitions.
    local_definitions: Locals<Arc<Value>>,
    /// Diagnostic messages collected during type checking.
    messages: Vec<Message>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me Globals) -> Context<'me> {
        Context {
            globals,
            item_declarations: HashMap::new(),
            item_definitions: HashMap::new(),
            local_declarations: Locals::new(),
            local_definitions: Locals::new(),
            messages: Vec::new(),
        }
    }

    /// Get the number of entries in the context.
    fn size(&self) -> LocalSize {
        self.local_declarations.size()
    }

    /// Push a local entry.
    fn push_local(&mut self, value: Arc<Value>, r#type: Arc<Value>) {
        self.local_declarations.push(r#type);
        self.local_definitions.push(value);
    }

    /// Push a local parameter.
    fn push_local_param(&mut self, r#type: Arc<Value>) -> Arc<Value> {
        let value = Arc::new(Value::local(self.size().next_level(), Vec::new()));
        self.push_local(value.clone(), r#type);
        value
    }

    /// Pop a local entry.
    #[allow(dead_code)]
    fn pop_local(&mut self) {
        self.local_declarations.pop();
        self.local_definitions.pop();
    }

    /// Truncate number of local entries to the given size.
    fn truncate_locals(&mut self, local_size: LocalSize) {
        self.local_declarations.truncate(local_size);
        self.local_definitions.truncate(local_size);
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

    /// Evaluate a [`core::Term`] into a [`Value`] in the current typing context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`core::Term`]: crate::lang::core::Term
    pub fn eval(&mut self, term: &Term) -> Arc<Value> {
        semantics::eval(
            self.globals,
            &self.item_definitions,
            &mut self.local_definitions,
            term,
        )
    }

    /// Read back a value into normal form using the current state of the elaborator.
    pub fn read_back(&self, value: &Value) -> Term {
        semantics::read_back(
            self.globals,
            &self.item_definitions,
            self.local_definitions.size(),
            value,
        )
    }

    /// Check that one [`Value`] is [computationally equal]
    /// to another [`Value`] in the current typing context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
    pub fn is_equal(&self, value0: &Value, value1: &Value) -> bool {
        semantics::is_equal(self.globals, &self.item_definitions, value0, value1)
    }

    /// Validate that a module is well-formed.
    #[debug_ensures(self.item_declarations.is_empty())]
    #[debug_ensures(self.item_definitions.is_empty())]
    #[debug_ensures(self.local_declarations.is_empty())]
    #[debug_ensures(self.local_definitions.is_empty())]
    pub fn is_module(&mut self, module: &Module) {
        for item in &module.items {
            use std::collections::hash_map::Entry;

            let (item_name, item_data, item_type) = match &item.data {
                ItemData::Constant(constant) => {
                    let r#type = self.synth_type(&constant.term);
                    let value = self.eval(&constant.term);

                    (
                        constant.name.clone(),
                        semantics::ItemData::Constant(value),
                        r#type,
                    )
                }
                ItemData::StructType(struct_type) => {
                    use std::collections::HashSet;

                    let initial_size = self.size();

                    // Check parameters
                    for (_, param_type) in struct_type.params.iter() {
                        self.synth_sort(param_type);
                    }
                    // Add parameters to the context
                    for (_, param_type) in struct_type.params.iter() {
                        let param_type = self.eval(param_type);
                        self.push_local_param(param_type);
                    }

                    // Field labels that have previously seen.
                    let mut seen_field_labels = HashSet::new();
                    let type_type = Arc::new(Value::Sort(Sort::Type));

                    // Check the field declarations
                    for field in struct_type.fields.iter() {
                        self.check_type(&field.type_, &type_type);
                        let field_type = self.eval(&field.type_);

                        if seen_field_labels.insert(field.label.data.clone()) {
                            self.push_local_param(field_type);
                        } else {
                            self.push_message(CoreTypingMessage::FieldRedeclaration {
                                field_name: field.label.data.clone(),
                                record_location: item.location,
                            });
                        }
                    }

                    // Clean up the type checking context
                    self.truncate_locals(initial_size);

                    // Build up the return type
                    let mut r#type = type_type;
                    for (_, param_type) in struct_type.params.iter().rev() {
                        let param_type = self.eval(param_type);
                        r#type = Arc::new(Value::FunctionType(param_type, r#type));
                    }

                    let item_data = semantics::ItemData::StructType(
                        struct_type.params.len(),
                        struct_type.fields.clone(),
                    );

                    (struct_type.name.clone(), item_data, r#type)
                }
                ItemData::StructFormat(struct_format) => {
                    use std::collections::HashSet;

                    let initial_size = self.size();

                    // Check parameters
                    for (_, param_type) in struct_format.params.iter() {
                        self.synth_sort(param_type);
                    }
                    // Add parameters to the context
                    for (_, param_type) in struct_format.params.iter() {
                        let param_type = self.eval(param_type);
                        self.push_local_param(param_type);
                    }

                    // Field labels that have previously seen.
                    let mut seen_field_labels = HashSet::new();
                    let format_type = Arc::new(Value::FormatType);

                    // Check the field declarations
                    for field in struct_format.fields.iter() {
                        self.check_type(&field.type_, &format_type);
                        let field_type = semantics::repr(self.eval(&field.type_));

                        if seen_field_labels.insert(field.label.data.clone()) {
                            self.push_local_param(field_type);
                        } else {
                            self.push_message(CoreTypingMessage::FieldRedeclaration {
                                field_name: field.label.data.clone(),
                                record_location: item.location,
                            });
                        }
                    }

                    // Clean up the type checking context
                    self.truncate_locals(initial_size);

                    // Build up the return type
                    let mut r#type = format_type;
                    for (_, param_type) in struct_format.params.iter().rev() {
                        let param_type = self.eval(param_type);
                        r#type = Arc::new(Value::FunctionType(param_type, r#type));
                    }

                    let item_data = semantics::ItemData::StructFormat(
                        struct_format.params.len(),
                        struct_format.fields.clone(),
                    );

                    (struct_format.name.clone(), item_data, r#type)
                }
            };

            match self.item_definitions.entry(item_name.clone()) {
                Entry::Vacant(entry) => {
                    self.item_declarations.insert(item_name, item_type);
                    entry.insert(semantics::Item::new(item.location, item_data));
                }
                Entry::Occupied(entry) => {
                    let original_location = entry.get().location;
                    self.push_message(CoreTypingMessage::ItemRedefinition {
                        name: item_name,
                        found_location: item.location,
                        original_location,
                    });
                }
            }
        }

        self.item_declarations.clear();
        self.item_definitions.clear();
    }

    /// Validate that that a term is a well-formed type.
    #[debug_ensures(self.item_declarations.len() == old(self.item_declarations.len()))]
    #[debug_ensures(self.item_definitions.len() == old(self.item_definitions.len()))]
    #[debug_ensures(self.local_declarations.size() == old(self.local_declarations.size()))]
    #[debug_ensures(self.local_definitions.size() == old(self.local_definitions.size()))]
    pub fn synth_sort(&mut self, term: &Term) -> Option<Sort> {
        match self.synth_type(term).as_ref() {
            Value::Error => None,
            Value::Sort(sort) => Some(*sort),
            r#type => {
                self.push_message(CoreTypingMessage::UniverseMismatch {
                    term_location: term.location,
                    found_type: self.read_back(&r#type),
                });
                None
            }
        }
    }

    /// Validate that a term is an element of the given type.
    #[debug_ensures(self.item_declarations.len() == old(self.item_declarations.len()))]
    #[debug_ensures(self.item_definitions.len() == old(self.item_definitions.len()))]
    #[debug_ensures(self.local_declarations.size() == old(self.local_declarations.size()))]
    #[debug_ensures(self.local_definitions.size() == old(self.local_definitions.size()))]
    pub fn check_type(&mut self, term: &Term, expected_type: &Arc<Value>) {
        match (&term.data, expected_type.as_ref()) {
            (TermData::Error, _) | (_, Value::Error) => {}

            (TermData::StructTerm(field_definitions), _) => {
                use std::collections::btree_map::Entry;
                use std::collections::BTreeMap;

                // Resolve the struct type definition in the context.
                let field_declarations = match self.force_field_declarations(expected_type) {
                    Some(field_declarations) => field_declarations,
                    None => {
                        self.push_message(CoreTypingMessage::UnexpectedStructTerm {
                            term_location: term.location,
                            expected_type: self.read_back(expected_type),
                        });
                        return;
                    }
                };

                // Initial pass over the fields, looking for duplicate fields.
                let mut pending_field_definitions = BTreeMap::new();
                let mut duplicate_labels = Vec::new();
                for field_definition in field_definitions {
                    match pending_field_definitions.entry(&field_definition.label.data) {
                        Entry::Vacant(entry) => drop(entry.insert(field_definition)),
                        Entry::Occupied(_) => duplicate_labels.push(field_definition.label.clone()),
                    }
                }

                // Check that the fields match the types from the type definition.
                let mut missing_labels = Vec::new();

                field_declarations.for_each_field(
                    self.globals,
                    &self.item_definitions.clone(), // FIXME: avoid clone
                    |label, r#type| match (pending_field_definitions.remove(&label.data), r#type) {
                        (Some(field_definition), Some(r#type)) => {
                            self.check_type(&field_definition.term, &r#type);
                            self.eval(&field_definition.term)
                        }
                        (Some(_), None) => Arc::new(Value::Error),
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
                if !duplicate_labels.is_empty() {
                    self.push_message(CoreTypingMessage::DuplicateStructFields {
                        duplicate_labels,
                    });
                }
                if !missing_labels.is_empty() {
                    self.push_message(CoreTypingMessage::MissingStructFields {
                        term_location: term.location,
                        missing_labels,
                    });
                }
                if !unexpected_labels.is_empty() {
                    self.push_message(CoreTypingMessage::UnexpectedStructFields {
                        term_location: term.location,
                        unexpected_labels,
                    });
                }
            }

            (TermData::ArrayTerm(elem_terms), _) => match expected_type.try_global() {
                Some(("Array", [Elim::Function(len), Elim::Function(elem_type)])) => {
                    for elem_term in elem_terms {
                        self.check_type(elem_term, elem_type);
                    }

                    match len.as_ref() {
                        Value::Primitive(Primitive::Int(len))
                            if *len == elem_terms.len().into() => {}
                        _ => {
                            let found_len =
                                Arc::new(Value::Primitive(Primitive::Int(elem_terms.len().into())));
                            self.push_message(CoreTypingMessage::TypeMismatch {
                                term_location: term.location,
                                expected_type: self.read_back(expected_type),
                                found_type: self.read_back(&Value::global(
                                    "Array",
                                    vec![
                                        Elim::Function(found_len),
                                        Elim::Function(elem_type.clone()),
                                    ],
                                )),
                            });
                        }
                    }
                }
                Some(_) | None => {
                    self.push_message(CoreTypingMessage::UnexpectedArrayTerm {
                        term_location: term.location,
                        expected_type: self.read_back(expected_type),
                    });
                }
            },

            (TermData::BoolElim(term, if_true, if_false), _) => {
                let bool_type = Arc::new(Value::global("Bool", Vec::new()));
                self.check_type(term, &bool_type);
                self.check_type(if_true, expected_type);
                self.check_type(if_false, expected_type);
            }
            (TermData::IntElim(head, branches, default), _) => {
                let int_type = Arc::new(Value::global("Int", Vec::new()));
                self.check_type(head, &int_type);
                for term in branches.values() {
                    self.check_type(term, expected_type);
                }
                self.check_type(default, expected_type);
            }

            (_, expected_type) => match self.synth_type(term) {
                found_type if self.is_equal(&found_type, expected_type) => {}
                found_type => self.push_message(CoreTypingMessage::TypeMismatch {
                    term_location: term.location,
                    expected_type: self.read_back(expected_type),
                    found_type: self.read_back(&found_type),
                }),
            },
        }
    }

    /// Synthesize the type of a term.
    #[debug_ensures(self.item_declarations.len() == old(self.item_declarations.len()))]
    #[debug_ensures(self.item_definitions.len() == old(self.item_definitions.len()))]
    #[debug_ensures(self.local_declarations.size() == old(self.local_declarations.size()))]
    #[debug_ensures(self.local_definitions.size() == old(self.local_definitions.size()))]
    pub fn synth_type(&mut self, term: &Term) -> Arc<Value> {
        match &term.data {
            TermData::Global(global_name) => match self.globals.get(global_name) {
                Some((r#type, _)) => self.eval(r#type),
                None => {
                    self.push_message(CoreTypingMessage::GlobalNameNotFound {
                        global_name: global_name.clone(),
                        global_name_location: term.location,
                    });
                    Arc::new(Value::Error)
                }
            },
            TermData::Item(item_name) => match self.item_declarations.get(item_name) {
                Some(r#type) => r#type.clone(),
                None => {
                    self.push_message(CoreTypingMessage::ItemNameNotFound {
                        item_name: item_name.clone(),
                        item_name_location: term.location,
                    });
                    Arc::new(Value::Error)
                }
            },
            TermData::Local(local_index) => match self.local_declarations.get(*local_index) {
                Some(r#type) => r#type.clone(),
                None => {
                    self.push_message(CoreTypingMessage::LocalIndexNotFound {
                        local_index: *local_index,
                        local_index_location: term.location,
                    });
                    Arc::new(Value::Error)
                }
            },

            TermData::Ann(term, r#type) => match self.synth_sort(r#type) {
                None => Arc::new(Value::Error),
                Some(_) => {
                    let r#type = self.eval(r#type);
                    self.check_type(term, &r#type);
                    r#type
                }
            },
            TermData::Sort(sort) => match axiom(*sort) {
                Some(sort) => Arc::new(Value::Sort(sort)),
                None => {
                    self.push_message(CoreTypingMessage::TermHasNoType {
                        term_location: term.location,
                    });
                    Arc::new(Value::Error)
                }
            },

            TermData::FunctionType(param_type, body_type) => {
                let param_sort = self.synth_sort(param_type);
                let body_sort = self.synth_sort(body_type);

                match (param_sort, body_sort) {
                    (Some(param_sort), Some(body_sort)) => {
                        Arc::new(Value::Sort(rule(param_sort, body_sort)))
                    }
                    (_, _) => Arc::new(Value::Error),
                }
            }
            TermData::FunctionElim(head, argument) => {
                match self.synth_type(head).as_ref() {
                    Value::FunctionType(param_type, body_type) => {
                        self.check_type(argument, &param_type);
                        (*body_type).clone() // FIXME: Clone
                    }
                    Value::Error => Arc::new(Value::Error),
                    head_type => {
                        self.push_message(CoreTypingMessage::NotAFunction {
                            head_location: head.location,
                            head_type: self.read_back(head_type),
                            argument_location: argument.location,
                        });
                        Arc::new(Value::Error)
                    }
                }
            }

            TermData::StructTerm(_) => {
                self.push_message(CoreTypingMessage::AmbiguousTerm {
                    term_location: term.location,
                });
                Arc::new(Value::Error)
            }
            TermData::StructElim(head, label) => {
                let head_type = self.synth_type(head);
                if let Value::Error = head_type.as_ref() {
                    return Arc::new(Value::Error);
                }

                if let Some(field_declarations) = self.force_field_declarations(&head_type) {
                    let head_value = self.eval(&head);

                    let field_type = field_declarations.get_field_type(
                        self.globals,
                        &self.item_definitions,
                        head_value,
                        label,
                    );

                    if let Some(field_type) = field_type {
                        return field_type;
                    }
                }

                self.push_message(CoreTypingMessage::FieldNotFound {
                    head_location: head.location,
                    head_type: self.read_back(&head_type),
                    label: label.clone(),
                });
                Arc::new(Value::Error)
            }

            TermData::ArrayTerm(_) => {
                self.push_message(CoreTypingMessage::AmbiguousTerm {
                    term_location: term.location,
                });
                Arc::new(Value::Error)
            }

            TermData::Primitive(primitive) => match primitive {
                Primitive::Int(_) => Arc::new(Value::global("Int", Vec::new())),
                Primitive::F32(_) => Arc::new(Value::global("F32", Vec::new())),
                Primitive::F64(_) => Arc::new(Value::global("F64", Vec::new())),
                Primitive::Pos(_) => Arc::new(Value::global("Pos", Vec::new())),
            },
            TermData::BoolElim(head, if_true, if_false) => {
                let bool_type = Arc::new(Value::global("Bool", Vec::new()));
                self.check_type(head, &bool_type);
                let if_true_type = self.synth_type(if_true);
                let if_false_type = self.synth_type(if_false);

                if self.is_equal(&if_true_type, &if_false_type) {
                    if_true_type
                } else {
                    self.push_message(CoreTypingMessage::TypeMismatch {
                        term_location: if_false.location,
                        expected_type: self.read_back(&if_true_type),
                        found_type: self.read_back(&if_false_type),
                    });
                    Arc::new(Value::Error)
                }
            }
            TermData::IntElim(_, _, _) => {
                self.push_message(CoreTypingMessage::AmbiguousTerm {
                    term_location: term.location,
                });
                Arc::new(Value::Error)
            }

            TermData::FormatType => Arc::new(Value::Sort(Sort::Kind)),

            TermData::Repr => Arc::new(Value::FunctionType(
                Arc::new(Value::FormatType),
                Arc::new(Value::Sort(Sort::Type)),
            )),

            TermData::Error => Arc::new(Value::Error),
        }
    }
}
