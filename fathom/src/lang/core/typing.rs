//! Type-checking of Fathom's core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use contracts::debug_ensures;
use std::collections::HashMap;
use std::sync::Arc;

use crate::lang::core::semantics::{self, Elim, Value};
use crate::lang::core::{
    FieldDeclaration, Globals, ItemData, LocalLevel, Locals, Module, Primitive, Sort, Term,
    TermData,
};
use crate::lang::{FileId, Range};
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

    /// Get the next level to be used for a local entry.
    fn next_level(&self) -> LocalLevel {
        self.local_declarations.size().next_level()
    }

    /// Push a local entry.
    fn push_local(&mut self, value: Arc<Value>, r#type: Arc<Value>) {
        self.local_declarations.push(r#type);
        self.local_definitions.push(value);
    }

    /// Push a local parameter.
    fn push_local_param(&mut self, r#type: Arc<Value>) -> Arc<Value> {
        let value = Arc::new(Value::local(self.next_level(), Vec::new()));
        self.push_local(value.clone(), r#type);
        value
    }

    /// Pop a local entry.
    #[allow(dead_code)]
    fn pop_local(&mut self) {
        self.local_declarations.pop();
        self.local_definitions.pop();
    }

    /// Pop the given number of local entries.
    fn pop_many_locals(&mut self, count: usize) {
        self.local_declarations.pop_many(count);
        self.local_definitions.pop_many(count);
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
    ) -> Option<(Range, &'context semantics::ItemData, &'value [Elim])> {
        let (name, elims) = value.try_item()?;
        match self.item_definitions.get(name) {
            Some(item) => Some((item.range, &item.data, elims)),
            None => panic!("could not find an item called `{}` in the context", name),
        }
    }

    /// Force a value to resolve to a struct type, returning `None` if the value did
    /// not refer to an item.
    fn force_struct_type<'context, 'value>(
        &'context self,
        value: &'value Value,
    ) -> Option<(Range, Arc<[FieldDeclaration]>, &'value [Elim])> {
        match self.force_item(value) {
            Some((range, semantics::ItemData::StructType(field_declarations), elims)) => {
                Some((range, field_declarations.clone(), elims))
            }
            Some(_) | None => None,
        }
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

    /// Evaluate a term using the supplied local environment.
    fn eval_with_locals(&mut self, locals: &mut Locals<Arc<Value>>, term: &Term) -> Arc<Value> {
        semantics::eval(self.globals, &self.item_definitions, locals, term)
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
        let file_id = module.file_id;

        for item in &module.items {
            use std::collections::hash_map::Entry;

            let (item_name, item_data, item_type) = match &item.data {
                ItemData::Constant(constant) => {
                    let r#type = self.synth_type(file_id, &constant.term);
                    let value = self.eval(&constant.term);

                    (
                        constant.name.clone(),
                        semantics::ItemData::Constant(value),
                        r#type,
                    )
                }
                ItemData::StructType(struct_type) => {
                    use std::collections::HashSet;

                    // Field labels that have previously seen.
                    let mut seen_field_labels = HashSet::new();
                    let type_type = Arc::new(Value::Sort(Sort::Type));

                    for field in struct_type.fields.iter() {
                        self.check_type(file_id, &field.type_, &type_type);
                        let field_type = self.eval(&field.type_);

                        if seen_field_labels.insert(field.label.data.clone()) {
                            self.push_local_param(field_type);
                        } else {
                            self.push_message(CoreTypingMessage::FieldRedeclaration {
                                file_id,
                                field_name: field.label.data.clone(),
                                record_range: item.range,
                            });
                        }
                    }

                    self.pop_many_locals(seen_field_labels.len());

                    (
                        struct_type.name.clone(),
                        semantics::ItemData::StructType(struct_type.fields.clone()),
                        type_type.clone(),
                    )
                }
                ItemData::StructFormat(struct_format) => {
                    use std::collections::HashSet;

                    // Field labels that have previously seen.
                    let mut seen_field_labels = HashSet::new();
                    let format_type = Arc::new(Value::FormatType);

                    for field in struct_format.fields.iter() {
                        self.check_type(file_id, &field.type_, &format_type);
                        let field_type = semantics::apply_repr(self.eval(&field.type_));

                        if seen_field_labels.insert(field.label.data.clone()) {
                            self.push_local_param(field_type);
                        } else {
                            self.push_message(CoreTypingMessage::FieldRedeclaration {
                                file_id,
                                field_name: field.label.data.clone(),
                                record_range: item.range,
                            });
                        }
                    }

                    self.pop_many_locals(seen_field_labels.len());

                    (
                        struct_format.name.clone(),
                        semantics::ItemData::StructFormat(struct_format.fields.clone()),
                        format_type.clone(),
                    )
                }
            };

            match self.item_definitions.entry(item_name.clone()) {
                Entry::Vacant(entry) => {
                    self.item_declarations.insert(item_name, item_type);
                    entry.insert(semantics::Item::new(item.range, item_data));
                }
                Entry::Occupied(entry) => {
                    let original_range = entry.get().range;
                    self.push_message(CoreTypingMessage::ItemRedefinition {
                        file_id,
                        name: item_name,
                        found_range: item.range,
                        original_range,
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
    pub fn synth_sort(&mut self, file_id: FileId, term: &Term) -> Option<Sort> {
        match self.synth_type(file_id, term).as_ref() {
            Value::Error => None,
            Value::Sort(sort) => Some(*sort),
            r#type => {
                self.push_message(CoreTypingMessage::UniverseMismatch {
                    file_id,
                    term_range: term.range,
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
    pub fn check_type(&mut self, file_id: FileId, term: &Term, expected_type: &Arc<Value>) {
        match (&term.data, expected_type.as_ref()) {
            (TermData::Error, _) | (_, Value::Error) => {}

            (TermData::StructTerm(field_definitions), _) => {
                use std::collections::btree_map::Entry;
                use std::collections::BTreeMap;

                // Resolve the struct type definition in the context.
                let field_declarations = match self.force_struct_type(expected_type) {
                    Some((_, field_declarations, [])) => field_declarations,
                    Some((_, _, _)) => {
                        self.push_message(crate::reporting::Message::NotYetImplemented {
                            file_id,
                            range: term.range,
                            feature_name: "struct parameters",
                        });
                        return;
                    }
                    None => {
                        let expected_type = self.read_back(expected_type);
                        self.push_message(CoreTypingMessage::UnexpectedStructTerm {
                            file_id,
                            term_range: term.range,
                            expected_type,
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
                // Local environment for evaluating the field types with the
                // values defined in the terms.
                let mut type_locals = Locals::new();
                for field_declaration in field_declarations.iter() {
                    match pending_field_definitions.remove(&field_declaration.label.data) {
                        Some(field_definition) => {
                            // NOTE: It should be safe to evaluate the field type
                            // because we trust that struct items have been checked.
                            let r#type =
                                self.eval_with_locals(&mut type_locals, &field_declaration.type_);
                            // TODO: stop on errors
                            self.check_type(file_id, &field_definition.term, &r#type);
                            let value = self.eval(&field_definition.term);

                            type_locals.push(value);
                        }
                        None => missing_labels.push(field_declaration.label.clone()),
                    }
                }

                // Collect unexpected fields that were defined in the term but
                // were not declared in the type.
                let unexpected_labels = pending_field_definitions
                    .into_iter()
                    .map(|(_, field)| field.label.clone())
                    .collect::<Vec<_>>();

                // Record any errors in the context.
                if !duplicate_labels.is_empty() {
                    self.push_message(CoreTypingMessage::DuplicateStructFields {
                        file_id,
                        duplicate_labels,
                    });
                }
                if !missing_labels.is_empty() {
                    self.push_message(CoreTypingMessage::MissingStructFields {
                        file_id,
                        term_range: term.range,
                        missing_labels,
                    });
                }
                if !unexpected_labels.is_empty() {
                    self.push_message(CoreTypingMessage::UnexpectedStructFields {
                        file_id,
                        term_range: term.range,
                        unexpected_labels,
                    });
                }
            }

            (TermData::ArrayTerm(elem_terms), _) => match expected_type.try_global() {
                Some(("Array", [Elim::Function(len), Elim::Function(elem_type)])) => {
                    for elem_term in elem_terms {
                        self.check_type(file_id, elem_term, elem_type);
                    }

                    match len.as_ref() {
                        Value::Primitive(Primitive::Int(len))
                            if *len == elem_terms.len().into() => {}
                        _ => {
                            let found_len =
                                Arc::new(Value::Primitive(Primitive::Int(elem_terms.len().into())));
                            self.push_message(CoreTypingMessage::TypeMismatch {
                                file_id,
                                term_range: term.range,
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
                        file_id,
                        term_range: term.range,
                        expected_type: self.read_back(expected_type),
                    });
                }
            },

            (TermData::BoolElim(term, if_true, if_false), _) => {
                let bool_type = Arc::new(Value::global("Bool", Vec::new()));
                self.check_type(file_id, term, &bool_type);
                self.check_type(file_id, if_true, expected_type);
                self.check_type(file_id, if_false, expected_type);
            }
            (TermData::IntElim(head, branches, default), _) => {
                let int_type = Arc::new(Value::global("Int", Vec::new()));
                self.check_type(file_id, head, &int_type);
                for term in branches.values() {
                    self.check_type(file_id, term, expected_type);
                }
                self.check_type(file_id, default, expected_type);
            }

            (_, expected_type) => match self.synth_type(file_id, term) {
                found_type if self.is_equal(&found_type, expected_type) => {}
                found_type => self.push_message(CoreTypingMessage::TypeMismatch {
                    file_id,
                    term_range: term.range,
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
    pub fn synth_type(&mut self, file_id: FileId, term: &Term) -> Arc<Value> {
        match &term.data {
            TermData::Global(global_name) => match self.globals.get(global_name) {
                Some((r#type, _)) => self.eval(r#type),
                None => {
                    self.push_message(CoreTypingMessage::GlobalNameNotFound {
                        file_id,
                        global_name: global_name.clone(),
                        global_name_range: term.range,
                    });
                    Arc::new(Value::Error)
                }
            },
            TermData::Item(item_name) => match self.item_declarations.get(item_name) {
                Some(r#type) => r#type.clone(),
                None => {
                    self.push_message(CoreTypingMessage::ItemNameNotFound {
                        file_id,
                        item_name: item_name.clone(),
                        item_name_range: term.range,
                    });
                    Arc::new(Value::Error)
                }
            },
            TermData::Local(local_index) => match self.local_declarations.get(*local_index) {
                Some(r#type) => r#type.clone(),
                None => {
                    self.push_message(CoreTypingMessage::LocalIndexNotFound {
                        file_id,
                        local_index: *local_index,
                        local_index_range: term.range,
                    });
                    Arc::new(Value::Error)
                }
            },

            TermData::Ann(term, r#type) => match self.synth_sort(file_id, r#type) {
                None => Arc::new(Value::Error),
                Some(_) => {
                    let r#type = self.eval(r#type);
                    self.check_type(file_id, term, &r#type);
                    r#type
                }
            },
            TermData::Sort(sort) => match axiom(*sort) {
                Some(sort) => Arc::new(Value::Sort(sort)),
                None => {
                    self.push_message(CoreTypingMessage::TermHasNoType {
                        file_id,
                        term_range: term.range,
                    });
                    Arc::new(Value::Error)
                }
            },

            TermData::FunctionType(param_type, body_type) => {
                let param_sort = self.synth_sort(file_id, param_type);
                let body_sort = self.synth_sort(file_id, body_type);

                match (param_sort, body_sort) {
                    (Some(param_sort), Some(body_sort)) => {
                        Arc::new(Value::Sort(rule(param_sort, body_sort)))
                    }
                    (_, _) => Arc::new(Value::Error),
                }
            }
            TermData::FunctionElim(head, argument) => {
                match self.synth_type(file_id, head).as_ref() {
                    Value::FunctionType(param_type, body_type) => {
                        self.check_type(file_id, argument, &param_type);
                        (*body_type).clone() // FIXME: Clone
                    }
                    Value::Error => Arc::new(Value::Error),
                    head_type => {
                        self.push_message(CoreTypingMessage::NotAFunction {
                            file_id,
                            head_range: head.range,
                            head_type: self.read_back(head_type),
                            argument_range: argument.range,
                        });
                        Arc::new(Value::Error)
                    }
                }
            }

            TermData::StructTerm(_) => {
                self.push_message(CoreTypingMessage::AmbiguousTerm {
                    file_id,
                    term_range: term.range,
                });
                Arc::new(Value::Error)
            }
            TermData::StructElim(head, label) => {
                let head_type = self.synth_type(file_id, head);
                if let Value::Error = head_type.as_ref() {
                    return Arc::new(Value::Error);
                }

                match self.force_struct_type(&head_type) {
                    Some((_, field_declarations, [])) => {
                        // This head value will be used when adding the dependent
                        // fields to the context.
                        let head_value = self.eval(&head);
                        // Local environment where the field types will be evaluated.
                        let mut type_locals = Locals::new();

                        for field_declaration in field_declarations.iter() {
                            if field_declaration.label.data == *label {
                                // NOTE: It should be safe to evaluate the field type
                                // because we trust that struct items have been checked.
                                return self
                                    .eval_with_locals(&mut type_locals, &field_declaration.type_);
                            } else {
                                type_locals.push(semantics::apply_struct_elim(
                                    head_value.clone(),
                                    &field_declaration.label.data,
                                ));
                            }
                        }
                    }
                    Some((_, _, _)) => {
                        self.push_message(crate::reporting::Message::NotYetImplemented {
                            file_id,
                            range: term.range,
                            feature_name: "struct parameters",
                        });
                        return Arc::new(Value::Error);
                    }
                    None => {}
                }

                let head_type = self.read_back(&head_type);
                self.push_message(CoreTypingMessage::FieldNotFound {
                    file_id,
                    head_range: head.range,
                    head_type,
                    label: label.clone(),
                });
                Arc::new(Value::Error)
            }

            TermData::ArrayTerm(_) => {
                self.push_message(CoreTypingMessage::AmbiguousTerm {
                    file_id,
                    term_range: term.range,
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
                self.check_type(file_id, head, &bool_type);
                let if_true_type = self.synth_type(file_id, if_true);
                let if_false_type = self.synth_type(file_id, if_false);

                if self.is_equal(&if_true_type, &if_false_type) {
                    if_true_type
                } else {
                    self.push_message(CoreTypingMessage::TypeMismatch {
                        file_id,
                        term_range: if_false.range,
                        expected_type: self.read_back(&if_true_type),
                        found_type: self.read_back(&if_false_type),
                    });
                    Arc::new(Value::Error)
                }
            }
            TermData::IntElim(_, _, _) => {
                self.push_message(CoreTypingMessage::AmbiguousTerm {
                    file_id,
                    term_range: term.range,
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
