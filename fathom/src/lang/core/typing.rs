//! Type-checking of Fathom's core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use std::collections::HashMap;
use std::sync::Arc;

use crate::lang::core::semantics::{self, Value};
use crate::lang::core::{Globals, Item, ItemData, Module, Primitive, Sort, Term, TermData};
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
    /// Labels that have previously been used for items.
    items: HashMap<String, Item>,
    /// List of types currently bound in this context.
    /// These could either refer to items or local bindings.
    types: Vec<(String, Arc<Value>)>,
    /// Diagnostic messages collected during type checking.
    messages: Vec<Message>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me Globals) -> Context<'me> {
        Context {
            globals,
            items: HashMap::new(),
            types: Vec::new(),
            messages: Vec::new(),
        }
    }

    /// Store a diagnostic message in the context for later reporting.
    fn push_message(&mut self, message: impl Into<Message>) {
        self.messages.push(message.into());
    }

    /// Drain the collected diagnostic messages from the context.
    pub fn drain_messages<'a>(&'a mut self) -> impl 'a + Iterator<Item = Message> {
        self.messages.drain(..)
    }

    /// Lookup the type of a binding corresponding to `name` in the context,
    /// returning `None` if `name` was not yet bound.
    pub fn lookup_type(&self, name: &str) -> Option<&Arc<Value>> {
        Some(&self.types.iter().rev().find(|(n, _)| *n == name)?.1)
    }

    /// Evaluate a [`core::Term`] into a [`Value`] in the current typing context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`core::Term`]: crate::lang::core::Term
    pub fn eval(&self, term: &Term) -> Arc<Value> {
        semantics::eval(self.globals, &self.items, term)
    }

    /// Read back a value into normal form using the current state of the elaborator.
    pub fn read_back(&self, value: &Value) -> Term {
        semantics::read_back(value)
    }

    /// Check that one [`Value`] is [computationally equal]
    /// to another [`Value`] in the current typing context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
    pub fn is_equal(&self, value0: &Value, value1: &Value) -> bool {
        semantics::is_equal(self.globals, &self.items, value0, value1)
    }

    /// Validate that a module is well-formed.
    pub fn is_module(&mut self, module: &Module) {
        let file_id = module.file_id;

        for item in &module.items {
            use std::collections::hash_map::Entry;

            let (item_name, item_type) = match &item.data {
                ItemData::Alias(alias) => {
                    let alias_type = self.synth_type(file_id, &alias.term);

                    (alias.name.clone(), alias_type)
                }
                ItemData::StructType(struct_type) => {
                    use std::collections::HashSet;

                    // Field names that have previously seen.
                    let mut seen_field_names = HashSet::new();
                    let type_type = Arc::new(Value::Sort(Sort::Type));

                    for field in &struct_type.fields {
                        self.check_type(file_id, &field.term, &type_type);

                        if !seen_field_names.insert(field.name.clone()) {
                            self.push_message(CoreTypingMessage::FieldRedeclaration {
                                file_id,
                                field_name: field.name.clone(),
                                record_range: item.range(),
                            });
                        }
                    }

                    (struct_type.name.clone(), type_type.clone())
                }
                ItemData::StructFormat(struct_format) => {
                    use std::collections::HashSet;

                    // Field names that have previously seen.
                    let mut seen_field_names = HashSet::new();
                    let format_type = Arc::new(Value::FormatType);

                    for field in &struct_format.fields {
                        self.check_type(file_id, &field.term, &format_type);

                        if !seen_field_names.insert(field.name.clone()) {
                            self.push_message(CoreTypingMessage::FieldRedeclaration {
                                file_id,
                                field_name: field.name.clone(),
                                record_range: item.range(),
                            });
                        }
                    }

                    (struct_format.name.clone(), format_type.clone())
                }
            };

            match self.items.entry(item_name.clone()) {
                Entry::Vacant(entry) => {
                    self.types.push((item_name, item_type));
                    entry.insert(item.clone());
                }
                Entry::Occupied(entry) => {
                    let original_range = entry.get().range();
                    self.push_message(CoreTypingMessage::ItemRedefinition {
                        file_id,
                        name: item_name,
                        found_range: item.range(),
                        original_range,
                    });
                }
            }
        }

        self.items.clear();
        self.types.clear();
    }

    /// Validate that that a term is a well-formed type.
    pub fn synth_sort(&mut self, file_id: usize, term: &Term) -> Option<Sort> {
        match self.synth_type(file_id, term).as_ref() {
            Value::Error => None,
            Value::Sort(sort) => Some(*sort),
            r#type => {
                self.push_message(CoreTypingMessage::UniverseMismatch {
                    file_id,
                    term_range: term.range(),
                    found_type: self.read_back(&r#type),
                });
                None
            }
        }
    }

    /// Validate that a term is an element of the given type.
    pub fn check_type(&mut self, file_id: usize, term: &Term, expected_type: &Arc<Value>) {
        match (&term.data, expected_type.as_ref()) {
            (TermData::Error, _) | (_, Value::Error) => {}
            (TermData::BoolElim(term, if_true, if_false), _) => {
                let bool_type = Arc::new(Value::global("Bool"));
                self.check_type(file_id, term, &bool_type);
                self.check_type(file_id, if_true, expected_type);
                self.check_type(file_id, if_false, expected_type);
            }
            (TermData::IntElim(head, branches, default), _) => {
                let int_type = Arc::new(Value::global("Int"));
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
                    term_range: term.range(),
                    expected_type: self.read_back(expected_type),
                    found_type: self.read_back(&found_type),
                }),
            },
        }
    }

    /// Synthesize the type of a term.
    pub fn synth_type(&mut self, file_id: usize, term: &Term) -> Arc<Value> {
        match &term.data {
            TermData::Global(name) => match self.globals.get(name) {
                Some((r#type, _)) => self.eval(r#type),
                None => {
                    self.push_message(CoreTypingMessage::GlobalNameNotFound {
                        file_id,
                        name: name.clone(),
                        name_range: term.range(),
                    });
                    Arc::new(Value::Error)
                }
            },
            TermData::Item(name) => match self.lookup_type(name) {
                Some(r#type) => r#type.clone(),
                None => {
                    self.push_message(CoreTypingMessage::ItemNameNotFound {
                        file_id,
                        name: name.clone(),
                        name_range: term.range(),
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
                        term_range: term.range(),
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
                            head_range: head.range(),
                            head_type: self.read_back(head_type),
                            argument_range: argument.range(),
                        });
                        Arc::new(Value::Error)
                    }
                }
            }

            TermData::Primitive(primitive) => match primitive {
                // TODO: Lookup globals in environment
                Primitive::Int(_) => Arc::new(Value::global("Int")),
                Primitive::F32(_) => Arc::new(Value::global("F32")),
                Primitive::F64(_) => Arc::new(Value::global("F64")),
            },
            TermData::BoolElim(head, if_true, if_false) => {
                // TODO: Lookup globals in environment
                let bool_type = Arc::new(Value::global("Bool"));
                self.check_type(file_id, head, &bool_type);
                let if_true_type = self.synth_type(file_id, if_true);
                let if_false_type = self.synth_type(file_id, if_false);

                if self.is_equal(&if_true_type, &if_false_type) {
                    if_true_type
                } else {
                    self.push_message(CoreTypingMessage::TypeMismatch {
                        file_id,
                        term_range: if_false.range(),
                        expected_type: self.read_back(&if_true_type),
                        found_type: self.read_back(&if_false_type),
                    });
                    Arc::new(Value::Error)
                }
            }
            TermData::IntElim(_, _, _) => {
                self.push_message(CoreTypingMessage::AmbiguousIntElim {
                    file_id,
                    term_range: term.range(),
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
