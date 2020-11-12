//! Elaboration from the surface syntax into the core syntax.
//!
//! Performs the following:
//!
//! - name resolution
//! - desugaring
//! - pattern compilation (TODO)
//! - bidirectional type checking (TODO)
//! - unification (TODO)

use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use crate::lang::core::semantics::{self, Head, Value};
use crate::lang::core::{self, Sort};
use crate::lang::surface::{ItemData, Module, Pattern, PatternData, StructType, Term, TermData};
use crate::literal;
use crate::pass::core_to_surface;
use crate::reporting::{Message, SurfaceToCoreMessage};

/// Contextual information to be used during elaboration.
pub struct Context<'me> {
    /// The global environment.
    globals: &'me core::Globals,
    /// Labels that have previously been used for items.
    items: HashMap<String, core::Item>,
    /// List of types currently bound in this context.
    /// These could either refer to items or local bindings.
    types: Vec<(String, Arc<Value>)>,
    /// Diagnostic messages collected during type checking.
    messages: Vec<Message>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me core::Globals) -> Context<'me> {
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

    /// Evaluate a [`core::Term`] into a [`Value`] in the current elaboration context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`core::Term`]: crate::lang::core::Term
    pub fn eval(&self, term: &core::Term) -> Arc<Value> {
        semantics::eval(self.globals, &self.items, term)
    }

    /// Read back a [`Value`] to a [`core::Term`] using the current
    /// state of the elaborator.
    ///
    /// Unstuck eliminations are not unfolded, making this useful for printing
    /// terms and types in user-facing diagnostics.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`core::Term`]: crate::lang::core::Term
    pub fn read_back(&self, value: &Value) -> core::Term {
        semantics::read_back(value)
    }

    /// Check that one [`Value`] is [computationally equal]
    /// to another [`Value`] in the current elaboration context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
    pub fn is_equal(&self, value0: &Value, value1: &Value) -> bool {
        semantics::is_equal(self.globals, &self.items, value0, value1)
    }

    /// Distill a [`core::Term`] into a [`surface::Term`].
    ///
    /// [`core::Term`]: crate::lang::core::Term
    /// [`surface::Term`]: crate::lang::surface::Term
    pub fn core_to_surface(&mut self, core_term: &core::Term) -> Term {
        core_to_surface::from_term(&core_term)
    }

    /// Read back a [`Value`] into a [`surface::Term`] using the
    /// current state of the elaborator.
    ///
    /// Unstuck eliminations are not unfolded, making this useful for printing
    /// terms and types in user-facing diagnostics.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [`surface::Term`]: crate::lang::surface::Term
    pub fn read_back_to_surface(&mut self, value: &Value) -> Term {
        let core_term = self.read_back(value);
        self.core_to_surface(&core_term)
    }

    /// Translate a surface module into a core module,
    /// while validating that it is well-formed.
    pub fn from_module(&mut self, surface_module: &Module) -> core::Module {
        let file_id = surface_module.file_id;
        let mut core_items = Vec::new();

        for item in surface_module.items.iter() {
            use std::collections::hash_map::Entry;

            let (name, item_data, r#type) = match &item.data {
                ItemData::Constant(constant) => {
                    let (core_term, r#type) = match &constant.type_ {
                        Some(surface_type) => {
                            let (core_type, _) = self.is_type(file_id, surface_type);
                            match &core_type.data {
                                core::TermData::Error => (
                                    core::Term::new(constant.term.range(), core::TermData::Error),
                                    Arc::new(Value::Error),
                                ),
                                _ => {
                                    let r#type = self.eval(&core_type);
                                    let term_data = core::TermData::Ann(
                                        Arc::new(self.check_type(file_id, &constant.term, &r#type)),
                                        Arc::new(core_type),
                                    );

                                    (core::Term::new(constant.term.range(), term_data), r#type)
                                }
                            }
                        }
                        None => self.synth_type(file_id, &constant.term),
                    };

                    let item_data = core::ItemData::Constant(core::Constant {
                        doc: constant.doc.clone(),
                        name: constant.name.data.clone(),
                        term: Arc::new(core_term),
                    });

                    (&constant.name, item_data, r#type)
                }
                ItemData::StructType(struct_type) => match &struct_type.type_ {
                    None => {
                        self.push_message(SurfaceToCoreMessage::MissingStructAnnotation {
                            file_id,
                            name: struct_type.name.data.clone(),
                            name_range: struct_type.name.range(),
                        });
                        continue;
                    }
                    Some(r#type) => {
                        let (core_type, _) = self.is_type(file_id, &r#type);
                        let r#type = self.eval(&core_type);
                        let item_data = match r#type.as_ref() {
                            Value::Sort(Sort::Type) => self.is_struct_type(file_id, struct_type),
                            Value::FormatType => self.is_struct_format(file_id, struct_type),
                            Value::Error => continue,
                            r#type => {
                                let ann_type = self.read_back_to_surface(r#type);
                                self.push_message(SurfaceToCoreMessage::InvalidStructAnnotation {
                                    file_id,
                                    name: struct_type.name.data.clone(),
                                    ann_type,
                                    ann_range: core_type.range(),
                                });
                                continue;
                            }
                        };

                        (&struct_type.name, item_data, r#type)
                    }
                },
            };

            // FIXME: Avoid shadowing builtin definitions
            match self.items.entry(name.data.clone()) {
                Entry::Vacant(entry) => {
                    let core_item = core::Item::new(item.range(), item_data);
                    core_items.push(core_item.clone());
                    self.types.push((entry.key().clone(), r#type));
                    entry.insert(core_item);
                }
                Entry::Occupied(entry) => {
                    let original_range = entry.get().range();
                    self.push_message(SurfaceToCoreMessage::ItemRedefinition {
                        file_id,
                        name: name.data.clone(),
                        found_range: item.range.clone(),
                        original_range,
                    });
                }
            }
        }

        self.items.clear();
        self.types.clear();

        core::Module {
            file_id,
            doc: surface_module.doc.clone(),
            items: core_items,
        }
    }

    pub fn is_struct_type(&mut self, file_id: usize, struct_type: &StructType) -> core::ItemData {
        use std::collections::hash_map::Entry;

        // Field labels that have previously seen, along with the source
        // range where they were introduced (for diagnostic reporting).
        let mut seen_field_labels = HashMap::new();
        // Fields that have been elaborated into the core syntax.
        let mut core_fields = Vec::with_capacity(struct_type.fields.len());

        for field in &struct_type.fields {
            let field_range = field.label.range().start..field.term.range().end;
            let format_type = Arc::new(Value::Sort(Sort::Type));
            let r#type = self.check_type(file_id, &field.term, &format_type);

            match seen_field_labels.entry(field.label.data.clone()) {
                Entry::Vacant(entry) => {
                    core_fields.push(core::FieldDeclaration {
                        doc: field.doc.clone(),
                        label: field.label.clone(),
                        term: Arc::new(r#type),
                    });

                    entry.insert(field_range);
                }
                Entry::Occupied(entry) => {
                    self.push_message(SurfaceToCoreMessage::FieldRedeclaration {
                        file_id,
                        name: entry.key().clone(),
                        found_range: field_range,
                        original_range: entry.get().clone(),
                    });
                }
            }
        }

        core::ItemData::StructType(core::StructType {
            doc: struct_type.doc.clone(),
            name: struct_type.name.data.clone(),
            fields: core_fields,
        })
    }

    pub fn is_struct_format(&mut self, file_id: usize, struct_type: &StructType) -> core::ItemData {
        use std::collections::hash_map::Entry;

        // Field names that have previously seen, along with the source
        // range where they were introduced (for diagnostic reporting).
        let mut seen_field_labels = HashMap::new();
        // Fields that have been elaborated into the core syntax.
        let mut core_fields = Vec::with_capacity(struct_type.fields.len());

        for field in &struct_type.fields {
            let field_range = field.label.range().start..field.term.range().end;
            let format_type = Arc::new(Value::FormatType);
            let r#type = self.check_type(file_id, &field.term, &format_type);

            match seen_field_labels.entry(field.label.data.clone()) {
                Entry::Vacant(entry) => {
                    core_fields.push(core::FieldDeclaration {
                        doc: field.doc.clone(),
                        label: field.label.clone(),
                        term: Arc::new(r#type),
                    });

                    entry.insert(field_range);
                }
                Entry::Occupied(entry) => {
                    self.push_message(SurfaceToCoreMessage::FieldRedeclaration {
                        file_id,
                        name: entry.key().clone(),
                        found_range: field_range,
                        original_range: entry.get().clone(),
                    });
                }
            }
        }

        core::ItemData::StructFormat(core::StructFormat {
            doc: struct_type.doc.clone(),
            name: struct_type.name.data.clone(),
            fields: core_fields,
        })
    }

    /// Validate that a surface term is a type, and translate it into the core syntax.
    pub fn is_type(
        &mut self,
        file_id: usize,
        surface_term: &Term,
    ) -> (core::Term, Option<core::Sort>) {
        let range = surface_term.range();

        let (core_term, core_type) = self.synth_type(file_id, surface_term);
        match core_type.as_ref() {
            Value::Error => (core::Term::new(range, core::TermData::Error), None),
            Value::Sort(sort) => (core_term, Some(*sort)),
            core_type => {
                let found_type = self.read_back_to_surface(core_type);
                self.push_message(SurfaceToCoreMessage::UniverseMismatch {
                    file_id,
                    term_range: range.clone(),
                    found_type,
                });
                (core::Term::new(range, core::TermData::Error), None)
            }
        }
    }

    /// Check that a surface term is an element of a type, and translate it into the
    /// core syntax.
    pub fn check_type(
        &mut self,
        file_id: usize,
        surface_term: &Term,
        expected_type: &Arc<Value>,
    ) -> core::Term {
        let range = surface_term.range();

        match (&surface_term.data, expected_type.as_ref()) {
            (TermData::Error, _) => core::Term::new(range, core::TermData::Error),
            (_, Value::Error) => core::Term::new(range, core::TermData::Error),

            (TermData::NumberLiteral(source), _) => {
                let parse_state =
                    literal::State::new(file_id, range.clone(), source, &mut self.messages);
                let term_data = match expected_type.as_ref() {
                    // TODO: Lookup globals in environment
                    Value::Stuck(Head::Global(name), elims) if elims.is_empty() => {
                        match name.as_str() {
                            "Int" => parse_state
                                .number_to_big_int()
                                .map(core::Primitive::Int)
                                .map_or(core::TermData::Error, core::TermData::Primitive),
                            "F32" => parse_state
                                .number_to_float()
                                .map(core::Primitive::F32)
                                .map_or(core::TermData::Error, core::TermData::Primitive),
                            "F64" => parse_state
                                .number_to_float()
                                .map(core::Primitive::F64)
                                .map_or(core::TermData::Error, core::TermData::Primitive),
                            _ => {
                                let expected_type = self.read_back_to_surface(expected_type);
                                self.push_message(
                                    SurfaceToCoreMessage::NumericLiteralNotSupported {
                                        file_id,
                                        literal_range: range.clone(),
                                        expected_type,
                                    },
                                );
                                core::TermData::Error
                            }
                        }
                    }
                    Value::Error => core::TermData::Error,
                    _ => {
                        let expected_type = self.read_back_to_surface(expected_type);
                        self.push_message(SurfaceToCoreMessage::NumericLiteralNotSupported {
                            file_id,
                            literal_range: range.clone(),
                            expected_type,
                        });
                        core::TermData::Error
                    }
                };

                core::Term::new(range, term_data)
            }
            (TermData::If(surface_head, surface_if_true, surface_if_false), _) => {
                // TODO: Lookup globals in environment
                let bool_type = Arc::new(Value::global("Bool"));
                let term_data = core::TermData::BoolElim(
                    Arc::new(self.check_type(file_id, surface_head, &bool_type)),
                    Arc::new(self.check_type(file_id, surface_if_true, expected_type)),
                    Arc::new(self.check_type(file_id, surface_if_false, expected_type)),
                );

                core::Term::new(range, term_data)
            }
            (TermData::Match(surface_head, surface_branches), _) => {
                let (head, head_type) = self.synth_type(file_id, surface_head);

                let term_data = match head_type.as_ref() {
                    Value::Stuck(Head::Global(name), elims) if elims.is_empty() => {
                        // TODO: Lookup globals in environment
                        match name.as_str() {
                            "Bool" => {
                                self.push_message(Message::NotYetImplemented {
                                    file_id,
                                    range: range.clone(),
                                    feature_name: "boolean patterns",
                                });
                                core::TermData::Error
                            }
                            "Int" => {
                                let (branches, default) = self.from_int_branches(
                                    file_id,
                                    surface_head.range(),
                                    surface_branches,
                                    expected_type,
                                );
                                core::TermData::IntElim(Arc::new(head), branches, default)
                            }
                            _ => {
                                let found_type = self.read_back_to_surface(&head_type);
                                self.push_message(SurfaceToCoreMessage::UnsupportedPatternType {
                                    file_id,
                                    scrutinee_range: surface_head.range(),
                                    found_type,
                                });
                                core::TermData::Error
                            }
                        }
                    }
                    Value::Error => core::TermData::Error,
                    _ => {
                        let found_type = self.read_back_to_surface(&head_type);
                        self.push_message(SurfaceToCoreMessage::UnsupportedPatternType {
                            file_id,
                            scrutinee_range: surface_head.range(),
                            found_type,
                        });
                        core::TermData::Error
                    }
                };

                core::Term::new(range, term_data)
            }

            (_, expected_type) => match self.synth_type(file_id, surface_term) {
                (core_term, found_type) if self.is_equal(&found_type, expected_type) => core_term,
                (_, found_type) => {
                    let expected_type = self.read_back_to_surface(expected_type);
                    let found_type = self.read_back_to_surface(&found_type);
                    self.push_message(SurfaceToCoreMessage::TypeMismatch {
                        file_id,
                        term_range: range.clone(),
                        expected_type,
                        found_type,
                    });
                    core::Term::new(range, core::TermData::Error)
                }
            },
        }
    }

    /// Synthesize the type of a surface term, and elaborate it into the core syntax.
    pub fn synth_type(&mut self, file_id: usize, surface_term: &Term) -> (core::Term, Arc<Value>) {
        let range = surface_term.range();
        let error_term = || core::Term::new(surface_term.range(), core::TermData::Error);

        match &surface_term.data {
            TermData::Name(name) => {
                if let Some((r#type, _)) = self.globals.get(name) {
                    let core_term = core::Term::new(range, core::TermData::Global(name.to_owned()));
                    return (core_term, self.eval(r#type));
                }
                if let Some(r#type) = self.lookup_type(name) {
                    let core_term = core::Term::new(range, core::TermData::Item(name.to_owned()));
                    return (core_term, r#type.clone());
                }

                self.push_message(SurfaceToCoreMessage::VarNameNotFound {
                    file_id,
                    name: name.clone(),
                    name_range: surface_term.range(),
                });
                (error_term(), Arc::new(Value::Error))
            }

            TermData::Ann(surface_term, surface_type) => {
                let (core_type, _) = self.is_type(file_id, surface_type);
                match &core_type.data {
                    core::TermData::Error => (error_term(), Arc::new(Value::Error)),
                    _ => {
                        let r#type = self.eval(&core_type);
                        let term_data = core::TermData::Ann(
                            Arc::new(self.check_type(file_id, surface_term, &r#type)),
                            Arc::new(core_type),
                        );

                        (core::Term::new(surface_term.range(), term_data), r#type)
                    }
                }
            }

            TermData::KindType => {
                self.push_message(SurfaceToCoreMessage::TermHasNoType {
                    file_id,
                    term_range: surface_term.range(),
                });
                (error_term(), Arc::new(Value::Error))
            }
            TermData::TypeType => (
                core::Term::new(range, core::TermData::Sort(Sort::Type)),
                Arc::new(Value::Sort(Sort::Kind)),
            ),

            TermData::FunctionType(param_type, body_type) => {
                let (core_param_type, param_sort) = self.is_type(file_id, param_type);
                let (core_body_type, body_sort) = self.is_type(file_id, body_type);

                match (param_sort, body_sort) {
                    (Some(param_sort), Some(body_sort)) => {
                        let term_data = core::TermData::FunctionType(
                            Arc::new(core_param_type),
                            Arc::new(core_body_type),
                        );
                        (
                            core::Term::new(range, term_data),
                            Arc::new(Value::Sort(core::typing::rule(param_sort, body_sort))),
                        )
                    }
                    (_, _) => (error_term(), Arc::new(Value::Error)),
                }
            }
            TermData::FunctionElim(head, arguments) => {
                let (mut core_head, mut head_type) = self.synth_type(file_id, head);

                for argument in arguments {
                    match head_type.as_ref() {
                        Value::FunctionType(param_type, body_type) => {
                            let term_data = core::TermData::FunctionElim(
                                Arc::new(core_head),
                                Arc::new(self.check_type(file_id, argument, &param_type)),
                            );
                            core_head = core::Term::new(range.clone(), term_data);
                            head_type = body_type.clone();
                        }
                        Value::Error => return (error_term(), Arc::new(Value::Error)),
                        head_type => {
                            let head_type = self.read_back_to_surface(head_type);
                            self.push_message(SurfaceToCoreMessage::NotAFunction {
                                file_id,
                                head_range: head.range(),
                                head_type,
                                argument_range: argument.range(),
                            });
                            return (error_term(), Arc::new(Value::Error));
                        }
                    }
                }

                (core_head, head_type)
            }

            TermData::NumberLiteral(_) => {
                self.push_message(SurfaceToCoreMessage::AmbiguousNumericLiteral {
                    file_id,
                    literal_range: surface_term.range(),
                });

                (error_term(), Arc::new(Value::Error))
            }
            TermData::If(surface_head, surface_if_true, surface_if_false) => {
                // TODO: Lookup globals in environment
                let bool_type = Arc::new(Value::global("Bool"));
                let head = self.check_type(file_id, surface_head, &bool_type);
                let (if_true, if_true_type) = self.synth_type(file_id, surface_if_true);
                let (if_false, if_false_type) = self.synth_type(file_id, surface_if_false);

                if self.is_equal(&if_true_type, &if_false_type) {
                    let term_data = core::TermData::BoolElim(
                        Arc::new(head),
                        Arc::new(if_true),
                        Arc::new(if_false),
                    );
                    (core::Term::new(range, term_data), if_true_type)
                } else {
                    let expected_type = self.read_back_to_surface(&if_true_type);
                    let found_type = self.read_back_to_surface(&if_false_type);
                    self.push_message(SurfaceToCoreMessage::TypeMismatch {
                        file_id,
                        term_range: surface_if_false.range(),
                        expected_type,
                        found_type,
                    });
                    (error_term(), Arc::new(Value::Error))
                }
            }
            TermData::Match(_, _) => {
                self.push_message(SurfaceToCoreMessage::AmbiguousMatchExpression {
                    file_id,
                    term_range: surface_term.range(),
                });
                (error_term(), Arc::new(Value::Error))
            }

            TermData::FormatType => (
                core::Term::new(range, core::TermData::FormatType),
                Arc::new(Value::Sort(Sort::Kind)),
            ),

            TermData::Repr => (
                core::Term::new(range, core::TermData::Repr),
                Arc::new(Value::FunctionType(
                    Arc::new(Value::FormatType),
                    Arc::new(Value::Sort(Sort::Type)),
                )),
            ),

            TermData::Error => (error_term(), Arc::new(Value::Error)),
        }
    }

    fn from_int_branches(
        &mut self,
        file_id: usize,
        range: Range<usize>,
        surface_branches: &[(Pattern, Term)],
        expected_type: &Arc<Value>,
    ) -> (BTreeMap<BigInt, Arc<core::Term>>, Arc<core::Term>) {
        use std::collections::btree_map::Entry;

        let mut branches = BTreeMap::new();
        let mut default = None;

        for (pattern, surface_term) in surface_branches {
            let unreachable_pattern = || SurfaceToCoreMessage::UnreachablePattern {
                file_id,
                pattern_range: pattern.range(),
            };

            match &pattern.data {
                PatternData::NumberLiteral(source) => {
                    let core_term = self.check_type(file_id, surface_term, expected_type);
                    let parse_state =
                        literal::State::new(file_id, range.clone(), source, &mut self.messages);
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
                    let core_term = self.check_type(file_id, surface_term, expected_type);
                    match &default {
                        None => default = Some(Arc::new(core_term)),
                        Some(_) => self.push_message(unreachable_pattern()),
                    }
                }
            }
        }

        let default = default.unwrap_or_else(|| {
            self.push_message(SurfaceToCoreMessage::NoDefaultPattern {
                file_id,
                match_range: range.clone(),
            });
            Arc::new(core::Term::new(range, core::TermData::Error))
        });

        (branches, default)
    }
}
