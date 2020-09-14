//! Type-checking of Fathom's core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use codespan_reporting::diagnostic::{Diagnostic, Severity};
use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use crate::diagnostics;
use crate::lang::core::semantics::{self, Value};
use crate::lang::core::{Constant, Globals, Item, ItemData, Module, StructType, Term, TermData};

/// Validate that a module is well-formed.
pub fn is_module(globals: &Globals, module: &Module, report: &mut dyn FnMut(Diagnostic<usize>)) {
    Context::new(globals, module.file_id).is_items(&module.items, report);
}

/// Contextual information to be used during validation.
pub struct Context<'me> {
    /// The global environment.
    globals: &'me Globals,
    /// The file where these items are defined (for diagnostic reporting).
    file_id: usize,
    /// Labels that have previously been used for items.
    items: HashMap<&'me str, Item>,
    /// List of types currently bound in this context. These could either
    /// refer to items or local bindings.
    types: Vec<(&'me str, Arc<Value>)>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me Globals, file_id: usize) -> Context<'me> {
        Context {
            globals,
            file_id,
            items: HashMap::new(),
            types: Vec::new(),
        }
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

    /// Check that one [`Value`] is [computationally equal]
    /// to another [`Value`] in the current typing context.
    ///
    /// [`Value`]: crate::lang::core::semantics::Value
    /// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
    pub fn is_equal(&self, value0: &Value, value1: &Value) -> bool {
        semantics::is_equal(self.globals, &self.items, value0, value1)
    }

    /// Validate that the items are well-formed.
    pub fn is_items(mut self, items: &'me [Item], report: &mut dyn FnMut(Diagnostic<usize>)) {
        for item in items {
            use std::collections::hash_map::Entry;

            match &item.data {
                ItemData::Alias(alias) => {
                    let r#type = self.synth_type(&alias.term, report);

                    // FIXME: Avoid shadowing builtin definitions
                    match self.items.entry(&alias.name) {
                        Entry::Vacant(entry) => {
                            self.types.push((*entry.key(), r#type));
                            entry.insert(item.clone());
                        }
                        Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                            Severity::Bug,
                            self.file_id,
                            &alias.name,
                            item.range(),
                            entry.get().range(),
                        )),
                    }
                }
                ItemData::Struct(struct_type) => {
                    self.is_struct_type(item.range(), &struct_type, report);

                    // FIXME: Avoid shadowing builtin definitions
                    match self.items.entry(&struct_type.name) {
                        Entry::Vacant(entry) => {
                            self.types.push((*entry.key(), Arc::new(Value::FormatType)));
                            entry.insert(item.clone());
                        }
                        Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                            Severity::Bug,
                            self.file_id,
                            &struct_type.name,
                            item.range(),
                            entry.get().range(),
                        )),
                    }
                }
            }
        }
    }

    /// Validate that the structure type is well-formed.
    pub fn is_struct_type(
        &self,
        range: Range<usize>,
        struct_type: &StructType,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) {
        use std::collections::HashSet;

        // Field names that have previously seen.
        let mut seen_field_names = HashSet::new();

        for field in &struct_type.fields {
            let format_type = Arc::new(Value::FormatType);
            self.check_type(&field.term, &format_type, report);

            if !seen_field_names.insert(field.name.clone()) {
                report(diagnostics::bug::field_redeclaration(
                    self.file_id,
                    &field.name,
                    range.clone(),
                ));
            }
        }
    }

    /// Validate that that a term is a well-formed type.
    pub fn is_type(&self, term: &Term, report: &mut dyn FnMut(Diagnostic<usize>)) -> bool {
        match &term.data {
            TermData::FormatType | TermData::TypeType => true,
            _ => {
                let found_type = self.synth_type(term, report);
                match found_type.as_ref() {
                    Value::FormatType | Value::TypeType => true,
                    Value::Error => false,
                    _ => {
                        report(diagnostics::universe_mismatch(
                            Severity::Bug,
                            self.file_id,
                            term.range(),
                            &found_type,
                        ));
                        false
                    }
                }
            }
        }
    }

    /// Validate that a term is an element of the given type.
    pub fn check_type(
        &self,
        term: &Term,
        expected_type: &Arc<Value>,
        report: &mut dyn FnMut(Diagnostic<usize>),
    ) {
        match (&term.data, expected_type.as_ref()) {
            (TermData::Error, _) | (_, Value::Error) => {}
            (TermData::BoolElim(term, if_true, if_false), _) => {
                let bool_type = Arc::new(Value::global("Bool"));
                self.check_type(term, &bool_type, report);
                self.check_type(if_true, expected_type, report);
                self.check_type(if_false, expected_type, report);
            }
            (TermData::IntElim(head, branches, default), _) => {
                let int_type = Arc::new(Value::global("Int"));
                self.check_type(head, &int_type, report);
                for term in branches.values() {
                    self.check_type(term, expected_type, report);
                }
                self.check_type(default, expected_type, report);
            }
            (_, expected_type) => match self.synth_type(term, report) {
                found_type if self.is_equal(&found_type, expected_type) => {}
                found_type => report(diagnostics::type_mismatch(
                    Severity::Bug,
                    self.file_id,
                    term.range(),
                    expected_type,
                    &found_type,
                )),
            },
        }
    }

    /// Synthesize the type of a term.
    pub fn synth_type(&self, term: &Term, report: &mut dyn FnMut(Diagnostic<usize>)) -> Arc<Value> {
        match &term.data {
            TermData::Global(name) => match self.globals.get(name) {
                Some((r#type, _)) => self.eval(r#type),
                None => {
                    report(diagnostics::bug::global_name_not_found(
                        self.file_id,
                        &name,
                        term.range(),
                    ));
                    Arc::new(Value::Error)
                }
            },
            TermData::Item(name) => match self.lookup_type(name) {
                Some(r#type) => r#type.clone(),
                None => {
                    report(diagnostics::bug::item_name_not_found(
                        self.file_id,
                        &name,
                        term.range(),
                    ));
                    Arc::new(Value::Error)
                }
            },
            TermData::Ann(term, r#type) => {
                self.is_type(r#type, report);
                let r#type = self.eval(r#type);
                self.check_type(term, &r#type, report);
                r#type
            }
            TermData::FormatType | TermData::TypeType => {
                report(diagnostics::term_has_no_type(
                    Severity::Bug,
                    self.file_id,
                    term.range(),
                ));
                Arc::new(Value::Error)
            }
            TermData::FunctionType(param_type, body_type) => Arc::new(
                match (
                    self.is_type(param_type, report),
                    self.is_type(body_type, report),
                ) {
                    (true, true) => Value::TypeType,
                    (_, _) => Value::Error,
                },
            ),
            TermData::FunctionElim(head, argument) => {
                match self.synth_type(head, report).as_ref() {
                    Value::FunctionType(param_type, body_type) => {
                        self.check_type(argument, &param_type, report);
                        (*body_type).clone() // FIXME: Clone
                    }
                    Value::Error => Arc::new(Value::Error),
                    head_type => {
                        report(diagnostics::not_a_function(
                            Severity::Bug,
                            self.file_id,
                            head.range(),
                            head_type,
                            argument.range(),
                        ));
                        Arc::new(Value::Error)
                    }
                }
            }
            TermData::Constant(constant) => match constant {
                // TODO: Lookup globals in environment
                Constant::Int(_) => Arc::new(Value::global("Int")),
                Constant::F32(_) => Arc::new(Value::global("F32")),
                Constant::F64(_) => Arc::new(Value::global("F64")),
            },
            TermData::BoolElim(head, if_true, if_false) => {
                // TODO: Lookup globals in environment
                let bool_type = Arc::new(Value::global("Bool"));
                self.check_type(head, &bool_type, report);
                let if_true_type = self.synth_type(if_true, report);
                let if_false_type = self.synth_type(if_false, report);

                if self.is_equal(&if_true_type, &if_false_type) {
                    if_true_type
                } else {
                    report(diagnostics::type_mismatch(
                        Severity::Bug,
                        self.file_id,
                        if_false.range(),
                        &if_true_type,
                        &if_false_type,
                    ));
                    Arc::new(Value::Error)
                }
            }
            TermData::IntElim(_, _, _) => {
                report(diagnostics::ambiguous_match_expression(
                    Severity::Bug,
                    self.file_id,
                    term.range(),
                ));
                Arc::new(Value::Error)
            }
            TermData::Error => Arc::new(Value::Error),
        }
    }
}
