//! Type-checking of Fathom's core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use codespan_reporting::diagnostic::{Diagnostic, Severity};
use std::collections::HashMap;
use std::sync::Arc;

use crate::diagnostics;
use crate::lang::core::semantics::{self, Value};
use crate::lang::core::{Constant, Globals, Item, Module, Term, TypeField};

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
    /// Labels that have previously been used for items, along with the source range
    /// where they were introduced (for diagnostic reporting).
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

    /// Validate that the items are well-formed.
    pub fn is_items(mut self, items: &'me [Item], report: &mut dyn FnMut(Diagnostic<usize>)) {
        for item in items {
            use std::collections::hash_map::Entry;

            match item {
                Item::Alias(alias) => {
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
                            alias.range.clone(),
                            entry.get().range(),
                        )),
                    }
                }
                Item::Struct(struct_type) => {
                    self.is_fields(&struct_type.fields, report);

                    // FIXME: Avoid shadowing builtin definitions
                    match self.items.entry(&struct_type.name) {
                        Entry::Vacant(entry) => {
                            let r#type = Arc::new(Value::FormatType(0..0));
                            self.types.push((*entry.key(), r#type));
                            entry.insert(item.clone());
                        }
                        Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                            Severity::Bug,
                            self.file_id,
                            &struct_type.name,
                            struct_type.range.clone(),
                            entry.get().range(),
                        )),
                    }
                }
            }
        }
    }

    /// Validate that the structure type fields are well-formed.
    pub fn is_fields(&self, fields: &[TypeField], report: &mut dyn FnMut(Diagnostic<usize>)) {
        // Field names that have previously seen, along with the source range
        // where they were introduced (for diagnostic reporting).
        let mut seen_field_names = HashMap::new();

        for field in fields {
            use std::collections::hash_map::Entry;

            let format_type = Arc::new(Value::FormatType(0..0));
            self.check_type(&field.term, &format_type, report);

            match seen_field_names.entry(field.name.clone()) {
                Entry::Vacant(entry) => {
                    entry.insert(field.range());
                }
                Entry::Occupied(entry) => report(diagnostics::field_redeclaration(
                    Severity::Bug,
                    self.file_id,
                    &field.name,
                    field.range(),
                    entry.get().clone(),
                )),
            }
        }
    }

    /// Validate that that a term is a well-formed type.
    pub fn is_type(&self, term: &Term, report: &mut dyn FnMut(Diagnostic<usize>)) -> bool {
        match term {
            Term::FormatType(_) | Term::TypeType(_) => true,
            term => {
                let r#type = self.synth_type(term, report);
                match r#type.as_ref() {
                    Value::FormatType(_) | Value::TypeType(_) => true,
                    Value::Error(_) => false,
                    _ => {
                        report(diagnostics::universe_mismatch(
                            Severity::Bug,
                            self.file_id,
                            term.range(),
                            &r#type,
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
        match (term, expected_type.as_ref()) {
            (Term::Error(_), _) | (_, Value::Error(_)) => {}
            (Term::BoolElim(_, term, if_true, if_false), _) => {
                let bool_type = Arc::new(Value::global(0..0, "Bool"));
                self.check_type(term, &bool_type, report);
                self.check_type(if_true, expected_type, report);
                self.check_type(if_false, expected_type, report);
            }
            (Term::IntElim(_, head, branches, default), _) => {
                let int_type = Arc::new(Value::global(0..0, "Int"));
                self.check_type(head, &int_type, report);
                for term in branches.values() {
                    self.check_type(term, expected_type, report);
                }
                self.check_type(default, expected_type, report);
            }
            (term, expected_type) => {
                let synth_type = self.synth_type(term, report);

                if !semantics::equal(&synth_type, expected_type) {
                    report(diagnostics::type_mismatch(
                        Severity::Bug,
                        self.file_id,
                        term.range(),
                        expected_type,
                        &synth_type,
                    ));
                }
            }
        }
    }

    /// Synthesize the type of a term.
    pub fn synth_type(&self, term: &Term, report: &mut dyn FnMut(Diagnostic<usize>)) -> Arc<Value> {
        match term {
            Term::Global(range, name) => match self.globals.get(name) {
                Some((r#type, _)) => semantics::eval(self.globals, &self.items, r#type),
                None => {
                    report(diagnostics::bug::global_name_not_found(
                        self.file_id,
                        &name,
                        range.clone(),
                    ));
                    Arc::new(Value::Error(0..0))
                }
            },
            Term::Item(range, name) => match self.lookup_type(name) {
                Some(r#type) => r#type.clone(),
                None => {
                    report(diagnostics::bug::item_name_not_found(
                        self.file_id,
                        &name,
                        range.clone(),
                    ));
                    Arc::new(Value::Error(0..0))
                }
            },
            Term::Ann(term, r#type) => {
                self.is_type(r#type, report);
                let r#type = semantics::eval(self.globals, &self.items, r#type);
                self.check_type(term, &r#type, report);
                r#type
            }
            Term::FormatType(range) | Term::TypeType(range) => {
                report(diagnostics::term_has_no_type(
                    Severity::Bug,
                    self.file_id,
                    range.clone(),
                ));
                Arc::new(Value::Error(0..0))
            }
            Term::FunctionType(param_type, body_type) => Arc::new(
                match (
                    self.is_type(param_type, report),
                    self.is_type(body_type, report),
                ) {
                    (true, true) => Value::TypeType(0..0),
                    (_, _) => Value::Error(0..0),
                },
            ),
            Term::FunctionElim(head, argument) => {
                match self.synth_type(head, report).as_ref() {
                    Value::FunctionType(param_type, body_type) => {
                        self.check_type(argument, &param_type, report);
                        (*body_type).clone() // FIXME: Clone
                    }
                    Value::Error(_) => Arc::new(Value::Error(0..0)),
                    head_type => {
                        report(diagnostics::not_a_function(
                            Severity::Bug,
                            self.file_id,
                            head.range(),
                            head_type,
                            argument.range(),
                        ));
                        Arc::new(Value::Error(0..0))
                    }
                }
            }
            Term::Constant(_, constant) => match constant {
                // TODO: Lookup globals in environment
                Constant::Int(_) => Arc::new(Value::global(0..0, "Int")),
                Constant::F32(_) => Arc::new(Value::global(0..0, "F32")),
                Constant::F64(_) => Arc::new(Value::global(0..0, "F64")),
            },
            Term::BoolElim(_, head, if_true, if_false) => {
                // TODO: Lookup globals in environment
                let bool_type = Arc::new(Value::global(0..0, "Bool"));
                self.check_type(head, &bool_type, report);
                let if_true_type = self.synth_type(if_true, report);
                let if_false_type = self.synth_type(if_false, report);

                if semantics::equal(&if_true_type, &if_false_type) {
                    if_true_type
                } else {
                    report(diagnostics::type_mismatch(
                        Severity::Bug,
                        self.file_id,
                        if_false.range(),
                        &if_true_type,
                        &if_false_type,
                    ));
                    Arc::new(Value::Error(0..0))
                }
            }
            Term::IntElim(range, _, _, _) => {
                report(diagnostics::ambiguous_match_expression(
                    Severity::Bug,
                    self.file_id,
                    range.clone(),
                ));
                Arc::new(Value::Error(0..0))
            }
            Term::Error(_) => Arc::new(Value::Error(0..0)),
        }
    }
}
