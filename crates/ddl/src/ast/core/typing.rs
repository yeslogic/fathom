//! Type-checking of the core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use codespan_reporting::diagnostic::{Diagnostic, Severity};
use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::core::{semantics, Constant, Globals, Item, Module, Term, TypeField, Value};
use crate::diagnostics;

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
    tys: Vec<(&'me str, Arc<Value>)>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me Globals, file_id: usize) -> Context<'me> {
        Context {
            globals,
            file_id,
            items: HashMap::new(),
            tys: Vec::new(),
        }
    }

    /// Lookup the type of a binding corresponding to `name` in the context,
    /// returning `None` if `name` was not yet bound.
    pub fn lookup_ty(&self, name: &str) -> Option<&Arc<Value>> {
        Some(&self.tys.iter().rev().find(|(n, _)| *n == name)?.1)
    }
}

/// Validate that a module is well-formed.
pub fn wf_module(globals: &Globals, module: &Module, report: &mut dyn FnMut(Diagnostic<usize>)) {
    wf_items(Context::new(globals, module.file_id), &module.items, report);
}

/// Validate that the items are well-formed.
pub fn wf_items<'items>(
    mut context: Context<'items>,
    items: &'items [Item],
    report: &mut dyn FnMut(Diagnostic<usize>),
) {
    for item in items {
        use std::collections::hash_map::Entry;

        match item {
            Item::Alias(alias) => {
                let ty = synth_ty(&context, &alias.term, report);

                // FIXME: Avoid shadowing builtin definitions
                match context.items.entry(&alias.name) {
                    Entry::Vacant(entry) => {
                        context.tys.push((*entry.key(), ty));
                        entry.insert(item.clone());
                    }
                    Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                        Severity::Bug,
                        context.file_id,
                        &alias.name,
                        alias.range.clone(),
                        entry.get().range(),
                    )),
                }
            }
            Item::Struct(struct_ty) => {
                wf_struct_ty_fields(&context, &struct_ty.fields, report);

                // FIXME: Avoid shadowing builtin definitions
                match context.items.entry(&struct_ty.name) {
                    Entry::Vacant(entry) => {
                        let ty = Arc::new(Value::FormatType(0..0));
                        context.tys.push((*entry.key(), ty));
                        entry.insert(item.clone());
                    }
                    Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                        Severity::Bug,
                        context.file_id,
                        &struct_ty.name,
                        struct_ty.range.clone(),
                        entry.get().range(),
                    )),
                }
            }
        }
    }
}

/// Validate that the structure type fields are well-formed.
pub fn wf_struct_ty_fields(
    context: &Context<'_>,
    fields: &[TypeField],
    report: &mut dyn FnMut(Diagnostic<usize>),
) {
    // Field names that have previously seen, along with the source range
    // where they were introduced (for diagnostic reporting).
    let mut seen_field_names = HashMap::new();

    for field in fields {
        use std::collections::hash_map::Entry;

        let format_ty = Arc::new(Value::FormatType(0..0));
        check_ty(&context, &field.term, &format_ty, report);

        match seen_field_names.entry(field.name.clone()) {
            Entry::Vacant(entry) => {
                entry.insert(field.range());
            }
            Entry::Occupied(entry) => report(diagnostics::field_redeclaration(
                Severity::Bug,
                context.file_id,
                &field.name,
                field.range(),
                entry.get().clone(),
            )),
        }
    }
}

/// Validate that that a term is a well-formed type.
pub fn wf_ty(
    context: &Context<'_>,
    term: &Term,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> bool {
    match term {
        Term::FormatType(_) | Term::TypeType(_) => true,
        term => {
            let ty = synth_ty(context, term, report);
            match ty.as_ref() {
                Value::FormatType(_) | Value::TypeType(_) => true,
                Value::Error(_) => false,
                _ => {
                    report(diagnostics::universe_mismatch(
                        Severity::Bug,
                        context.file_id,
                        term.range(),
                        &ty,
                    ));
                    false
                }
            }
        }
    }
}

/// Validate that a term is an element of the given type.
pub fn check_ty(
    context: &Context<'_>,
    term: &Term,
    expected_ty: &Arc<Value>,
    report: &mut dyn FnMut(Diagnostic<usize>),
) {
    match (term, expected_ty.as_ref()) {
        (Term::Error(_), _) | (_, Value::Error(_)) => {}
        (Term::BoolElim(_, term, if_true, if_false), _) => {
            let bool_ty = Arc::new(Value::global(0..0, "Bool"));
            check_ty(context, term, &bool_ty, report);
            check_ty(context, if_true, expected_ty, report);
            check_ty(context, if_false, expected_ty, report);
        }
        (Term::IntElim(_, head, branches, default), _) => {
            let int_ty = Arc::new(Value::global(0..0, "Int"));
            check_ty(context, head, &int_ty, report);
            for term in branches.values() {
                check_ty(context, term, expected_ty, report);
            }
            check_ty(context, default, expected_ty, report);
        }
        (term, expected_ty) => {
            let synth_ty = synth_ty(context, term, report);

            if !semantics::equal(&synth_ty, expected_ty) {
                report(diagnostics::type_mismatch(
                    Severity::Bug,
                    context.file_id,
                    term.range(),
                    expected_ty,
                    &synth_ty,
                ));
            }
        }
    }
}

/// Synthesize the type of a term.
pub fn synth_ty(
    context: &Context<'_>,
    term: &Term,
    report: &mut dyn FnMut(Diagnostic<usize>),
) -> Arc<Value> {
    match term {
        Term::Global(range, name) => match context.globals.get(name) {
            Some((r#type, _)) => semantics::eval(context.globals, &context.items, r#type),
            None => {
                report(diagnostics::bug::global_name_not_found(
                    context.file_id,
                    &name,
                    range.clone(),
                ));
                Arc::new(Value::Error(0..0))
            }
        },
        Term::Item(range, name) => match context.lookup_ty(name) {
            Some(ty) => ty.clone(),
            None => {
                report(diagnostics::bug::item_name_not_found(
                    context.file_id,
                    &name,
                    range.clone(),
                ));
                Arc::new(Value::Error(0..0))
            }
        },
        Term::Ann(term, ty) => {
            wf_ty(context, ty, report);
            let ty = semantics::eval(context.globals, &context.items, ty);
            check_ty(context, term, &ty, report);
            ty
        }
        Term::FormatType(range) | Term::TypeType(range) => {
            report(diagnostics::term_has_no_type(
                Severity::Bug,
                context.file_id,
                range.clone(),
            ));
            Arc::new(Value::Error(0..0))
        }
        Term::FunctionType(param_type, body_type) => Arc::new(
            match (
                wf_ty(context, param_type, report),
                wf_ty(context, body_type, report),
            ) {
                (true, true) => Value::TypeType(0..0),
                (_, _) => Value::Error(0..0),
            },
        ),
        Term::FunctionElim(head, argument) => {
            match synth_ty(context, head, report).as_ref() {
                Value::FunctionType(param_type, body_type) => {
                    check_ty(context, argument, &param_type, report);
                    (*body_type).clone() // FIXME: Clone
                }
                Value::Error(_) => Arc::new(Value::Error(0..0)),
                head_ty => {
                    report(diagnostics::not_a_function(
                        Severity::Bug,
                        context.file_id,
                        head.range(),
                        head_ty,
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
            let bool_ty = Arc::new(Value::global(0..0, "Bool"));
            check_ty(context, head, &bool_ty, report);
            let if_true_ty = synth_ty(context, if_true, report);
            let if_false_ty = synth_ty(context, if_false, report);

            if semantics::equal(&if_true_ty, &if_false_ty) {
                if_true_ty
            } else {
                report(diagnostics::type_mismatch(
                    Severity::Bug,
                    context.file_id,
                    if_false.range(),
                    &if_true_ty,
                    &if_false_ty,
                ));
                Arc::new(Value::Error(0..0))
            }
        }
        Term::IntElim(range, _, _, _) => {
            report(diagnostics::ambiguous_match_expression(
                Severity::Bug,
                context.file_id,
                range.clone(),
            ));
            Arc::new(Value::Error(0..0))
        }
        Term::Error(_) => Arc::new(Value::Error(0..0)),
    }
}
