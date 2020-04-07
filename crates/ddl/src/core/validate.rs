//! Type-checking of the core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use std::collections::HashMap;
use std::sync::Arc;

use crate::core::{semantics, Constant, Globals, Item, Module, Term, TypeField, Universe, Value};
use crate::diagnostics;

/// Validate a module.
pub fn validate_module(
    globals: &Globals,
    module: &Module,
    report: &mut dyn FnMut(Diagnostic<FileId>),
) {
    validate_items(Context::new(globals, module.file_id), &module.items, report);
}

/// Contextual information to be used during validation.
pub struct Context<'me> {
    /// The global environment.
    globals: &'me Globals,
    /// The file where these items are defined (for error reporting).
    file_id: FileId,
    /// Labels that have previously been used for items, along with the span
    /// where they were introduced (for error reporting).
    items: HashMap<&'me str, Item>,
    /// List of types currently bound in this context. These could either
    /// refer to items or local bindings.
    tys: Vec<(&'me str, Arc<Value>)>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me Globals, file_id: FileId) -> Context<'me> {
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

/// Validate items.
pub fn validate_items<'items>(
    mut context: Context<'items>,
    items: &'items [Item],
    report: &mut dyn FnMut(Diagnostic<FileId>),
) {
    for item in items {
        use std::collections::hash_map::Entry;

        match item {
            Item::Alias(alias) => {
                let ty = synth_term(&context, &alias.term, report);

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
                        alias.span,
                        entry.get().span(),
                    )),
                }
            }
            Item::Struct(struct_ty) => {
                validate_struct_ty_fields(&context, &struct_ty.fields, report);

                // FIXME: Avoid shadowing builtin definitions
                match context.items.entry(&struct_ty.name) {
                    Entry::Vacant(entry) => {
                        let ty = Arc::new(Value::Universe(Span::initial(), Universe::Format));
                        context.tys.push((*entry.key(), ty));
                        entry.insert(item.clone());
                    }
                    Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                        Severity::Bug,
                        context.file_id,
                        &struct_ty.name,
                        struct_ty.span,
                        entry.get().span(),
                    )),
                }
            }
        }
    }
}

/// Validate structure type fields.
pub fn validate_struct_ty_fields(
    context: &Context<'_>,
    fields: &[TypeField],
    report: &mut dyn FnMut(Diagnostic<FileId>),
) {
    // Field names that have previously seen, along with the span
    // where they were introduced (for error reporting).
    let mut seen_field_names = HashMap::new();

    for field in fields {
        use std::collections::hash_map::Entry;

        let format_ty = Arc::new(Value::Universe(Span::initial(), Universe::Format));
        check_term(&context, &field.term, &format_ty, report);

        match seen_field_names.entry(field.name.clone()) {
            Entry::Vacant(entry) => {
                entry.insert(field.span());
            }
            Entry::Occupied(entry) => report(diagnostics::field_redeclaration(
                Severity::Bug,
                context.file_id,
                &field.name,
                field.span(),
                *entry.get(),
            )),
        }
    }
}

/// Validate that a term is a host type, format type, or kind.
pub fn validate_universe(
    context: &Context<'_>,
    term: &Term,
    report: &mut dyn FnMut(Diagnostic<FileId>),
) -> Option<Universe> {
    match term {
        Term::Universe(_, universe) => Some(*universe),
        term => {
            let ty = synth_term(context, term, report);
            match ty.as_ref() {
                Value::Universe(_, universe) => Some(*universe),
                Value::Error(_) => None,
                _ => {
                    report(diagnostics::universe_mismatch(
                        Severity::Bug,
                        context.file_id,
                        term.span(),
                        &ty,
                    ));
                    None
                }
            }
        }
    }
}

/// Check a surface term against the given type.
pub fn check_term(
    context: &Context<'_>,
    term: &Term,
    expected_ty: &Arc<Value>,
    report: &mut dyn FnMut(Diagnostic<FileId>),
) {
    match (term, expected_ty.as_ref()) {
        (Term::Error(_), _) | (_, Value::Error(_)) => {}
        (Term::BoolElim(_, term, if_true, if_false), _) => {
            let bool_ty = Arc::new(Value::global(Span::initial(), "Bool"));
            check_term(context, term, &bool_ty, report);
            check_term(context, if_true, expected_ty, report);
            check_term(context, if_false, expected_ty, report);
        }
        (Term::IntElim(_, head, branches, default), _) => {
            let int_ty = Arc::new(Value::global(Span::initial(), "Int"));
            check_term(context, head, &int_ty, report);
            for (_, term) in branches {
                check_term(context, term, expected_ty, report);
            }
            check_term(context, default, expected_ty, report);
        }
        (term, expected_ty) => {
            let synth_ty = synth_term(context, term, report);

            if !semantics::equal(&synth_ty, expected_ty) {
                report(diagnostics::type_mismatch(
                    Severity::Bug,
                    context.file_id,
                    term.span(),
                    expected_ty,
                    &synth_ty,
                ));
            }
        }
    }
}

/// Synthesize the type of a surface term.
pub fn synth_term(
    context: &Context<'_>,
    term: &Term,
    report: &mut dyn FnMut(Diagnostic<FileId>),
) -> Arc<Value> {
    match term {
        Term::Global(span, name) => match context.globals.get(name) {
            Some((r#type, _)) => semantics::eval(context.globals, &context.items, r#type),
            None => {
                report(diagnostics::bug::global_name_not_found(
                    context.file_id,
                    &name,
                    *span,
                ));
                Arc::new(Value::Error(Span::initial()))
            }
        },
        Term::Item(span, name) => match context.lookup_ty(name) {
            Some(ty) => ty.clone(),
            None => {
                report(diagnostics::bug::item_name_not_found(
                    context.file_id,
                    &name,
                    *span,
                ));
                Arc::new(Value::Error(Span::initial()))
            }
        },
        Term::Ann(term, ty) => {
            validate_universe(context, ty, report);
            let ty = semantics::eval(context.globals, &context.items, ty);
            check_term(context, term, &ty, report);
            ty
        }
        Term::Universe(span, universe) => match universe {
            Universe::Host | Universe::Format => {
                Arc::new(Value::Universe(Span::initial(), Universe::Kind))
            }
            Universe::Kind => {
                report(diagnostics::kind_has_no_type(
                    Severity::Bug,
                    context.file_id,
                    *span,
                ));
                Arc::new(Value::Error(Span::initial()))
            }
        },
        Term::FunctionType(param_type, body_type) => Arc::new(
            match (
                validate_universe(context, param_type, report),
                validate_universe(context, body_type, report),
            ) {
                (Some(Universe::Host), Some(Universe::Host)) => {
                    Value::Universe(Span::initial(), Universe::Host)
                }
                (Some(Universe::Host), Some(Universe::Kind))
                | (Some(Universe::Kind), Some(Universe::Kind)) => {
                    Value::Universe(Span::initial(), Universe::Kind)
                }
                (_, _) => Value::Error(Span::initial()),
            },
        ),
        Term::FunctionElim(head, argument) => {
            match synth_term(context, head, report).as_ref() {
                Value::FunctionType(param_type, body_type) => {
                    check_term(context, argument, &param_type, report);
                    (*body_type).clone() // FIXME: Clone
                }
                Value::Error(_) => Arc::new(Value::Error(Span::initial())),
                head_ty => {
                    report(diagnostics::not_a_function(
                        Severity::Bug,
                        context.file_id,
                        head.span(),
                        head_ty,
                        argument.span(),
                    ));
                    Arc::new(Value::Error(Span::initial()))
                }
            }
        }
        Term::Constant(_, constant) => match constant {
            // TODO: Lookup globals in environment
            Constant::Int(_) => Arc::new(Value::global(Span::initial(), "Int")),
            Constant::F32(_) => Arc::new(Value::global(Span::initial(), "F32")),
            Constant::F64(_) => Arc::new(Value::global(Span::initial(), "F64")),
        },
        Term::BoolElim(_, head, if_true, if_false) => {
            // TODO: Lookup globals in environment
            let bool_ty = Arc::new(Value::global(Span::initial(), "Bool"));
            check_term(context, head, &bool_ty, report);
            let if_true_ty = synth_term(context, if_true, report);
            let if_false_ty = synth_term(context, if_false, report);

            if semantics::equal(&if_true_ty, &if_false_ty) {
                if_true_ty
            } else {
                report(diagnostics::type_mismatch(
                    Severity::Bug,
                    context.file_id,
                    if_false.span(),
                    &if_true_ty,
                    &if_false_ty,
                ));
                Arc::new(Value::Error(Span::initial()))
            }
        }
        Term::IntElim(span, _, _, _) => {
            report(diagnostics::ambiguous_match_expression(
                Severity::Bug,
                context.file_id,
                *span,
            ));
            Arc::new(Value::Error(Span::initial()))
        }
        Term::Error(_) => Arc::new(Value::Error(Span::initial())),
    }
}
