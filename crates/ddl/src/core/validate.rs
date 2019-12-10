//! Type-checking of the core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use std::collections::HashMap;

use crate::core::{semantics, Constant, Head, Item, Module, Term, TypeField, Universe, Value};
use crate::diagnostics;

/// Validate a module.
pub fn validate_module(module: &Module, report: &mut dyn FnMut(Diagnostic)) {
    validate_items(Context::new(module.file_id), &module.items, report);
}

/// Contextual information to be used during validation.
pub struct Context {
    /// The file where these items are defined (for error reporting).
    file_id: FileId,
    /// Labels that have previously been used for items, along with the span
    /// where they were introduced (for error reporting).
    items: HashMap<String, (Span, Value)>,
}

impl Context {
    /// Create a new context.
    pub fn new(file_id: FileId) -> Context {
        Context {
            file_id,
            items: HashMap::new(),
        }
    }
}

/// Validate items.
pub fn validate_items(mut context: Context, items: &[Item], report: &mut dyn FnMut(Diagnostic)) {
    for item in items {
        use std::collections::hash_map::Entry;

        match item {
            Item::Alias(alias) => {
                let ty = synth_term(&context, &alias.term, report);

                // FIXME: Avoid shadowing builtin definitions
                match context.items.entry(alias.name.clone()) {
                    Entry::Vacant(entry) => {
                        entry.insert((alias.span, ty));
                    }
                    Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                        Severity::Bug,
                        context.file_id,
                        &alias.name,
                        alias.span,
                        entry.get().0,
                    )),
                }
            }
            Item::Struct(struct_ty) => {
                validate_struct_ty_fields(&context, &struct_ty.fields, report);

                // FIXME: Avoid shadowing builtin definitions
                match context.items.entry(struct_ty.name.clone()) {
                    Entry::Vacant(entry) => {
                        entry.insert((struct_ty.span, Value::Universe(Universe::Format)));
                    }
                    Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                        Severity::Bug,
                        context.file_id,
                        &struct_ty.name,
                        struct_ty.span,
                        entry.get().0,
                    )),
                }
            }
        }
    }
}

/// Validate structure type fields.
pub fn validate_struct_ty_fields(
    context: &Context,
    fields: &[TypeField],
    report: &mut dyn FnMut(Diagnostic),
) {
    // Field names that have previously seen, along with the span
    // where they were introduced (for error reporting).
    let mut seen_field_names = HashMap::new();

    for field in fields {
        use std::collections::hash_map::Entry;

        let format_ty = Value::Universe(Universe::Format);
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

/// Validate that a term is a type or kind.
pub fn validate_universe(context: &Context, term: &Term, report: &mut dyn FnMut(Diagnostic)) {
    match term {
        Term::Universe(_, _) => {}
        term => match synth_term(context, term, report) {
            Value::Universe(_) | Value::Error => {}
            ty => report(diagnostics::universe_mismatch(
                Severity::Bug,
                context.file_id,
                term.span(),
                &ty,
            )),
        },
    }
}

/// Check a surface term against the given type.
pub fn check_term(
    context: &Context,
    term: &Term,
    expected_ty: &Value,
    report: &mut dyn FnMut(Diagnostic),
) {
    match (term, expected_ty) {
        (Term::Error(_), _) | (_, Value::Error) => {}
        (Term::BoolElim(_, term, if_true, if_false), expected_ty) => {
            let bool_ty = Value::Neutral(Head::Item("Bool".to_owned()), Vec::new());
            check_term(context, term, &bool_ty, report);
            check_term(context, if_true, expected_ty, report);
            check_term(context, if_false, expected_ty, report);
        }
        (Term::IntElim(_, head, branches, default), expected_ty) => {
            let int_ty = Value::Neutral(Head::Item("Int".to_owned()), Vec::new());
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
pub fn synth_term(context: &Context, term: &Term, report: &mut dyn FnMut(Diagnostic)) -> Value {
    match term {
        Term::Item(span, name) => match context.items.get(name) {
            Some((_, ty)) => ty.clone(),
            None => match name.as_str() {
                "U8" | "U16Le" | "U16Be" | "U32Le" | "U32Be" | "U64Le" | "U64Be" | "S8"
                | "S16Le" | "S16Be" | "S32Le" | "S32Be" | "S64Le" | "S64Be" | "F32Le" | "F32Be"
                | "F64Le" | "F64Be" => Value::Universe(Universe::Format),
                "Bool" | "Int" | "F32" | "F64" => Value::Universe(Universe::Type),
                "true" | "false" => Value::Neutral(Head::Item("Bool".to_owned()), Vec::new()),
                _ => {
                    report(diagnostics::bug::item_name_not_found(
                        context.file_id,
                        &name,
                        *span,
                    ));
                    Value::Error
                }
            },
        },
        Term::Ann(term, ty) => {
            validate_universe(context, ty, report);
            let ty = semantics::eval(ty);
            check_term(context, term, &ty, report);
            ty
        }
        Term::Universe(span, universe) => match universe {
            Universe::Type | Universe::Format => Value::Universe(Universe::Kind),
            Universe::Kind => {
                report(diagnostics::kind_has_no_type(
                    Severity::Bug,
                    context.file_id,
                    *span,
                ));
                Value::Error
            }
        },
        Term::Constant(_, Constant::Int(_)) => {
            Value::Neutral(Head::Item("Int".to_owned()), Vec::new())
        }
        Term::Constant(_, Constant::F32(_)) => {
            Value::Neutral(Head::Item("F32".to_owned()), Vec::new())
        }
        Term::Constant(_, Constant::F64(_)) => {
            Value::Neutral(Head::Item("F64".to_owned()), Vec::new())
        }
        Term::BoolElim(_, head, if_true, if_false) => {
            let bool_ty = Value::Neutral(Head::Item("Bool".to_owned()), Vec::new());
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
                Value::Error
            }
        }
        Term::IntElim(span, _, _, _) => {
            report(diagnostics::ambiguous_match_expression(
                Severity::Bug,
                context.file_id,
                *span,
            ));
            Value::Error
        }
        Term::Error(_) => Value::Error,
    }
}
