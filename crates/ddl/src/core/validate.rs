//! Type-checking of the core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use std::collections::HashMap;

use crate::core::{semantics, Constant, Item, Label, Module, Term, TypeField, Universe, Value};
use crate::diagnostics;

/// Validate a module.
pub fn validate_module(module: &Module, report: &mut dyn FnMut(Diagnostic)) {
    validate_items(ItemContext::new(module.file_id), &module.items, report);
}

/// Contextual information to be used when validating items.
pub struct ItemContext {
    /// The file where these items are defined (for error reporting).
    file_id: FileId,
    /// Labels that have previously been used for items, along with the span
    /// where they were introduced (for error reporting).
    items: HashMap<Label, (Span, Value)>,
}

impl ItemContext {
    /// Create a new item context.
    pub fn new(file_id: FileId) -> ItemContext {
        ItemContext {
            file_id,
            items: HashMap::new(),
        }
    }

    /// Create a field context based on this item context.
    pub fn field_context(&self) -> FieldContext<'_> {
        FieldContext::new(self.file_id, &self.items)
    }

    /// Create a term context based on this item context.
    pub fn term_context(&self) -> TermContext<'_> {
        TermContext::new(self.file_id, &self.items)
    }
}

/// Validate items.
pub fn validate_items(
    mut context: ItemContext,
    items: &[Item],
    report: &mut dyn FnMut(Diagnostic),
) {
    for item in items {
        use std::collections::hash_map::Entry;

        match item {
            Item::Alias(alias) => {
                let ty = synth_term(&context.term_context(), &alias.term, report);

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
                validate_struct_ty_fields(context.field_context(), &struct_ty.fields, report);

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

/// Contextual information to be used when validating structure type fields.
pub struct FieldContext<'items> {
    /// The file where these fields are defined (for error reporting).
    file_id: FileId,
    /// Previously validated items.
    items: &'items HashMap<Label, (Span, Value)>,
    /// Labels that have previously been used for fields, along with the span
    /// where they were introduced (for error reporting).
    fields: HashMap<Label, Span>,
}

impl<'items> FieldContext<'items> {
    /// Create a new field context.
    pub fn new(
        file_id: FileId,
        items: &'items HashMap<Label, (Span, Value)>,
    ) -> FieldContext<'items> {
        FieldContext {
            file_id,
            items,
            fields: HashMap::new(),
        }
    }

    /// Create a term context based on this field context.
    pub fn term_context(&self) -> TermContext<'_> {
        TermContext::new(self.file_id, self.items)
    }
}

/// Validate structure type fields.
pub fn validate_struct_ty_fields(
    mut context: FieldContext<'_>,
    fields: &[TypeField],
    report: &mut dyn FnMut(Diagnostic),
) {
    for field in fields {
        use std::collections::hash_map::Entry;

        check_term(
            &context.term_context(),
            &field.term,
            &Value::Universe(Universe::Format),
            report,
        );

        match context.fields.entry(field.name.clone()) {
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

/// Contextual information to be used when validating terms.
pub struct TermContext<'items> {
    /// The file where the term is defined (for error reporting).
    file_id: FileId,
    /// Previously validated items.
    items: &'items HashMap<Label, (Span, Value)>,
}

impl<'items> TermContext<'items> {
    /// Create a new term context.
    pub fn new(
        file_id: FileId,
        items: &'items HashMap<Label, (Span, Value)>,
    ) -> TermContext<'items> {
        TermContext { file_id, items }
    }
}

/// Validate that a term is a type or kind.
pub fn validate_universe(
    context: &TermContext<'_>,
    term: &Term,
    report: &mut dyn FnMut(Diagnostic),
) {
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
    context: &TermContext<'_>,
    term: &Term,
    expected_ty: &Value,
    report: &mut dyn FnMut(Diagnostic),
) {
    match (term, expected_ty) {
        (Term::Error(_), _) | (_, Value::Error) => {}
        (Term::BoolElim(_, term, if_true, if_false), expected_ty) => {
            check_term(context, term, &Value::BoolType, report);
            check_term(context, if_true, expected_ty, report);
            check_term(context, if_false, expected_ty, report);
        }
        (Term::IntElim(_, head, branches, default), expected_ty) => {
            check_term(context, head, &Value::IntType, report);
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
    context: &TermContext<'_>,
    term: &Term,
    report: &mut dyn FnMut(Diagnostic),
) -> Value {
    match term {
        Term::Item(span, label) => match context.items.get(label) {
            Some((_, ty)) => ty.clone(),
            None => {
                report(diagnostics::bug::item_name_not_found(
                    context.file_id,
                    &label.0,
                    *span,
                ));
                Value::Error
            }
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
        Term::U8Type(_)
        | Term::U16LeType(_)
        | Term::U16BeType(_)
        | Term::U32LeType(_)
        | Term::U32BeType(_)
        | Term::U64LeType(_)
        | Term::U64BeType(_)
        | Term::S8Type(_)
        | Term::S16LeType(_)
        | Term::S16BeType(_)
        | Term::S32LeType(_)
        | Term::S32BeType(_)
        | Term::S64LeType(_)
        | Term::S64BeType(_)
        | Term::F32LeType(_)
        | Term::F32BeType(_)
        | Term::F64LeType(_)
        | Term::F64BeType(_) => Value::Universe(Universe::Format),
        Term::BoolType(_) | Term::IntType(_) | Term::F32Type(_) | Term::F64Type(_) => {
            Value::Universe(Universe::Type)
        }
        Term::Constant(_, Constant::Bool(_)) => Value::BoolType,
        Term::Constant(_, Constant::Int(_)) => Value::IntType,
        Term::Constant(_, Constant::F32(_)) => Value::F32Type,
        Term::Constant(_, Constant::F64(_)) => Value::F64Type,
        Term::BoolElim(_, head, if_true, if_false) => {
            check_term(context, head, &Value::BoolType, report);
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
