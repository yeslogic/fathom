//! Type-checking of the core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use std::collections::HashMap;

use crate::core::{Item, Label, Module, Term, TypeField};
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
    items: HashMap<Label, Span>,
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
                validate_ty(context.term_context(), &alias.term, report);

                match context.items.entry(alias.name.clone()) {
                    Entry::Vacant(entry) => {
                        entry.insert(alias.span);
                    }
                    Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                        Severity::Bug,
                        context.file_id,
                        &alias.name,
                        alias.span,
                        *entry.get(),
                    )),
                }
            }
            Item::Struct(struct_ty) => {
                validate_struct_ty_fields(context.field_context(), &struct_ty.fields, report);

                match context.items.entry(struct_ty.name.clone()) {
                    Entry::Vacant(entry) => {
                        entry.insert(struct_ty.span);
                    }
                    Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                        Severity::Bug,
                        context.file_id,
                        &struct_ty.name,
                        struct_ty.span,
                        *entry.get(),
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
    items: &'items HashMap<Label, Span>,
    /// Labels that have previously been used for fields, along with the span
    /// where they were introduced (for error reporting).
    fields: HashMap<Label, Span>,
}

impl<'items> FieldContext<'items> {
    /// Create a new field context.
    pub fn new(file_id: FileId, items: &'items HashMap<Label, Span>) -> FieldContext<'items> {
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

        validate_ty(context.term_context(), &field.term, report);

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
    items: &'items HashMap<Label, Span>,
}

impl<'items> TermContext<'items> {
    /// Create a new term context.
    pub fn new(file_id: FileId, items: &'items HashMap<Label, Span>) -> TermContext<'items> {
        TermContext { file_id, items }
    }
}

/// Validate that a term is a type.
pub fn validate_ty(context: TermContext<'_>, term: &Term, report: &mut dyn FnMut(Diagnostic)) {
    match term {
        Term::Item(span, label) => match context.items.get(label) {
            Some(_) => {}
            None => report(diagnostics::var_name_not_found(
                Severity::Bug,
                context.file_id,
                &label.0,
                *span,
            )),
        },
        Term::U8(_)
        | Term::U16Le(_)
        | Term::U16Be(_)
        | Term::U32Le(_)
        | Term::U32Be(_)
        | Term::U64Le(_)
        | Term::U64Be(_)
        | Term::S8(_)
        | Term::S16Le(_)
        | Term::S16Be(_)
        | Term::S32Le(_)
        | Term::S32Be(_)
        | Term::S64Le(_)
        | Term::S64Be(_)
        | Term::F32Le(_)
        | Term::F32Be(_)
        | Term::F64Le(_)
        | Term::F64Be(_)
        | Term::Error(_) => {}
    }
}
