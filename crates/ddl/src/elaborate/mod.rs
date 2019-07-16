//! Elaboration from the concrete syntax into the core syntax.
//!
//! Performs the following:
//!
//! - name resolution
//! - desugaring
//! - pattern compilation (TODO)
//! - bidirectional type checking (TODO)
//! - unification (TODO)

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use std::collections::HashMap;

use crate::{concrete, core, diagnostics};

/// Elaborate a module in the concrete syntax into the core syntax.
pub fn elaborate_module(
    concrete_module: &concrete::Module,
    report: &mut dyn FnMut(Diagnostic),
) -> core::Module {
    let item_context = ItemContext::new(concrete_module.file_id);
    core::Module {
        file_id: concrete_module.file_id,
        items: elaborate_items(item_context, &concrete_module.items, report),
    }
}

/// Contextual information to be used when elaborating items.
pub struct ItemContext {
    /// The file where these items are defined (for error reporting).
    file_id: FileId,
    /// Labels that have previously been used for items, along with the span
    /// where they were introduced (for error reporting).
    labels: HashMap<core::Label, Span>,
}

impl ItemContext {
    /// Create a new item context.
    pub fn new(file_id: FileId) -> ItemContext {
        ItemContext {
            file_id,
            labels: HashMap::new(),
        }
    }

    /// Create a field context based on this item context.
    pub fn field_context(&self) -> FieldContext {
        FieldContext::new(self.file_id)
    }
}

/// Elaborate items in the concrete syntax into items in the core syntax.
pub fn elaborate_items(
    mut context: ItemContext,
    concrete_items: &[concrete::Item],
    report: &mut dyn FnMut(Diagnostic),
) -> Vec<core::Item> {
    let mut core_items = Vec::new();

    for item in concrete_items.iter() {
        use std::collections::hash_map::Entry;

        match item {
            concrete::Item::Struct(struct_ty) => {
                let label = core::Label(struct_ty.name.to_string());
                let field_context = context.field_context();
                let core_fields =
                    elaborate_struct_ty_fields(field_context, &struct_ty.fields, report);

                match context.labels.entry(label) {
                    Entry::Vacant(entry) => {
                        let item = core::StructType {
                            span: struct_ty.span,
                            doc: struct_ty.doc.clone(),
                            name: entry.key().clone(),
                            fields: core_fields,
                        };

                        core_items.push(core::Item::Struct(item));
                        entry.insert(struct_ty.span);
                    }
                    Entry::Occupied(entry) => report(diagnostics::item_redefinition(
                        Severity::Error,
                        context.file_id,
                        entry.key(),
                        struct_ty.span,
                        *entry.get(),
                    )),
                }
            }
        }
    }

    core_items
}

/// Contextual information to be used when elaborating structure type fields.
pub struct FieldContext {
    /// The file where these fields are defined (for error reporting).
    file_id: FileId,
    /// Labels that have previously been used for fields, along with the span
    /// where they were introduced (for error reporting).
    labels: HashMap<core::Label, Span>,
}

impl FieldContext {
    /// Create a new field context.
    pub fn new(file_id: FileId) -> FieldContext {
        FieldContext {
            file_id,
            labels: HashMap::new(),
        }
    }

    /// Create a term context based on this field context.
    pub fn term_context(&self) -> TermContext {
        TermContext::new(self.file_id)
    }
}

/// Elaborate structure type fields in the concrete syntax into structure type
/// fields in the core syntax.
pub fn elaborate_struct_ty_fields(
    mut context: FieldContext,
    concrete_fields: &[concrete::TypeField],
    report: &mut dyn FnMut(Diagnostic),
) -> Vec<core::TypeField> {
    let mut core_fields = Vec::with_capacity(concrete_fields.len());

    for field in concrete_fields {
        use std::collections::hash_map::Entry;

        let label = core::Label(field.name.to_string());
        let field_span = Span::merge(field.name.span(), field.term.span());
        let ty = elaborate_ty(context.term_context(), &field.term, report);

        match context.labels.entry(label) {
            Entry::Vacant(entry) => {
                core_fields.push(core::TypeField {
                    doc: field.doc.clone(),
                    start: field_span.start(),
                    name: entry.key().clone(),
                    term: ty,
                });

                entry.insert(field_span);
            }
            Entry::Occupied(entry) => report(diagnostics::field_redeclaration(
                Severity::Error,
                context.file_id,
                entry.key(),
                field_span,
                *entry.get(),
            )),
        }
    }

    core_fields
}

/// Contextual information to be used when elaborating terms.
pub struct TermContext {
    /// The file where this term is located (for error reporting).
    file_id: FileId,
}

impl TermContext {
    /// Create a new term context.
    pub fn new(file_id: FileId) -> TermContext {
        TermContext { file_id }
    }
}

/// Check that a concrete term is a type, and elaborate it into the core syntax.
pub fn elaborate_ty(
    context: TermContext,
    concrete_term: &concrete::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> core::Term {
    match concrete_term {
        concrete::Term::Var(name) => match name.as_str() {
            "U8" => core::Term::U8(name.span()),
            "U16Le" => core::Term::U16Le(name.span()),
            "U16Be" => core::Term::U16Be(name.span()),
            "U32Le" => core::Term::U32Le(name.span()),
            "U32Be" => core::Term::U32Be(name.span()),
            "U64Le" => core::Term::U64Le(name.span()),
            "U64Be" => core::Term::U64Be(name.span()),
            "S8" => core::Term::S8(name.span()),
            "S16Le" => core::Term::S16Le(name.span()),
            "S16Be" => core::Term::S16Be(name.span()),
            "S32Le" => core::Term::S32Le(name.span()),
            "S32Be" => core::Term::S32Be(name.span()),
            "S64Le" => core::Term::S64Le(name.span()),
            "S64Be" => core::Term::S64Be(name.span()),
            _ => {
                report(diagnostics::var_name_not_found(
                    Severity::Error,
                    context.file_id,
                    name.as_str(),
                    name.span(),
                ));

                core::Term::Error(name.span())
            }
        },
        concrete::Term::Error(span) => core::Term::Error(*span),
    }
}
