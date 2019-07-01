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
use codespan_reporting::{Diagnostic, Severity};

use crate::{concrete, core, diagnostics};

/// Elaborate a module in the concrete syntax into the core syntax.
pub fn elaborate_module(concrete_module: &concrete::Module) -> (core::Module, Vec<Diagnostic>) {
    use std::collections::HashMap;

    let file_id = concrete_module.file_id;
    let mut diagnostics = Vec::new();

    let mut used_names = HashMap::new();
    let mut core_module = core::Module {
        file_id,
        items: Vec::new(),
    };

    for item in concrete_module.items.iter() {
        use std::collections::hash_map::Entry;

        match item {
            concrete::Item::Struct {
                span,
                doc,
                name,
                fields,
            } => {
                let mut used_field_names = HashMap::new();
                let mut core_fields = Vec::with_capacity(fields.len());

                for (doc, field_name, concrete_field_ty) in fields {
                    let field_span = Span::merge(field_name.span(), concrete_field_ty.span());
                    match used_field_names.entry(field_name.as_str()) {
                        Entry::Vacant(entry) => {
                            entry.insert(field_span);
                            core_fields.push(core::TypeField {
                                doc: doc.clone(),
                                start: field_span.start(),
                                name: core::Label(field_name.to_string()),
                                term: check_term_ty(file_id, concrete_field_ty, &mut diagnostics),
                            });
                        }
                        Entry::Occupied(entry) => {
                            diagnostics.push(diagnostics::field_redeclaration(
                                Severity::Error,
                                file_id,
                                field_name.as_str(),
                                field_span,
                                *entry.get(),
                            ));
                        }
                    }
                }

                match used_names.entry(name.as_str()) {
                    Entry::Vacant(entry) => {
                        let item = core::StructType {
                            span: *span,
                            doc: doc.clone(),
                            name: core::Label(name.to_string()),
                            fields: core_fields,
                        };

                        core_module.items.push(core::Item::Struct(item));
                        entry.insert(*span);
                    }
                    Entry::Occupied(entry) => diagnostics.push(diagnostics::item_redefinition(
                        Severity::Error,
                        file_id,
                        name.as_str(),
                        *span,
                        *entry.get(),
                    )),
                }
            }
        }
    }

    (core_module, diagnostics)
}

/// Check that a concrete term is a type, and elaborate it into the core syntax.
pub fn check_term_ty(
    file_id: FileId,
    concrete_term: &concrete::Term,
    diagnostics: &mut Vec<Diagnostic>,
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
                diagnostics.push(diagnostics::var_name_not_found(
                    Severity::Error,
                    file_id,
                    name.as_str(),
                    name.span(),
                ));

                core::Term::Error(name.span())
            }
        },
        concrete::Term::Error(span) => core::Term::Error(*span),
    }
}
