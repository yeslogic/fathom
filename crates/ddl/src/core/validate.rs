//! Type-checking of the core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use codespan::{FileId, Span};
use codespan_reporting::{Diagnostic, Severity};

use crate::core::{Item, Module, Term};
use crate::diagnostics;

/// Validate a module.
pub fn validate_module(module: &Module) -> Vec<Diagnostic> {
    use std::collections::HashMap;

    let file_id = module.file_id;
    let mut used_names = HashMap::new();
    let mut diagnostics = Vec::new();

    for item in module.items.iter() {
        use std::collections::hash_map::Entry;

        match item {
            Item::Struct {
                span, name, fields, ..
            } => {
                let mut used_field_names = HashMap::new();

                for (_, start, field_name, field_ty) in fields {
                    let field_span = Span::new(*start, field_ty.span().end());
                    match used_field_names.entry(field_name) {
                        Entry::Vacant(entry) => {
                            entry.insert(field_span);
                            synth_term_ty(file_id, field_ty, &mut diagnostics)
                        }
                        Entry::Occupied(entry) => {
                            diagnostics.push(diagnostics::field_redeclaration(
                                Severity::Bug,
                                file_id,
                                &field_name.0,
                                field_span,
                                *entry.get(),
                            ));
                        }
                    }
                }

                match used_names.entry(name) {
                    Entry::Vacant(entry) => {
                        entry.insert(*span);
                    }
                    Entry::Occupied(entry) => diagnostics.push(diagnostics::item_redefinition(
                        Severity::Bug,
                        file_id,
                        &name.0,
                        *span,
                        *entry.get(),
                    )),
                }
            }
        }
    }

    diagnostics
}

/// Check that a term is a type.
pub fn synth_term_ty(_file_id: FileId, term: &Term, _diagnostics: &mut Vec<Diagnostic>) {
    match term {
        Term::U8(_) | Term::Error(_) => {}
    }
}
