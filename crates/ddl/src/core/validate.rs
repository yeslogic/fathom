//! Type-checking of the core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use codespan::FileId;
use codespan_reporting::diagnostic::{Diagnostic, Severity};

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
            Item::Struct(struct_ty) => {
                let mut used_field_names = HashMap::new();

                for field in &struct_ty.fields {
                    match used_field_names.entry(&field.name) {
                        Entry::Vacant(entry) => {
                            entry.insert(field.span());
                            synth_term_ty(file_id, &field.term, &mut diagnostics)
                        }
                        Entry::Occupied(entry) => {
                            diagnostics.push(diagnostics::field_redeclaration(
                                Severity::Bug,
                                file_id,
                                &field.name.0,
                                field.span(),
                                *entry.get(),
                            ));
                        }
                    }
                }

                match used_names.entry(&struct_ty.name) {
                    Entry::Vacant(entry) => {
                        entry.insert(struct_ty.span);
                    }
                    Entry::Occupied(entry) => diagnostics.push(diagnostics::item_redefinition(
                        Severity::Bug,
                        file_id,
                        &struct_ty.name.0,
                        struct_ty.span,
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
