//! Type-checking of the core syntax.
//!
//! This is used to verify that the core syntax is correctly formed, for
//! debugging purposes.

use codespan_reporting::{Diagnostic, Label as DiagnosticLabel};
use std::collections::HashMap;

use crate::core::{Item, Module};

pub fn validate_module(module: &Module) -> Vec<Diagnostic> {
    let file_id = module.file_id;
    let mut used_names = HashMap::new();
    let mut diagnostics = Vec::new();

    for item in module.items.iter() {
        use std::collections::hash_map::Entry;

        match item {
            Item::Struct { span, name, .. } => match used_names.entry(name.clone()) {
                Entry::Vacant(entry) => {
                    entry.insert(*span);
                }
                Entry::Occupied(entry) => {
                    let diagnostic = Diagnostic::new_error(
                        format!("the name `{}` is defined multiple times", name),
                        DiagnosticLabel::new(file_id, *span, "redefined here"),
                    )
                    .with_notes(vec![format!(
                        "`{}` must be defined only once in this module",
                        name
                    )])
                    .with_secondary_labels(vec![DiagnosticLabel::new(
                        file_id,
                        *entry.get(),
                        "previous definition here",
                    )]);

                    diagnostics.push(diagnostic);
                }
            },
        }
    }

    diagnostics
}
