//! Parser for the data description language.

use codespan_reporting::{Diagnostic, Label};
use std::collections::HashMap;

use crate::{concrete, core};

pub fn elaborate_module(concrete_module: &concrete::Module) -> (core::Module, Vec<Diagnostic>) {
    let file_id = concrete_module.file_id;
    let mut used_names = HashMap::new();
    let mut diagnostics = Vec::new();
    let mut core_module = core::Module {
        file_id,
        items: Vec::new(),
    };

    for item in concrete_module.items.iter() {
        use std::collections::hash_map::Entry;

        match item {
            concrete::Item::Struct { span, doc, name } => {
                let label = core::Label(name.clone());

                match used_names.entry(label) {
                    Entry::Vacant(entry) => {
                        let item = core::Item::Struct {
                            span: *span,
                            doc: doc.clone(),
                            name: entry.key().clone(),
                        };

                        core_module.items.push(item);
                        entry.insert(*span);
                    }
                    Entry::Occupied(entry) => {
                        let diagnostic = Diagnostic::new_error(
                            format!("the name `{}` is defined multiple times", name),
                            Label::new(file_id, *span, "redefined here"),
                        )
                        .with_notes(vec![format!(
                            "`{}` must be defined only once in this module",
                            name
                        )])
                        .with_secondary_labels(vec![Label::new(
                            file_id,
                            *entry.get(),
                            "previous definition here",
                        )]);

                        diagnostics.push(diagnostic);
                    }
                }
            }
        }
    }

    (core_module, diagnostics)
}
