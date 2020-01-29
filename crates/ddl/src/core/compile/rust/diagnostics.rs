//! Diagnostics.

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};

pub fn non_format_type_as_host_type(severity: Severity, file_id: FileId, span: Span) -> Diagnostic {
    Diagnostic {
        severity,
        code: None,
        message: "attempted to compile a non-format type as a host type".to_owned(),
        primary_label: Label::new(file_id, span, "not a format type"),
        secondary_labels: vec![],
        notes: vec![],
    }
}

pub mod error {
    use super::*;

    pub fn unconstrained_int(file_id: FileId, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Error,
            code: None,
            message: "cannot compile unconstrained integer types".to_owned(),
            primary_label: Label::new(file_id, span, "unconstrained integer type"),
            secondary_labels: vec![],
            notes: vec![],
        }
    }
}

pub mod bug {
    pub use super::*;

    pub fn item_name_reused(
        file_id: FileId,
        name: &str,
        found: Span,
        original: Span,
    ) -> Diagnostic {
        Diagnostic {
            severity: Severity::Bug,
            code: None,
            message: format!(
                "attempted to compile an item named `{}` multiple times",
                name,
            ),
            primary_label: Label::new(file_id, found, "redefined here"),
            secondary_labels: vec![Label::new(file_id, original, "original item here")],
            notes: vec![format!(
                "`{}` must be defined only once in this module",
                name,
            )],
        }
    }

    pub fn oversaturated_fun_elim(file_id: FileId, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Bug,
            code: None,
            message: "attempted to compile an oversaturated function elimination".to_owned(),
            primary_label: Label::new(file_id, span, "too many eliminations"),
            secondary_labels: vec![],
            notes: vec![],
        }
    }

    pub fn unexpected_elim(file_id: FileId, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Bug,
            code: None,
            message: "unexpected elimination".to_owned(),
            primary_label: Label::new(file_id, span, "unexpected elimination"),
            secondary_labels: vec![],
            notes: vec![],
        }
    }

    pub fn integer_out_of_bounds(file_id: FileId, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Bug,
            code: None,
            message: "attempted to compile an out of bounds integer".to_owned(),
            primary_label: Label::new(file_id, span, "integer out of bounds"),
            secondary_labels: vec![],
            notes: vec![],
        }
    }

    pub fn expected_integer(file_id: FileId, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Bug,
            code: None,
            message: "attempted to compile this expression as an integer".to_owned(),
            primary_label: Label::new(file_id, span, "not an integer"),
            secondary_labels: vec![],
            notes: vec![],
        }
    }

    pub fn expected_type(file_id: FileId, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Bug,
            code: None,
            message: "attempted to compile this expression as a type".to_owned(),
            primary_label: Label::new(file_id, span, "not a type"),
            secondary_labels: vec![],
            notes: vec![],
        }
    }

    pub fn unbound_item(file_id: FileId, name: &str, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Bug,
            code: None,
            message: format!(
                "attempted to compile an item `{}` that was not yet bound",
                name,
            ),
            primary_label: Label::new(file_id, span, "item not found in this scope"),
            secondary_labels: vec![],
            // TODO: provide suggestions
            notes: vec![],
        }
    }
}
