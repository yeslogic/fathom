//! Diagnostics.

use codespan::{FileId, Span};
use codespan_reporting::{Diagnostic, Label, Severity};

pub fn field_redeclaration(
    severity: Severity,
    file_id: FileId,
    name: &str,
    found: Span,
    original: Span,
) -> Diagnostic {
    Diagnostic {
        severity,
        code: None,
        message: format!("field `{}` is already declared", name),
        primary_label: Label::new(file_id, found, "field already declared"),
        secondary_labels: vec![Label::new(
            file_id,
            original,
            "previous field declaration here",
        )],
        notes: vec![format!("`{}` must be defined only per struct", name)],
    }
}

pub fn item_redefinition(
    severity: Severity,
    file_id: FileId,
    name: &str,
    found: Span,
    original: Span,
) -> Diagnostic {
    Diagnostic {
        severity,
        code: None,
        message: format!("the name `{}` is defined multiple times", name),
        primary_label: Label::new(file_id, found, "redefined here"),
        secondary_labels: vec![Label::new(file_id, original, "previous definition here")],
        notes: vec![format!(
            "`{}` must be defined only once in this module",
            name,
        )],
    }
}

pub fn var_name_not_found(
    severity: Severity,
    file_id: FileId,
    name: &str,
    span: Span,
) -> Diagnostic {
    Diagnostic {
        severity,
        code: None,
        message: format!("cannot find `{}` in this scope", name),
        primary_label: Label::new(file_id, span, "not found in this scope"),
        secondary_labels: vec![],
        // TODO: provide suggestions
        notes: vec![],
    }
}
