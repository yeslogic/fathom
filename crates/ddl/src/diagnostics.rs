//! Diagnostics.

use codespan::{ByteIndex, FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};

use crate::core;

pub fn field_redeclaration(
    severity: Severity,
    file_id: FileId,
    name: &core::Label,
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
    name: &core::Label,
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

pub fn type_mismatch(
    severity: Severity,
    file_id: FileId,
    term_span: Span,
    expected_ty: &core::Value,
    found_ty: &core::Value,
) -> Diagnostic {
    let arena = pretty::Arena::new();

    let expected_ty = core::semantics::readback(expected_ty);
    let found_ty = core::semantics::readback(found_ty);
    let pretty::DocBuilder(_, expected_ty) = expected_ty.doc(&arena);
    let pretty::DocBuilder(_, found_ty) = found_ty.doc(&arena);
    let expected_ty = expected_ty.pretty(100);
    let found_ty = found_ty.pretty(100);

    Diagnostic {
        severity,
        code: None,
        message: "type mismatch".to_owned(),
        primary_label: Label::new(
            file_id,
            term_span,
            format!("expected `{}`, found `{}`", expected_ty, found_ty),
        ),
        secondary_labels: vec![],
        notes: vec![[
            format!("expected `{}`", expected_ty),
            format!("   found `{}`", found_ty),
        ]
        .join("\n")],
    }
}

pub fn universe_mismatch(
    severity: Severity,
    file_id: FileId,
    term_span: Span,
    found_ty: &core::Value,
) -> Diagnostic {
    let arena = pretty::Arena::new();

    let found_ty = core::semantics::readback(found_ty);
    let pretty::DocBuilder(_, found_ty) = found_ty.doc(&arena);
    let found_ty = found_ty.pretty(100);

    Diagnostic {
        severity,
        code: None,
        message: "universe mismatch".to_owned(),
        primary_label: Label::new(
            file_id,
            term_span,
            format!("expected `Type` or `Kind`, found `{}`", found_ty),
        ),
        secondary_labels: vec![],
        notes: vec![[
            format!("expected `Type` or `Kind`"),
            format!("   found `{}`", found_ty),
        ]
        .join("\n")],
    }
}

pub fn kind_has_no_type(severity: Severity, file_id: FileId, span: Span) -> Diagnostic {
    Diagnostic {
        severity,
        code: None,
        message: "`Kind` does not have a type or kind".to_owned(),
        primary_label: Label::new(file_id, span, "cannot synthesize type"),
        secondary_labels: vec![],
        // TODO: provide suggestions
        notes: vec![],
    }
}

pub mod error {
    use lalrpop_util::ParseError;
    use std::fmt;

    use crate::lexer::Token;

    use super::*;

    pub fn parse(file_id: FileId, error: ParseError<ByteIndex, Token, Diagnostic>) -> Diagnostic {
        match error {
            ParseError::InvalidToken { location: _ } => unreachable!(),

            ParseError::UnrecognizedEOF { location, expected } => Diagnostic {
                severity: Severity::Error,
                code: None,
                message: "unexpected end of file".to_owned(),
                primary_label: Label::new(file_id, location..location, "unexpected end of file"),
                secondary_labels: vec![],
                notes: vec![format!("expected one of {}", display_expected(&expected),)],
            },

            ParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Diagnostic {
                severity: Severity::Error,
                code: None,
                message: format!("unexpected token \"{}\"", token),
                primary_label: Label::new(file_id, start..end, "unexpected token"),
                secondary_labels: vec![],
                notes: vec![format!("expected one of {}", display_expected(&expected),)],
            },

            ParseError::ExtraToken {
                token: (start, token, end),
            } => Diagnostic {
                severity: Severity::Error,
                code: None,
                message: format!("extra token \"{}\"", token),
                primary_label: Label::new(file_id, start..end, "extra token"),
                secondary_labels: vec![],
                notes: vec![],
            },

            ParseError::User { error } => error,
        }
    }

    fn display_expected<'a, Item: fmt::Display>(items: &'a [Item]) -> impl 'a + fmt::Display {
        struct DisplayExpected<'a, Item>(&'a [Item]);

        impl<'a, Item: fmt::Display> fmt::Display for DisplayExpected<'a, Item> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                for (i, item) in self.0.iter().enumerate() {
                    match i {
                        0 => write!(f, "{}", item)?,
                        i if i >= self.0.len() => write!(f, ", or {}", item)?,
                        _ => write!(f, ", {}", item)?,
                    }
                }

                Ok(())
            }
        }

        DisplayExpected(items)
    }
}
