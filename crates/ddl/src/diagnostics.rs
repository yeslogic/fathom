//! Diagnostics.

use codespan::{ByteIndex, FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};

use crate::core;
use crate::surface::delaborate;

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

pub fn type_mismatch(
    severity: Severity,
    file_id: FileId,
    term_span: Span,
    expected_ty: &core::Value,
    found_ty: &core::Value,
) -> Diagnostic {
    let arena = pretty::Arena::new();

    let expected_ty = delaborate::delaborate_term(&core::semantics::readback(expected_ty));
    let found_ty = delaborate::delaborate_term(&core::semantics::readback(found_ty));
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

    let found_ty = delaborate::delaborate_term(&core::semantics::readback(found_ty));
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
    use codespan::ByteOffset;
    use lalrpop_util::ParseError;
    use std::fmt;

    use crate::lexer::Token;

    use super::*;

    pub fn unexpected_char(
        file_id: FileId,
        start: ByteIndex,
        found: char,
        expected: &[&str],
    ) -> Diagnostic {
        let end = start + ByteOffset::from_char_len(found);
        Diagnostic {
            severity: Severity::Error,
            code: None,
            message: format!("unexpected character `{}`", found),
            primary_label: Label::new(file_id, start..end, "unexpected character"),
            secondary_labels: vec![],
            notes: vec![format!("expected one of {}", format_expected(&expected))],
        }
    }

    pub fn unexpected_eof(file_id: FileId, eof: ByteIndex, expected: &[&str]) -> Diagnostic {
        Diagnostic {
            severity: Severity::Error,
            code: None,
            message: "unexpected end of file".to_owned(),
            primary_label: Label::new(file_id, eof..eof, "unexpected end of file"),
            secondary_labels: vec![],
            notes: vec![format!("expected one of {}", format_expected(&expected))],
        }
    }

    pub fn parse(file_id: FileId, error: ParseError<ByteIndex, Token, Diagnostic>) -> Diagnostic {
        match error {
            ParseError::InvalidToken { location: _ } => unreachable!(),

            ParseError::UnrecognizedEOF { location, expected } => Diagnostic {
                severity: Severity::Error,
                code: None,
                message: "unexpected end of file".to_owned(),
                primary_label: Label::new(file_id, location..location, "unexpected end of file"),
                secondary_labels: vec![],
                notes: vec![format!("expected one of {}", format_expected(&expected))],
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
                notes: vec![format!("expected one of {}", format_expected(&expected))],
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

    fn format_expected<'a>(items: &'a [impl fmt::Display]) -> impl 'a + fmt::Display {
        use itertools::Itertools;

        struct DisplayExpected<'a, Item>(&'a [Item]);

        impl<'a, Item: fmt::Display> fmt::Display for DisplayExpected<'a, Item> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.split_last().map_or(Ok(()), |items| match items {
                    (last, []) => write!(f, "{}", last),
                    (last, items) => write!(f, "{}, or {}", items.iter().format(", "), last),
                })
            }
        }

        DisplayExpected(items)
    }

    pub fn var_name_not_found(file_id: FileId, name: &str, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Error,
            code: None,
            message: format!("cannot find `{}` in this scope", name),
            primary_label: Label::new(file_id, span, "not found in this scope"),
            secondary_labels: vec![],
            // TODO: provide suggestions
            notes: vec![],
        }
    }
}

pub mod bug {
    pub use super::*;

    pub fn not_yet_implemented(file_id: FileId, span: Span, feature_name: &str) -> Diagnostic {
        Diagnostic {
            severity: Severity::Error,
            code: None,
            message: format!("not yet implemented: {}", feature_name),
            primary_label: Label::new(file_id, span, "relies on an unimplemented language feature"),
            secondary_labels: vec![],
            notes: vec![],
        }
    }

    pub fn item_name_not_found(file_id: FileId, name: &str, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Bug,
            code: None,
            message: format!("cannot find item `{}` in this scope", name),
            primary_label: Label::new(file_id, span, "item not found in this scope"),
            secondary_labels: vec![],
            // TODO: provide suggestions
            notes: vec![],
        }
    }

    pub fn unknown_global(file_id: FileId, name: &str, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Bug,
            code: None,
            message: format!("unknown global `{}`", name),
            primary_label: Label::new(file_id, span, "unknown global"),
            secondary_labels: vec![],
            // TODO: provide suggestions
            notes: vec![],
        }
    }
}
