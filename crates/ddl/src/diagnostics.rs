//! Diagnostics.

use codespan::{ByteIndex, FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};

use crate::{core, surface};

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

pub fn type_mismatch(
    severity: Severity,
    file_id: FileId,
    term_span: Span,
    expected_ty: &core::Value,
    found_ty: &core::Value,
) -> Diagnostic {
    let arena = pretty::Arena::new();

    let expected_ty =
        surface::delaborate::delaborate_term(&core::semantics::read_back(expected_ty));
    let found_ty = surface::delaborate::delaborate_term(&core::semantics::read_back(found_ty));
    let pretty::DocBuilder(_, expected_ty) = surface::pretty::pretty_term(&arena, &expected_ty);
    let pretty::DocBuilder(_, found_ty) = surface::pretty::pretty_term(&arena, &found_ty);
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

    let found_ty = surface::delaborate::delaborate_term(&core::semantics::read_back(found_ty));
    let pretty::DocBuilder(_, found_ty) = surface::pretty::pretty_term(&arena, &found_ty);
    let found_ty = found_ty.pretty(100);

    Diagnostic {
        severity,
        code: None,
        message: "universe mismatch".to_owned(),
        primary_label: Label::new(
            file_id,
            term_span,
            format!("expected a universe, found `{}`", found_ty),
        ),
        secondary_labels: vec![],
        notes: vec![[
            format!("expected a universe"),
            format!("   found `{}`", found_ty),
        ]
        .join("\n")],
    }
}

pub fn kind_has_no_type(severity: Severity, file_id: FileId, span: Span) -> Diagnostic {
    Diagnostic {
        severity,
        code: None,
        message: "cannot synthesize the type of `Kind`".to_owned(),
        primary_label: Label::new(file_id, span, "cannot synthesize type"),
        secondary_labels: vec![],
        // TODO: provide suggestions
        notes: vec![format!("`Kind` has no corresponding type")],
    }
}

pub fn not_a_function(
    severity: Severity,
    file_id: FileId,
    head: Span,
    head_ty: &core::Value,
    argument: Span,
) -> Diagnostic {
    let arena = pretty::Arena::new();

    let head_ty = surface::delaborate::delaborate_term(&core::semantics::read_back(head_ty));
    let pretty::DocBuilder(_, found_ty) = surface::pretty::pretty_term(&arena, &head_ty);
    let head_ty = found_ty.pretty(100);

    Diagnostic {
        severity,
        code: None,
        message: format!("applied something that is not a function to an argument"),
        primary_label: Label::new(
            file_id,
            head,
            format!("expected a function, found `{}`", head_ty),
        ),
        secondary_labels: vec![Label::new(file_id, argument, "applied to this argument")],
        notes: vec![[
            format!("expected a function"),
            format!("   found `{}`", head_ty),
        ]
        .join("\n")],
    }
}

pub fn ambiguous_match_expression(severity: Severity, file_id: FileId, span: Span) -> Diagnostic {
    Diagnostic {
        severity,
        code: None,
        message: "ambiguous match expression".to_owned(),
        primary_label: Label::new(file_id, span, "type annotation required"),
        secondary_labels: vec![],
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

    pub fn numeric_literal_not_supported(
        file_id: FileId,
        span: Span,
        found_ty: &core::Value,
    ) -> Diagnostic {
        let arena = pretty::Arena::new();

        let found_ty = surface::delaborate::delaborate_term(&core::semantics::read_back(found_ty));
        let pretty::DocBuilder(_, found_ty) = surface::pretty::pretty_term(&arena, &found_ty);
        let found_ty = found_ty.pretty(100);

        Diagnostic {
            severity: Severity::Error,
            code: None,
            message: format!("cannot construct a `{}` from a numeric literal", found_ty),
            primary_label: Label::new(
                file_id,
                span,
                format!("numeric literals not supported for type `{}`", found_ty),
            ),
            secondary_labels: vec![],
            notes: vec![],
        }
    }

    pub fn ambiguous_numeric_literal(file_id: FileId, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Error,
            code: None,
            message: "ambiguous numeric literal".to_owned(),
            primary_label: Label::new(file_id, span, "type annotation required"),
            secondary_labels: vec![],
            notes: vec![],
        }
    }

    pub fn unsupported_pattern_ty(
        file_id: FileId,
        span: Span,
        found_ty: &core::Value,
    ) -> Diagnostic {
        let arena = pretty::Arena::new();

        let found_ty = surface::delaborate::delaborate_term(&core::semantics::read_back(found_ty));
        let pretty::DocBuilder(_, found_ty) = surface::pretty::pretty_term(&arena, &found_ty);
        let found_ty = found_ty.pretty(100);

        Diagnostic {
            severity: Severity::Error,
            code: None,
            message: format!("unsupported pattern type: `{}`", found_ty),
            primary_label: Label::new(
                file_id,
                span,
                format!("unsupported pattern type: `{}`", found_ty),
            ),
            secondary_labels: vec![],
            notes: vec![
                "can only currently match against terms of type `Bool` or `Int`".to_owned(),
            ],
        }
    }

    pub fn no_default_pattern(file_id: FileId, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Error,
            code: None,
            message: "non-exhaustive patterns".to_owned(),
            primary_label: Label::new(file_id, span, "missing default pattern"),
            secondary_labels: vec![],
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

    pub fn global_name_not_found(file_id: FileId, name: &str, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Bug,
            code: None,
            message: format!("global `{}` is not defined", name),
            primary_label: Label::new(file_id, span, "global is not defined"),
            secondary_labels: vec![],
            // TODO: provide suggestions
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

pub mod warning {
    pub use super::*;

    pub fn unreachable_pattern(file_id: FileId, span: Span) -> Diagnostic {
        Diagnostic {
            severity: Severity::Warning,
            code: None,
            message: "unreachable pattern".to_owned(),
            primary_label: Label::new(file_id, span, "unreachable pattern"),
            secondary_labels: vec![],
            notes: vec![],
        }
    }
}
