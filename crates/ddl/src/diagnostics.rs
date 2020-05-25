//! Diagnostics.

#![allow(clippy::useless_format)]

use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use std::ops::Range;

use crate::{core, surface};

pub fn field_redeclaration(
    severity: Severity,
    file_id: usize,
    name: &str,
    found: Range<usize>,
    original: Range<usize>,
) -> Diagnostic<usize> {
    Diagnostic::new(severity)
        .with_message(format!("field `{}` is already declared", name))
        .with_labels(vec![
            Label::primary(file_id, found).with_message("field already declared"),
            Label::secondary(file_id, original).with_message("previous field declaration here"),
        ])
        .with_notes(vec![format!("`{}` must be defined only per struct", name)])
}

pub fn item_redefinition(
    severity: Severity,
    file_id: usize,
    name: &str,
    found: Range<usize>,
    original: Range<usize>,
) -> Diagnostic<usize> {
    Diagnostic::new(severity)
        .with_message(format!("the name `{}` is defined multiple times", name))
        .with_labels(vec![
            Label::primary(file_id, found).with_message("redefined here"),
            Label::secondary(file_id, original).with_message("previous definition here"),
        ])
        .with_notes(vec![format!(
            "`{}` must be defined only once in this module",
            name,
        )])
}

pub fn type_mismatch(
    severity: Severity,
    file_id: usize,
    term_range: Range<usize>,
    expected_ty: &core::Value,
    found_ty: &core::Value,
) -> Diagnostic<usize> {
    let arena = pretty::Arena::new();

    let expected_ty =
        surface::delaborate::delaborate_term(&core::semantics::read_back(expected_ty));
    let found_ty = surface::delaborate::delaborate_term(&core::semantics::read_back(found_ty));
    let pretty::DocBuilder(_, expected_ty) = surface::pretty::pretty_term(&arena, &expected_ty);
    let pretty::DocBuilder(_, found_ty) = surface::pretty::pretty_term(&arena, &found_ty);
    let expected_ty = expected_ty.pretty(100);
    let found_ty = found_ty.pretty(100);

    Diagnostic::new(severity)
        .with_message("type mismatch")
        .with_labels(vec![Label::primary(file_id, term_range).with_message(
            format!("expected `{}`, found `{}`", expected_ty, found_ty),
        )])
        .with_notes(vec![[
            format!("expected `{}`", expected_ty),
            format!("   found `{}`", found_ty),
        ]
        .join("\n")])
}

pub fn universe_mismatch(
    severity: Severity,
    file_id: usize,
    term_range: Range<usize>,
    found_ty: &core::Value,
) -> Diagnostic<usize> {
    let arena = pretty::Arena::new();

    let found_ty = surface::delaborate::delaborate_term(&core::semantics::read_back(found_ty));
    let pretty::DocBuilder(_, found_ty) = surface::pretty::pretty_term(&arena, &found_ty);
    let found_ty = found_ty.pretty(100);

    Diagnostic::new(severity)
        .with_message("universe mismatch")
        .with_labels(vec![Label::primary(file_id, term_range)
            .with_message(format!("expected a universe, found `{}`", found_ty))])
        .with_notes(vec![[
            format!("expected a universe"),
            format!("   found `{}`", found_ty),
        ]
        .join("\n")])
}

pub fn term_has_no_type(
    severity: Severity,
    file_id: usize,
    range: Range<usize>,
) -> Diagnostic<usize> {
    // TODO: provide suggestions
    Diagnostic::new(severity)
        .with_message("term has no type")
        .with_labels(vec![
            Label::primary(file_id, range).with_message("cannot synthesize type")
        ])
        .with_notes(vec![format!("term has no type")])
}

pub fn not_a_function(
    severity: Severity,
    file_id: usize,
    head: Range<usize>,
    head_ty: &core::Value,
    argument: Range<usize>,
) -> Diagnostic<usize> {
    let arena = pretty::Arena::new();

    let head_ty = surface::delaborate::delaborate_term(&core::semantics::read_back(head_ty));
    let pretty::DocBuilder(_, found_ty) = surface::pretty::pretty_term(&arena, &head_ty);
    let head_ty = found_ty.pretty(100);

    Diagnostic::new(severity)
        .with_message(format!(
            "applied something that is not a function to an argument"
        ))
        .with_labels(vec![
            Label::primary(file_id, head)
                .with_message(format!("expected a function, found `{}`", head_ty)),
            Label::secondary(file_id, argument).with_message("applied to this argument"),
        ])
        .with_notes(vec![[
            format!("expected a function"),
            format!("   found `{}`", head_ty),
        ]
        .join("\n")])
}

pub fn ambiguous_match_expression(
    severity: Severity,
    file_id: usize,
    range: Range<usize>,
) -> Diagnostic<usize> {
    Diagnostic::new(severity)
        .with_message("ambiguous match expression")
        .with_labels(vec![
            Label::primary(file_id, range).with_message("type annotation required")
        ])
}

pub mod error {
    use lalrpop_util::ParseError;
    use std::fmt;

    use crate::lexer::Token;

    use super::*;

    pub fn unexpected_char(
        file_id: usize,
        start: usize,
        found: char,
        expected: &[&str],
    ) -> Diagnostic<usize> {
        let end = start + found.len_utf8();
        let range = start..end;

        Diagnostic::error()
            .with_message(format!("unexpected character `{}`", found))
            .with_labels(vec![
                Label::primary(file_id, range).with_message("unexpected character")
            ])
            .with_notes(vec![format!(
                "expected one of {}",
                format_expected(&expected),
            )])
    }

    pub fn unexpected_eof(file_id: usize, eof: usize, expected: &[&str]) -> Diagnostic<usize> {
        let eof = eof;

        Diagnostic::error()
            .with_message("unexpected end of file")
            .with_labels(vec![
                Label::primary(file_id, eof..eof).with_message("unexpected end of file")
            ])
            .with_notes(vec![format!(
                "expected one of {}",
                format_expected(&expected),
            )])
    }

    pub fn parse(
        file_id: usize,
        error: ParseError<usize, Token, Diagnostic<usize>>,
    ) -> Diagnostic<usize> {
        match error {
            ParseError::InvalidToken { .. } => unreachable!(),

            ParseError::UnrecognizedEOF { location, expected } => {
                let location = location;

                Diagnostic::error()
                    .with_message("unexpected end of file")
                    .with_labels(vec![Label::primary(file_id, location..location)
                        .with_message("unexpected end of file")])
                    .with_notes(vec![format!(
                        "expected one of {}",
                        format_expected(&expected),
                    )])
            }

            ParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => {
                let range = start..end;

                Diagnostic::error()
                    .with_message(format!("unexpected token \"{}\"", token))
                    .with_labels(vec![
                        Label::primary(file_id, range).with_message("unexpected token")
                    ])
                    .with_notes(vec![format!(
                        "expected one of {}",
                        format_expected(&expected),
                    )])
            }

            ParseError::ExtraToken {
                token: (start, token, end),
            } => {
                let range = start..end;

                Diagnostic::error()
                    .with_message(format!("extra token \"{}\"", token))
                    .with_labels(vec![
                        Label::primary(file_id, range).with_message("extra token")
                    ])
            }

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

    pub fn var_name_not_found(
        file_id: usize,
        name: &str,
        range: Range<usize>,
    ) -> Diagnostic<usize> {
        // TODO: provide suggestions
        Diagnostic::error()
            .with_message(format!("cannot find `{}` in this scope", name))
            .with_labels(vec![
                Label::primary(file_id, range).with_message("not found in this scope")
            ])
    }

    pub fn numeric_literal_not_supported(
        file_id: usize,
        range: Range<usize>,
        found_ty: &core::Value,
    ) -> Diagnostic<usize> {
        let arena = pretty::Arena::new();

        let found_ty = surface::delaborate::delaborate_term(&core::semantics::read_back(found_ty));
        let pretty::DocBuilder(_, found_ty) = surface::pretty::pretty_term(&arena, &found_ty);
        let found_ty = found_ty.pretty(100);

        Diagnostic::error()
            .with_message(format!(
                "cannot construct a `{}` from a numeric literal",
                found_ty,
            ))
            .with_labels(vec![Label::primary(file_id, range).with_message(format!(
                "numeric literals not supported for type `{}`",
                found_ty,
            ))])
    }

    pub fn ambiguous_numeric_literal(file_id: usize, range: Range<usize>) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("ambiguous numeric literal")
            .with_labels(vec![
                Label::primary(file_id, range).with_message("type annotation required")
            ])
    }

    pub fn unsupported_pattern_ty(
        file_id: usize,
        range: Range<usize>,
        found_ty: &core::Value,
    ) -> Diagnostic<usize> {
        let arena = pretty::Arena::new();

        let found_ty = surface::delaborate::delaborate_term(&core::semantics::read_back(found_ty));
        let pretty::DocBuilder(_, found_ty) = surface::pretty::pretty_term(&arena, &found_ty);
        let found_ty = found_ty.pretty(100);

        Diagnostic::error()
            .with_message(format!("unsupported pattern type: `{}`", found_ty))
            .with_labels(vec![Label::primary(file_id, range)
                .with_message(format!("unsupported pattern type: `{}`", found_ty))])
            .with_notes(vec![
                "can only currently match against terms of type `Bool` or `Int`".to_owned(),
            ])
    }

    pub fn no_default_pattern(file_id: usize, range: Range<usize>) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("non-exhaustive patterns")
            .with_labels(vec![
                Label::primary(file_id, range).with_message("missing default pattern")
            ])
    }
}

pub mod bug {
    pub use super::*;

    pub fn not_yet_implemented(
        file_id: usize,
        range: Range<usize>,
        feature_name: &str,
    ) -> Diagnostic<usize> {
        Diagnostic::bug()
            .with_message(format!("not yet implemented: {}", feature_name))
            .with_labels(vec![Label::primary(file_id, range)
                .with_message("relies on an unimplemented language feature")])
    }

    pub fn global_name_not_found(
        file_id: usize,
        name: &str,
        range: Range<usize>,
    ) -> Diagnostic<usize> {
        // TODO: provide suggestions
        Diagnostic::bug()
            .with_message(format!("global `{}` is not defined", name))
            .with_labels(vec![
                Label::primary(file_id, range).with_message("global is not defined")
            ])
    }

    pub fn item_name_not_found(
        file_id: usize,
        name: &str,
        range: Range<usize>,
    ) -> Diagnostic<usize> {
        // TODO: provide suggestions
        Diagnostic::bug()
            .with_message(format!("cannot find item `{}` in this scope", name))
            .with_labels(vec![
                Label::primary(file_id, range).with_message("item not found in this scope")
            ])
    }

    pub fn unknown_global(file_id: usize, name: &str, range: Range<usize>) -> Diagnostic<usize> {
        // TODO: provide suggestions
        Diagnostic::bug()
            .with_message(format!("unknown global `{}`", name))
            .with_labels(vec![
                Label::primary(file_id, range).with_message("unknown global")
            ])
    }
}

pub mod warning {
    pub use super::*;

    pub fn unreachable_pattern(file_id: usize, range: Range<usize>) -> Diagnostic<usize> {
        Diagnostic::warning()
            .with_message("unreachable pattern")
            .with_labels(vec![
                Label::primary(file_id, range).with_message("unreachable pattern")
            ])
    }
}
