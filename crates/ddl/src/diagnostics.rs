//! Diagnostics.

use codespan::{FileId, Span, ByteIndex};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use lalrpop_util::ParseError;
use std::fmt;

use crate::lexer::Token;

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

pub fn parse_error(
    file_id: FileId,
    error: ParseError<ByteIndex, Token, Diagnostic>,
) -> Diagnostic {
    match error {
        ParseError::InvalidToken { location: _ } => unreachable!(),

        ParseError::UnrecognizedEOF { location, expected } => Diagnostic::new_error(
            "unexpected end of file",
            Label::new(file_id, location..location, "unexpected end of file"),
        )
        .with_notes(vec![format!(
            "expected one of {}",
            display_expected(&expected),
        )]),

        ParseError::UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => Diagnostic::new_error(
            format!("unexpected token \"{}\"", token),
            Label::new(file_id, start..end, "unexpected token"),
        )
        .with_notes(vec![format!(
            "expected one of {}",
            display_expected(&expected),
        )]),

        ParseError::ExtraToken {
            token: (start, token, end),
        } => Diagnostic::new_error(
            format!("extra token \"{}\"", token),
            Label::new(file_id, start..end, "extra token"),
        ),

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
