use codespan_reporting::diagnostic::{Diagnostic, Label};
use itertools::Itertools;
use std::cell::RefCell;

use crate::source::{ByteRange, FileId};
use crate::surface::elaboration::{unification, FlexSource};
use crate::{StringId, StringInterner};

/// Elaboration diagnostic messages.
#[derive(Debug, Clone)]
pub enum Message {
    /// The name was not previously bound in the current scope.
    UnboundName {
        range: ByteRange,
        name: StringId,
    },
    UnknownField {
        head_range: ByteRange,
        // TODO: add head type
        // head_type: Doc<_>,
        label_range: ByteRange,
        label: StringId,
    },
    MismatchedFieldLabels {
        range: ByteRange,
        expr_labels: Vec<(ByteRange, StringId)>,
        type_labels: Vec<StringId>,
        // TODO: add expected type
        // expected_type: Doc<_>,
    },
    DuplicateFieldLabels {
        range: ByteRange,
        labels: Vec<(ByteRange, StringId)>,
    },
    InvalidNumericLiteral {
        range: ByteRange,
        message: String,
        // expected_type: Doc<_>,
    },
    ArrayLiteralNotSupported {
        range: ByteRange,
        // expected_type: Doc<_>,
    },
    MismatchedArrayLength {
        range: ByteRange,
        found_len: usize,
        // expected_len: Doc<_>,
    },
    AmbiguousArrayLiteral {
        range: ByteRange,
    },
    NumericLiteralNotSupported {
        range: ByteRange,
        // expected_type: Doc<_>,
    },
    AmbiguousNumericLiteral {
        range: ByteRange,
    },
    /// Unification errors.
    FailedToUnify {
        range: ByteRange,
        // TODO: add lhs and rhs values
        // lhs: Doc<_>,
        // rhs: Doc<_>,
        error: unification::Error,
    },
    /// A solution for a flexible variable could not be found.
    UnsolvedFlexibleVar {
        source: FlexSource,
        // TODO: add type
        // type: Doc<_>,
    },
    HoleSolution {
        range: ByteRange,
        name: StringId,
        // TODO: add type and solution expr
        // type: Doc<_>,
        // expr: Doc<_>,
    },
}

impl Message {
    pub fn to_diagnostic(
        &self,
        interner: &RefCell<StringInterner>,
        file_id: FileId,
    ) -> Diagnostic<FileId> {
        match self {
            Message::UnboundName { range, name } => {
                let interner = interner.borrow();
                let name = interner.resolve(*name).unwrap();

                Diagnostic::error()
                    .with_message(format!("unbound name `{}`", name))
                    .with_labels(vec![
                        Label::primary(file_id, *range).with_message("unbound name")
                    ])
            }
            Message::UnknownField {
                head_range: _,
                label_range,
                label,
            } => {
                let interner = interner.borrow();
                let label = interner.resolve(*label).unwrap();

                Diagnostic::error()
                    .with_message(format!("unknown field `{}`", label))
                    .with_labels(vec![
                        Label::primary(file_id, *label_range).with_message("unknown field")
                    ])
            }
            Message::MismatchedFieldLabels {
                range,
                expr_labels,
                type_labels,
            } => {
                let interner = interner.borrow();
                let mut diagnostic_labels = Vec::new();
                {
                    let mut expr_labels = expr_labels.iter().peekable();
                    let mut type_labels = type_labels.iter().peekable();

                    'expr_labels: while let Some((range, expr_label)) = expr_labels.next() {
                        'type_labels: loop {
                            match type_labels.next() {
                                None => {
                                    let expr_label = interner.resolve(*expr_label).unwrap();
                                    diagnostic_labels.push(
                                        Label::primary(file_id, *range).with_message(format!(
                                            "unexpected field `{}`",
                                            expr_label,
                                        )),
                                    );
                                    continue 'expr_labels;
                                }
                                Some(type_label) if expr_label == type_label => {
                                    continue 'expr_labels;
                                }
                                Some(type_label) => {
                                    let type_label = interner.resolve(*type_label).unwrap();
                                    diagnostic_labels.push(
                                        Label::primary(file_id, *range).with_message(format!(
                                            "expected field `{}`",
                                            type_label,
                                        )),
                                    );
                                    continue 'type_labels;
                                }
                            }
                        }
                    }

                    if type_labels.peek().is_some() {
                        diagnostic_labels.push(Label::primary(file_id, *range).with_message(
                            format!(
                            "missing fields {}",
                            type_labels
                                .map(|label| interner.resolve(*label).unwrap())
                                .format_with(", ", |label, f| f(&format_args!("`{}`", label))),
                        ),
                        ));
                    } else {
                        diagnostic_labels.push(
                            Label::secondary(file_id, *range).with_message("the record literal"),
                        );
                    }
                }

                let found_labels = (expr_labels.iter())
                    .map(|(_, label)| interner.resolve(*label).unwrap())
                    .format_with(", ", |label, f| f(&format_args!("`{}`", label)));
                let expected_labels = (type_labels.iter())
                    .map(|label| interner.resolve(*label).unwrap())
                    .format_with(", ", |label, f| f(&format_args!("`{}`", label)));

                Diagnostic::error()
                    .with_message(format!("mismatched field labels in record literal"))
                    .with_labels(diagnostic_labels)
                    .with_notes(vec![
                        format!("expected fields {}", expected_labels),
                        format!("   found fields {}", found_labels),
                    ])
            }
            Message::DuplicateFieldLabels { range, labels } => {
                let interner = interner.borrow();
                let diagnostic_labels = (labels.iter())
                    .map(|(range, _)| {
                        Label::primary(file_id, *range).with_message("duplicate field")
                    })
                    .chain(std::iter::once(
                        Label::secondary(file_id, *range).with_message("the record literal"),
                    ))
                    .collect();

                Diagnostic::error()
                    .with_message(format!("duplicate labels found in record"))
                    .with_labels(diagnostic_labels)
                    .with_notes(vec![format!(
                        "duplicate fields {}",
                        (labels.iter())
                            .map(|(_, label)| interner.resolve(*label).unwrap())
                            .format_with(", ", |label, f| f(&format_args!("`{}`", label)))
                    )])
            }
            Message::InvalidNumericLiteral { range, message } => Diagnostic::error()
                .with_message("failed to parse numeric literal")
                .with_labels(vec![(Label::primary(file_id, *range)).with_message(message)]),
            Message::ArrayLiteralNotSupported { range } => Diagnostic::error()
                .with_message("array literal not supported for expected type")
                .with_labels(vec![Label::primary(file_id, *range)]),
            Message::MismatchedArrayLength { range, found_len } => Diagnostic::error()
                .with_message("mismatched array length")
                .with_labels(vec![Label::primary(file_id, *range)
                    .with_message(format!("found length: {}", found_len))]),
            Message::AmbiguousArrayLiteral { range } => Diagnostic::error()
                .with_message("ambiguous array literal")
                .with_labels(vec![Label::primary(file_id, *range)]),
            Message::NumericLiteralNotSupported { range } => Diagnostic::error()
                .with_message("numeric literal not supported for expected type")
                .with_labels(vec![Label::primary(file_id, *range)]),
            Message::AmbiguousNumericLiteral { range } => Diagnostic::error()
                .with_message("ambiguous numeric literal")
                .with_labels(vec![Label::primary(file_id, *range)]),
            Message::FailedToUnify { range, error } => match error {
                unification::Error::Mismatched => Diagnostic::error()
                    .with_message("type mismatch")
                    .with_labels(vec![Label::primary(file_id, *range)]),
                unification::Error::NonLinearSpine => Diagnostic::error()
                    .with_message("non linear spine") // TODO: user-friendly message
                    .with_labels(vec![Label::primary(file_id, *range)]),
                unification::Error::EscapingRigidVar => Diagnostic::error()
                    .with_message("escaping rigid variable") // TODO: user-friendly message
                    .with_labels(vec![Label::primary(file_id, *range)]),
                unification::Error::InfiniteSolution => Diagnostic::error()
                    .with_message("infinite solution") // TODO: user-friendly message
                    .with_labels(vec![Label::primary(file_id, *range)]),
            },
            Message::HoleSolution { range, name } => {
                let interner = interner.borrow();
                let name = interner.resolve(*name).unwrap();

                Diagnostic::note()
                    .with_message(format!("solution found for `?{}`", name))
                    .with_labels(vec![Label::primary(file_id, *range)])
            }
            Message::UnsolvedFlexibleVar { source } => {
                let (range, source_name) = match source {
                    FlexSource::HoleType(range, _) => (*range, "hole type"), // should never appear in user-facing output
                    FlexSource::HoleExpr(range, _) => (*range, "hole expression"),
                    FlexSource::PlaceholderType(range) => (*range, "placeholder type"), // should never appear in user-facing output
                    FlexSource::PlaceholderExpr(range) => (*range, "placeholder expression"),
                    FlexSource::FunInputType(range, _) => (*range, "function input type"),
                    FlexSource::FunOutputType(range) => (*range, "function output type"),
                    FlexSource::ReportedErrorType(range) => (*range, "error type"), // should never appear in user-facing output
                };

                Diagnostic::error()
                    .with_message(format!("failed to infer {}", source_name))
                    .with_labels(vec![Label::primary(file_id, range)])
            }
        }
    }
}
