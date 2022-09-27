use codespan_reporting::diagnostic::{Diagnostic, Label};
use itertools::Itertools;
use std::cell::RefCell;

use crate::source::{ByteRange, FileId};
use crate::surface::elaboration::{unification, FlexSource};
use crate::surface::BinOp;
use crate::{StringId, StringInterner, BUG_REPORT_URL};

/// Elaboration diagnostic messages.
#[derive(Debug, Clone)]
pub enum Message {
    /// The name was not previously bound in the current scope.
    UnboundName {
        range: ByteRange,
        name: StringId,
    },
    RefutablePattern {
        pattern_range: ByteRange,
    },
    NonExhaustiveMatchExpr {
        match_expr_range: ByteRange,
        scrutinee_expr_range: ByteRange,
    },
    UnreachablePattern {
        range: ByteRange,
    },
    UnexpectedParameter {
        param_range: ByteRange,
    },
    UnexpectedArgument {
        head_range: ByteRange,
        head_type: String,
        arg_range: ByteRange,
    },
    UnknownField {
        head_range: ByteRange,
        head_type: String,
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
    ArrayLiteralNotSupported {
        range: ByteRange,
        expected_type: String,
    },
    MismatchedArrayLength {
        range: ByteRange,
        found_len: usize,
        expected_len: String,
    },
    AmbiguousArrayLiteral {
        range: ByteRange,
    },
    AmbiguousStringLiteral {
        range: ByteRange,
    },
    MismatchedStringLiteralByteLength {
        range: ByteRange,
        expected_len: usize,
        found_len: usize,
    },
    NonAsciiStringLiteral {
        invalid_range: ByteRange,
    },
    StringLiteralNotSupported {
        range: ByteRange,
        expected_type: String,
    },
    InvalidNumericLiteral {
        range: ByteRange,
        message: String,
    },
    NumericLiteralNotSupported {
        range: ByteRange,
        expected_type: String,
    },
    AmbiguousNumericLiteral {
        range: ByteRange,
    },
    BooleanLiteralNotSupported {
        range: ByteRange,
    },
    /// Unification errors.
    FailedToUnify {
        range: ByteRange,
        lhs: String,
        rhs: String,
        error: unification::Error,
    },
    BinOpMismatchedTypes {
        range: ByteRange,
        lhs_range: ByteRange,
        rhs_range: ByteRange,
        op: BinOp<ByteRange>,
        lhs: String,
        rhs: String,
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
        // TODO: add type
        // type: Doc<_>,
        expr: String,
    },
    /// A cycle between module items was detected.
    CycleDetected {
        names: Vec<StringId>,
    },
    /// Core term lacked span information
    MissingSpan {
        range: ByteRange,
    },
}

impl Message {
    pub fn to_diagnostic(&self, interner: &RefCell<StringInterner>) -> Diagnostic<FileId> {
        let primary_label = |range: &ByteRange| Label::primary(range.file_id(), *range);
        let secondary_label = |range: &ByteRange| Label::secondary(range.file_id(), *range);

        match self {
            Message::UnboundName { range, name } => {
                let interner = interner.borrow();
                let name = interner.resolve(*name).unwrap();

                Diagnostic::error()
                    .with_message(format!("cannot find `{}` in scope", name))
                    .with_labels(vec![primary_label(range).with_message("unbound name")])
                // TODO: list suggestions
            }
            Message::RefutablePattern { pattern_range } => Diagnostic::error()
                .with_message("refutable patterns found in binding")
                .with_labels(vec![
                    primary_label(pattern_range).with_message("refutable pattern")
                ])
                .with_notes(vec!["expected an irrefutable pattern".to_owned()]),
            Message::NonExhaustiveMatchExpr {
                match_expr_range,
                scrutinee_expr_range,
            } => Diagnostic::error()
                .with_message("non-exhaustive patterns in match expression")
                .with_labels(vec![
                    primary_label(scrutinee_expr_range).with_message("patterns not covered"),
                    secondary_label(match_expr_range).with_message("in match expression"),
                ]),
            Message::UnreachablePattern { range } => Diagnostic::warning()
                .with_message("unreachable pattern")
                .with_labels(vec![primary_label(range)]),
            Message::UnexpectedParameter { param_range } => Diagnostic::error()
                .with_message("too many parameters in function literal")
                .with_labels(vec![
                    primary_label(param_range).with_message("unexpected parameter")
                ])
                .with_notes(vec!["this parameter can be removed".to_owned()]),
            Message::UnexpectedArgument {
                head_range,
                head_type,
                arg_range,
            } => Diagnostic::error()
                .with_message("expression was applied to an unexpected argument")
                .with_labels(vec![
                    primary_label(arg_range).with_message("unexpected argument"),
                    secondary_label(head_range)
                        .with_message(format!("expression of type {}", head_type)),
                ]),
            Message::UnknownField {
                head_range,
                head_type,
                label_range,
                label,
            } => {
                let interner = interner.borrow();
                let label = interner.resolve(*label).unwrap();

                Diagnostic::error()
                    .with_message(format!("cannot find `{}` in expression", label))
                    .with_labels(vec![
                        primary_label(label_range).with_message("unknown label"),
                        secondary_label(head_range)
                            .with_message(format!("expression of type {}", head_type)),
                    ])
                // TODO: list suggestions
            }
            Message::MismatchedFieldLabels {
                range,
                expr_labels,
                type_labels,
            } => {
                let interner = interner.borrow();
                let mut diagnostic_labels = Vec::new();
                {
                    let mut type_labels = type_labels.iter().peekable();

                    'expr_labels: for (range, expr_label) in expr_labels.iter() {
                        'type_labels: loop {
                            match type_labels.next() {
                                None => {
                                    let expr_label = interner.resolve(*expr_label).unwrap();
                                    diagnostic_labels.push(primary_label(range).with_message(
                                        format!("unexpected field `{}`", expr_label,),
                                    ));
                                    continue 'expr_labels;
                                }
                                Some(type_label) if expr_label == type_label => {
                                    continue 'expr_labels;
                                }
                                Some(type_label) => {
                                    let type_label = interner.resolve(*type_label).unwrap();
                                    diagnostic_labels.push(
                                        primary_label(range).with_message(format!(
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
                        diagnostic_labels.push(primary_label(range).with_message(format!(
                            "missing fields {}",
                            type_labels
                                .map(|label| interner.resolve(*label).unwrap())
                                .format_with(", ", |label, f| f(&format_args!("`{}`", label))),
                        )));
                    } else {
                        diagnostic_labels
                            .push(secondary_label(range).with_message("the record literal"));
                    }
                }

                let found_labels = (expr_labels.iter())
                    .map(|(_, label)| interner.resolve(*label).unwrap())
                    .format_with(", ", |label, f| f(&format_args!("`{}`", label)));
                let expected_labels = (type_labels.iter())
                    .map(|label| interner.resolve(*label).unwrap())
                    .format_with(", ", |label, f| f(&format_args!("`{}`", label)));

                Diagnostic::error()
                    .with_message("mismatched field labels in record literal")
                    .with_labels(diagnostic_labels)
                    .with_notes(vec![
                        format!("expected fields {}", expected_labels),
                        format!("   found fields {}", found_labels),
                    ])
            }
            Message::DuplicateFieldLabels { range, labels } => {
                let interner = interner.borrow();
                let diagnostic_labels = (labels.iter())
                    .map(|(range, _)| primary_label(range).with_message("duplicate field"))
                    .chain(std::iter::once(
                        secondary_label(range).with_message("the record literal"),
                    ))
                    .collect();

                Diagnostic::error()
                    .with_message("duplicate labels found in record")
                    .with_labels(diagnostic_labels)
                    .with_notes(vec![format!(
                        "duplicate fields {}",
                        (labels.iter())
                            .map(|(_, label)| interner.resolve(*label).unwrap())
                            .format_with(", ", |label, f| f(&format_args!("`{}`", label)))
                    )])
            }
            Message::ArrayLiteralNotSupported {
                range,
                expected_type,
            } => Diagnostic::error()
                .with_message("array literal not supported")
                .with_labels(vec![
                    primary_label(range).with_message(format!("expected `{}`", expected_type))
                ])
                .with_notes(vec![format!("expected `{}`", expected_type)]),
            Message::MismatchedArrayLength {
                range,
                found_len,
                expected_len,
            } => Diagnostic::error()
                .with_message("mismatched array length")
                .with_labels(vec![
                    primary_label(range).with_message("array with invalid length")
                ])
                .with_notes(vec![
                    format!("expected length {}", expected_len),
                    format!("   found length {}", found_len),
                ]),
            Message::AmbiguousArrayLiteral { range } => Diagnostic::error()
                .with_message("ambiguous array literal")
                .with_labels(vec![
                    primary_label(range).with_message("type annotations needed")
                ]),
            Message::MismatchedStringLiteralByteLength {
                range,
                expected_len,
                found_len,
            } => Diagnostic::error()
                .with_message("mismatched number of bytes in string literal")
                .with_labels(vec![
                    primary_label(range).with_message("invalid string literal")
                ])
                .with_notes(vec![
                    format!("expected byte length {}", expected_len),
                    format!("   found byte length {}", found_len),
                ]),
            Message::NonAsciiStringLiteral { invalid_range } => Diagnostic::error()
                .with_message("non-ASCII character found in string literal")
                .with_labels(vec![
                    primary_label(invalid_range).with_message("non-ASCII character")
                ]),
            Message::StringLiteralNotSupported {
                range,
                expected_type,
            } => Diagnostic::error()
                .with_message("string literal not supported")
                .with_labels(vec![
                    primary_label(range).with_message(format!("expected `{}`", expected_type))
                ])
                .with_notes(vec![format!("expected `{}`", expected_type)]),
            Message::AmbiguousStringLiteral { range } => Diagnostic::error()
                .with_message("ambiguous string literal")
                .with_labels(vec![
                    primary_label(range).with_message("type annotations needed")
                ]),
            Message::InvalidNumericLiteral { range, message } => Diagnostic::error()
                .with_message("failed to parse numeric literal")
                .with_labels(vec![(primary_label(range)).with_message(message)]),
            Message::NumericLiteralNotSupported {
                range,
                expected_type,
            } => Diagnostic::error()
                .with_message("numeric literal not supported")
                .with_labels(vec![
                    primary_label(range).with_message(format!("expected `{}`", expected_type))
                ])
                .with_notes(vec![format!("expected `{}`", expected_type)]),
            Message::AmbiguousNumericLiteral { range } => Diagnostic::error()
                .with_message("ambiguous numeric literal")
                .with_labels(vec![
                    primary_label(range).with_message("type annotations needed")
                ]),
            Message::BooleanLiteralNotSupported { range } => Diagnostic::error()
                .with_message("boolean literal not supported for expected type")
                .with_labels(vec![primary_label(range)]),
            Message::BinOpMismatchedTypes {
                range: _,
                lhs_range,
                rhs_range,
                op,
                lhs,
                rhs,
            } => Diagnostic::error()
                .with_message("mismatched types")
                .with_labels(vec![
                    primary_label(lhs_range).with_message(format!("has type `{}`", lhs)),
                    primary_label(rhs_range).with_message(format!("has type `{}`", rhs)),
                    secondary_label(&op.range())
                        .with_message(format!("no implementation for `{} {} {}`", lhs, op, rhs)),
                ]),
            Message::FailedToUnify {
                range,
                lhs,
                rhs,
                error,
            } => {
                use unification::{Error, RenameError, SpineError};

                // TODO: Make these errors more user-friendly
                match error {
                    Error::Mismatch => Diagnostic::error()
                        .with_message("mismatched types")
                        .with_labels(vec![primary_label(range).with_message(format!(
                            "type mismatch, expected `{}`, found `{}`",
                            lhs, rhs
                        ))])
                        .with_notes(vec![[
                            format!("expected `{}`", lhs),
                            format!("   found `{}`", rhs),
                        ]
                        .join("\n")]),
                    // TODO: reduce confusion around ‘problem spines’
                    Error::Spine(error) => match error {
                        SpineError::NonLinearSpine(_var) => Diagnostic::error()
                            .with_message("variable appeared more than once in problem spine")
                            .with_labels(vec![primary_label(range)]),
                        SpineError::NonRigidFunApp => Diagnostic::error()
                            .with_message("non-variable function application in problem spine")
                            .with_labels(vec![primary_label(range)]),
                        SpineError::RecordProj(_label) => Diagnostic::error()
                            .with_message("record projection found in problem spine")
                            .with_labels(vec![primary_label(range)]),
                        SpineError::ConstMatch => Diagnostic::error()
                            .with_message("constant match found in problem spine")
                            .with_labels(vec![primary_label(range)]),
                    },
                    Error::Rename(error) => match error {
                        RenameError::EscapingRigidVar(_var) => Diagnostic::error()
                            .with_message("escaping rigid variable")
                            .with_labels(vec![primary_label(range)]),
                        RenameError::InfiniteSolution => Diagnostic::error()
                            .with_message("infinite solution")
                            .with_labels(vec![primary_label(range)]),
                    },
                }
            }
            Message::HoleSolution { range, name, expr } => {
                let interner = interner.borrow();
                let name = interner.resolve(*name).unwrap();

                Diagnostic::note()
                    .with_message(format!("solution found for hole `?{}`", name))
                    .with_labels(vec![primary_label(range).with_message("solution found")])
                    .with_notes(vec![format!(
                        "hole `?{}` can be replaced with `{}`",
                        name, expr,
                    )])
            }
            Message::UnsolvedFlexibleVar { source } => {
                let (range, source_name) = match source {
                    FlexSource::HoleType(range, _) => (range, "hole type"), // should never appear in user-facing output
                    FlexSource::HoleExpr(range, _) => (range, "hole expression"),
                    FlexSource::PlaceholderType(range) => (range, "placeholder type"), // should never appear in user-facing output
                    FlexSource::PlaceholderExpr(range) => (range, "placeholder expression"),
                    FlexSource::PlaceholderPatternType(range) => {
                        (range, "placeholder pattern type")
                    }
                    FlexSource::NamedPatternType(range, _) => (range, "named pattern type"),
                    FlexSource::MatchExprType(range) => (range, "match expression type"),
                    FlexSource::ReportedErrorType(range) => (range, "error type"), // should never appear in user-facing output
                };

                Diagnostic::error()
                    .with_message(format!("failed to infer {}", source_name))
                    .with_labels(vec![
                        primary_label(range).with_message(format!("unsolved {}", source_name))
                    ])
            }
            Message::CycleDetected { names } => {
                let interner = interner.borrow();
                let names: Vec<_> = names
                    .iter()
                    .map(|id| interner.resolve(*id).unwrap())
                    .collect();
                let cycle = names.join(" → ");
                Diagnostic::error()
                    .with_message("cycle detected")
                    .with_notes(vec![cycle])
            }
            Message::MissingSpan { range } => Diagnostic::bug()
                .with_message("produced core term without span")
                .with_labels(vec![primary_label(range)])
                .with_notes(vec![format!(
                    "please file a bug report at: {}",
                    BUG_REPORT_URL
                )]),
        }
    }
}
