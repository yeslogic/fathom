use std::cell::RefCell;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use itertools::Itertools;

use crate::files::FileId;
use crate::source::{FileRange, StringId, StringInterner};
use crate::surface::elaboration::{unification, MetaSource};
use crate::surface::{BinOp, Plicity};
use crate::BUG_REPORT_URL;

/// Elaboration diagnostic messages.
#[derive(Debug, Clone)]
pub enum Message {
    /// The name was not previously bound in the current scope.
    UnboundName {
        range: FileRange,
        name: StringId,
        suggestion: Option<StringId>,
    },
    RefutablePattern {
        pattern_range: FileRange,
    },
    NonExhaustiveMatchExpr {
        match_expr_range: FileRange,
        scrutinee_expr_range: FileRange,
    },
    UnreachablePattern {
        range: FileRange,
    },
    UnexpectedParameter {
        param_range: FileRange,
    },
    UnexpectedArgument {
        head_range: FileRange,
        head_type: String,
        arg_range: FileRange,
    },
    PlicityArgumentMismatch {
        head_range: FileRange,
        head_plicity: Plicity,
        head_type: String,
        arg_range: FileRange,
        arg_plicity: Plicity,
    },
    UnknownField {
        head_range: FileRange,
        head_type: String,
        label_range: FileRange,
        label: StringId,
        suggestion: Option<StringId>,
    },
    MismatchedFieldLabels {
        range: FileRange,
        expr_labels: Vec<(FileRange, StringId)>,
        type_labels: Vec<StringId>,
        // TODO: add expected type
        // expected_type: Doc<_>,
    },
    DuplicateFieldLabels {
        range: FileRange,
        labels: Vec<(FileRange, StringId)>,
    },
    ArrayLiteralNotSupported {
        range: FileRange,
        expected_type: String,
    },
    MismatchedArrayLength {
        range: FileRange,
        found_len: usize,
        expected_len: String,
    },
    AmbiguousArrayLiteral {
        range: FileRange,
    },
    AmbiguousStringLiteral {
        range: FileRange,
    },
    MismatchedStringLiteralByteLength {
        range: FileRange,
        expected_len: usize,
        found_len: usize,
    },
    NonAsciiStringLiteral {
        invalid_range: FileRange,
    },
    StringLiteralNotSupported {
        range: FileRange,
        expected_type: String,
    },
    InvalidNumericLiteral {
        range: FileRange,
        message: String,
    },
    NumericLiteralNotSupported {
        range: FileRange,
        expected_type: String,
    },
    AmbiguousNumericLiteral {
        range: FileRange,
    },
    BooleanLiteralNotSupported {
        range: FileRange,
    },
    /// Unification errors.
    FailedToUnify {
        range: FileRange,
        found: String,
        expected: String,
        error: unification::Error,
    },
    BinOpMismatchedTypes {
        range: FileRange,
        lhs_range: FileRange,
        rhs_range: FileRange,
        op: BinOp<FileRange>,
        lhs: String,
        rhs: String,
    },
    /// A solution for a metavariable could not be found.
    UnsolvedMetaVar {
        source: MetaSource,
        // TODO: add type
        // type: Doc<_>,
    },
    HoleSolution {
        range: FileRange,
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
        range: FileRange,
    },
}

impl Message {
    pub fn to_diagnostic(&self, interner: &RefCell<StringInterner>) -> Diagnostic<FileId> {
        let primary_label = |range: &FileRange| Label::primary(range.file_id(), *range);
        let secondary_label = |range: &FileRange| Label::secondary(range.file_id(), *range);

        match self {
            Message::UnboundName {
                range,
                name,
                suggestion,
            } => {
                let interner = interner.borrow();
                let name = interner.resolve(*name).unwrap();

                let mut diagnostic = Diagnostic::error()
                    .with_message(format!("cannot find `{name}` in scope"))
                    .with_labels(vec![primary_label(range).with_message("unbound name")]);

                if let Some(suggestion) = suggestion {
                    diagnostic = diagnostic.with_notes(vec![format!(
                        "help: did you mean `{}`?",
                        interner.resolve(*suggestion).unwrap()
                    )])
                }
                diagnostic
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
                        .with_message(format!("expression of type {head_type}")),
                ]),
            Message::PlicityArgumentMismatch {
                head_range,
                head_plicity,
                head_type,
                arg_range,
                arg_plicity,
            } => Diagnostic::error()
                .with_message(format!(
                    "{arg_plicity} argument was applied to an {head_plicity} function"
                ))
                .with_labels(vec![
                    primary_label(arg_range).with_message(format!("{arg_plicity} argument")),
                    secondary_label(head_range)
                        .with_message(format!("{head_plicity} function of type {head_type}")),
                ]),
            Message::UnknownField {
                head_range,
                head_type,
                label_range,
                label,
                suggestion,
            } => {
                let interner = interner.borrow();
                let label = interner.resolve(*label).unwrap();

                let mut diagnostic = Diagnostic::error()
                    .with_message(format!("cannot find `{label}` in expression"))
                    .with_labels(vec![
                        primary_label(label_range).with_message("unknown label"),
                        secondary_label(head_range)
                            .with_message(format!("expression of type {head_type}")),
                    ]);
                if let Some(suggestion) = suggestion {
                    diagnostic = diagnostic.with_notes(vec![format!(
                        "help: did you mean `{}`?",
                        interner.resolve(*suggestion).unwrap()
                    )]);
                }
                diagnostic
            }
            Message::MismatchedFieldLabels {
                range,
                expr_labels,
                type_labels,
            } => {
                let interner = interner.borrow();
                let mut diagnostic_labels = Vec::with_capacity(expr_labels.len());
                {
                    let mut type_labels = type_labels.iter().peekable();

                    'expr_labels: for (range, expr_label) in expr_labels.iter() {
                        'type_labels: loop {
                            match type_labels.next() {
                                None => {
                                    let expr_label = interner.resolve(*expr_label).unwrap();
                                    diagnostic_labels.push(
                                        primary_label(range).with_message(format!(
                                            "unexpected field `{expr_label}`"
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
                                        primary_label(range).with_message(format!(
                                            "expected field `{type_label}`",
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
                                .format_with(", ", |label, f| f(&format_args!("`{label}`"))),
                        )));
                    } else {
                        diagnostic_labels
                            .push(secondary_label(range).with_message("the record literal"));
                    }
                }

                let found_labels = (expr_labels.iter())
                    .map(|(_, label)| interner.resolve(*label).unwrap())
                    .format_with(", ", |label, f| f(&format_args!("`{label}`")));
                let expected_labels = (type_labels.iter())
                    .map(|label| interner.resolve(*label).unwrap())
                    .format_with(", ", |label, f| f(&format_args!("`{label}`")));

                Diagnostic::error()
                    .with_message("mismatched field labels in record literal")
                    .with_labels(diagnostic_labels)
                    .with_notes(vec![
                        format!("expected fields {expected_labels}"),
                        format!("   found fields {found_labels}"),
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
                            .format_with(", ", |label, f| f(&format_args!("`{label}`")))
                    )])
            }
            Message::ArrayLiteralNotSupported {
                range,
                expected_type,
            } => Diagnostic::error()
                .with_message("array literal not supported")
                .with_labels(vec![
                    primary_label(range).with_message(format!("expected `{expected_type}`"))
                ])
                .with_notes(vec![format!("expected `{expected_type}`")]),
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
                    format!("expected length {expected_len}"),
                    format!("   found length {found_len}"),
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
                    format!("expected byte length {expected_len}"),
                    format!("   found byte length {found_len}"),
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
                    primary_label(range).with_message(format!("expected `{expected_type}`"))
                ])
                .with_notes(vec![format!("expected `{expected_type}`")]),
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
                    primary_label(range).with_message(format!("expected `{expected_type}`"))
                ])
                .with_notes(vec![format!("expected `{expected_type}`")]),
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
                    primary_label(lhs_range).with_message(format!("has type `{lhs}`")),
                    primary_label(rhs_range).with_message(format!("has type `{rhs}`")),
                    secondary_label(&op.range())
                        .with_message(format!("no implementation for `{lhs} {op} {rhs}`")),
                ]),
            Message::FailedToUnify {
                range,
                found,
                expected,
                error,
            } => {
                use unification::{Error, RenameError, SpineError};

                // TODO: Make these errors more user-friendly
                match error {
                    Error::Mismatch => Diagnostic::error()
                        .with_message("mismatched types")
                        .with_labels(vec![primary_label(range).with_message(format!(
                            "type mismatch, expected `{expected}`, found `{found}`"
                        ))])
                        .with_notes(vec![[
                            format!("expected `{expected}`"),
                            format!("   found `{found}`"),
                        ]
                        .join("\n")]),
                    // TODO: reduce confusion around ‘problem spines’
                    Error::Spine(error) => match error {
                        SpineError::NonLinearSpine(_var) => Diagnostic::error()
                            .with_message("variable appeared more than once in problem spine")
                            .with_labels(vec![primary_label(range)]),
                        SpineError::NonLocalFunApp => Diagnostic::error()
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
                        RenameError::EscapingLocalVar(_var) => Diagnostic::error()
                            .with_message("escaping local variable")
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
                    .with_message(format!("solution found for hole `?{name}`"))
                    .with_labels(vec![primary_label(range).with_message("solution found")])
                    .with_notes(vec![format!(
                        "hole `?{name}` can be replaced with `{expr}`",
                    )])
            }
            Message::UnsolvedMetaVar { source } => {
                let (range, source_name) = match source {
                    MetaSource::ImplicitArg(range, _) => (range, "implicit argument"),
                    MetaSource::HoleExpr(range, _) => (range, "hole expression"),
                    MetaSource::PlaceholderExpr(range) => (range, "placeholder expression"),
                    MetaSource::PlaceholderPatternType(range) => {
                        (range, "placeholder pattern type")
                    }
                    MetaSource::NamedPatternType(range, _) => (range, "named pattern type"),
                    MetaSource::MatchExprType(range) => (range, "match expression type"),

                    // The following should never appear in user-facing output:
                    MetaSource::HoleType(range, _) => (range, "hole type"),
                    MetaSource::PlaceholderType(range) => (range, "placeholder type"),
                    MetaSource::ReportedErrorType(range) => (range, "error type"),
                };

                Diagnostic::error()
                    .with_message(format!("failed to infer {source_name}"))
                    .with_labels(vec![
                        primary_label(range).with_message(format!("unsolved {source_name}"))
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
                    "please file a bug report at: {BUG_REPORT_URL}"
                )]),
        }
    }
}
