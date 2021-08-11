use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::core::semantics;
use crate::surface::elaboration::{unification, FlexSource};
use crate::{ByteRange, StringId, StringInterner};

/// Elaboration diagnostic messages.
#[derive(Debug, Clone)]
pub enum Message {
    /// The name was not previously bound in the current scope.
    UnboundName { range: ByteRange, name: StringId },
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
        range: ByteRange,
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
    /// An error occurred during evaluation, and is almost certainly a bug.
    Semantics(semantics::Error),
}

impl From<semantics::Error> for Message {
    fn from(error: semantics::Error) -> Message {
        Message::Semantics(error)
    }
}

impl Message {
    pub fn to_diagnostic(&self, interner: &StringInterner) -> Diagnostic<()> {
        fn semantics_diagnostic(error: &semantics::Error) -> Diagnostic<()> {
            Diagnostic::bug()
                .with_message(match error {
                    semantics::Error::InvalidRigidVar => "invalid rigid variable",
                    semantics::Error::InvalidFlexibleVar => "invalid flexible variable",
                    semantics::Error::InvalidFunctionElimHead => "invalid function elim head",
                })
                .with_notes(vec![
                    "This is almost certainly a bug!".to_owned(),
                    format!(
                        "If possible, please file a bug report at {}.",
                        env!("CARGO_PKG_REPOSITORY"),
                    ),
                ])
        }

        match self {
            Message::UnboundName { range, name } => {
                let name = interner.resolve(*name).unwrap();

                Diagnostic::error()
                    .with_message(format!("unbound name `{}`", name))
                    .with_labels(vec![Label::primary((), *range).with_message("unbound name")])
            }
            Message::FailedToUnify { range, error } => match error {
                unification::Error::Mismatched => Diagnostic::error()
                    .with_message("type mismatch")
                    .with_labels(vec![Label::primary((), *range)]),
                unification::Error::NonLinearSpine => Diagnostic::error()
                    .with_message("non linear spine") // TODO: user-friendly message
                    .with_labels(vec![Label::primary((), *range)]),
                unification::Error::EscapingRigidVar => Diagnostic::error()
                    .with_message("escaping rigid variable") // TODO: user-friendly message
                    .with_labels(vec![Label::primary((), *range)]),
                unification::Error::InfiniteSolution => Diagnostic::error()
                    .with_message("infinite solution") // TODO: user-friendly message
                    .with_labels(vec![Label::primary((), *range)]),
                unification::Error::Semantics(error) => semantics_diagnostic(error),
            },
            Message::HoleSolution { range, name } => {
                let name = interner.resolve(*name).unwrap();

                Diagnostic::note()
                    .with_message(format!("solution found for `?{}`", name))
                    .with_labels(vec![Label::primary((), *range)])
            }
            Message::UnsolvedFlexibleVar { range, source } => {
                let source_name = match source {
                    FlexSource::HoleType(_) => "type of hole", // should never appear in user-facing output
                    FlexSource::HoleExpr(_) => "hole",
                    FlexSource::FunInputType(_) => "type of function input",
                    FlexSource::FunOutputType => "type of function output",
                    FlexSource::ReportedErrorType => "type of error", // should never appear in user-facing output
                };

                Diagnostic::error()
                    .with_message(format!("failed to infer {}", source_name))
                    .with_labels(vec![Label::primary((), *range)])
            }
            Message::Semantics(error) => semantics_diagnostic(error),
        }
    }
}
