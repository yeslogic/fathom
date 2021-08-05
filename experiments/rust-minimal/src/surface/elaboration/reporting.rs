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
            Diagnostic::bug().with_message(match error {
                semantics::Error::InvalidRigidVar => "invalid rigid variable",
                semantics::Error::InvalidFlexibleVar => "invalid flexible variable",
                semantics::Error::InvalidFunctionElimHead => "invalid function elim head",
            })
        }

        match self {
            Message::UnboundName { range, name } => {
                let name = interner.resolve(*name).unwrap();

                Diagnostic::error()
                    .with_message(format!("unbound name `{}`", name))
                    .with_labels(vec![
                        Label::primary((), range.start..range.end).with_message("unbound name")
                    ])
            }
            Message::FailedToUnify { range, error } => match error {
                unification::Error::Mismatched => Diagnostic::error()
                    .with_message("type mismatch")
                    .with_labels(vec![Label::primary((), range.start..range.end)]),
                unification::Error::NonLinearSpine => Diagnostic::error()
                    .with_message("non linear spine") // TODO: better error messages!
                    .with_labels(vec![Label::primary((), range.start..range.end)]),
                unification::Error::EscapingRigidVar => Diagnostic::error()
                    .with_message("escaping rigid variable") // TODO: better error messages!
                    .with_labels(vec![Label::primary((), range.start..range.end)]),
                unification::Error::InfiniteSolution => Diagnostic::error()
                    .with_message("infinite solution") // TODO: better error messages!
                    .with_labels(vec![Label::primary((), range.start..range.end)]),
                unification::Error::Semantics(error) => semantics_diagnostic(error),
            },
            Message::UnsolvedFlexibleVar { range, source } => {
                let source_name = match source {
                    FlexSource::HoleType(_) => "type of hole",
                    FlexSource::HoleExpr(_) => "hole",
                    FlexSource::FunInputType(_) => "type of input",
                    FlexSource::FunOutputType => "type of output",
                    FlexSource::ReportedErrorType => "type of error",
                };

                Diagnostic::error()
                    .with_message(format!("failed to infer {}", source_name))
                    .with_labels(vec![Label::primary((), range.start..range.end)])
            }
            Message::Semantics(error) => semantics_diagnostic(error),
        }
    }
}
