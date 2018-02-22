use codespan::ByteSpan;
use codespan_reporting::{Diagnostic, Label, LabelStyle, Severity};

use name::{Ident, Name};
use syntax::core::{Binop, RcExpr, RcKind, RcType, Scope};

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedType {
    Array,
    Arrow,
    Unsigned,
    Signed,
    Numeric,
    Actual(RcType),
}

/// An error that was encountered during type checking
#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    /// A variable of the requested name was not defined in this scope
    UndefinedName { var_span: ByteSpan, name: Name },
    /// Variable bound in the context was not at the value level
    ExpectedExpr { var_span: ByteSpan, found: Scope },
    /// One type was expected, but another was found
    Mismatch {
        expr: RcExpr,
        // span: ByteSpan, // TODO
        found: RcType,
        expected: ExpectedType,
        // expected: RcType, // TODO
    },
    /// One type was expected, but another was found
    InferenceMismatch {
        span: ByteSpan,
        found: RcType,
        expected: RcType,
    },
    /// Unexpected operand types in a binary operator expression
    BinaryOperands {
        context: Binop,
        expr: RcExpr,
        lhs_ty: RcType,
        rhs_ty: RcType,
    },
    /// A field was missing when projecting on a record
    MissingField {
        expr: RcExpr,
        struct_ty: RcType,
        field_name: Ident,
    },
    /// A variant was missing when introducing on a union
    MissingVariant {
        expr: RcExpr,
        union_ty: RcType,
        variant_name: Ident,
    },
    /// An invalid type was supplied to the cast expression
    InvalidCastType { expr: RcExpr, found: RcType },
}

impl TypeError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            TypeError::UndefinedName { ref name, var_span } => Diagnostic {
                severity: Severity::Bug,
                message: format!("cannot find `{}` in scope", name),
                labels: vec![
                    Label {
                        message: Some("not found in this scope".into()),
                        style: LabelStyle::Primary,
                        span: var_span,
                    },
                ],
            },
            TypeError::ExpectedExpr {
                ref found,
                var_span,
            } => Diagnostic {
                severity: Severity::Error,
                message: format!("expected a value, but found a type `{:?}`", found), // TODO: pretty print
                labels: vec![
                    Label {
                        message: Some("the type".into()),
                        style: LabelStyle::Primary,
                        span: var_span,
                    },
                ],
            },
            // TypeError::Mismatch {
            //     span,
            //     ref found,
            //     ref expected,
            // } => Diagnostic {
            //     severity: Severity::Error,
            //     message: format!(
            //         "found an expression of type `{:?}`, but expected an expression of type `{:?}`", // TODO: pretty print
            //         found, expected,
            //     ),
            //     labels: vec![
            //         Label {
            //             message: Some("the expression".into()),
            //             style: LabelStyle::Primary,
            //             span,
            //         },
            //     ],
            // },
            TypeError::InferenceMismatch {
                span,
                ref found,
                ref expected,
            } => Diagnostic {
                severity: Severity::Error,
                message: format!(
                    "found an expression of type `{:?}`, but expected an expression of type `{:?}`", // TODO: pretty print
                    found, expected,
                ),
                labels: vec![
                    Label {
                        message: Some("the expression".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },
            _ => unimplemented!(),
        }
    }
}

/// An error that was encountered during kind checking
#[derive(Debug, Clone, PartialEq)]
pub enum KindError {
    UndefinedName {
        var_span: ByteSpan,
        name: Name,
    },
    ExpectedType {
        span: ByteSpan,
        found: Scope,
    },
    NotATypeConstructor {
        fn_span: ByteSpan,
        arg_spans: Vec<ByteSpan>,
        found: RcKind,
    },
    Mismatch {
        span: ByteSpan,
        expected: RcKind,
        found: RcKind,
    },
    Type(TypeError),
}

impl KindError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        use std::iter;

        match *self {
            KindError::UndefinedName { ref name, var_span } => Diagnostic {
                severity: Severity::Bug,
                message: format!("cannot find `{}` in scope", name),
                labels: vec![
                    Label {
                        message: Some("not found in this scope".into()),
                        style: LabelStyle::Primary,
                        span: var_span,
                    },
                ],
            },
            KindError::NotATypeConstructor {
                fn_span,
                ref arg_spans,
                ref found,
            } => Diagnostic {
                severity: Severity::Error,
                message: format!(
                    "applied {} arguments to a type that was not expecting them - found `{:?}`", // TODO: pretty print
                    arg_spans.len(),
                    found,
                ),
                labels: iter::once(Label {
                    message: Some("the type".into()),
                    style: LabelStyle::Primary,
                    span: fn_span,
                }).chain(arg_spans.iter().map(|&span| Label {
                    message: Some("applied argument".into()),
                    style: LabelStyle::Secondary,
                    span,
                }))
                    .collect(),
            },
            KindError::Mismatch {
                span,
                ref found,
                ref expected,
            } => Diagnostic {
                severity: Severity::Error,
                message: format!(
                    "found a type of kind `{:?}`, but expected a type of kind `{:?}`", // TODO: pretty print
                    found, expected,
                ),
                labels: vec![
                    Label {
                        message: Some("the type".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },
            KindError::ExpectedType { ref found, span } => Diagnostic {
                severity: Severity::Error,
                message: format!("expected type, found value `{:?}`", found), // TODO: pretty print
                labels: vec![
                    Label {
                        message: Some("the value".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },
            KindError::Type(ref err) => err.to_diagnostic(),
        }
    }
}

impl From<TypeError> for KindError {
    fn from(src: TypeError) -> KindError {
        KindError::Type(src)
    }
}
