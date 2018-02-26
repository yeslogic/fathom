use codespan::ByteSpan;
use codespan_reporting::Diagnostic;

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
        expr_span: ByteSpan,
        binop: Binop,
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
            TypeError::UndefinedName { ref name, var_span } => {
                Diagnostic::new_error(format!("cannot find `{}` in scope", name))
                    .with_primary_label(var_span, "not found in this scope")
            }
            TypeError::ExpectedExpr {
                ref found,
                var_span,
            } => Diagnostic::new_error(format!("expected a value, but found a type `{:?}`", found))
                .with_primary_label(var_span, "the type"),
            // TypeError::Mismatch {
            //     span,
            //     ref found,
            //     ref expected,
            // } => Diagnostic::new_error(format!(
            //     "found an expression of type `{:?}`, but expected an expression of type `{:?}`",
            //     found, expected,
            // )).with_primary_label(span, "the term"),
            TypeError::InferenceMismatch {
                span,
                ref found,
                ref expected,
            } => Diagnostic::new_error(format!(
                "found an expression of type `{:?}`, but expected an expression of type `{:?}`",
                found, expected,
            )).with_primary_label(span, "the term"),
            TypeError::BinaryOperands {
                expr_span,
                binop,
                lhs_ty: _, // TODO
                rhs_ty: _, // TODO
            } => Diagnostic::new_error(format!(
                "mismatched arguments passed to operator `{:?}`",
                binop,
            )).with_primary_label(expr_span, "the expression"),
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
        match *self {
            KindError::UndefinedName { ref name, var_span } => {
                Diagnostic::new_error(format!("cannot find `{}` in scope", name))
                    .with_primary_label(var_span, "not found in this scope")
            }
            KindError::NotATypeConstructor {
                fn_span,
                ref arg_spans,
                ref found,
            } => {
                let mut diagnostic = Diagnostic::new_error(format!(
                    "applied {} arguments to a type that was not expecting them - found `{:?}`",
                    arg_spans.len(),
                    found,
                )).with_primary_label(fn_span, "the type");
                for &span in arg_spans {
                    diagnostic = diagnostic.with_secondary_label(span, "applied argument");
                }
                diagnostic
            }
            KindError::Mismatch {
                span,
                ref found,
                ref expected,
            } => Diagnostic::new_error(format!(
                "found a type of kind `{:?}`, but expected a type of kind `{:?}`",
                found, expected,
            )).with_primary_label(span, "the type"),
            KindError::ExpectedType { ref found, span } => {
                Diagnostic::new_error(format!("expected type, found value `{:?}`", found))
                    .with_primary_label(span, "the value")
            }
            KindError::Type(ref err) => err.to_diagnostic(),
        }
    }
}

impl From<TypeError> for KindError {
    fn from(src: TypeError) -> KindError {
        KindError::Type(src)
    }
}
