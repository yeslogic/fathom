use codespan::ByteSpan;
use codespan_reporting::Diagnostic;

use name::{Ident, Name};
use syntax::core::{Binop, RcKind, RcType, Scope};

/// An error that was encountered during type checking
#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UndefinedName {
        var_span: ByteSpan,
        name: Name,
    },
    ExpectedExpr {
        var_span: ByteSpan,
        found: Scope,
    },
    Mismatch {
        span: ByteSpan,
        found: RcType,
        expected: RcType,
    },
    BinaryOperands {
        expr_span: ByteSpan,
        binop: Binop,
        lhs_ty: RcType,
        rhs_ty: RcType,
    },
    MissingField {
        span: ByteSpan,
        struct_ty: RcType,
        field_name: Ident,
    },
    MissingVariant {
        span: ByteSpan,
        union_ty: RcType,
        variant_name: Ident,
    },
    ArgsAppliedToNonFunction {
        fn_span: ByteSpan,
        arg_span: ByteSpan,
        found: RcType,
    },
    SubscriptOnNonArray {
        index_span: ByteSpan,
        target_span: ByteSpan,
        target_ty: RcType,
    },
    NegOnUnsigned {
        operand_span: ByteSpan,
        operand_ty: RcType,
    },
    UnexpectedIndexType {
        index_span: ByteSpan,
        found: RcType,
    },
    InvalidCast {
        src_span: ByteSpan,
        dst_span: ByteSpan,
        src_ty: RcType,
        dst_ty: RcType,
    },
}

impl TypeError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            TypeError::UndefinedName { ref name, var_span } => {
                let message = format!("cannot find `{}` in scope", name);
                Diagnostic::new_error(message)
                    .with_primary_label(var_span, "not found in this scope")
            }
            TypeError::ExpectedExpr {
                ref found,
                var_span,
            } => {
                let message = format!("expected a value, but found a type `{:?}`", found);
                Diagnostic::new_error(message).with_primary_label(var_span, "the type")
            }
            TypeError::Mismatch {
                span,
                ref found,
                ref expected,
            } => {
                let message = format!(
                    "found an expression of type `{:?}`, but expected an expression of type `{:?}`",
                    found, expected,
                );

                Diagnostic::new_error(message).with_primary_label(span, "the term")
            }
            TypeError::BinaryOperands {
                expr_span,
                binop,
                lhs_ty: _, // TODO
                rhs_ty: _, // TODO
            } => {
                let message = format!("mismatched arguments passed to operator `{:?}`", binop,);
                Diagnostic::new_error(message).with_primary_label(expr_span, "the expression")
            }
            TypeError::MissingField {
                span: _,       // TODO
                struct_ty: _,  // TODO
                field_name: _, // TODO
            } => unimplemented!(),
            TypeError::MissingVariant {
                span: _,         // TODO
                union_ty: _,     // TODO
                variant_name: _, // TODO
            } => unimplemented!(),
            TypeError::ArgsAppliedToNonFunction {
                fn_span,
                arg_span,
                ref found,
            } => {
                let message = format!(
                    "applied an argument to a term of type `{:?}`, expected a function",
                    found,
                );

                Diagnostic::new_error(message)
                    .with_primary_label(fn_span, "the term")
                    .with_secondary_label(arg_span, "the applied argument")
            }
            TypeError::NegOnUnsigned {
                operand_span,
                ref operand_ty,
            } => {
                let message = format!(
                    "negated an operand of type `{:?}`, expected a float or unsigned integer",
                    operand_ty,
                );

                Diagnostic::new_error(message).with_primary_label(operand_span, "the operand")
            }
            TypeError::SubscriptOnNonArray {
                index_span,
                target_span,
                ref target_ty,
            } => {
                let message = format!(
                    "attempted subscript an expression that was not an array, found: `{:?}`",
                    target_ty,
                );

                Diagnostic::new_error(message)
                    .with_primary_label(target_span, "the expression")
                    .with_secondary_label(index_span, "the subscript")
            }
            TypeError::UnexpectedIndexType {
                index_span,
                ref found,
            } => {
                let message = format!(
                    "found an index expression of type `{:?}`, but expected an unsigned integer",
                    found,
                );

                Diagnostic::new_error(message)
                    .with_primary_label(index_span, "the index expression")
            }
            TypeError::InvalidCast {
                src_span,
                dst_span,
                ref src_ty,
                ref dst_ty,
            } => {
                let message = format!("invalid cast from `{:?}` to `{:?}`", src_ty, dst_ty,);
                Diagnostic::new_error(message)
                    .with_primary_label(src_span, "the source expression")
                    .with_secondary_label(dst_span, "the type to cast to")
            }
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
    UnexpectedArraySizeType {
        size_span: ByteSpan,
        found: RcType,
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
                let message = format!("cannot find `{}` in scope", name);
                Diagnostic::new_error(message)
                    .with_primary_label(var_span, "not found in this scope")
            }
            KindError::NotATypeConstructor {
                fn_span,
                ref arg_spans,
                ref found,
            } => {
                let message = format!(
                    "applied {} arguments to a type that was not expecting them - found `{:?}`",
                    arg_spans.len(),
                    found,
                );

                let mut diagnostic =
                    Diagnostic::new_error(message).with_primary_label(fn_span, "the type");
                for &span in arg_spans {
                    diagnostic = diagnostic.with_secondary_label(span, "applied argument");
                }

                diagnostic
            }
            KindError::Mismatch {
                span,
                ref found,
                ref expected,
            } => {
                let message = format!(
                    "found a type of kind `{:?}`, but expected a type of kind `{:?}`",
                    found, expected,
                );

                Diagnostic::new_error(message).with_primary_label(span, "the type")
            }
            KindError::ExpectedType { ref found, span } => {
                let message = format!("expected type, found value `{:?}`", found);
                Diagnostic::new_error(message).with_primary_label(span, "the value")
            }
            KindError::UnexpectedArraySizeType {
                size_span,
                ref found,
            } => {
                let message = format!(
                    "found an array expression of type `{:?}`, but expected an unsigned integer",
                    found,
                );

                Diagnostic::new_error(message).with_primary_label(size_span, "the size expression")
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
