use codespan::ByteSpan;
use codespan_reporting::{Diagnostic, Label};

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
                Diagnostic::new_error(message).with_label(
                    Label::new_primary(var_span).with_message("not found in this scope"),
                )
            }
            TypeError::ExpectedExpr {
                ref found,
                var_span,
            } => {
                let message = format!("expected a value, but found a type `{:?}`", found);
                Diagnostic::new_error(message)
                    .with_label(Label::new_primary(var_span).with_message("the type"))
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

                Diagnostic::new_error(message)
                    .with_label(Label::new_primary(span).with_message("the term"))
            }
            TypeError::BinaryOperands {
                expr_span,
                binop,
                lhs_ty: _, // TODO
                rhs_ty: _, // TODO
            } => {
                let message = format!("mismatched arguments passed to operator `{:?}`", binop,);
                Diagnostic::new_error(message)
                    .with_label(Label::new_primary(expr_span).with_message("the expression"))
            }
            TypeError::MissingField {
                span, // TODO
                ref struct_ty,
                ref field_name,
            } => {
                let message = format!(
                    "no field `{}` found for an expression of type {:?}",
                    field_name, struct_ty,
                );

                Diagnostic::new_error(message)
                    .with_label(Label::new_primary(span).with_message("the expression"))
            }
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
                    .with_label(Label::new_primary(fn_span).with_message("the term"))
                    .with_label(Label::new_secondary(arg_span).with_message("the applied argument"))
            }
            TypeError::NegOnUnsigned {
                operand_span,
                ref operand_ty,
            } => {
                let message = format!(
                    "negated an operand of type `{:?}`, expected a float or unsigned integer",
                    operand_ty,
                );

                Diagnostic::new_error(message)
                    .with_label(Label::new_primary(operand_span).with_message("the operand"))
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
                    .with_label(Label::new_primary(target_span).with_message("the expression"))
                    .with_label(Label::new_secondary(index_span).with_message("the subscript"))
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
                    .with_label(Label::new_primary(index_span).with_message("the index expression"))
            }
            TypeError::InvalidCast {
                src_span,
                dst_span,
                ref src_ty,
                ref dst_ty,
            } => {
                let message = format!("invalid cast from `{:?}` to `{:?}`", src_ty, dst_ty,);
                Diagnostic::new_error(message)
                    .with_label(Label::new_primary(src_span).with_message("the source expression"))
                    .with_label(Label::new_secondary(dst_span).with_message("the type to cast to"))
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
                Diagnostic::new_error(message).with_label(
                    Label::new_primary(var_span).with_message("not found in this scope"),
                )
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

                Diagnostic::new_error(message)
                    .with_label(Label::new_primary(fn_span).with_message("the type"))
                    .with_labels(
                        arg_spans.iter().map(|&span| {
                            Label::new_secondary(span).with_message("applied argument")
                        }),
                    )
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

                Diagnostic::new_error(message)
                    .with_label(Label::new_primary(span).with_message("the type"))
            }
            KindError::ExpectedType { ref found, span } => {
                let message = format!("expected type, found value `{:?}`", found);
                Diagnostic::new_error(message)
                    .with_label(Label::new_primary(span).with_message("the value"))
            }
            KindError::UnexpectedArraySizeType {
                size_span,
                ref found,
            } => {
                let message = format!(
                    "found an array expression of type `{:?}`, but expected an unsigned integer",
                    found,
                );

                Diagnostic::new_error(message)
                    .with_label(Label::new_primary(size_span).with_message("the size expression"))
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
