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
    /// A variable of the requested name was not bound in this scope
    UnboundVariable { expr: RcExpr, name: Name },
    /// Variable bound in the context was not at the value level
    ExprBindingExpected { expr: RcExpr, found: Scope },
    /// One type was expected, but another was found
    Mismatch {
        expr: RcExpr,
        found: RcType,
        expected: ExpectedType,
    },
    /// One type was expected, but another was found
    InferenceMismatch {
        expr: RcExpr,
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

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedKind {
    Arrow,
    Actual(RcKind),
}

/// An error that was encountered during kind checking
#[derive(Debug, Clone, PartialEq)]
pub enum KindError {
    /// A variable of the requested name was not bound in this scope
    UnboundVariable { ty: RcType, name: Name },
    /// Variable bound in the context was not at the type level
    TypeBindingExpected { ty: RcType, found: Scope },
    /// One kind was expected, but another was found
    Mismatch {
        ty: RcType,
        expected: ExpectedKind,
        found: RcKind,
    },
    /// A type error
    Type(TypeError),
}

impl From<TypeError> for KindError {
    fn from(src: TypeError) -> KindError {
        KindError::Type(src)
    }
}
