//! The syntax of our data description language

use std::fmt;

use source::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Type,
}

/// A type definition
///
/// ```plain
/// Point = {
///     x : u16,
///     y : u16,
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition {
    pub span: Span,
    pub name: String,
    pub ty: Type,
}

impl Definition {
    pub fn new<Sp, S>(span: Sp, name: S, ty: Type) -> Definition
    where
        Sp: Into<Span>,
        S: Into<String>,
    {
        let span = span.into();
        let name = name.into();

        Definition { span, name, ty }
    }
}

/// A boolean unary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BoolUnop {
    /// Not: eg. `!x`
    Not,
}

/// A boolean binary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BoolBinop {
    /// Disjunction: eg. `x | y`
    Or,
    /// Conjunction: eg. `x & y`
    And,
}

/// A comparison operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Cmp {
    /// Equality: eg. `x == y`
    Eq,
    /// Inequality: eg. `x != y`
    Ne,
    /// Less than or equal: eg. `x <= y`
    Le,
    /// Less than: eg. `x < y`
    Lt,
    /// Greater than: eg. `x > y`
    Gt,
    /// Reater than or equal: eg. `x >= y`
    Ge,
}

/// A boolean expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoolExpr {
    /// A boolean constant: eg. `true`, `false`
    Const(Span, bool),
    Unop(Span, BoolUnop, Box<BoolExpr>),
    /// A binary operator expression
    Binop(Span, BoolBinop, Box<BoolExpr>, Box<BoolExpr>),
    /// A comparison operator expression
    Cmp(Span, Cmp, Box<Expr>, Box<Expr>),
}

impl BoolExpr {
    /// A boolean constant: eg. `true`, `false`
    pub fn const_<Sp>(span: Sp, value: bool) -> BoolExpr
    where
        Sp: Into<Span>,
    {
        BoolExpr::Const(span.into(), value)
    }

    /// A boolean binary operator
    pub fn unop<Sp, T>(span: Sp, op: BoolUnop, value: T) -> BoolExpr
    where
        Sp: Into<Span>,
        T: Into<Box<BoolExpr>>,
    {
        BoolExpr::Unop(span.into(), op, value.into())
    }

    /// A binary operator expression
    pub fn binop<Sp, T, U>(span: Sp, op: BoolBinop, lhs: T, rhs: U) -> BoolExpr
    where
        Sp: Into<Span>,
        T: Into<Box<BoolExpr>>,
        U: Into<Box<BoolExpr>>,
    {
        BoolExpr::Binop(span.into(), op, lhs.into(), rhs.into())
    }

    /// A comparison operator expression
    pub fn cmp<Sp, T, U>(span: Sp, op: Cmp, lhs: T, rhs: U) -> BoolExpr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
        U: Into<Box<Expr>>,
    {
        BoolExpr::Cmp(span.into(), op, lhs.into(), rhs.into())
    }
}

/// An unary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Unop {
    /// Negation: eg. `-x`
    Neg,
}

/// A binary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Binop {
    /// Addition: eg. `x + y`
    Add,
    /// Subtraction: eg. `x - y`
    Sub,
    /// Multiplication: eg. `x * y`
    Mul,
    /// Division: eg. `x / y`
    Div,
}

/// An expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// An integer constant: eg. `0`, `1`, `2`, ...
    Const(Span, u64),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Span, String),
    /// An unary operator expression
    Unop(Span, Unop, Box<Expr>),
    /// A binary operator expression
    Binop(Span, Binop, Box<Expr>, Box<Expr>),
}

impl Expr {
    /// An integer constant: eg. `0`, `1`, `2`, ...
    pub fn const_<Sp>(span: Sp, value: u64) -> Expr
    where
        Sp: Into<Span>,
    {
        Expr::Const(span.into(), value)
    }

    /// A variable, referring to an integer that exists in the current context: eg. `len`, `num_tables`
    pub fn var<Sp, S>(span: Sp, name: S) -> Expr
    where
        Sp: Into<Span>,
        S: Into<String>,
    {
        Expr::Var(span.into(), name.into())
    }

    /// An unary operator expression
    pub fn unop<Sp, T>(span: Sp, op: Unop, x: T) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
    {
        Expr::Unop(span.into(), op, x.into())
    }

    /// A binary operator expression
    pub fn binop<Sp, T, U>(span: Sp, op: Binop, x: T, y: U) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
        U: Into<Box<Expr>>,
    {
        Expr::Binop(span.into(), op, x.into(), y.into())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Endianness {
    Little,
    Big,
    Target,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum TypeConst {
    /// Unsigned integer
    U(usize, Endianness),
    /// Signed integer
    I(usize, Endianness),
    /// IEEE 754 floating point
    F(usize, Endianness),
}

impl fmt::Debug for TypeConst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TypeConst::U(b, e) => write!(f, "U({:?}, {:?})", b, e),
            TypeConst::I(b, e) => write!(f, "I({:?}, {:?})", b, e),
            TypeConst::F(b, e) => write!(f, "F({:?}, {:?})", b, e),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A type constant
    Const(Span, TypeConst),
    /// A type variable: eg. `T`
    Var(Span, String),
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Span, Box<Type>, Expr),
    /// A union of types: eg. `union { T, ... }`
    Union(Span, Vec<Type>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Span, Vec<Field>),
    /// A type constrained by a predicate: eg. `T where x => x == 3`
    Where(Span, Box<Type>, String, BoolExpr),
}

impl Type {
    /// A unsigned integer type
    pub fn u<Sp>(span: Sp, bytes: usize, endianness: Endianness) -> Type
    where
        Sp: Into<Span>,
    {
        Type::Const(span.into(), TypeConst::U(bytes, endianness))
    }

    /// A signed integer type
    pub fn i<Sp>(span: Sp, bytes: usize, endianness: Endianness) -> Type
    where
        Sp: Into<Span>,
    {
        Type::Const(span.into(), TypeConst::I(bytes, endianness))
    }

    /// An IEEE 754 floating point type
    pub fn f<Sp>(span: Sp, bytes: usize, endianness: Endianness) -> Type
    where
        Sp: Into<Span>,
    {
        Type::Const(span.into(), TypeConst::F(bytes, endianness))
    }

    /// A type variable: eg. `T`
    pub fn var<Sp, S>(span: Sp, name: S) -> Type
    where
        Sp: Into<Span>,
        S: Into<String>,
    {
        Type::Var(span.into(), name.into())
    }

    /// An array of the specified type, with a size: eg. `[T; n]`
    pub fn array<Sp, T>(span: Sp, ty: T, size: Expr) -> Type
    where
        Sp: Into<Span>,
        T: Into<Box<Type>>,
    {
        Type::Array(span.into(), ty.into(), size)
    }

    /// A union of types: eg. `union { T, ... }`
    pub fn union<Sp>(span: Sp, tys: Vec<Type>) -> Type
    where
        Sp: Into<Span>,
    {
        Type::Union(span.into(), tys)
    }

    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_<Sp>(span: Sp, fields: Vec<Field>) -> Type
    where
        Sp: Into<Span>,
    {
        Type::Struct(span.into(), fields)
    }

    /// A type constrained by a predicate: eg. `T where x => x == 3`
    pub fn where_<Sp, T, S>(span: Sp, ty: T, param: S, pred: BoolExpr) -> Type
    where
        Sp: Into<Span>,
        T: Into<Box<Type>>,
        S: Into<String>,
    {
        Type::Where(span.into(), ty.into(), param.into(), pred)
    }
}

/// A field in a struct type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub span: Span,
    pub name: String,
    pub ty: Type,
}

impl Field {
    pub fn new<Sp, S>(span: Sp, name: S, ty: Type) -> Field
    where
        Sp: Into<Span>,
        S: Into<String>,
    {
        let span = span.into();
        let name = name.into();

        Field { span, name, ty }
    }
}
