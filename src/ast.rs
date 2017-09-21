//! The syntax of our data description language

use std::fmt;

use source::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Type,
    Binary,
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Const {
    /// A boolean constant: eg. `true`, `false`
    Bool(bool),
    /// An integer constant: eg. `0`, `1`, `2`, ...
    UInt(u64),
}

impl fmt::Debug for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Const::Bool(value) => write!(f, "Bool({:?})", value),
            Const::UInt(value) => write!(f, "UInt({:?})", value),
        }
    }
}

/// An unary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Unop {
    /// Not: eg. `!x`
    Not,
    /// Negation: eg. `-x`
    Neg,
}

/// A binary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Binop {
    /// Disjunction: eg. `x | y`
    Or,
    /// Conjunction: eg. `x & y`
    And,
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
    /// Greater than or equal: eg. `x >= y`
    Ge,
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
    /// A constant value
    Const(Span, Const),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Span, String),
    /// An unary operator expression
    Unop(Span, Unop, Box<Expr>),
    /// A binary operator expression
    Binop(Span, Binop, Box<Expr>, Box<Expr>),
}

impl Expr {
    /// A boolean constant: eg. `true`, `false`
    pub fn bool<Sp>(span: Sp, value: bool) -> Expr
    where
        Sp: Into<Span>,
    {
        Expr::Const(span.into(), Const::Bool(value))
    }

    /// An integer constant: eg. `0`, `1`, `2`, ...
    pub fn uint<Sp>(span: Sp, value: u64) -> Expr
    where
        Sp: Into<Span>,
    {
        Expr::Const(span.into(), Const::UInt(value))
    }

    /// A variable, referring to an integer that exists in the current context:
    /// eg. `len`, `num_tables`
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// Boolean
    Bool,
    /// A singleton unsigned integer - matches exactly one integer
    ///
    /// This is usually produced by integer literals
    SingletonUInt(u64),
    /// Unsigned integer
    UInt(usize, Endianness),
    /// Signed integer
    SInt(usize, Endianness),
    /// IEEE 754 floating point
    Float(usize, Endianness),
    /// A type variable: eg. `T`
    Var(Span, String),
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Span, Box<Type>, Expr),
    /// A union of types: eg. `union { T, ... }`
    Union(Span, Vec<Type>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Span, Vec<Field>),
    /// A type constrained by a predicate: eg. `T where x => x == 3`
    Where(Span, Box<Type>, String, Expr),
}

impl Type {
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
    pub fn where_<Sp, T, S>(span: Sp, ty: T, param: S, pred: Expr) -> Type
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
