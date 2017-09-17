//! The syntax of our data description language

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

/// An expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// An integer constant: eg. `0`, `1`, `2`, ...
    Const(Span, u64),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Span, String),
    /// Integer negation: eg. `-x`
    Neg(Span, Box<Expr>),
    /// Integer addition: eg. `x + y`
    Add(Span, Box<Expr>, Box<Expr>),
    /// Integer subtraction: eg. `x - y`
    Sub(Span, Box<Expr>, Box<Expr>),
    /// Integer multiplication: eg. `x * y`
    Mul(Span, Box<Expr>, Box<Expr>),
    /// Integer division: eg. `x / y`
    Div(Span, Box<Expr>, Box<Expr>),
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

    /// Integer negation: eg. `-x`
    pub fn neg<Sp, T>(span: Sp, x: T) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
    {
        Expr::Neg(span.into(), x.into())
    }

    /// Integer addition: eg. `x + y`
    pub fn add<Sp, T, U>(span: Sp, x: T, y: U) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
        U: Into<Box<Expr>>,
    {
        Expr::Add(span.into(), x.into(), y.into())
    }

    /// Integer subtraction: eg. `x - y`
    pub fn sub<Sp, T, U>(span: Sp, x: T, y: U) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
        U: Into<Box<Expr>>,
    {
        Expr::Sub(span.into(), x.into(), y.into())
    }

    /// Integer multiplication: eg. `x * y`
    pub fn mul<Sp, T, U>(span: Sp, x: T, y: U) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
        U: Into<Box<Expr>>,
    {
        Expr::Mul(span.into(), x.into(), y.into())
    }

    /// Integer division: eg. `x / y`
    pub fn div<Sp, T, U>(span: Sp, x: T, y: U) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
        U: Into<Box<Expr>>,
    {
        Expr::Div(span.into(), x.into(), y.into())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Endianness {
    Little,
    Big,
    Target,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeConst {
    /// Unsigned integer
    U(usize, Endianness),
    /// Signed integer
    I(usize, Endianness),
    /// IEEE 754 floating point
    F(usize, Endianness),
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
