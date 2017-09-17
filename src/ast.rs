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
    /// An integer constant: `0`, `1`, `2`, ...
    Const(Span, u64),
    /// A variable, referring to an integer that exists in the current context: `len`, `num_tables`
    Var(Span, String),
    /// Integer negation: `-x`
    Neg(Span, Box<Expr>),
    /// Integer addition: `x + y`
    Add(Span, Box<Expr>, Box<Expr>),
    /// Integer subtraction: `x - y`
    Sub(Span, Box<Expr>, Box<Expr>),
    /// Integer multiplication: `x * y`
    Mul(Span, Box<Expr>, Box<Expr>),
    /// Integer division: `x / y`
    Div(Span, Box<Expr>, Box<Expr>),
}

impl Expr {
    /// An integer constant: `0`, `1`, `2`, ...
    pub fn const_<Sp>(span: Sp, value: u64) -> Expr
    where
        Sp: Into<Span>,
    {
        Expr::Const(span.into(), value)
    }

    /// A variable, referring to an integer that exists in the current context: `len`, `num_tables`
    pub fn var<Sp, S>(span: Sp, name: S) -> Expr
    where
        Sp: Into<Span>,
        S: Into<String>,
    {
        Expr::Var(span.into(), name.into())
    }

    /// Integer negation: `-x`
    pub fn neg<Sp, T>(span: Sp, x: T) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
    {
        Expr::Neg(span.into(), x.into())
    }

    /// Integer addition: `x + y`
    pub fn add<Sp, T, U>(span: Sp, x: T, y: U) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
        U: Into<Box<Expr>>,
    {
        Expr::Add(span.into(), x.into(), y.into())
    }

    /// Integer subtraction: `x - y`
    pub fn sub<Sp, T, U>(span: Sp, x: T, y: U) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
        U: Into<Box<Expr>>,
    {
        Expr::Sub(span.into(), x.into(), y.into())
    }

    /// Integer multiplication: `x * y`
    pub fn mul<Sp, T, U>(span: Sp, x: T, y: U) -> Expr
    where
        Sp: Into<Span>,
        T: Into<Box<Expr>>,
        U: Into<Box<Expr>>,
    {
        Expr::Mul(span.into(), x.into(), y.into())
    }

    /// Integer division: `x / y`
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
pub enum TypeConst {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A type constant
    Const(Span, TypeConst),
    /// A type identifier: `T1`, `u16be`, `u32`
    Ident(Span, String),
    /// An array of the specified type, with a size: `[T; n]`
    Array(Span, Box<Type>, Expr),
    /// A union of types: `union { T, ... }`
    Union(Span, Vec<Type>),
    /// A struct type, with fields: `struct { field : T, ... }`
    Struct(Span, Vec<Field>),
}

impl Type {
    /// A type constant
    pub fn const_<Sp>(span: Sp, ty: TypeConst) -> Type
    where
        Sp: Into<Span>,
    {
        Type::Const(span.into(), ty)
    }

    /// A type identifier: `T1`, `u16be`, `u32`
    pub fn ident<Sp, S>(span: Sp, name: S) -> Type
    where
        Sp: Into<Span>,
        S: Into<String>,
    {
        Type::Ident(span.into(), name.into())
    }

    /// An array of the specified type, with a size: `[T; n]`
    pub fn array<Sp, T>(span: Sp, ty: T, size: Expr) -> Type
    where
        Sp: Into<Span>,
        T: Into<Box<Type>>,
    {
        Type::Array(span.into(), ty.into(), size)
    }

    /// A union of types: `union { T, ... }`
    pub fn union<Sp>(span: Sp, tys: Vec<Type>) -> Type
    where
        Sp: Into<Span>,
    {
        Type::Union(span.into(), tys)
    }

    /// A struct type, with fields: `struct { field : T, ... }`
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
