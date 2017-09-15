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
    Const(Span, u32),
    /// An variable, referring to an integer that exists in the current context: `len`, `num_tables`
    Var(Span, String),
}

impl Expr {
    pub fn const_<Sp>(span: Sp, value: u32) -> Expr
    where
        Sp: Into<Span>,
    {
        Expr::Const(span.into(), value)
    }

    pub fn var<Sp, S>(span: Sp, name: S) -> Expr
    where
        Sp: Into<Span>,
        S: Into<String>,
    {
        Expr::Var(span.into(), name.into())
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A type constant
    Const(Span, TypeConst),
    /// A type identifier: `T1`, `u16be`, `u32`
    Ident(Span, String),
    /// An array of the specified type, with a size: `[T; n]`
    Array(Span, Box<Type>, Expr),
    /// A union of two types: `T1 | T2`
    Union(Span, Vec<Type>),
    /// A struct type, with fields: `{ field : T, ... }`
    Struct(Span, Vec<Field>),
}

impl Type {
    pub fn const_<Sp>(span: Sp, ty: TypeConst) -> Type
    where
        Sp: Into<Span>,
    {
        Type::Const(span.into(), ty)
    }

    pub fn ident<Sp, S>(span: Sp, name: S) -> Type
    where
        Sp: Into<Span>,
        S: Into<String>,
    {
        Type::Ident(span.into(), name.into())
    }

    pub fn array<Sp, T>(span: Sp, ty: T, size: Expr) -> Type
    where
        Sp: Into<Span>,
        T: Into<Box<Type>>,
    {
        Type::Array(span.into(), ty.into(), size)
    }

    pub fn union<Sp>(span: Sp, tys: Vec<Type>) -> Type
    where
        Sp: Into<Span>,
    {
        Type::Union(span.into(), tys)
    }

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
