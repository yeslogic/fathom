//! The syntax of our data description language

use source::Span;

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
    pub fn new<Sp: Into<Span>, S: Into<String>>(span: Sp, name: S, ty: Type) -> Definition {
        let span = span.into();
        let name = name.into();
        Definition { span, name, ty }
    }
}

/// An integer expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntExpr {
    /// An integer constant: `0`, `1`, `2`, ...
    Const(Span, u32),
    /// An variable, referring to an integer that exists in the current context: `len`, `num_tables`
    Var(Span, String),
}

impl IntExpr {
    pub fn const_<Sp: Into<Span>>(span: Sp, value: u32) -> IntExpr {
        IntExpr::Const(span.into(), value)
    }

    pub fn var<Sp: Into<Span>, S: Into<String>>(span: Sp, name: S) -> IntExpr {
        IntExpr::Var(span.into(), name.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A type identifier: `T1`, `u16be`, `u32`
    Ident(Span, String),
    /// An array of the specified type, with a size: `[T; n]`
    Array(Span, Box<Type>, IntExpr),
    /// A union of two types: `T1 | T2`
    Union(Span, Vec<Type>),
    /// A struct type, with fields: `{ field : T, ... }`
    Struct(Span, Vec<Field>),
}

impl Type {
    pub fn ident<Sp: Into<Span>, S: Into<String>>(span: Sp, name: S) -> Type {
        Type::Ident(span.into(), name.into())
    }

    pub fn array<Sp: Into<Span>, T: Into<Box<Type>>>(span: Sp, ty: T, size: IntExpr) -> Type {
        Type::Array(span.into(), ty.into(), size)
    }

    pub fn union<Sp: Into<Span>>(span: Sp, tys: Vec<Type>) -> Type {
        Type::Union(span.into(), tys)
    }

    pub fn struct_<Sp: Into<Span>>(span: Sp, fields: Vec<Field>) -> Type {
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
    pub fn new<Sp: Into<Span>, S: Into<String>>(span: Sp, name: S, ty: Type) -> Field {
        let span = span.into();
        let name = name.into();
        Field { span, name, ty }
    }
}
