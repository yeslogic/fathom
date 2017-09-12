//! The syntax of our data description language

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
    pub name: String,
    pub ty: Type,
}

impl Definition {
    pub fn new<S: Into<String>>(name: S, ty: Type) -> Definition {
        Definition {
            name: name.into(),
            ty,
        }
    }
}

/// An integer expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntExpr {
    /// An integer constant: `0`, `1`, `2`, ...
    Const(u32),
    /// An variable, referring to an integer that exists in the current context: `len`, `num_tables`
    Var(String),
}

impl IntExpr {
    pub fn var<S: Into<String>>(name: S) -> IntExpr {
        IntExpr::Var(name.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A type identifier: `T1`, `u16be`, `u32`
    Ident(String),
    /// An array of the specified type, with a size: `[T; n]`
    Array(Box<Type>, IntExpr),
    /// A union of two types: `T1 | T2`
    Union(Vec<Type>),
    /// A struct type, with fields: `{ field : T, ... }`
    Struct(Vec<Field>),
}

impl Type {
    pub fn ident<S: Into<String>>(name: S) -> Type {
        Type::Ident(name.into())
    }
}

/// A field in a struct type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

impl Field {
    pub fn new<S: Into<String>>(name: S, ty: Type) -> Field {
        Field {
            name: name.into(),
            ty,
        }
    }
}
