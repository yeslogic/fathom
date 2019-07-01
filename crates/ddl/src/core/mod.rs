//! The core type theory of the data description language.

use codespan::{ByteIndex, FileId, Span};
use std::rc::Rc;

pub mod validate;

/// A label.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display)]
pub struct Label(pub String);

/// A module of items.
#[derive(Debug, Clone)]
pub struct Module {
    /// The file in which this module was defined.
    pub file_id: FileId,
    /// The items in this module.
    pub items: Vec<Item>,
}

/// Items in a module.
#[derive(Debug, Clone)]
pub enum Item {
    /// Struct definitions.
    Struct(StructType),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Struct(struct_ty) => struct_ty.span,
        }
    }
}

/// A struct type definition.
#[derive(Debug, Clone)]
pub struct StructType {
    /// The full span of this definition.
    pub span: Span,
    /// Doc comment.
    pub doc: Rc<str>,
    /// Name of this definition.
    pub name: Label,
    /// Fields in the struct.
    pub fields: Vec<TypeField>,
}

/// A field in a struct type definition.
#[derive(Debug, Clone)]
pub struct TypeField {
    pub doc: Rc<str>,
    pub start: ByteIndex,
    pub name: Label,
    pub term: Term,
}

impl TypeField {
    pub fn span(&self) -> Span {
        Span::new(self.start, self.term.span().end())
    }
}

/// Terms.
#[derive(Debug, Clone)]
pub enum Term {
    /// Unsigned 8-bit integers.
    U8(Span),
    /// Unsigned 16-bit integers (little endian).
    U16Le(Span),
    /// Unsigned 16-bit integers (big endian).
    U16Be(Span),
    /// Unsigned 32-bit integers (little endian).
    U32Le(Span),
    /// Unsigned 32-bit integers (big endian).
    U32Be(Span),
    /// Unsigned 64-bit integers (little endian).
    U64Le(Span),
    /// Unsigned 64-bit integers (big endian).
    U64Be(Span),
    /// Signed, two's complement 8-bit integers.
    S8(Span),
    /// Signed, two's complement 16-bit integers (little endian).
    S16Le(Span),
    /// Signed, two's complement 16-bit integers (big endian).
    S16Be(Span),
    /// Signed, two's complement 32-bit integers (little endian).
    S32Le(Span),
    /// Signed, two's complement 32-bit integers (big endian).
    S32Be(Span),
    /// Signed, two's complement 64-bit integers (little endian).
    S64Le(Span),
    /// Signed, two's complement 64-bit integers (big endian).
    S64Be(Span),
    /// IEEE754 single-precision floating point numbers (little endian).
    F32Le(Span),
    /// IEEE754 single-precision floating point numbers (big endian).
    F32Be(Span),
    /// IEEE754 double-precision floating point numbers (little endian).
    F64Le(Span),
    /// IEEE754 double-precision floating point numbers (big endian).
    F64Be(Span),
    /// Error sentinel.
    Error(Span),
}

impl Term {
    pub fn span(&self) -> Span {
        match self {
            Term::U8(span)
            | Term::U16Le(span)
            | Term::U16Be(span)
            | Term::U32Le(span)
            | Term::U32Be(span)
            | Term::U64Le(span)
            | Term::U64Be(span)
            | Term::S8(span)
            | Term::S16Le(span)
            | Term::S16Be(span)
            | Term::S32Le(span)
            | Term::S32Be(span)
            | Term::S64Le(span)
            | Term::S64Be(span)
            | Term::F32Le(span)
            | Term::F32Be(span)
            | Term::F64Le(span)
            | Term::F64Be(span)
            | Term::Error(span) => *span,
        }
    }
}
