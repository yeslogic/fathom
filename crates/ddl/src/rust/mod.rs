use std::borrow::Cow;
use std::sync::Arc;

pub mod emit;

/// A module of items.
#[derive(Debug, Clone)]
pub struct Module {
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// The items in this module.
    pub items: Vec<Item>,
}

/// Compiled items.
#[derive(Debug, Clone)]
pub enum Item {
    Const(Const),
    Function(Function),
    Alias(Alias),
    Struct(StructType),
}

/// Compiled constants.
#[derive(Debug, Clone)]
pub struct Const {
    pub doc: Arc<[String]>,
    pub name: String,
    pub ty: Type,
    pub term: Term,
}

/// Compiled constants.
#[derive(Debug, Clone)]
pub struct Function {
    pub doc: Arc<[String]>,
    pub is_const: bool,
    pub name: String,
    pub ty: Type,
    pub term: Term,
}

/// Compiled type aliases.
#[derive(Debug, Clone)]
pub struct Alias {
    pub doc: Arc<[String]>,
    pub name: String,
    pub ty: Type,
}

/// Compiled structure types.
#[derive(Debug, Clone)]
pub struct StructType {
    pub derives: Vec<String>,
    pub doc: Arc<[String]>,
    pub name: String,
    pub fields: Vec<TypeField>,
}

/// Compiled type fields types.
#[derive(Debug, Clone)]
pub struct TypeField {
    pub doc: Arc<[String]>,
    pub name: String,
    pub format_ty: Type,
    pub host_ty: Type,
    pub by_ref: bool,
}

/// Compiled types.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Name(Cow<'static, str>, Vec<Type>),
    If(Box<Term>, Box<Type>, Box<Type>),
}

impl Type {
    pub fn name(name: impl Into<Cow<'static, str>>, arguments: Vec<Type>) -> Type {
        Type::Name(name.into(), arguments)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Name(Cow<'static, str>),
    Panic(Cow<'static, str>),

    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),

    If(Box<Term>, Box<Term>, Box<Term>),
    Match(Box<Term>, Vec<(Pattern, Term)>),
    Call(Box<Term>),
}

impl Term {
    pub fn name(name: impl Into<Cow<'static, str>>) -> Term {
        Term::Name(name.into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Name(Cow<'static, str>),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
}

impl Pattern {
    pub fn name(name: impl Into<Cow<'static, str>>) -> Pattern {
        Pattern::Name(name.into())
    }
}
