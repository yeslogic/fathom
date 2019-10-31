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
    Var(String),

    If(Box<Term>, Box<Type>, Box<Type>),

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
    Bool,

    Rt(RtType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RtType {
    Either(Box<Type>, Box<Type>),
    U8,
    U16Le,
    U16Be,
    U32Le,
    U32Be,
    U64Le,
    U64Be,
    I8,
    I16Le,
    I16Be,
    I32Le,
    I32Be,
    I64Le,
    I64Be,
    F32Le,
    F32Be,
    F64Le,
    F64Be,
    InvalidDataDescription,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(String),

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
    Call(Box<Term>),
}
