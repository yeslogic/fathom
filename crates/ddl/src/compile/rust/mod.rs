use std::borrow::Cow;
use std::sync::Arc;

pub mod compile;
pub mod emit;

/// A module of items.
#[derive(Debug, Clone)]
pub struct Module {
    /// The items in this module.
    pub items: Vec<Item>,
}

/// Compiled items.
#[derive(Debug, Clone)]
pub enum Item {
    TypeAlias(TypeAlias),
    Struct(StructType),
}

/// Compiled type aliases.
#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub doc: Arc<[String]>,
    pub name: String,
    pub ty: Type,
}

/// Compiled structure types.
#[derive(Debug, Clone)]
pub struct StructType {
    pub doc: Arc<[String]>,
    pub name: String,
    pub fields: Vec<TypeField>,
}

/// Compiled type fields types.
#[derive(Debug, Clone)]
pub struct TypeField {
    doc: Arc<[String]>,
    name: String,
    format_ty: Type,
    host_ty: Type,
}

/// Compiled types.
#[derive(Debug, Clone)]
pub enum Type {
    Ident(Cow<'static, str>),
}
