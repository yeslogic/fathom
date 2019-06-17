//! The concrete syntax for the data description language.

#![warn(rust_2018_idioms)]

use codespan::Span;

/// A module of items.
#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Item>,
}

/// Items in a module.
#[derive(Debug, Clone)]
pub enum Item {
    /// Struct definitions.
    ///
    /// ```text
    /// struct <name> {}
    /// ```
    Struct(Span, String, String),
}
