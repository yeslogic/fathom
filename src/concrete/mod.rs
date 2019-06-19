//! The concrete syntax for the data description language.

use codespan::{FileId, Span};
use std::rc::Rc;

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
    ///
    /// ```text
    /// struct <name> {}
    /// ```
    Struct {
        /// The full span of this definition.
        span: Span,
        /// Doc comment.
        doc: Rc<str>,
        /// Name of this definition.
        name: String,
    },
}
