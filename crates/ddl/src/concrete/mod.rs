//! The concrete syntax for the data description language.

use codespan::{ByteIndex, ByteOffset, FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use std::sync::Arc;

use crate::diagnostics;
use crate::lexer::SpannedToken;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/concrete/grammar.rs"));
}

/// A module of items.
#[derive(Debug, Clone)]
pub struct Module {
    /// The file in which this module was defined.
    pub file_id: FileId,
    /// The items in this module.
    pub items: Vec<Item>,
}

impl Module {
    pub fn parse(
        file_id: FileId,
        tokens: impl IntoIterator<Item = Result<SpannedToken, Diagnostic>>,
        report: &mut dyn FnMut(Diagnostic),
    ) -> Module {
        grammar::ModuleParser::new()
            .parse(file_id, report, tokens)
            .unwrap_or_else(|error| {
                report(diagnostics::parse_error(file_id, error));
                Module {
                    file_id,
                    items: Vec::new(),
                }
            })
    }
}

/// Items in a module.
#[derive(Debug, Clone)]
pub enum Item {
    /// Struct definitions.
    ///
    /// ```text
    /// struct <name> {}
    /// ```
    Struct(StructType),
}

/// A struct type definition.
#[derive(Debug, Clone)]
pub struct StructType {
    /// The full span of this definition.
    pub span: Span,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: SpannedString,
    /// Fields in the struct.
    pub fields: Vec<TypeField>,
}

/// A field in a struct type definition.
#[derive(Debug, Clone)]
pub struct TypeField {
    pub doc: Arc<[String]>,
    pub name: SpannedString,
    pub term: Term,
}

/// Terms.
#[derive(Debug, Clone)]
pub enum Term {
    /// Variables.
    Var(SpannedString),

    /// Error sentinel terms.
    Error(Span),
}

impl Term {
    pub fn span(&self) -> Span {
        match self {
            Term::Var(name) => name.span(),
            Term::Error(span) => *span,
        }
    }
}

/// A string that is located in a source file.
#[derive(Debug, Clone, derive_more::Display)]
#[display(fmt = "{}", inner)]
pub struct SpannedString {
    pub start: ByteIndex,
    pub inner: String,
}

impl SpannedString {
    pub fn new(start: impl Into<ByteIndex>, inner: impl Into<String>) -> SpannedString {
        SpannedString {
            start: start.into(),
            inner: inner.into(),
        }
    }

    pub fn span(&self) -> Span {
        Span::new(
            self.start,
            self.start + ByteOffset::from_str_len(&self.inner),
        )
    }

    pub fn as_str(&self) -> &str {
        &self.inner
    }
}
