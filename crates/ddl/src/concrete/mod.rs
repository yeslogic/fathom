//! The concrete syntax for the data description language.

use codespan::{FileId, Span};
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
                report(diagnostics::error::parse(file_id, error));
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
    /// Alias definitions.
    ///
    /// ```text
    /// alias <name> = <term>;
    /// ```
    Alias(Alias),
}

/// A struct type definition.
#[derive(Debug, Clone)]
pub struct StructType {
    /// The full span of this definition.
    pub span: Span,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: (Span, String),
    /// Fields in the struct.
    pub fields: Vec<TypeField>,
}

/// Alias definition.
#[derive(Debug, Clone)]
pub struct Alias {
    /// The full span of this definition.
    pub span: Span,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: (Span, String),
    /// Optional type annotation
    pub ty: Option<Term>,
    /// Fields in the struct.
    pub term: Term,
}

/// A field in a struct type definition.
#[derive(Debug, Clone)]
pub struct TypeField {
    pub doc: Arc<[String]>,
    pub name: (Span, String),
    pub term: Term,
}

/// Terms.
#[derive(Debug, Clone)]
pub enum Term {
    /// Parenthesised expressions.
    Paren(Span, Box<Term>),
    /// Annotated terms.
    Ann(Box<Term>, Box<Term>),
    /// Variables.
    Var(Span, String),

    /// Error sentinel terms.
    Error(Span),
}

impl Term {
    pub fn span(&self) -> Span {
        match self {
            Term::Ann(term, ty) => Span::merge(term.span(), ty.span()),
            Term::Paren(span, _) | Term::Var(span, _) | Term::Error(span) => *span,
        }
    }
}
