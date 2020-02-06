//! The surface syntax for the data description language.

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use std::sync::Arc;

use crate::diagnostics;
use crate::lexer::SpannedToken;
use crate::literal;

pub mod compile;
pub mod delaborate;
pub mod elaborate;
pub mod pretty;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/surface/grammar.rs"));
}

/// A module of items.
#[derive(Debug, Clone)]
pub struct Module {
    /// The file in which this module was defined.
    pub file_id: FileId,
    /// Doc comment.
    pub doc: Arc<[String]>,
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
                    doc: Arc::new([]),
                    items: Vec::new(),
                }
            })
    }
}

/// Items in a module.
#[derive(Debug, Clone)]
pub enum Item {
    /// Alias definitions.
    ///
    /// ```text
    /// alias <name> = <term>;
    /// ```
    Alias(Alias),
    /// Struct definitions.
    ///
    /// ```text
    /// struct <name> {}
    /// ```
    Struct(StructType),
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

/// A field in a struct type definition.
#[derive(Debug, Clone)]
pub struct TypeField {
    pub doc: Arc<[String]>,
    pub name: (Span, String),
    pub term: Term,
}

/// Patterns.
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Named patterns.
    Name(Span, String),
    /// Numeric literals.
    NumberLiteral(Span, literal::Number),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Name(span, _) | Pattern::NumberLiteral(span, _) => *span,
        }
    }
}

/// Terms.
#[derive(Debug, Clone)]
pub enum Term {
    /// Annotated terms.
    Ann(Box<Term>, Box<Term>),
    /// Names.
    Name(Span, String),
    /// Type of format types.
    Format(Span),
    /// Type of host types.
    Host(Span),
    /// The type of types.
    Kind(Span),
    /// Function types.
    FunctionType(Box<Term>, Box<Term>),
    /// Function eliminations (function application).
    FunctionElim(Box<Term>, Vec<Term>),
    /// Numeric literals.
    NumberLiteral(Span, literal::Number),
    /// If-else expressions.
    If(Span, Box<Term>, Box<Term>, Box<Term>),
    /// Match expressions.
    Match(Span, Box<Term>, Vec<(Pattern, Term)>),

    /// Error sentinel terms.
    Error(Span),
}

impl Term {
    pub fn span(&self) -> Span {
        match self {
            Term::Ann(term, ty) => Span::merge(term.span(), ty.span()),
            Term::Name(span, _)
            | Term::Format(span)
            | Term::Host(span)
            | Term::Kind(span)
            | Term::NumberLiteral(span, _)
            | Term::If(span, _, _, _)
            | Term::Match(span, _, _)
            | Term::Error(span) => *span,
            Term::FunctionType(param_ty, body_ty) => Span::merge(param_ty.span(), body_ty.span()),
            Term::FunctionElim(head, arguments) => match arguments.last() {
                Some(argument) => Span::merge(head.span(), argument.span()),
                None => head.span(),
            },
        }
    }
}
