//! The surface syntax for the data description language.

use codespan_reporting::diagnostic::Diagnostic;
use std::ops::Range;
use std::sync::Arc;

use crate::diagnostics;
use crate::lexer::SpannedToken;
use crate::literal;

pub mod compile;
pub mod delaborate;
pub mod elaborate;
pub mod pretty;

#[allow(clippy::style, clippy::complexity, clippy::perf)]
mod grammar {
    include!(concat!(env!("OUT_DIR"), "/surface/grammar.rs"));
}

/// A module of items.
#[derive(Debug, Clone)]
pub struct Module {
    /// The file in which this module was defined.
    pub file_id: usize,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// The items in this module.
    pub items: Vec<Item>,
}

impl Module {
    pub fn parse(
        file_id: usize,
        tokens: impl IntoIterator<Item = Result<SpannedToken, Diagnostic<usize>>>,
        report: &mut dyn FnMut(Diagnostic<usize>),
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
    /// The full source range of this definition.
    pub range: Range<usize>,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: (Range<usize>, String),
    /// Optional type annotation
    pub ty: Option<Term>,
    /// Fields in the struct.
    pub term: Term,
}

/// A struct type definition.
#[derive(Debug, Clone)]
pub struct StructType {
    /// The full source range of this definition.
    pub range: Range<usize>,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: (Range<usize>, String),
    /// Fields in the struct.
    pub fields: Vec<TypeField>,
}

/// A field in a struct type definition.
#[derive(Debug, Clone)]
pub struct TypeField {
    pub doc: Arc<[String]>,
    pub name: (Range<usize>, String),
    pub term: Term,
}

/// Patterns.
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Named patterns.
    Name(Range<usize>, String),
    /// Numeric literals.
    NumberLiteral(Range<usize>, literal::Number),
}

impl Pattern {
    pub fn range(&self) -> Range<usize> {
        match self {
            Pattern::Name(range, _) | Pattern::NumberLiteral(range, _) => range.clone(),
        }
    }
}

/// Terms.
#[derive(Debug, Clone)]
pub enum Term {
    /// Annotated terms.
    Ann(Box<Term>, Box<Term>),
    /// Names.
    Name(Range<usize>, String),
    /// Type of types.
    TypeType(Range<usize>),
    /// Function types.
    FunctionType(Box<Term>, Box<Term>),
    /// Function eliminations (function application).
    FunctionElim(Box<Term>, Vec<Term>),
    /// Numeric literals.
    NumberLiteral(Range<usize>, literal::Number),
    /// If-else expressions.
    If(Range<usize>, Box<Term>, Box<Term>, Box<Term>),
    /// Match expressions.
    Match(Range<usize>, Box<Term>, Vec<(Pattern, Term)>),
    /// Type of format types.
    FormatType(Range<usize>),

    /// Error sentinel terms.
    Error(Range<usize>),
}

impl Term {
    pub fn range(&self) -> Range<usize> {
        match self {
            Term::Ann(term, ty) => term.range().start..ty.range().end,
            Term::Name(range, _)
            | Term::TypeType(range)
            | Term::NumberLiteral(range, _)
            | Term::If(range, _, _, _)
            | Term::Match(range, _, _)
            | Term::FormatType(range)
            | Term::Error(range) => range.clone(),
            Term::FunctionType(param_ty, body_ty) => param_ty.range().start..body_ty.range().end,
            Term::FunctionElim(head, arguments) => match arguments.last() {
                Some(argument) => head.range().start..argument.range().end,
                None => head.range(),
            },
        }
    }
}
