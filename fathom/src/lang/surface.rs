//! The surface syntax for Fathom.

use codespan_reporting::diagnostic::Diagnostic;
use std::sync::Arc;

use crate::diagnostics;
use crate::lang::Ranged;
use crate::lexer::SpannedToken;
use crate::literal;

#[allow(clippy::style, clippy::complexity, clippy::perf)]
mod grammar {
    include!(concat!(env!("OUT_DIR"), "/lang/surface/grammar.rs"));
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

/// Items in the surface language.
pub type Item = Ranged<ItemData>;

/// Items in a module.
#[derive(Debug, Clone)]
pub enum ItemData {
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
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: Ranged<String>,
    /// Optional type annotation
    // FIXME: can't use `r#type` in LALRPOP grammars
    pub type_: Option<Term>,
    /// Fields in the struct.
    pub term: Term,
}

/// A struct type definition.
#[derive(Debug, Clone)]
pub struct StructType {
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: Ranged<String>,
    /// Fields in the struct.
    pub fields: Vec<TypeField>,
}

/// A field in a struct type definition.
#[derive(Debug, Clone)]
pub struct TypeField {
    pub doc: Arc<[String]>,
    pub name: Ranged<String>,
    pub term: Term,
}

/// Patterns in the surface language.
pub type Pattern = Ranged<PatternData>;

/// Pattern data.
#[derive(Debug, Clone)]
pub enum PatternData {
    /// Named patterns.
    Name(String),
    /// Numeric literals.
    NumberLiteral(literal::Number),
}

/// Terms in the surface language.
pub type Term = Ranged<TermData>;

/// Term data.
#[derive(Debug, Clone)]
pub enum TermData {
    /// Annotated terms.
    Ann(Box<Term>, Box<Term>),
    /// Names.
    Name(String),
    /// Type of types.
    TypeType,
    /// Function types.
    FunctionType(Box<Term>, Box<Term>),
    /// Function eliminations (function application).
    FunctionElim(Box<Term>, Vec<Term>),
    /// Numeric literals.
    NumberLiteral(literal::Number),
    /// If-else expressions.
    If(Box<Term>, Box<Term>, Box<Term>),
    /// Match expressions.
    Match(Box<Term>, Vec<(Pattern, Term)>),

    /// Type of format types.
    FormatType,

    /// Error sentinel terms.
    Error,
}
