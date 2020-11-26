//! The surface syntax for Fathom.

use std::sync::Arc;

use crate::lang::Ranged;
use crate::reporting::Message;

mod lexer;

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
    pub fn parse(file_id: usize, source: &str, messages: &mut Vec<Message>) -> Module {
        let tokens = lexer::tokens(file_id, source);
        grammar::ModuleParser::new()
            .parse(file_id, tokens)
            .unwrap_or_else(|error| {
                messages.push(Message::from_lalrpop(file_id, error));
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
    /// Constant definitions.
    ///
    /// ```text
    /// alias <name> = <term>;
    /// ```
    Constant(Constant),
    /// Struct definitions.
    ///
    /// ```text
    /// struct <name> {}
    /// ```
    StructType(StructType),
}

/// Constant definition.
#[derive(Debug, Clone)]
pub struct Constant {
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
    /// Type of this struct definition.
    // FIXME: can't use `r#type` in LALRPOP grammars
    pub type_: Option<Term>,
    /// Fields in the struct.
    pub fields: Vec<FieldDeclaration>,
}

/// Patterns in the surface language.
pub type Pattern = Ranged<PatternData>;

/// Pattern data.
#[derive(Debug, Clone)]
pub enum PatternData {
    /// Named patterns.
    Name(String),
    /// Numeric literals.
    NumberLiteral(String),
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
    /// Type of kinds.
    KindType,

    /// Function types.
    FunctionType(Box<Term>, Box<Term>),
    /// Function eliminations (function application).
    FunctionElim(Box<Term>, Vec<Term>),

    /// Struct terms.
    StructTerm(Vec<FieldDefinition>),
    /// Struct term eliminations (field lookup).
    StructElim(Box<Term>, Ranged<String>),

    /// Sequence terms.
    SequenceTerm(Vec<Term>),

    /// Numeric literals.
    NumberLiteral(String),
    /// If-else expressions.
    If(Box<Term>, Box<Term>, Box<Term>),
    /// Match expressions.
    Match(Box<Term>, Vec<(Pattern, Term)>),

    /// Type of format descriptions.
    FormatType,

    /// Convert a format to its host representation.
    Repr,

    /// Error sentinel terms.
    Error,
}

/// A field in a struct type.
#[derive(Debug, Clone)]
pub struct FieldDeclaration {
    pub doc: Arc<[String]>,
    pub label: Ranged<String>,
    // FIXME: can't use `r#type` in LALRPOP grammars
    pub type_: Term,
}

/// A field in a struct term.
#[derive(Debug, Clone)]
pub struct FieldDefinition {
    pub label: Ranged<String>,
    pub term: Term,
}
