//! The core type theory of the data description language.

use codespan::{ByteIndex, FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::sync::Arc;

use crate::lexer::SpannedToken;
use crate::{diagnostics, ieee754};

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/core/grammar.rs"));
}

pub mod compile;
pub mod pretty;
pub mod semantics;
pub mod validate;

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

impl PartialEq for Module {
    fn eq(&self, other: &Module) -> bool {
        self.items == other.items
    }
}

/// Items in a module.
#[derive(Debug, Clone)]
pub enum Item {
    /// Alias definitions
    Alias(Alias),
    /// Struct definitions.
    Struct(StructType),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Struct(struct_ty) => struct_ty.span,
            Item::Alias(alias) => alias.span,
        }
    }
}

impl PartialEq for Item {
    fn eq(&self, other: &Item) -> bool {
        match (self, other) {
            (Item::Alias(alias0), Item::Alias(alias1)) => *alias0 == *alias1,
            (Item::Struct(struct_ty0), Item::Struct(struct_ty1)) => *struct_ty0 == *struct_ty1,
            (_, _) => false,
        }
    }
}

/// An alias definition.
#[derive(Debug, Clone)]
pub struct Alias {
    /// The full span of this definition.
    pub span: Span,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: String,
    /// The term that is aliased.
    pub term: Arc<Term>,
}

impl PartialEq for Alias {
    fn eq(&self, other: &Alias) -> bool {
        self.name == other.name && self.term == other.term
    }
}

/// A struct type definition.
#[derive(Debug, Clone)]
pub struct StructType {
    /// The full span of this definition.
    pub span: Span,
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: String,
    /// Fields in the struct.
    pub fields: Vec<TypeField>,
}

impl PartialEq for StructType {
    fn eq(&self, other: &StructType) -> bool {
        self.name == other.name && self.fields == other.fields
    }
}

/// A field in a struct type definition.
#[derive(Debug, Clone)]
pub struct TypeField {
    pub doc: Arc<[String]>,
    pub start: ByteIndex,
    pub name: String,
    pub term: Arc<Term>,
}

impl TypeField {
    pub fn span(&self) -> Span {
        Span::new(self.start, self.term.span().end())
    }
}

impl PartialEq for TypeField {
    fn eq(&self, other: &TypeField) -> bool {
        self.name == other.name && self.term == other.term
    }
}

/// Universes.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Universe {
    Host,
    Format,
    Kind,
}

/// Universes.
#[derive(Debug, Clone)]
pub enum Constant {
    /// Host integer constants.
    Int(BigInt),
    /// Host IEEE-754 single-precision floating point constants.
    F32(f32),
    /// Host IEEE-754 double-precision floating point constants.
    F64(f64),
}

impl PartialEq for Constant {
    fn eq(&self, other: &Constant) -> bool {
        match (self, other) {
            (Constant::Int(val0), Constant::Int(val1)) => val0 == val1,
            (Constant::F32(val0), Constant::F32(val1)) => ieee754::logical_eq(*val0, *val1),
            (Constant::F64(val0), Constant::F64(val1)) => ieee754::logical_eq(*val0, *val1),
            (_, _) => false,
        }
    }
}

/// Terms.
#[derive(Debug, Clone)]
pub enum Term {
    /// Global variables.
    Global(Span, String),
    /// Item variables.
    Item(Span, String),
    /// Terms annotated with types.
    Ann(Arc<Term>, Arc<Term>),
    /// Universes.
    Universe(Span, Universe),
    /// Constants.
    Constant(Span, Constant),
    /// A boolean elimination.
    BoolElim(Span, Arc<Term>, Arc<Term>, Arc<Term>),
    /// A integer elimination.
    IntElim(Span, Arc<Term>, BTreeMap<BigInt, Arc<Term>>, Arc<Term>),

    /// Error sentinel.
    Error(Span),
}

impl Term {
    pub fn span(&self) -> Span {
        match self {
            Term::Global(span, _)
            | Term::Item(span, _)
            | Term::Universe(span, _)
            | Term::Constant(span, _)
            | Term::BoolElim(span, _, _, _)
            | Term::IntElim(span, _, _, _)
            | Term::Error(span) => *span,
            Term::Ann(term, ty) => Span::merge(term.span(), ty.span()),
        }
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Term) -> bool {
        match (self, other) {
            (Term::Global(_, name0), Term::Global(_, name1)) => name0 == name1,
            (Term::Item(_, name0), Term::Item(_, name1)) => name0 == name1,
            (Term::Ann(term0, ty0), Term::Ann(term1, ty1)) => term0 == term1 && ty0 == ty1,
            (Term::Universe(_, universe0), Term::Universe(_, universe1)) => universe0 == universe1,
            (Term::Constant(_, constant0), Term::Constant(_, constant1)) => constant0 == constant1,
            (
                Term::BoolElim(_, head0, if_true0, if_false0),
                Term::BoolElim(_, head1, if_true1, if_false1),
            ) => head0 == head1 && if_true0 == if_true1 && if_false0 == if_false1,
            (
                Term::IntElim(_, head0, branches0, default0),
                Term::IntElim(_, head1, branches1, default1),
            ) => head0 == head1 && branches0 == branches1 && default0 == default1,
            (Term::Error(_), Term::Error(_)) => true,
            (_, _) => false,
        }
    }
}

/// The head of a neutral term.
#[derive(Debug, Clone, PartialEq)]
pub enum Head {
    /// Global variables.
    Global(String),
    /// Item variables.
    Item(String),
    /// Errors.
    Error,
}

/// An eliminator that is 'stuck' on some head.
#[derive(Debug, Clone, PartialEq)]
pub enum Elim {
    // FIXME: environment?
    Bool(Arc<Term>, Arc<Term>),
    Int(BTreeMap<BigInt, Arc<Term>>, Arc<Term>),
}

/// Values.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Neutral terms
    Neutral(Head, Vec<Elim>),
    /// Universes.
    Universe(Universe),
    /// Constants.
    Constant(Constant),

    /// Error sentinel.
    Error,
}

impl Value {
    /// Create a global variable.
    pub fn global(name: impl Into<String>) -> Value {
        Value::Neutral(Head::Global(name.into()), Vec::new())
    }
}

/// An environment of global definitions.
pub struct Globals {
    entries: BTreeMap<String, (Arc<Term>, Option<Arc<Term>>)>,
}

impl Globals {
    pub fn new(entries: BTreeMap<String, (Arc<Term>, Option<Arc<Term>>)>) -> Globals {
        Globals { entries }
    }

    pub fn get(&self, name: &str) -> Option<&(Arc<Term>, Option<Arc<Term>>)> {
        self.entries.get(name)
    }

    pub fn entries(&self) -> impl Iterator<Item = (&String, &(Arc<Term>, Option<Arc<Term>>))> {
        self.entries.iter()
    }
}

impl Default for Globals {
    #[rustfmt::skip]
    fn default() -> Globals {
        let mut entries = BTreeMap::new();
        let span = Span::initial();

        entries.insert("U8".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("U16Le".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("U16Be".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("U32Le".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("U32Be".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("U64Le".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("U64Be".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("S8".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("S16Le".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("S16Be".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("S32Le".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("S32Be".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("S64Le".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("S64Be".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("F32Le".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("F32Be".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("F64Le".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));
        entries.insert("F64Be".to_owned(), (Arc::new(Term::Universe(span, Universe::Format)), None));

        entries.insert("Int".to_owned(), (Arc::new(Term::Universe(span, Universe::Host)), None));
        entries.insert("F32".to_owned(), (Arc::new(Term::Universe(span, Universe::Host)), None));
        entries.insert("F64".to_owned(), (Arc::new(Term::Universe(span, Universe::Host)), None));
        entries.insert("Bool".to_owned(), (Arc::new(Term::Universe(span, Universe::Host)), None));
        entries.insert("true".to_owned(), (Arc::new(Term::Global(span, "Bool".to_owned())), None));
        entries.insert("false".to_owned(), (Arc::new(Term::Global(span, "Bool".to_owned())), None));

        Globals::new(entries)
    }
}
