//! The core type theory of Fathom.

use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::sync::Arc;

use crate::ieee754;
use crate::lang::Ranged;
use crate::reporting::Message;

mod lexer;

#[allow(clippy::style, clippy::complexity, clippy::perf)]
mod grammar {
    include!(concat!(env!("OUT_DIR"), "/lang/core/grammar.rs"));
}

pub mod binary;
pub mod semantics;
pub mod typing;

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
            .parse(file_id, messages, tokens)
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

impl PartialEq for Module {
    /// Ignores source location metadata.
    fn eq(&self, other: &Module) -> bool {
        self.doc == other.doc && self.items == other.items
    }
}

/// Items in the core language.
pub type Item = Ranged<ItemData>;

/// Items in a module.
#[derive(Debug, Clone, PartialEq)]
pub enum ItemData {
    /// Constant definitions
    Constant(Constant),
    /// Struct type definitions.
    StructType(StructType),
    /// Struct format definitions.
    StructFormat(StructFormat),
}

/// A constant definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Constant {
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: String,
    /// The term that is aliased.
    pub term: Arc<Term>,
}

/// A struct type definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: String,
    /// Fields in the struct.
    pub fields: Vec<TypeField>,
}

/// A struct format definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StructFormat {
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: String,
    /// Fields in the struct.
    pub fields: Vec<TypeField>,
}

/// A field in a struct type definition.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeField {
    pub doc: Arc<[String]>,
    pub name: Ranged<String>,
    pub term: Arc<Term>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Sort {
    Type,
    Kind,
}

/// Primitives.
#[derive(Debug, Clone)]
pub enum Primitive {
    /// Integer constants.
    Int(BigInt),
    /// IEEE-754 single-precision floating point constants.
    F32(f32),
    /// IEEE-754 double-precision floating point constants.
    F64(f64),
}

impl PartialEq for Primitive {
    fn eq(&self, other: &Primitive) -> bool {
        match (self, other) {
            (Primitive::Int(val0), Primitive::Int(val1)) => val0 == val1,
            (Primitive::F32(val0), Primitive::F32(val1)) => ieee754::logical_eq(*val0, *val1),
            (Primitive::F64(val0), Primitive::F64(val1)) => ieee754::logical_eq(*val0, *val1),
            (_, _) => false,
        }
    }
}

/// Terms in the core language.
pub type Term = Ranged<TermData>;

/// Terms.
#[derive(Debug, Clone, PartialEq)]
pub enum TermData {
    /// Global variables.
    Global(String),
    /// Item variables.
    Item(String),

    /// Terms annotated with types.
    Ann(Arc<Term>, Arc<Term>),
    /// Sorts.
    Sort(Sort),

    /// Function types.
    FunctionType(Arc<Term>, Arc<Term>),
    /// Function eliminations (function application).
    FunctionElim(Arc<Term>, Arc<Term>),

    /// Primitives.
    Primitive(Primitive),
    /// A boolean elimination.
    BoolElim(Arc<Term>, Arc<Term>, Arc<Term>),
    /// A integer elimination.
    IntElim(Arc<Term>, BTreeMap<BigInt, Arc<Term>>, Arc<Term>),

    /// Type of format types.
    FormatType,

    /// Convert a format to its host representation.
    Repr,

    /// Error sentinel.
    Error,
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
    fn default() -> Globals {
        use self::Sort::*;
        use self::TermData::*;

        let mut entries = BTreeMap::new();

        entries.insert("Int".to_owned(), (Arc::new(Term::from(Sort(Type))), None));
        entries.insert("F32".to_owned(), (Arc::new(Term::from(Sort(Type))), None));
        entries.insert("F64".to_owned(), (Arc::new(Term::from(Sort(Type))), None));
        entries.insert("Bool".to_owned(), (Arc::new(Term::from(Sort(Type))), None));
        entries.insert(
            "true".to_owned(),
            (Arc::new(Term::from(Global("Bool".to_owned()))), None),
        );
        entries.insert(
            "false".to_owned(),
            (Arc::new(Term::from(Global("Bool".to_owned()))), None),
        );
        entries.insert(
            "Array".to_owned(),
            (
                Arc::new(Term::from(FunctionType(
                    Arc::new(Term::from(Global("Int".to_owned()))),
                    Arc::new(Term::from(FunctionType(
                        Arc::new(Term::from(Sort(Type))),
                        Arc::new(Term::from(Sort(Type))),
                    ))),
                ))),
                None,
            ),
        );

        entries.insert("U8".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("U16Le".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("U16Be".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("U32Le".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("U32Be".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("U64Le".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("U64Be".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("S8".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("S16Le".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("S16Be".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("S32Le".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("S32Be".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("S64Le".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("S64Be".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("F32Le".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("F32Be".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("F64Le".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert("F64Be".to_owned(), (Arc::new(Term::from(FormatType)), None));
        entries.insert(
            "FormatArray".to_owned(),
            (
                Arc::new(Term::from(FunctionType(
                    Arc::new(Term::from(Global("Int".to_owned()))),
                    Arc::new(Term::from(FunctionType(
                        Arc::new(Term::from(FormatType)),
                        Arc::new(Term::from(FormatType)),
                    ))),
                ))),
                None,
            ),
        );

        Globals::new(entries)
    }
}
