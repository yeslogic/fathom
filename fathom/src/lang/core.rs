//! The core type theory of Fathom.

use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;

use crate::ieee754;
use crate::lang::{FileId, Located};
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
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// The items in this module.
    pub items: Vec<Item>,
}

impl Module {
    pub fn parse(file_id: FileId, source: &str, messages: &mut Vec<Message>) -> Module {
        let tokens = lexer::tokens(file_id, source);
        grammar::ModuleParser::new()
            .parse(file_id, messages, tokens)
            .unwrap_or_else(|error| {
                messages.push(Message::from_lalrpop(file_id, error));
                Module {
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
pub type Item = Located<ItemData>;

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
    /// Parameter telescope.
    pub params: Vec<(Located<String>, Arc<Term>)>,
    /// Fields in the struct.
    pub fields: Arc<[FieldDeclaration]>,
}

/// A struct format definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StructFormat {
    /// Doc comment.
    pub doc: Arc<[String]>,
    /// Name of this definition.
    pub name: String,
    /// Parameter telescope.
    pub params: Vec<(Located<String>, Arc<Term>)>,
    /// Fields in the struct.
    pub fields: Arc<[FieldDeclaration]>,
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
    /// Positions in the byte stream.
    ///
    /// These should only appear when using the binary interpreter.
    Pos(usize),
}

impl PartialEq for Primitive {
    fn eq(&self, other: &Primitive) -> bool {
        match (self, other) {
            (Primitive::Int(val0), Primitive::Int(val1)) => val0 == val1,
            (Primitive::F32(val0), Primitive::F32(val1)) => ieee754::logical_eq(*val0, *val1),
            (Primitive::F64(val0), Primitive::F64(val1)) => ieee754::logical_eq(*val0, *val1),
            (Primitive::Pos(val0), Primitive::Pos(val1)) => val0 == val1,
            (_, _) => false,
        }
    }
}

/// Terms in the core language.
pub type Term = Located<TermData>;

/// Terms.
#[derive(Debug, Clone, PartialEq)]
pub enum TermData {
    /// Global variables.
    Global(String),
    /// Item variables.
    Item(String),
    /// Local variables
    Local(LocalIndex),

    /// Terms annotated with types.
    Ann(Arc<Term>, Arc<Term>),
    /// Sorts.
    Sort(Sort),

    /// Function types.
    FunctionType(Arc<Term>, Arc<Term>),
    /// Function eliminations (function application).
    FunctionElim(Arc<Term>, Arc<Term>),

    /// Struct terms.
    StructTerm(Vec<FieldDefinition>),
    /// Struct term eliminations (field lookup).
    StructElim(Arc<Term>, String),

    /// Array terms.
    ArrayTerm(Vec<Arc<Term>>),

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

/// A field in a struct type definition.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDeclaration {
    pub doc: Arc<[String]>,
    pub label: Located<String>,
    // FIXME: can't use `r#type` in LALRPOP grammars
    pub type_: Arc<Term>,
}

/// A field in a struct term.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDefinition {
    pub label: Located<String>,
    pub term: Arc<Term>,
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

        let term = Term::generated;

        let mut entries = BTreeMap::new();

        entries.insert("Int".to_owned(), (Arc::new(term(Sort(Type))), None));
        entries.insert("F32".to_owned(), (Arc::new(term(Sort(Type))), None));
        entries.insert("F64".to_owned(), (Arc::new(term(Sort(Type))), None));
        entries.insert("Bool".to_owned(), (Arc::new(term(Sort(Type))), None));
        entries.insert(
            "true".to_owned(),
            (Arc::new(term(Global("Bool".to_owned()))), None),
        );
        entries.insert(
            "false".to_owned(),
            (Arc::new(term(Global("Bool".to_owned()))), None),
        );
        entries.insert(
            "Array".to_owned(),
            (
                Arc::new(term(FunctionType(
                    Arc::new(term(Global("Int".to_owned()))),
                    Arc::new(term(FunctionType(
                        Arc::new(term(Sort(Type))),
                        Arc::new(term(Sort(Type))),
                    ))),
                ))),
                None,
            ),
        );
        entries.insert("Pos".to_owned(), (Arc::new(term(Sort(Type))), None));

        entries.insert("U8".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("U16Le".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("U16Be".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("U32Le".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("U32Be".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("U64Le".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("U64Be".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("S8".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("S16Le".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("S16Be".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("S32Le".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("S32Be".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("S64Le".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("S64Be".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("F32Le".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("F32Be".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("F64Le".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert("F64Be".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert(
            "FormatArray".to_owned(),
            (
                Arc::new(term(FunctionType(
                    Arc::new(term(Global("Int".to_owned()))),
                    Arc::new(term(FunctionType(
                        Arc::new(term(FormatType)),
                        Arc::new(term(FormatType)),
                    ))),
                ))),
                None,
            ),
        );
        entries.insert("CurrentPos".to_owned(), (Arc::new(term(FormatType)), None));
        entries.insert(
            "Link".to_owned(),
            (
                Arc::new(term(FunctionType(
                    Arc::new(term(Global("Pos".to_owned()))),
                    Arc::new(term(FunctionType(
                        Arc::new(term(Global("Int".to_owned()))),
                        Arc::new(term(FunctionType(
                            Arc::new(term(FormatType)),
                            Arc::new(term(FormatType)),
                        ))),
                    ))),
                ))),
                None,
            ),
        );

        Globals::new(entries)
    }
}

/// A [de Bruijn index][de-bruijn-index] in the local environment.
///
/// De Bruijn indices describe an occurrence of a variable in terms of the
/// number of binders between the occurrence and its associated binder.
/// For example:
///
/// | Representation    | Example (S combinator)  |
/// | ----------------- | ----------------------- |
/// | Named             | `λx. λy. λz. x z (y z)` |
/// | De Bruijn indices | `λ_. λ_. λ_. 2 0 (1 0)` |
///
/// This is a helpful representation because it allows us to easily compare
/// terms for equivalence based on their binding structure without maintaining a
/// list of name substitutions. For example we want `λx. x` to be the same as
/// `λy. y`. With de Bruijn indices these would both be described as `λ 0`.
///
/// [de-bruijn-index]: https://en.wikipedia.org/wiki/De_Bruijn_index
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LocalIndex(u32);

impl LocalIndex {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

/// A de Bruijn level in the local environment.
///
/// This describes an occurrence of a variable by counting the binders inwards
/// from the top of the term until the occurrence is reached. For example:
///
/// | Representation    | Example (S combinator)  |
/// | ----------------- | ----------------------- |
/// | Named             | `λx. λy. λz. x z (y z)` |
/// | De Bruijn levels  | `λ_. λ_. λ_. 0 2 (1 2)` |
///
/// Levels are used in [values][semantics::Value] because they are not context-
/// dependent (this is in contrast to [indices][LocalIndex]). Because of this,
/// we're able to sidestep the need for expensive variable shifting in the
/// semantics. More information can be found in Soham Chowdhury's blog post,
/// “[Real-world type theory I: untyped normalisation by evaluation for λ-calculus][untyped-nbe-for-lc]”.
///
/// [untyped-nbe-for-lc]: https://colimit.net/posts/normalisation-by-evaluation/
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LocalLevel(u32);

impl LocalLevel {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

/// The size, or 'binding depth', of the local environment.
///
/// This is used for index-to-level conversions.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LocalSize(u32);

impl LocalSize {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }

    /// Increments the local environment size by one.
    ///
    /// This is usually used for 'stepping inside a binder' during read-back.
    pub fn increment(self) -> LocalSize {
        LocalSize(self.0 + 1)
    }

    /// Return the level of the next variable to be added to the environment.
    pub fn next_level(self) -> LocalLevel {
        LocalLevel(self.0)
    }

    /// Convert a local index to a local level in the current environment.
    ///
    /// `None` is returned if the environment is not large enough to
    /// contain the local variable.
    pub fn index_to_level(self, index: LocalIndex) -> Option<LocalLevel> {
        Some(LocalLevel(self.0.checked_sub(index.0)?.checked_sub(1)?))
    }

    /// Convert a local level to a local index in the current environment.
    ///
    /// `None` is returned if the environment is not large enough to
    /// contain the local variable.
    pub fn level_to_index(self, level: LocalLevel) -> Option<LocalIndex> {
        Some(LocalIndex(self.0.checked_sub(level.0)?.checked_sub(1)?))
    }
}

/// A local environment.
#[derive(Clone)]
pub struct Locals<Entry> {
    /// The local entries that are currently defined in the environment.
    entries: im::Vector<Entry>,
}

impl<Entry: Clone> Locals<Entry> {
    /// Create a new local environment.
    pub fn new() -> Locals<Entry> {
        Locals {
            entries: im::Vector::new(),
        }
    }

    /// Get the size of the environment.
    pub fn size(&self) -> LocalSize {
        LocalSize(self.entries.len() as u32)
    }

    /// Returns `true` if there are no entries in the environment.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Lookup an entry in the environment.
    pub fn get(&self, index: LocalIndex) -> Option<&Entry> {
        let level = self.size().index_to_level(index)?;
        self.entries.get(level.to_usize())
    }

    /// Push an entry onto the environment.
    pub fn push(&mut self, entry: Entry) {
        self.entries.push_back(entry); // FIXME: Check for overflow?
    }

    /// Pop an entry off the environment.
    pub fn pop(&mut self) -> Option<Entry> {
        self.entries.pop_back()
    }

    /// Truncate an environment to a certain length.
    pub fn truncate(&mut self, local_size: LocalSize) {
        self.entries.truncate(local_size.to_usize());
    }

    /// Clear the entries from the environment.
    pub fn clear(&mut self) {
        self.entries.clear();
    }

    /// Returns a reverse iterator over the entries in the environment and the
    /// indices where those entries were bound.
    pub fn iter_rev(&self) -> impl Iterator<Item = (LocalIndex, &Entry)> {
        (self.entries.iter().rev()).scan(0, |current_index, entry| {
            let local_index = LocalIndex(std::mem::replace(current_index, *current_index + 1));
            Some((local_index, entry))
        })
    }
}

impl<Entry: Clone + fmt::Debug> fmt::Debug for Locals<Entry> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Locals")
            .field("entries", &self.entries)
            .finish()
    }
}
