//! The core type theory of the data description language.

use codespan_reporting::diagnostic::Diagnostic;
use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::ops::Range;
use std::sync::Arc;

use crate::lexer::SpannedToken;
use crate::{diagnostics, ieee754};

#[allow(clippy::style, clippy::complexity, clippy::perf)]
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
    pub fn range(&self) -> Range<usize> {
        match self {
            Item::Struct(struct_ty) => struct_ty.range.clone(),
            Item::Alias(alias) => alias.range.clone(),
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
    /// The full source range of this definition.
    pub range: Range<usize>,
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
    /// The full source range of this definition.
    pub range: Range<usize>,
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
    pub start: usize,
    pub name: String,
    pub term: Arc<Term>,
}

impl TypeField {
    pub fn range(&self) -> Range<usize> {
        self.start..self.term.range().end
    }
}

impl PartialEq for TypeField {
    fn eq(&self, other: &TypeField) -> bool {
        self.name == other.name && self.term == other.term
    }
}

/// Constants.
#[derive(Debug, Clone)]
pub enum Constant {
    /// Integer constants.
    Int(BigInt),
    /// IEEE-754 single-precision floating point constants.
    F32(f32),
    /// IEEE-754 double-precision floating point constants.
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
    Global(Range<usize>, String),
    /// Item variables.
    Item(Range<usize>, String),
    /// Terms annotated with types.
    Ann(Arc<Term>, Arc<Term>),
    /// Type of types.
    TypeType(Range<usize>),
    /// Function types.
    FunctionType(Arc<Term>, Arc<Term>),
    /// Function eliminations (function application).
    FunctionElim(Arc<Term>, Arc<Term>),
    /// Constants.
    Constant(Range<usize>, Constant),
    /// A boolean elimination.
    BoolElim(Range<usize>, Arc<Term>, Arc<Term>, Arc<Term>),
    /// A integer elimination.
    IntElim(
        Range<usize>,
        Arc<Term>,
        BTreeMap<BigInt, Arc<Term>>,
        Arc<Term>,
    ),
    /// Type of format types.
    FormatType(Range<usize>),

    /// Error sentinel.
    Error(Range<usize>),
}

impl Term {
    pub fn range(&self) -> Range<usize> {
        match self {
            Term::Global(range, _)
            | Term::Item(range, _)
            | Term::TypeType(range)
            | Term::Constant(range, _)
            | Term::BoolElim(range, _, _, _)
            | Term::IntElim(range, _, _, _)
            | Term::FormatType(range)
            | Term::Error(range) => range.clone(),
            Term::Ann(term, ty) => term.range().start..ty.range().end,
            Term::FunctionType(param_ty, body_ty) => param_ty.range().start..body_ty.range().end,
            Term::FunctionElim(head, argument) => head.range().start..argument.range().end,
        }
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Term) -> bool {
        match (self, other) {
            (Term::Global(_, name0), Term::Global(_, name1)) => name0 == name1,
            (Term::Item(_, name0), Term::Item(_, name1)) => name0 == name1,
            (Term::Ann(term0, ty0), Term::Ann(term1, ty1)) => term0 == term1 && ty0 == ty1,
            (Term::TypeType(_), Term::TypeType(_)) => true,
            (Term::FunctionType(param_ty0, body_ty0), Term::FunctionType(param_ty1, body_ty1)) => {
                param_ty0 == param_ty1 && body_ty0 == body_ty1
            }
            (Term::FunctionElim(head0, argument0), Term::FunctionElim(head1, argument1)) => {
                head0 == head1 && argument0 == argument1
            }
            (Term::Constant(_, constant0), Term::Constant(_, constant1)) => constant0 == constant1,
            (
                Term::BoolElim(_, head0, if_true0, if_false0),
                Term::BoolElim(_, head1, if_true1, if_false1),
            ) => head0 == head1 && if_true0 == if_true1 && if_false0 == if_false1,
            (
                Term::IntElim(_, head0, branches0, default0),
                Term::IntElim(_, head1, branches1, default1),
            ) => head0 == head1 && branches0 == branches1 && default0 == default1,
            (Term::FormatType(_), Term::FormatType(_)) => true,
            (Term::Error(_), Term::Error(_)) => true,
            (_, _) => false,
        }
    }
}

/// The head of a neutral term.
#[derive(Debug, Clone, PartialEq)]
pub enum Head {
    /// Global variables.
    Global(Range<usize>, String),
    /// Item variables.
    Item(Range<usize>, String),
    /// Errors.
    Error(Range<usize>),
}

impl Head {
    pub fn range(&self) -> Range<usize> {
        match self {
            Head::Global(range, _) | Head::Item(range, _) | Head::Error(range) => range.clone(),
        }
    }
}

/// An eliminator that is 'stuck' on some head.
#[derive(Debug, Clone, PartialEq)]
pub enum Elim {
    /// Function eliminatiors (function application).
    Function(Range<usize>, Arc<Value>),
    /// Boolean eliminators.
    // FIXME: environment?
    Bool(Range<usize>, Arc<Term>, Arc<Term>),
    /// Integer eliminators.
    Int(Range<usize>, BTreeMap<BigInt, Arc<Term>>, Arc<Term>),
}

impl Elim {
    pub fn range(&self) -> Range<usize> {
        match self {
            Elim::Function(range, _) | Elim::Bool(range, _, _) | Elim::Int(range, _, _) => {
                range.clone()
            }
        }
    }
}

/// Values.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Neutral terms
    Neutral(Head, Vec<Elim>),
    /// Type of types.
    TypeType(Range<usize>),
    /// Function types.
    FunctionType(Arc<Value>, Arc<Value>),
    /// Constants.
    Constant(Range<usize>, Constant),
    /// Type of format types.
    FormatType(Range<usize>),

    /// Error sentinel.
    Error(Range<usize>),
}

impl Value {
    /// Create a global variable.
    pub fn global(range: Range<usize>, name: impl Into<String>) -> Value {
        Value::Neutral(Head::Global(range, name.into()), Vec::new())
    }

    pub fn range(&self) -> Range<usize> {
        match self {
            Value::TypeType(range)
            | Value::Constant(range, _)
            | Value::FormatType(range)
            | Value::Error(range) => range.clone(),
            Value::FunctionType(param_ty, body_ty) => param_ty.range().start..body_ty.range().end,
            Value::Neutral(head, elims) => match elims.last() {
                Some(elim) => head.range().start..elim.range().end,
                None => head.range(),
            },
        }
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
    fn default() -> Globals {
        let mut entries = BTreeMap::new();

        entries.insert("U8".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("U16Le".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("U16Be".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("U32Le".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("U32Be".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("U64Le".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("U64Be".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("S8".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("S16Le".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("S16Be".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("S32Le".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("S32Be".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("S64Le".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("S64Be".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("F32Le".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("F32Be".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("F64Le".to_owned(), (Arc::new(Term::FormatType(0..0)), None));
        entries.insert("F64Be".to_owned(), (Arc::new(Term::FormatType(0..0)), None));

        entries.insert("Int".to_owned(), (Arc::new(Term::TypeType(0..0)), None));
        entries.insert("F32".to_owned(), (Arc::new(Term::TypeType(0..0)), None));
        entries.insert("F64".to_owned(), (Arc::new(Term::TypeType(0..0)), None));
        entries.insert("Bool".to_owned(), (Arc::new(Term::TypeType(0..0)), None));
        entries.insert(
            "true".to_owned(),
            (Arc::new(Term::Global(0..0, "Bool".to_owned())), None),
        );
        entries.insert(
            "false".to_owned(),
            (Arc::new(Term::Global(0..0, "Bool".to_owned())), None),
        );
        entries.insert(
            "Array".to_owned(),
            (
                Arc::new(Term::FunctionType(
                    Arc::new(Term::Global(0..0, "Int".to_owned())),
                    Arc::new(Term::FunctionType(
                        Arc::new(Term::FormatType(0..0)),
                        Arc::new(Term::FormatType(0..0)),
                    )),
                )),
                None,
            ),
        );
        entries.insert(
            "List".to_owned(),
            (
                Arc::new(Term::FunctionType(
                    Arc::new(Term::TypeType(0..0)),
                    Arc::new(Term::TypeType(0..0)),
                )),
                None,
            ),
        );

        Globals::new(entries)
    }
}
