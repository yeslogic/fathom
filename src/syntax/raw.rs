//! The syntax of the language, unchecked and with implicit parts that need to
//! be elaborated in a type-directed way during type checking and inference

use codespan::ByteSpan;
use moniker::{Binder, Embed, Nest, Scope, Var};
use num_bigint::BigInt;
use std::fmt;
use std::ops;
use std::rc::Rc;

use syntax::pretty::{self, ToDoc};
use syntax::{FloatFormat, IntFormat, Label, Level};

/// A module definition
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The items contained in the module
    pub items: Vec<Item>,
}

/// Top-level items within a module
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    /// Declares the type associated with a label, prior to its definition
    Declaration {
        /// The span of source code where the label was introduced
        label_span: ByteSpan,
        /// The external name for this declaration, to be used when referring
        /// to this item from other modules
        label: Label,
        /// The internal name for this declaration., to be used when binding
        /// this name to variables
        binder: Binder<String>,
        /// The type annotation for associated with the label
        term: RcTerm,
    },
    /// Defines the term that should be associated with a label
    Definition {
        /// The span of source code where the label was introduced
        label_span: ByteSpan,
        /// The external name for this definition, to be used when referring
        /// to this item from other modules
        label: Label,
        /// The internal name for this definition., to be used when binding
        /// this name to variables
        binder: Binder<String>,
        /// The definition associated with the label
        definition: Definition,
    },
}

pub type Telescope = Nest<(Binder<String>, Embed<RcTerm>)>;

pub type StructType = Scope<Telescope, Scope<Nest<(Label, Binder<String>, Embed<RcTerm>)>, ()>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    /// Alias definitions
    Alias(RcTerm),
    /// Dependent struct types
    StructType(ByteSpan, StructType),
}

impl Item {
    pub fn span(&self) -> ByteSpan {
        match *self {
            Item::Declaration {
                label_span,
                ref term,
                ..
            }
            | Item::Definition {
                label_span,
                definition: Definition::Alias(ref term),
                ..
            } => label_span.to(term.span()),
            Item::Definition {
                label_span,
                definition: Definition::StructType(span, _),
                ..
            } => label_span.to(span),
        }
    }
}

/// Literals
#[derive(Debug, Clone, PartialEq, PartialOrd, BoundTerm, BoundPattern)]
pub enum Literal {
    String(ByteSpan, String),
    Char(ByteSpan, char),
    Int(ByteSpan, BigInt, IntFormat),
    Float(ByteSpan, f64, FloatFormat),
}

impl Literal {
    pub fn span(&self) -> ByteSpan {
        match *self {
            Literal::String(span, _)
            | Literal::Char(span, _)
            | Literal::Int(span, _, _)
            | Literal::Float(span, _, _) => span,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

#[derive(Debug, Clone, PartialEq, BoundPattern)]
pub enum Pattern {
    /// Patterns annotated with types
    Ann(RcPattern, Embed<RcTerm>),
    /// Patterns that bind variables
    Binder(ByteSpan, Binder<String>),
    /// Patterns to be compared structurally with a variable in scope
    Var(ByteSpan, Embed<Var<String>>),
    /// Literal patterns
    Literal(Literal),
}

impl Pattern {
    /// Return the span of source code that this pattern originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Pattern::Ann(ref pattern, Embed(ref ty)) => pattern.span().to(ty.span()),
            Pattern::Var(span, _) | Pattern::Binder(span, _) => span,
            Pattern::Literal(ref literal) => literal.span(),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Reference counted patterns
#[derive(Debug, Clone, PartialEq, BoundPattern)]
pub struct RcPattern {
    pub inner: Rc<Pattern>,
}

impl From<Pattern> for RcPattern {
    fn from(src: Pattern) -> RcPattern {
        RcPattern {
            inner: Rc::new(src),
        }
    }
}

impl ops::Deref for RcPattern {
    type Target = Pattern;

    fn deref(&self) -> &Pattern {
        &self.inner
    }
}

impl fmt::Display for RcPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

/// Terms, unchecked and with implicit syntax that needs to be elaborated
///
/// For now the only implicit syntax we have is holes and lambdas that lack a
/// type annotation.
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(RcTerm, RcTerm),
    /// Universes
    Universe(ByteSpan, Level),
    /// Ranged integer types
    IntType(ByteSpan, Option<RcTerm>, Option<RcTerm>),
    /// Literals
    Literal(Literal),
    /// A hole
    Hole(ByteSpan),
    /// A variable
    Var(ByteSpan, Var<String>),
    /// An external definition
    Extern(ByteSpan, ByteSpan, String, RcTerm),
    /// Dependent function types
    Pi(ByteSpan, Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Lambda abstractions
    Lam(ByteSpan, Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Term application
    App(RcTerm, RcTerm),
    /// Dependent struct
    Struct(ByteSpan, Vec<(Label, RcTerm)>),
    /// Field projection
    Proj(ByteSpan, RcTerm, ByteSpan, Label),
    /// Match expressions
    Match(ByteSpan, RcTerm, Vec<Scope<RcPattern, RcTerm>>),
    /// Array literals
    Array(ByteSpan, Vec<RcTerm>),
}

impl Term {
    pub fn span(&self) -> ByteSpan {
        match *self {
            Term::Universe(span, _)
            | Term::IntType(span, _, _)
            | Term::Hole(span)
            | Term::Var(span, _)
            | Term::Extern(span, _, _, _)
            | Term::Pi(span, _)
            | Term::Lam(span, _)
            | Term::Struct(span, _)
            | Term::Proj(span, _, _, _)
            | Term::Match(span, _, _)
            | Term::Array(span, _) => span,
            Term::Literal(ref literal) => literal.span(),
            Term::Ann(ref expr, ref ty) => expr.span().to(ty.span()),
            Term::App(ref head, ref arg) => head.span().to(arg.span()),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Reference counted terms
#[derive(Debug, Clone, PartialEq, BoundTerm)]
pub struct RcTerm {
    pub inner: Rc<Term>,
}

impl From<Term> for RcTerm {
    fn from(src: Term) -> RcTerm {
        RcTerm {
            inner: Rc::new(src),
        }
    }
}

impl ops::Deref for RcTerm {
    type Target = Term;

    fn deref(&self) -> &Term {
        &self.inner
    }
}

impl fmt::Display for RcTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}
