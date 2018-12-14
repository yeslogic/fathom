//! The concrete syntax of the language

use codespan::{ByteIndex, ByteOffset, ByteSpan};
use num_bigint::BigInt;
use std::fmt;

use crate::syntax::pretty::{self, ToDoc};
use crate::syntax::{FloatFormat, IntFormat};

/// Commands entered in the REPL
#[derive(Debug, Clone)]
pub enum ReplCommand {
    /// Evaluate a term
    ///
    /// ```text
    /// <term>
    /// ```
    Eval(Box<Term>),
    /// Show the raw representation of a term
    ///
    /// ```text
    /// :raw <term>
    /// ```
    Raw(Box<Term>),
    /// Show the core representation of a term
    ///
    /// ```text
    /// :core <term>
    /// ```
    Core(Box<Term>),
    /// Print some help about using the REPL
    ///
    /// ```text
    /// :?
    /// :h
    /// :help
    /// ```
    Help,
    /// Add a declaration to the REPL environment
    ///
    /// ```text
    ///:let <name> = <term>
    /// ```
    Let(String, Box<Term>),
    ///  No command
    NoOp,
    /// Quit the REPL
    ///
    /// ```text
    /// :q
    /// :quit
    /// ```
    Quit,
    /// Print the type of the term
    ///
    /// ```text
    /// :t <term>
    /// :type <term>
    /// ```
    TypeOf(Box<Term>),
    /// Repl commands that could not be parsed correctly
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

/// Modules
#[derive(Debug, Clone, PartialEq)]
pub enum Module {
    /// A module definition:
    ///
    /// ```text
    /// module my-module;
    ///
    /// <items>
    /// ```
    Valid {
        name: (ByteIndex, String),
        items: Vec<Item>,
    },
    /// Modules commands that could not be parsed correctly
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// A group of lambda parameters that share an annotation
pub type LamParamGroup = (Vec<(ByteIndex, String)>, Option<Box<Term>>);

/// The parameters to a lambda abstraction
pub type LamParams = Vec<LamParamGroup>;

/// A group of parameters to a dependent function that share an annotation
pub type PiParamGroup = (Vec<(ByteIndex, String)>, Term);

/// The parameters to a dependent function type
pub type PiParams = Vec<PiParamGroup>;

#[derive(Debug, Clone, PartialEq)]
pub struct StructTypeField {
    pub label: (ByteIndex, String),
    pub binder: Option<(ByteIndex, String)>,
    pub ann: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructField {
    Punned {
        label: (ByteIndex, String),
    },
    Explicit {
        label: (ByteIndex, String),
        term: Term,
    },
}

/// Top-level items within a module
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    /// Imports a module into the current scope
    ///
    /// ```text
    /// import foo;
    /// import foo as my-foo;
    /// import foo as my-foo (..);
    /// ```
    Import {
        span: ByteSpan,
        name: (ByteIndex, String),
        rename: Option<(ByteIndex, String)>,
        exposing: Option<Exposing>,
    },
    /// Declares the type associated with a name, prior to its definition
    ///
    /// ```text
    /// foo : some-type
    /// ```
    Declaration {
        name: (ByteIndex, String),
        ann: Term,
    },
    /// Definitions
    Definition(Definition),
    /// Items that could not be correctly parsed
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl Item {
    /// Return the span of source code that this declaration originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Item::Import { span, .. } | Item::Error(span) => span,
            Item::Declaration {
                name: (start, _),
                ann: ref term,
            } => ByteSpan::new(start, term.span().end()),
            Item::Definition(ref definition) => definition.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    /// Alias definition
    ///
    /// ```text
    /// foo = some-body
    /// foo x (y : some-type) = some-body
    /// ```
    Alias {
        name: (ByteIndex, String),
        params: LamParams,
        return_ann: Option<Box<Term>>,
        term: Term,
    },
    /// Struct type definition
    ///
    /// ```text
    /// struct Foo (A : Type) { x : t1, .. }
    /// ```
    StructType {
        span: ByteSpan,
        name: (ByteIndex, String),
        params: Vec<(String, Term)>,
        fields: Vec<StructTypeField>,
    },
    /// Union type definition
    ///
    /// ```text
    /// union Foo (A : Type) { t1, .. }
    /// ```
    UnionType {
        span: ByteSpan,
        name: (ByteIndex, String),
        params: Vec<(String, Term)>,
        variants: Vec<Term>,
    },
}

impl Definition {
    /// Return the span of source code that this declaration originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Definition::Alias {
                name: (start, _),
                ref term,
                ..
            } => ByteSpan::new(start, term.span().end()),
            Definition::StructType { span, .. } | Definition::UnionType { span, .. } => span,
        }
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// A list of the definitions imported from a module
#[derive(Debug, Clone, PartialEq)]
pub enum Exposing {
    /// Import all the definitions in the module into the current scope
    ///
    /// ```text
    /// (..)
    /// ```
    All(ByteSpan),
    /// Import an exact set of definitions into the current scope
    ///
    /// ```text
    /// (foo, bar as baz)
    /// ```
    Exact(
        ByteSpan,
        Vec<((ByteIndex, String), Option<(ByteIndex, String)>)>,
    ),
    /// Exposing declarations that could not be correctly parsed
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl fmt::Display for Exposing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .group()
            .render_fmt(f.width().unwrap_or(10000), f)
    }
}

/// Literals
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// String literals
    // TODO: Preserve escapes?
    String(ByteSpan, String),
    /// Character literals
    // TODO: Preserve escapes?
    Char(ByteSpan, char),
    /// Integer literals
    // TODO: Preserve digit separators?
    Int(ByteSpan, BigInt, IntFormat),
    /// Floating point literals
    // TODO: Preserve digit separators?
    Float(ByteSpan, f64, FloatFormat),
}

impl Literal {
    /// Return the span of source code that the literal originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Literal::String(span, _)
            | Literal::Char(span, _)
            | Literal::Int(span, _, _)
            | Literal::Float(span, _, _) => span,
        }
    }
}

/// Patterns
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// A term that is surrounded with parentheses
    ///
    /// ```text
    /// (p)
    /// ```
    Parens(ByteSpan, Box<Pattern>),
    /// Patterns annotated with types
    ///
    /// ```text
    /// p : t
    /// ```
    Ann(Box<Pattern>, Box<Term>),
    /// Literal patterns
    Literal(Literal),
    /// Patterns that either introduce bound variables, or match by structural
    /// equality with a constant in-scope
    ///
    /// ```text
    /// x
    /// true
    /// false
    /// ```
    Name(ByteIndex, String),
    /// Terms that could not be correctly parsed
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl Pattern {
    /// Return the span of source code that this pattern originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Pattern::Parens(span, _) | Pattern::Error(span) => span,
            Pattern::Ann(ref pattern, ref ty) => pattern.span().to(ty.span()),
            Pattern::Literal(ref literal) => literal.span(),
            Pattern::Name(start, ref name) => {
                ByteSpan::from_offset(start, ByteOffset::from_str(name))
            },
        }
    }
}

/// Terms
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A term that is surrounded with parentheses
    ///
    /// ```text
    /// (e)
    /// ```
    Parens(ByteSpan, Box<Term>),
    /// A term annotated with a type
    ///
    /// ```text
    /// e : t
    /// ```
    Ann(Box<Term>, Box<Term>),
    /// Type of types
    ///
    /// ```text
    /// Type
    /// ```
    Universe(ByteSpan, Option<u32>),
    // TODO: exclusive ranges
    /// Byte span
    ///
    /// ```text
    /// int {..}
    /// int {1..}
    /// int {..10}
    /// int {4..10}
    /// ```
    IntType(ByteSpan, Option<Box<Term>>, Option<Box<Term>>),
    /// Singleton byte span
    ///
    /// ```text
    /// int {= 10}
    /// ```
    IntTypeSingleton(ByteSpan, Box<Term>),
    /// Literals
    Literal(Literal),
    /// Array literals
    Array(ByteSpan, Vec<Term>),
    /// Holes
    ///
    /// ```text
    /// _
    /// ```
    Hole(ByteSpan),
    /// Names
    ///
    /// ```text
    /// x
    /// ```
    Name(ByteIndex, String),
    /// Extern definitions
    ///
    /// ```text
    /// extern "extern-name"
    /// ```
    Extern(ByteSpan, ByteSpan, String),
    /// Lambda abstraction
    ///
    /// ```text
    /// \x => t
    /// \x y => t
    /// \x : t1 => t2
    /// \(x : t1) y (z : t2) => t3
    /// \(x y : t1) => t3
    /// ```
    Lam(ByteIndex, LamParams, Box<Term>),
    /// Dependent function type
    ///
    /// ```text
    /// (x : t1) -> t2
    /// (x y : t1) -> t2
    /// ```
    Pi(ByteIndex, PiParams, Box<Term>),
    /// Non-Dependent function type
    ///
    /// ```text
    /// t1 -> t2
    /// ```
    Arrow(Box<Term>, Box<Term>),
    /// Term application
    ///
    /// ```text
    /// e1 e2
    /// ```
    App(Box<Term>, Vec<Term>),
    /// If expression
    ///
    /// ```text
    /// if t1 then t2 else t3
    /// ```
    If(ByteSpan, Box<Term>, Box<Term>, Box<Term>),
    /// Refinement types
    ///
    /// ```text
    /// { x : t1 | pred x }
    /// ```
    Refinement(ByteSpan, ByteIndex, String, Box<Term>, Box<Term>),
    /// Match expression
    ///
    /// ```text
    /// match t1 { pat => t2; .. }
    /// ```
    Match(ByteSpan, Box<Term>, Vec<(Pattern, Term)>),
    /// Struct value
    ///
    /// ```text
    /// struct { x = t1, .. }
    /// struct { id (a : Type) (x : a) : a = x, .. }
    /// ```
    Struct(ByteSpan, Vec<StructField>),
    /// Struct field projection
    ///
    /// ```text
    /// e.l
    /// ```
    Proj(Box<Term>, ByteIndex, String),
    /// Terms that could not be correctly parsed
    ///
    /// This is used for error recovery
    Error(ByteSpan),
}

impl Term {
    /// Return the span of source code that this term originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            Term::Parens(span, _)
            | Term::Universe(span, _)
            | Term::IntTypeSingleton(span, _)
            | Term::IntType(span, _, _)
            | Term::Extern(span, _, _)
            | Term::Array(span, _)
            | Term::Hole(span)
            | Term::If(span, _, _, _)
            | Term::Match(span, _, _)
            | Term::Refinement(span, _, _, _, _)
            | Term::Struct(span, _)
            | Term::Error(span) => span,
            Term::Name(start, ref name) => ByteSpan::from_offset(start, ByteOffset::from_str(name)),
            Term::Literal(ref literal) => literal.span(),
            Term::Pi(start, _, ref body) | Term::Lam(start, _, ref body) => {
                ByteSpan::new(start, body.span().end())
            },
            Term::Ann(ref term, ref ty) => term.span().to(ty.span()),
            Term::Arrow(ref ann, ref body) => ann.span().to(body.span()),
            Term::App(ref head, ref arg) => head.span().to(arg.last().unwrap().span()),
            Term::Proj(ref term, label_start, ref label) => term
                .span()
                .with_end(label_start + ByteOffset::from_str(label)),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}
