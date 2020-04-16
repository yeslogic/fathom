//! The core syntax of the language

use moniker::{Binder, Embed, FreeVar, Nest, Scope, Var};
use num_bigint::BigInt;
use std::fmt;
use std::ops::Range;
use std::rc::Rc;

use crate::syntax::pretty::{self, ToDoc};
use crate::syntax::{FloatFormat, IntFormat, Label, Level};

/// A module definition
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The items contained in the module
    pub items: Nest<(Label, Binder<String>, Embed<Definition>)>,
}

pub type Telescope = Nest<(Binder<String>, Embed<RcTerm>)>;

pub type IntersectionType =
    Scope<Telescope, Scope<Nest<(Label, Binder<String>, Embed<RcTerm>)>, ()>>;
pub type StructType = Scope<Telescope, Scope<Nest<(Label, Binder<String>, Embed<RcTerm>)>, ()>>;
pub type UnionType = Scope<Telescope, Vec<RcTerm>>;

#[derive(Debug, Clone, PartialEq, moniker::BoundTerm)]
pub enum Definition {
    /// Alias definitions
    Alias { term: RcTerm, ty: RcTerm },
    /// Dependent intersection types
    IntersectionType { scope: IntersectionType },
    /// Dependent struct types
    StructType { scope: StructType },
    /// Union types
    UnionType { scope: UnionType },
}

/// Literals
///
/// We could church encode all the things, but that would be prohibitively expensive!
#[derive(Debug, Clone, PartialEq, PartialOrd, moniker::BoundTerm, moniker::BoundPattern)]
pub enum Literal {
    Bool(bool),
    String(String),
    Char(char),
    Pos(u64),
    Offset8(u64, u8),
    Offset16(u64, u16),
    Offset32(u64, u32),
    Offset64(u64, u64),
    Int(BigInt, IntFormat),
    F32(f32, FloatFormat),
    F64(f64, FloatFormat),
}

impl Literal {
    pub fn try_int(&self) -> Option<&BigInt> {
        match *self {
            Literal::Int(ref value, _) => Some(value),
            _ => None,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

#[derive(Debug, Clone, PartialEq, moniker::BoundPattern)]
pub enum Pattern {
    /// Patterns annotated with types
    Ann(RcPattern, Embed<RcTerm>),
    /// Patterns that bind variables
    Binder(Binder<String>),
    /// Patterns to be compared structurally with a variable in scope
    Var(Embed<Var<String>>),
    /// Literal patterns
    Literal(Literal),
    /// Array patterns
    Array(Vec<RcPattern>),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Reference counted patterns
#[derive(Debug, Clone, PartialEq, moniker::BoundPattern)]
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

/// The core term syntax
#[derive(Debug, Clone, PartialEq, moniker::BoundTerm)]
pub enum Term {
    /// A term annotated with a type
    Ann(RcTerm, RcTerm),
    /// Universes
    Universe(Level),
    /// Ranged integer types
    IntType(Option<RcTerm>, Option<RcTerm>),
    /// Literals
    Literal(Literal),
    /// A variable
    Var(Var<String>),
    /// An external definition
    Extern(String),
    /// Dependent function types
    Pi(Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Lambda abstractions
    Lam(Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Term application
    App(RcTerm, RcTerm),
    /// Refinement type
    Refinement(Scope<(Binder<String>, Embed<RcTerm>), RcTerm>),
    /// Dependent struct
    Struct(Vec<(Label, RcTerm)>),
    /// Field projection
    Proj(RcTerm, Label),
    /// Match expressions
    Match(RcTerm, Vec<Scope<RcPattern, RcTerm>>),
    /// Array literals
    Array(Vec<RcTerm>),
}

impl Term {
    pub fn universe(level: impl Into<Level>) -> Term {
        Term::Universe(level.into())
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Reference counted terms
#[derive(Debug, Clone, PartialEq, moniker::BoundTerm)]
pub struct RcTerm {
    pub inner: Rc<Term>,
}

impl RcTerm {
    pub fn substs(&self, mappings: &[(FreeVar<String>, RcTerm)]) -> RcTerm {
        match *self.inner {
            Term::Ann(ref term, ref ty) => {
                RcTerm::from(Term::Ann(term.substs(mappings), ty.substs(mappings)))
            },
            Term::IntType(ref min, ref max) => RcTerm::from(Term::IntType(
                min.as_ref().map(|x| x.substs(mappings)),
                max.as_ref().map(|x| x.substs(mappings)),
            )),
            Term::Universe(_) | Term::Literal(_) => self.clone(),
            Term::Var(ref var) => match mappings.iter().find(|&(ref name, _)| var == name) {
                Some(&(_, ref term)) => term.clone(),
                None => self.clone(),
            },
            Term::Extern(ref name) => RcTerm::from(Term::Extern(name.clone())),
            Term::Pi(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                RcTerm::from(Term::Pi(Scope {
                    unsafe_pattern: (name.clone(), Embed(ann.substs(mappings))),
                    unsafe_body: scope.unsafe_body.substs(mappings),
                }))
            },
            Term::Lam(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                RcTerm::from(Term::Lam(Scope {
                    unsafe_pattern: (name.clone(), Embed(ann.substs(mappings))),
                    unsafe_body: scope.unsafe_body.substs(mappings),
                }))
            },
            Term::App(ref head, ref arg) => {
                RcTerm::from(Term::App(head.substs(mappings), arg.substs(mappings)))
            },
            Term::Refinement(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                RcTerm::from(Term::Refinement(Scope {
                    unsafe_pattern: (name.clone(), Embed(ann.substs(mappings))),
                    unsafe_body: scope.unsafe_body.substs(mappings),
                }))
            },
            Term::Struct(ref fields) => RcTerm::from(Term::Struct(
                fields
                    .iter()
                    .map(|&(ref field, ref term)| (field.clone(), term.substs(mappings)))
                    .collect(),
            )),
            Term::Proj(ref expr, ref label) => {
                RcTerm::from(Term::Proj(expr.substs(mappings), label.clone()))
            },
            Term::Match(ref head, ref clauses) => RcTerm::from(Term::Match(
                head.substs(mappings),
                clauses
                    .iter()
                    .map(|scope| Scope {
                        unsafe_pattern: scope.unsafe_pattern.clone(), // subst?
                        unsafe_body: scope.unsafe_body.substs(mappings),
                    })
                    .collect(),
            )),
            Term::Array(ref elems) => RcTerm::from(Term::Array(
                elems.iter().map(|elem| elem.substs(mappings)).collect(),
            )),
        }
    }
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

/// Values
///
/// These are either in _normal form_ (they cannot be reduced further) or are
/// _neutral terms_ (there is a possibility of reducing further depending
/// on the bindings given in the context)
#[derive(Debug, Clone, PartialEq, moniker::BoundTerm)]
pub enum Value {
    /// Universes
    Universe(Level),
    /// Bounded integers
    IntType(Option<RcValue>, Option<RcValue>),
    /// Literals
    Literal(Literal),
    /// A pi type
    Pi(Scope<(Binder<String>, Embed<RcValue>), RcValue>),
    /// A lambda abstraction
    Lam(Scope<(Binder<String>, Embed<RcValue>), RcValue>),
    /// A refinement type
    Refinement(Scope<(Binder<String>, Embed<RcValue>), RcValue>),
    /// Dependent struct
    Struct(Vec<(Label, RcValue)>),
    /// Array literals
    Array(Vec<RcValue>),
    /// Neutral terms
    ///
    /// A term whose computation has stopped because of an attempt to compute an
    /// application `Head`.
    Neutral(RcNeutral),
}

impl Value {
    pub fn universe(level: impl Into<Level>) -> Value {
        Value::Universe(level.into())
    }

    pub fn var(var: impl Into<Var<String>>) -> Value {
        Value::Neutral(RcNeutral::from(Neutral::var(var)))
    }

    pub fn substs(&self, mappings: &[(FreeVar<String>, RcTerm)]) -> RcTerm {
        // FIXME: This seems quite wasteful!
        RcTerm::from(Term::from(self)).substs(mappings)
    }

    /// Returns `true` if the value is in weak head normal form
    pub fn is_whnf(&self) -> bool {
        match *self {
            Value::Universe(_)
            | Value::Literal(_)
            | Value::IntType(_, _)
            | Value::Pi(_)
            | Value::Lam(_)
            | Value::Refinement(_)
            | Value::Struct(_)
            | Value::Array(_) => true,
            Value::Neutral(_) => false,
        }
    }

    /// Returns `true` if the value is in normal form (ie. it contains no neutral terms within it)
    pub fn is_nf(&self) -> bool {
        match *self {
            Value::Universe(_) | Value::Literal(_) => true,
            Value::IntType(ref min, ref max) => {
                min.as_ref().map_or(true, |x| x.is_nf()) && max.as_ref().map_or(true, |x| x.is_nf())
            },
            Value::Pi(ref scope) | Value::Lam(ref scope) | Value::Refinement(ref scope) => {
                (scope.unsafe_pattern.1).0.is_nf() && scope.unsafe_body.is_nf()
            },
            Value::Struct(ref fields) => fields.iter().all(|&(_, ref term)| term.is_nf()),
            Value::Array(ref elems) => elems.iter().all(|elem| elem.is_nf()),
            Value::Neutral(_) => false,
        }
    }

    pub fn try_literal(&self) -> Option<&Literal> {
        match *self {
            Value::Literal(ref literal) => Some(literal),
            _ => None,
        }
    }

    pub fn try_neutral(&self) -> Option<&RcNeutral> {
        match *self {
            Value::Neutral(ref neutral) => Some(neutral),
            _ => None,
        }
    }

    pub fn head_app(&self) -> Option<(&Head, &Spine)> {
        if let Value::Neutral(ref neutral) = *self {
            if let Neutral::Head(ref head, ref spine) = **neutral {
                return Some((head, spine));
            }
        }
        None
    }

    pub fn free_var_app(&self) -> Option<(&FreeVar<String>, &[RcValue])> {
        self.head_app().and_then(|(head, spine)| match head {
            Head::Var(Var::Free(ref free_var)) => Some((free_var, &spine[..])),
            Head::Extern(_) | Head::Var(Var::Bound(_)) => None,
        })
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Reference counted values
#[derive(Debug, Clone, PartialEq, moniker::BoundTerm)]
pub struct RcValue {
    pub inner: Rc<Value>,
}

impl From<Value> for RcValue {
    fn from(src: Value) -> RcValue {
        RcValue {
            inner: Rc::new(src),
        }
    }
}

impl ops::Deref for RcValue {
    type Target = Value;

    fn deref(&self) -> &Value {
        &self.inner
    }
}

impl fmt::Display for RcValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

/// The head of an application
#[derive(Debug, Clone, PartialEq, moniker::BoundTerm)]
pub enum Head {
    /// Variables that have not yet been replaced with a definition
    Var(Var<String>),
    /// External definitions
    Extern(String),
    // TODO: Metavariables
}

/// The spine of a neutral term
///
/// These are arguments that are awaiting application
pub type Spine = Vec<RcValue>;

/// Neutral values
///
/// These might be able to be reduced further depending on the bindings in the
/// context
#[derive(Debug, Clone, PartialEq, moniker::BoundTerm)]
pub enum Neutral {
    /// Head of an application
    Head(Head, Spine),
    /// Field projection
    Proj(RcNeutral, Label, Spine),
    /// Match expressions
    Match(RcNeutral, Vec<Scope<RcPattern, RcValue>>, Spine),
}

impl Neutral {
    pub fn var(var: impl Into<Var<String>>) -> Neutral {
        Neutral::Head(Head::Var(var.into()), Spine::new())
    }
}

impl fmt::Display for Neutral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

/// Reference counted neutral values
#[derive(Debug, Clone, PartialEq, moniker::BoundTerm)]
pub struct RcNeutral {
    pub inner: Rc<Neutral>,
}

impl From<Neutral> for RcNeutral {
    fn from(src: Neutral) -> RcNeutral {
        RcNeutral {
            inner: Rc::new(src),
        }
    }
}

impl ops::Deref for RcNeutral {
    type Target = Neutral;

    fn deref(&self) -> &Neutral {
        &self.inner
    }
}

/// Types are at the term level, so this is just an alias
pub type Type = Value;

/// Types are at the term level, so this is just an alias
pub type RcType = RcValue;

impl From<Var<String>> for Neutral {
    fn from(src: Var<String>) -> Neutral {
        Neutral::Head(Head::Var(src), Spine::new())
    }
}

impl From<Var<String>> for Value {
    fn from(src: Var<String>) -> Value {
        Value::from(Neutral::from(src))
    }
}

impl From<Neutral> for Value {
    fn from(src: Neutral) -> Value {
        Value::Neutral(RcNeutral::from(src))
    }
}

impl<'a> From<&'a Value> for Term {
    fn from(src: &'a Value) -> Term {
        // Bypassing `Scope::new` and `Scope::unbind` here should be fine
        // because we aren't altering the structure of the scopes during this
        // transformation. This should save on some traversals of the AST!
        match *src {
            Value::Universe(level) => Term::Universe(level),
            Value::IntType(ref min, ref max) => Term::IntType(
                min.as_ref().map(|x| RcTerm::from(&**x)),
                max.as_ref().map(|x| RcTerm::from(&**x)),
            ),
            Value::Literal(ref lit) => Term::Literal(lit.clone()),
            Value::Pi(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                Term::Pi(Scope {
                    unsafe_pattern: (name.clone(), Embed(RcTerm::from(&**ann))),
                    unsafe_body: RcTerm::from(&*scope.unsafe_body),
                })
            },
            Value::Lam(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                Term::Lam(Scope {
                    unsafe_pattern: (name.clone(), Embed(RcTerm::from(&**ann))),
                    unsafe_body: RcTerm::from(&*scope.unsafe_body),
                })
            },
            Value::Refinement(ref scope) => {
                let (ref name, Embed(ref ann)) = scope.unsafe_pattern;
                Term::Refinement(Scope {
                    unsafe_pattern: (name.clone(), Embed(RcTerm::from(&**ann))),
                    unsafe_body: RcTerm::from(&*scope.unsafe_body),
                })
            },
            Value::Struct(ref fields) => Term::Struct(
                fields
                    .iter()
                    .map(|&(ref field, ref term)| (field.clone(), RcTerm::from(&**term)))
                    .collect(),
            ),
            Value::Array(ref elems) => {
                Term::Array(elems.iter().map(|elem| RcTerm::from(&**elem)).collect())
            },
            Value::Neutral(ref neutral) => Term::from(&*neutral.inner),
        }
    }
}

impl<'a> From<&'a Value> for RcTerm {
    fn from(src: &'a Value) -> RcTerm {
        RcTerm::from(Term::from(src))
    }
}

impl<'a> From<&'a Neutral> for Term {
    fn from(src: &'a Neutral) -> Term {
        let (head, spine) = match *src {
            Neutral::Head(ref head, ref spine) => (Term::from(head), spine),
            Neutral::Proj(ref expr, ref name, ref spine) => {
                (Term::Proj(RcTerm::from(&**expr), name.clone()), spine)
            },
            Neutral::Match(ref head, ref clauses, ref spine) => {
                let clauses = clauses
                    .iter()
                    .map(|clause| Scope {
                        unsafe_pattern: clause.unsafe_pattern.clone(),
                        unsafe_body: RcTerm::from(&*clause.unsafe_body),
                    })
                    .collect();

                (Term::Match(RcTerm::from(&**head), clauses), spine)
            },
        };

        spine.iter().fold(head, |acc, arg| {
            Term::App(RcTerm::from(acc), RcTerm::from(&**arg))
        })
    }
}

impl<'a> From<&'a Neutral> for RcTerm {
    fn from(src: &'a Neutral) -> RcTerm {
        RcTerm::from(Term::from(src))
    }
}

impl<'a> From<&'a Head> for Term {
    fn from(src: &'a Head) -> Term {
        match *src {
            Head::Var(ref var) => Term::Var(var.clone()),
            Head::Extern(ref name) => Term::Extern(name.clone()),
        }
    }
}
