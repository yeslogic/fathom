//! The core syntax of the language

use codespan::ByteSpan;
use nameless::{Debruijn, FreeName, LocallyNameless, GenId, Named, Scope, Var};
use rpds::List;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;
use std::usize;

use syntax::pretty::{self, ToDoc};

#[cfg(test)]
mod tests;

/// Source metadata that should be ignored when checking for alpha equality
#[derive(Debug, Copy, Clone)]
pub struct SourceMeta {
    pub span: ByteSpan,
}

impl Default for SourceMeta {
    fn default() -> SourceMeta {
        SourceMeta {
            span: ByteSpan::default(),
        }
    }
}

impl PartialEq for SourceMeta {
    fn eq(&self, _: &SourceMeta) -> bool {
        true
    }
}

/// The name of a free variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    /// Names originating from user input
    User(String),
    /// A generated id with an optional string that may have come from user input
    Gen(Named<Option<String>, GenId>),
}

impl Name {
    /// Create a name from a human-readable string
    pub fn user<S: Into<String>>(name: S) -> Name {
        Name::User(name.into())
    }

    pub fn name(&self) -> Option<&str> {
        match *self {
            Name::User(ref name) => Some(name),
            Name::Gen(Named { ref name, .. }) => name.as_ref().map(String::as_str),
        }
    }
}

impl FreeName for Name {
    type Hint = String;

    fn fresh(hint: Option<String>) -> Name {
        Name::Gen(Named::new(hint, GenId::fresh())) // FIXME
    }

    fn hint(&self) -> Option<String> {
        match *self {
            Name::User(ref name) => Some(name.clone()),
            Name::Gen(Named { ref name, .. }) => name.clone(),
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Name::User(ref name) => write!(f, "{}", name),
            Name::Gen(ref gen) => match gen.name {
                None => write!(f, "{}", gen.inner),
                Some(ref name) => write!(f, "{}{}", name, gen.inner),
            },
        }
    }
}

/// A universe level
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Level(pub u32);

impl Level {
    pub const ZERO: Level = Level(0);

    pub fn succ(self) -> Level {
        Level(self.0 + 1)
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A module definition
pub struct RawModule {
    /// The name of the module
    pub name: String,
    /// The definitions contained in the module
    pub definitions: Vec<RawDefinition>,
}

impl fmt::Display for RawModule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Top level definitions
pub struct RawDefinition {
    /// The name of the declaration
    pub name: String,
    /// The body of the definition
    pub term: RcRawTerm,
    /// An optional type annotation to aid in type inference
    pub ann: Option<RcRawTerm>,
}

impl fmt::Display for RawDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Raw terms, unchecked and with implicit syntax that needs to be elaborated
///
/// ```text
/// r,R ::= r:R         1. annotated terms
///       | Typeᵢ       2. universes
///       | x           3. variables
///       | Πx:R₁.R₂    4. dependent function types
///       | λx.r        5. lambda abstractions (no annotation)
///       | λx:R.r      5. lambda abstractions (with annotation)
///       | R₁ R₂       6. term application
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum RawTerm {
    /// A term annotated with a type
    Ann(SourceMeta, RcRawTerm, RcRawTerm), // 1.
    /// Universes
    Universe(SourceMeta, Level), // 2.
    /// A variable
    Var(SourceMeta, Var<Name, Debruijn>), // 3.
    /// Dependent function types
    Pi(SourceMeta, Scope<Named<Name, RcRawTerm>, RcRawTerm>), // 5.
    /// Lambda abstractions
    Lam(SourceMeta, Scope<Named<Name, Option<RcRawTerm>>, RcRawTerm>), // 4.
    /// RawTerm application
    App(SourceMeta, RcRawTerm, RcRawTerm), // 6.
}

impl RcRawTerm {
    pub fn span(&self) -> ByteSpan {
        match *self.inner {
            RawTerm::Ann(meta, _, _)
            | RawTerm::Universe(meta, _)
            | RawTerm::Var(meta, _)
            | RawTerm::Pi(meta, _)
            | RawTerm::Lam(meta, _)
            | RawTerm::App(meta, _, _) => meta.span,
        }
    }
}

impl fmt::Display for RawTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

impl LocallyNameless for RcRawTerm {
    type Name = Name;

    fn close_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            RawTerm::Ann(_, ref mut expr, ref mut ty) => {
                expr.close_at(level, name);
                ty.close_at(level, name);
            },
            RawTerm::Universe(_, _) => {},
            RawTerm::Var(_, ref mut var) => var.close_at(level, name),
            RawTerm::Pi(_, ref mut pi) => pi.close_at(level, name),
            RawTerm::Lam(_, ref mut lam) => lam.close_at(level, name),
            RawTerm::App(_, ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.close_at(level, name);
                arg_expr.close_at(level, name);
            },
        }
    }

    fn open_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            RawTerm::Ann(_, ref mut expr, ref mut ty) => {
                expr.open_at(level, name);
                ty.open_at(level, name);
            },
            RawTerm::Universe(_, _) => {},
            RawTerm::Var(_, ref mut var) => var.open_at(level, name),
            RawTerm::Pi(_, ref mut pi) => pi.open_at(level, name),
            RawTerm::Lam(_, ref mut lam) => lam.open_at(level, name),
            RawTerm::App(_, ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.open_at(level, name);
                arg_expr.open_at(level, name);
            },
        }
    }
}

impl RcRawTerm {
    fn visit_vars<F: FnMut(&Var<Name, Debruijn>)>(&self, on_var: &mut F) {
        match *self.inner {
            RawTerm::Ann(_, ref expr, ref ty) => {
                expr.visit_vars(on_var);
                ty.visit_vars(on_var);
            },
            RawTerm::Universe(_, _) => {},
            RawTerm::Var(_, ref var) => on_var(var),
            RawTerm::Pi(_, ref pi) => {
                pi.unsafe_binder.inner.visit_vars(on_var);
                pi.unsafe_body.visit_vars(on_var);
            },
            RawTerm::Lam(_, ref lam) => {
                if let Some(ref param) = lam.unsafe_binder.inner {
                    param.visit_vars(on_var);
                }
                lam.unsafe_body.visit_vars(on_var);
            },
            RawTerm::App(_, ref fn_expr, ref arg_expr) => {
                fn_expr.visit_vars(on_var);
                arg_expr.visit_vars(on_var);
            },
        };
    }

    pub fn free_vars(&self) -> HashSet<Name> {
        let mut free_vars = HashSet::new();
        self.visit_vars(&mut |var| match *var {
            Var::Bound(_) => {},
            Var::Free(ref name) => {
                free_vars.insert(name.clone());
            },
        });
        free_vars
    }
}

/// A typechecked and elaborated module
pub struct Module {
    /// The name of the module
    pub name: String,
    /// The definitions contained in the module
    pub definitions: Vec<Definition>,
}

/// A typechecked and elaborated definition
pub struct Definition {
    /// The name of the definition
    pub name: String,
    /// The elaborated value
    pub term: RcTerm,
    /// The type of the definition
    pub ann: RcType,
}

/// The core term syntax
///
/// ```text
/// t,T ::= t:T         1. annotated terms
///       | Typeᵢ       2. universes
///       | x           3. variables
///       | Πx:T₁.T₂    4. dependent function types
///       | λx:T.t      5. lambda abstractions
///       | t₁ t₂       6. term application
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A term annotated with a type
    Ann(SourceMeta, RcTerm, RcTerm), // 1.
    /// Universes
    Universe(SourceMeta, Level), // 2.
    /// A variable
    Var(SourceMeta, Var<Name, Debruijn>), // 3.
    /// Dependent function types
    Pi(SourceMeta, Scope<Named<Name, RcTerm>, RcTerm>), // 5.
    /// Lambda abstractions
    Lam(SourceMeta, Scope<Named<Name, RcTerm>, RcTerm>), // 4.
    /// Term application
    App(SourceMeta, RcTerm, RcTerm), // 6.
}

impl RcTerm {
    pub fn span(&self) -> ByteSpan {
        match *self.inner {
            Term::Ann(meta, _, _)
            | Term::Universe(meta, _)
            | Term::Var(meta, _)
            | Term::Lam(meta, _)
            | Term::Pi(meta, _)
            | Term::App(meta, _, _) => meta.span,
        }
    }
}

impl LocallyNameless for RcTerm {
    type Name = Name;

    fn close_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            Term::Ann(_, ref mut expr, ref mut ty) => {
                expr.close_at(level, name);
                ty.close_at(level, name);
            },
            Term::Universe(_, _) => {},
            Term::Var(_, ref mut var) => var.close_at(level, name),
            Term::Pi(_, ref mut pi) => pi.close_at(level, name),
            Term::Lam(_, ref mut lam) => lam.close_at(level, name),
            Term::App(_, ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.close_at(level, name);
                arg_expr.close_at(level, name);
            },
        }
    }

    fn open_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            Term::Ann(_, ref mut expr, ref mut ty) => {
                expr.open_at(level, name);
                ty.open_at(level, name);
            },
            Term::Universe(_, _) => {},
            Term::Var(_, ref mut var) => var.open_at(level, name),
            Term::Pi(_, ref mut pi) => pi.open_at(level, name),
            Term::Lam(_, ref mut lam) => lam.open_at(level, name),
            Term::App(_, ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.open_at(level, name);
                arg_expr.open_at(level, name);
            },
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Normal forms
///
/// ```text
/// v,V ::= Typeᵢ       1. universes
///       | Πx:V₁.V₂    2. dependent function types
///       | λx:V.v      3. lambda abstractions
///       | n           4. neutral terms
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Universes
    Universe(Level), // 1.
    /// A pi type
    Pi(Scope<Named<Name, RcValue>, RcValue>), // 2.
    /// A lambda abstraction
    Lam(Scope<Named<Name, RcValue>, RcValue>), // 3.
    /// Neutral terms
    Neutral(RcNeutral), // 4.
}

impl LocallyNameless for RcValue {
    type Name = Name;

    fn close_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            Value::Universe(_) => {},
            Value::Pi(ref mut pi) => pi.close_at(level, name),
            Value::Lam(ref mut lam) => lam.close_at(level, name),
            Value::Neutral(ref mut n) => n.close_at(level, name),
        }
    }

    fn open_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            Value::Universe(_) => {},
            Value::Pi(ref mut pi) => pi.open_at(level, name),
            Value::Lam(ref mut lam) => lam.open_at(level, name),
            Value::Neutral(ref mut n) => n.open_at(level, name),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

/// Neutral terms
///
/// These might be able to be reduced further depending on the bindings in the
/// context
///
/// ```text
/// n,N ::= x           1. variables
///       | n t         2. term application
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Neutral {
    /// Variables
    Var(Var<Name, Debruijn>), // 1.
    /// RawTerm application
    App(RcNeutral, RcTerm), // 2.
}

impl LocallyNameless for RcNeutral {
    type Name = Name;

    fn close_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            Neutral::Var(ref mut var) => var.close_at(level, name),
            Neutral::App(ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.close_at(level, name);
                arg_expr.close_at(level, name);
            },
        }
    }

    fn open_at(&mut self, level: Debruijn, name: &Name) {
        match *Rc::make_mut(&mut self.inner) {
            Neutral::Var(ref mut var) => var.open_at(level, name),
            Neutral::App(ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.open_at(level, name);
                arg_expr.open_at(level, name);
            },
        }
    }
}

impl fmt::Display for Neutral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

// Wrapper types

make_wrapper!(RcRawTerm, Rc, RawTerm);
make_wrapper!(RcTerm, Rc, Term);
make_wrapper!(RcValue, Rc, Value);
make_wrapper!(RcNeutral, Rc, Neutral);

/// Types are at the term level, so this is just an alias
pub type Type = Value;

/// Types are at the term level, so this is just an alias
pub type RcType = RcValue;

impl From<Neutral> for RcValue {
    fn from(src: Neutral) -> RcValue {
        Value::Neutral(src.into()).into()
    }
}

impl<'a> From<&'a RcValue> for RcTerm {
    fn from(src: &'a RcValue) -> RcTerm {
        let meta = SourceMeta::default();

        match *src.inner {
            Value::Universe(level) => Term::Universe(meta, level).into(),
            Value::Lam(ref lam) => {
                let (param, body) = lam.clone().unbind();
                let param = Named::new(param.name.clone(), RcTerm::from(&param.inner));

                Term::Lam(meta, Scope::bind(param, RcTerm::from(&body))).into()
            },
            Value::Pi(ref pi) => {
                let (param, body) = pi.clone().unbind();
                let param = Named::new(param.name.clone(), RcTerm::from(&param.inner));

                Term::Pi(meta, Scope::bind(param, RcTerm::from(&body))).into()
            },
            Value::Neutral(ref n) => RcTerm::from(n),
        }
    }
}

impl<'a> From<&'a RcNeutral> for RcTerm {
    fn from(src: &'a RcNeutral) -> RcTerm {
        let meta = SourceMeta::default();

        match *src.inner {
            Neutral::Var(ref var) => Term::Var(meta, var.clone()).into(),
            Neutral::App(ref fn_expr, ref arg_expr) => {
                Term::App(meta, RcTerm::from(fn_expr), arg_expr.clone()).into()
            },
        }
    }
}

/// A binder that introduces a variable into the context
///
/// ```text
/// b ::= λx:V           1. lambda abstraction
///     | Πx:V           2. dependent function
///     | let x:V = t    3. let binding
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Binder {
    /// A type introduced after entering a lambda abstraction
    Lam { name: Name, ann: RcType }, // 1.
    /// A type introduced after entering a pi type
    Pi { name: Name, ann: RcType }, // 2.
    /// A value and type binding that was introduced by passing over a let binding
    Let {
        name: Name,
        ann: RcType,
        value: RcTerm,
    }, // 3.
}

impl Binder {
    pub fn span(&self) -> ByteSpan {
        // TODO: real span
        ByteSpan::default()
    }

    pub fn name(&self) -> &Name {
        match *self {
            Binder::Lam { ref name, .. }
            | Binder::Pi { ref name, .. }
            | Binder::Let { ref name, .. } => name,
        }
    }
}

/// A list of binders that have been accumulated during typechecking
///
/// ```text
/// Γ ::= ε           1. empty context
///     | Γ,b         2. context extension
/// ```
#[derive(Clone, PartialEq)]
pub struct Context {
    pub binders: List<Binder>,
}

impl Context {
    /// Create a new, empty context
    pub fn new() -> Context {
        Context {
            binders: List::new(),
        }
    }

    /// Extend the context with a binder
    pub fn extend(&self, binder: Binder) -> Context {
        Context {
            binders: self.binders.push_front(binder),
        }
    }

    pub fn extend_lam(&self, name: Name, ann: RcType) -> Context {
        self.extend(Binder::Lam { name, ann })
    }

    pub fn extend_pi(&self, name: Name, ann: RcType) -> Context {
        self.extend(Binder::Pi { name, ann })
    }

    pub fn extend_let(&self, name: Name, ann: RcType, value: RcTerm) -> Context {
        self.extend(Binder::Let { name, ann, value })
    }

    pub fn lookup_binder(&self, name: &Name) -> Option<&Binder> {
        self.binders.iter().find(|binder| binder.name() == name)
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc(pretty::Options::default().with_debug_indices(f.alternate()))
            .group()
            .render_fmt(f.width().unwrap_or(usize::MAX), f)
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct FmtBinders<'a>(&'a List<Binder>);

        impl<'a> fmt::Debug for FmtBinders<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_list().entries(self.0).finish()
            }
        }

        f.debug_struct("Context")
            .field("binders", &FmtBinders(&self.binders))
            .finish()
    }
}
