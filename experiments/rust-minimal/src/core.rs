//! Core language.

use crate::env::{GlobalVar, LocalVar};
use crate::StringId;

pub mod semantics;

/// Core language terms.
#[derive(Debug, Clone)]
pub enum Term<'arena> {
    /// Bound variable occurrences.
    BoundVar(LocalVar),
    /// Unification variable occurrences.
    ///
    /// Also known as: metavariables.
    UnificationVar(GlobalVar),
    /// Annotated expressions.
    Ann(&'arena Term<'arena>, &'arena Term<'arena>),
    /// Let expressions.
    Let(StringId, &'arena Term<'arena>, &'arena Term<'arena>),
    /// The type of types.
    Universe,
    /// Dependent function types.
    ///
    /// Also known as: pi types, dependent product types.
    FunType(StringId, &'arena Term<'arena>, &'arena Term<'arena>),
    /// Function introductions.
    ///
    /// Also known as: lambda expressions, anonymous functions.
    FunIntro(StringId, &'arena Term<'arena>),
    /// Function eliminations.
    ///
    /// Also known as: function applications.
    FunElim(&'arena Term<'arena>, &'arena Term<'arena>),
    // RecordType(&'arena [StringId], &'arena [Term<'arena>]),
    // RecordIntro(&'arena [StringId], &'arena [Term<'arena>]),
    // RecordElim(&'arena Term<'arena>, StringId),
    /// Reported errors.
    ReportedError,
}

/// Arena for storing data related to [`Term`]s.
pub struct Arena<'arena> {
    terms: typed_arena::Arena<Term<'arena>>,
}

impl<'arena> Arena<'arena> {
    pub fn new() -> Arena<'arena> {
        Arena {
            terms: typed_arena::Arena::new(),
        }
    }

    pub fn alloc_term(&self, term: Term<'arena>) -> &mut Term<'arena> {
        self.terms.alloc(term)
    }
}
