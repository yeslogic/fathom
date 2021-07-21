//! Core language.

use crate::env::{GlobalVar, LocalVar, UniqueEnv};
use crate::StringId;

pub mod semantics;

/// The mode of a binding, used for problem insertion.
#[derive(Debug, Clone)]
pub enum BindingMode {
    Defined,
    Assumed,
}

/// Core language terms.
#[derive(Debug, Clone)]
pub enum Term<'arena> {
    /// Bound variable occurrences.
    BoundVar(LocalVar),
    /// Problem variable occurrences.
    ///
    /// Also known as: metavariables.
    ProblemVar(GlobalVar),
    /// A problem variable that has just been inserted.
    ///
    /// The bindings modes let us apply the bound assumptions in scope to the
    /// problem during evaluation. We could also represent this as a series of
    /// function eliminations, but encoding it as an environment is more direct
    /// and efficient.
    //
    // TODO: Bit-vectors might make this a bit more compact. For example:
    //
    // - https://lib.rs/crates/smallbitvec
    // - https://lib.rs/crates/bit-vec
    InsertedProblem(GlobalVar, UniqueEnv<BindingMode>),
    /// Annotated expressions.
    Ann(&'arena Term<'arena>, &'arena Term<'arena>),
    /// Let expressions.
    Let(StringId, &'arena Term<'arena>, &'arena Term<'arena>),
    /// The type of types.
    Universe,
    /// Dependent function types.
    ///
    /// Also known as: pi types, dependent product types.
    FunType(Option<StringId>, &'arena Term<'arena>, &'arena Term<'arena>),
    /// Function introductions.
    ///
    /// Also known as: lambda expressions, anonymous functions.
    FunIntro(Option<StringId>, &'arena Term<'arena>),
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
