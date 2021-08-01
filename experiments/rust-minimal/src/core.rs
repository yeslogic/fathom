//! Core language.

use crate::env::{GlobalVar, LocalVar, UniqueEnv};
use crate::StringId;

pub mod semantics;

/// Information about rigid entries. This is used for [flexible variable
/// insertion][Term::FlexibleInsertion].
//
// NOTE: Bikeshed of alternative names:
//
// - observability ::= ?
// - perceptibility ::= ?
// - denotion ::= ?
// - binder-info ::= ?
// - ? ::= transparent | opaque
// - ? ::= concrete | abstract
// - ? ::= definition | parameter
//
// See also: https://en.wikipedia.org/wiki/Abstract_and_concrete
#[derive(Debug, Clone)]
pub enum EntryInfo {
    Concrete,
    Abstract,
}

/// Core language terms.
#[derive(Debug, Clone)]
pub enum Term<'arena> {
    /// Rigid variable occurrences.
    ///
    /// These correspond to variables that were most likely bound as a result of
    /// user code, for example from [let expressions]), [function types] and
    /// [function introductions].
    ///
    /// [let expressions]: Term::Let
    /// [function types]: Term::FunType
    /// [function introductions]: Term::FunIntro
    ///
    /// ## References
    ///
    /// - [A unification algorithm for typed λ-calculus](https://doi.org/10.1016/0304-3975(75)90011-0)
    /// - [Type Classes: Rigid type variables](https://typeclasses.com/rigid-type-variables)
    RigidVar(LocalVar),
    /// Flexible variable occurrences.
    ///
    /// These are inserted during [elaboration] when we have something we want
    /// pattern unification to fill in for us. They are 'flexible' because the
    /// expressions that they correspond to might be updated (from unknown to
    /// known) during unification.
    ///
    /// Also known as: metavariables.
    ///
    /// [elaboration]: crate::surface::elaboration
    FlexibleVar(GlobalVar),
    /// A flexible variable that has just been inserted.
    ///
    /// The environment of [`EntryInfo`]s records the state of the rigid
    /// environment at the time of insertion, allowing us to apply the rigid
    /// parameters to the flexible variable during [evaluation].
    ///
    /// [evaluation]: semantics::EvalContext::eval
    //
    // TODO: Bit-vectors might make this a bit more compact. For example:
    //
    // - https://lib.rs/crates/smallbitvec
    // - https://lib.rs/crates/bit-vec
    FlexibleInsertion(GlobalVar, UniqueEnv<EntryInfo>),
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
