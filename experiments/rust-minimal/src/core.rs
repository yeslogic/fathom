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
    /// A flexible variable that has been inserted during elaboration, along
    /// with the [entry information] in the rigid environment at the time of
    /// insertion.
    ///
    /// The entry information will let us know what rigidly bound parameters to
    /// apply to the flexible variable during [evaluation]. The applied
    /// parameters will correspond to the [function introductions] that will be
    /// addded to the flexible solution during unification.
    ///
    /// We clone the entry information and perform the function eliminations
    /// during evaluation because elaborating to a series of [function
    /// eliminations] directly would involve expensively [quoting] each
    /// parameter.
    ///
    /// For example, given the following code:
    ///
    /// ```text
    /// let test : fn (A : Type) -> A -> A
    ///   = fn A => fn a =>
    ///        let b : A = a; ?x;
    /// Type
    /// ```
    ///
    /// This would be elaborated to:
    ///
    /// ```text
    /// let test : fn (A : Type) -> A -> A
    ///   = fn A => fn a =>
    /// //     │       │
    /// //     │       ╰────────────╮
    /// //     ╰──────────────────╮ │
    /// //                        │ │
    /// //                        ▼ ▼
    ///        let b : A = a; (?x A a);
    /// //                     ^^^^^^  the flexible insertion
    /// Type
    /// ```
    ///
    /// Notice how `A` and `a` are applied to the flexible variable `?x`,
    /// because they are bound as rigid parameters, where as `b` is _not_
    /// applied, because it is bound as a definition.
    ///
    /// [entry information]: EntryInfo
    /// [function introductions]: Term::FunIntro
    /// [function eliminations]: Term::FunElim
    /// [evaluation]: semantics::EvalContext::eval
    /// [quoting]: semantics::QuoteContext::quote
    //
    // TODO: Bit-vectors might make this a bit more compact and cheaper to
    //       construct. For example:
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
    /// Dependent record types.
    RecordType(&'arena [StringId], &'arena [Term<'arena>]),
    /// Record introductions.
    RecordIntro(&'arena [StringId], &'arena [Term<'arena>]),
    /// Record eliminations.
    RecordElim(&'arena Term<'arena>, StringId),

    /// Type of unsigned, 8-bit integers.
    U8Type,
    /// Type of unsigned, 16-bit integers.
    U16Type,
    /// Type of unsigned, 32-bit integers.
    U32Type,
    /// Type of unsigned, 64-bit integers.
    U64Type,
    /// Type of signed, two's complement, 8-bit integers.
    S8Type,
    /// Type of signed, two's complement, 16-bit integers.
    S16Type,
    /// Type of signed, two's complement, 32-bit integers.
    S32Type,
    /// Type of signed, two's complement, 64-bit integers.
    S64Type,
    /// Type of 32-bit, IEEE-754 floating point numbers.
    F32Type,
    /// Type of 64-bit, IEEE-754 floating point numbers.
    F64Type,

    /// Type of format descriptions.
    FormatType,
    /// Record formats, consisting of a list of dependent formats.
    FormatRecord(&'arena [StringId], &'arena [Term<'arena>]),
    /// A format that always fails to parse.
    FormatFail,
    /// Unsigned, 8-bit integer formats.
    FormatU8,
    /// Unsigned, 16-bit integer formats (big-endian).
    FormatU16Be,
    /// Unsigned, 16-bit integer formats (little-endian).
    FormatU16Le,
    /// Unsigned, 32-bit integer formats (big-endian).
    FormatU32Be,
    /// Unsigned, 32-bit integer formats (little-endian).
    FormatU32Le,
    /// Unsigned, 64-bit integer formats (big-endian).
    FormatU64Be,
    /// Unsigned, 64-bit integer formats (little-endian).
    FormatU64Le,
    /// Signed, two's complement, 8-bit integer formats.
    FormatS8,
    /// Signed, two's complement, 16-bit integer formats (big-endian).
    FormatS16Be,
    /// Signed, two's complement, 16-bit integer formats (little-endian).
    FormatS16Le,
    /// Signed, two's complement, 32-bit integer formats (big-endian).
    FormatS32Be,
    /// Signed, two's complement, 32-bit integer formats (little-endian).
    FormatS32Le,
    /// Signed, two's complement, 64-bit integer formats (big-endian).
    FormatS64Be,
    /// Signed, two's complement, 64-bit integer formats (little-endian).
    FormatS64Le,
    /// 32-bit, IEEE-754 floating point formats (big-endian).
    FormatF32Be,
    /// 32-bit, IEEE-754 floating point formats (little-endian).
    FormatF32Le,
    /// 64-bit, IEEE-754 floating point formats (big-endian).
    FormatF64Be,
    /// 64-bit, IEEE-754 floating point formats (little-endian).
    FormatF64Le,
    /// Format representations.
    FormatRepr(&'arena Term<'arena>),

    /// Reported errors.
    ReportedError,
}
