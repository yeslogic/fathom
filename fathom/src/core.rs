//! Core language.

use std::fmt;

use crate::env::{Index, Level};
use crate::source::{Span, StringId};

pub mod binary;
pub mod pretty;
pub mod prim;
pub mod semantics;

/// Modules
pub struct Module<'arena> {
    pub items: &'arena [Item<'arena>],
}

/// Top-level items
pub enum Item<'arena> {
    /// Top-level definitions
    Def {
        /// The label that identifies this definition
        label: StringId,
        /// The type of the defined expression
        r#type: &'arena Term<'arena>,
        /// The defined expression
        expr: &'arena Term<'arena>,
    },
}

/// Information about how local variables were bound. This is  used when
/// inserting [metavariables][Term::InsertedMeta] during elaboration.
//
// See also: https://en.wikipedia.org/wiki/Abstract_and_concrete
#[derive(Debug, Copy, Clone)]
pub enum LocalInfo {
    /// The entry was bound as a definition in the environment.
    Def,
    /// The entry was bound as a parameter in the environment
    Param,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Plicity {
    Explicit,
    Implicit,
}

impl fmt::Display for Plicity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Plicity::Explicit => write!(f, "explicit"),
            Plicity::Implicit => write!(f, "implicit"),
        }
    }
}

/// Core language terms.
#[derive(Debug, Clone)]
pub enum Term<'arena> {
    /// Item variable occurrences.
    ///
    /// These refer to [items][Item] bound at the top-level of a
    /// [module][Module].
    ItemVar(Span, Level),
    /// Local variable occurrences.
    ///
    /// These correspond to variables that were most likely bound as a result of
    /// user code, for example from [let expressions]), [function types] and
    /// [function literals].
    ///
    /// [let expressions]: Term::Let
    /// [function types]: Term::FunType
    /// [function literals]: Term::FunLit
    ///
    /// ## References
    ///
    /// - [A unification algorithm for typed λ-calculus](https://doi.org/10.1016/0304-3975(75)90011-0)
    /// - [Type Classes: Rigid type variables](https://typeclasses.com/local-type-variables)
    LocalVar(Span, Index),
    /// Metavariable occurrences.
    ///
    /// These refer to unification problems that were originally inserted during
    /// [elaboration]. when we have a term that we want pattern unification to
    /// infer for us based on how the variable is used later on in the program.
    ///
    /// [elaboration]: crate::surface::elaboration
    MetaVar(Span, Level),
    /// A metavariable that has been inserted during elaboration, along
    /// with the [entry information] in the local environment at the time of
    /// insertion.
    ///
    /// The entry information will let us know what locally bound parameters to
    /// apply to the metavariable during [evaluation]. The applied parameters
    /// will correspond to the [function literals] that will be added to the
    /// meta solution during unification.
    ///
    /// We clone the entry information and perform the function applications
    /// during evaluation because elaborating to a series of [function
    /// applications] directly would involve expensively [quoting] each
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
    /// //                     ^^^^^^  the inserted metavariable
    /// Type
    /// ```
    ///
    /// Notice how `A` and `a` are applied to the metavariable `?x`,
    /// because they are bound as local parameters, where as `b` is _not_
    /// applied, because it is bound as a definition.
    ///
    /// [entry information]: EntryInfo
    /// [function literals]: Term::FunLit
    /// [function applications]: Term::FunApp
    /// [evaluation]: semantics::EvalEnv::eval
    /// [quoting]: semantics::QuoteEnv::quote
    //
    // TODO: Bit-vectors might make this a bit more compact and cheaper to
    //       construct. For example:
    //
    // - https://lib.rs/crates/smallbitvec
    // - https://lib.rs/crates/bit-vec
    InsertedMeta(Span, Level, &'arena [LocalInfo]),
    /// Annotated expressions.
    Ann(Span, &'arena Term<'arena>, &'arena Term<'arena>),
    /// Let expressions.
    Let(
        Span,
        Option<StringId>,
        &'arena Term<'arena>,
        &'arena Term<'arena>,
        &'arena Term<'arena>,
    ),

    /// The type of types.
    Universe(Span),

    /// Dependent function types.
    ///
    /// Also known as: pi types, dependent product types.
    FunType(
        Span,
        Plicity,
        Option<StringId>,
        &'arena Term<'arena>,
        &'arena Term<'arena>,
    ),
    /// Function literals.
    ///
    /// Also known as: lambda expressions, anonymous functions.
    FunLit(Span, Plicity, Option<StringId>, &'arena Term<'arena>),
    /// Function applications.
    FunApp(Span, Plicity, &'arena Term<'arena>, &'arena Term<'arena>),

    /// Dependent record types.
    RecordType(Span, &'arena [StringId], &'arena [Term<'arena>]),
    /// Record literals.
    RecordLit(Span, &'arena [StringId], &'arena [Term<'arena>]),
    /// Record projections.
    RecordProj(Span, &'arena Term<'arena>, StringId),

    /// Array literals.
    ArrayLit(Span, &'arena [Term<'arena>]),

    /// Record formats, consisting of a list of dependent formats.
    FormatRecord(Span, &'arena [StringId], &'arena [Term<'arena>]),
    /// Conditional format, consisting of a format and predicate.
    FormatCond(Span, StringId, &'arena Term<'arena>, &'arena Term<'arena>),
    /// Overlap formats, consisting of a list of dependent formats, overlapping
    /// in memory.
    FormatOverlap(Span, &'arena [StringId], &'arena [Term<'arena>]),

    /// Primitives.
    Prim(Span, Prim),

    /// Constant literals.
    ConstLit(Span, Const),
    /// Match on a constant. The pattern branches should be unique, and listed
    /// in lexicographic order.
    ConstMatch(
        Span,
        &'arena Term<'arena>,
        &'arena [(Const, Term<'arena>)],
        Option<(Option<StringId>, &'arena Term<'arena>)>,
    ),
}

impl<'arena> Term<'arena> {
    /// Get the source span of the term.
    pub fn span(&self) -> Span {
        match self {
            Term::ItemVar(span, _)
            | Term::LocalVar(span, _)
            | Term::MetaVar(span, _)
            | Term::InsertedMeta(span, _, _)
            | Term::Ann(span, _, _)
            | Term::Let(span, _, _, _, _)
            | Term::Universe(span)
            | Term::FunType(span, ..)
            | Term::FunLit(span, ..)
            | Term::FunApp(span, ..)
            | Term::RecordType(span, _, _)
            | Term::RecordLit(span, _, _)
            | Term::RecordProj(span, _, _)
            | Term::ArrayLit(span, _)
            | Term::FormatRecord(span, _, _)
            | Term::FormatCond(span, _, _, _)
            | Term::FormatOverlap(span, _, _)
            | Term::Prim(span, _)
            | Term::ConstLit(span, _)
            | Term::ConstMatch(span, _, _, _) => *span,
        }
    }

    /// Returns `true` if the term contains an occurrence of the local variable.
    pub fn binds_local(&self, mut var: Index) -> bool {
        match self {
            Term::LocalVar(_, v) => *v == var,
            Term::ItemVar(_, _)
            | Term::MetaVar(_, _)
            | Term::InsertedMeta(_, _, _)
            | Term::Universe(_)
            | Term::Prim(_, _)
            | Term::ConstLit(_, _) => false,

            Term::Ann(_, expr, r#type) => expr.binds_local(var) || r#type.binds_local(var),
            Term::Let(_, _, def_type, def_expr, body_expr) => {
                def_type.binds_local(var)
                    || def_expr.binds_local(var)
                    || body_expr.binds_local(var.prev())
            }
            Term::FunType(.., param_type, body_type) => {
                param_type.binds_local(var) || body_type.binds_local(var.prev())
            }
            Term::FunLit(.., body_expr) => body_expr.binds_local(var.prev()),
            Term::FunApp(.., head_expr, arg_expr) => {
                head_expr.binds_local(var) || arg_expr.binds_local(var)
            }
            Term::RecordType(_, _, terms)
            | Term::RecordLit(_, _, terms)
            | Term::FormatRecord(_, _, terms)
            | Term::FormatOverlap(_, _, terms) => terms.iter().any(|term| {
                let result = term.binds_local(var);
                var = var.prev();
                result
            }),
            Term::RecordProj(_, head_expr, _) => head_expr.binds_local(var),
            Term::ArrayLit(_, elem_exprs) => elem_exprs.iter().any(|term| term.binds_local(var)),
            Term::FormatCond(_, _, format, pred) => {
                format.binds_local(var) || pred.binds_local(var.prev())
            }
            Term::ConstMatch(_, scrut, branches, default_expr) => {
                scrut.binds_local(var)
                    || branches.iter().any(|(_, term)| term.binds_local(var))
                    || default_expr.map_or(false, |(_, term)| term.binds_local(var.prev()))
            }
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Term::Prim(_, Prim::ReportedError))
    }
}

macro_rules! def_prims {
    ($($(#[$prim_attr:meta])* $PrimName:ident => $prim_name:literal),* $(,)?) => {
        /// Primitives.
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum Prim {
            $($(#[$prim_attr])* $PrimName),*
        }

        impl Prim {
            pub const fn name(&self) -> &'static str {
                match self {
                    $(Prim::$PrimName => $prim_name),*
                }
            }
        }
    };
}

def_prims! {
    /// Void type.
    VoidType => "Void",

    /// Void eliminator.
    Absurd => "absurd",

    /// Type of booleans.
    BoolType => "Bool",
    /// Type of unsigned, 8-bit integers.
    U8Type => "U8",
    /// Type of unsigned, 16-bit integers.
    U16Type => "U16",
    /// Type of unsigned, 32-bit integers.
    U32Type => "U32",
    /// Type of unsigned, 64-bit integers.
    U64Type => "U64",
    /// Type of signed, two's complement, 8-bit integers.
    S8Type => "S8",
    /// Type of signed, two's complement, 16-bit integers.
    S16Type => "S16",
    /// Type of signed, two's complement, 32-bit integers.
    S32Type => "S32",
    /// Type of signed, two's complement, 64-bit integers.
    S64Type => "S64",
    /// Type of 32-bit, IEEE-754 floating point numbers.
    F32Type => "F32",
    /// Type of 64-bit, IEEE-754 floating point numbers.
    F64Type => "F64",
    /// Type of optional data.
    OptionType => "Option",
    /// Type of dynamically sized arrays.
    ArrayType => "Array",
    /// Type of arrays, with 8-bit indices.
    Array8Type => "Array8",
    /// Type of arrays, with 16-bit indices.
    Array16Type => "Array16",
    /// Type of arrays, with 32-bit indices.
    Array32Type => "Array32",
    /// Type of arrays, with 64-bit indices.
    Array64Type => "Array64",
    /// Type of stream positions.
    PosType => "Pos",
    /// Type of stream references.
    RefType => "Ref",

    /// Type of format descriptions.
    FormatType => "Format",
    /// Unsigned, 8-bit integer formats.
    FormatU8 => "u8",
    /// Unsigned, 16-bit integer formats (big-endian).
    FormatU16Be => "u16be",
    /// Unsigned, 16-bit integer formats (little-endian).
    FormatU16Le => "u16le",
    /// Unsigned, 32-bit integer formats (big-endian).
    FormatU32Be => "u32be",
    /// Unsigned, 32-bit integer formats (little-endian).
    FormatU32Le => "u32le",
    /// Unsigned, 64-bit integer formats (big-endian).
    FormatU64Be => "u64be",
    /// Unsigned, 64-bit integer formats (little-endian).
    FormatU64Le => "u64le",
    /// Signed, two's complement, 8-bit integer formats.
    FormatS8 => "s8",
    /// Signed, two's complement, 16-bit integer formats (big-endian).
    FormatS16Be => "s16be",
    /// Signed, two's complement, 16-bit integer formats (little-endian).
    FormatS16Le => "s16le",
    /// Signed, two's complement, 32-bit integer formats (big-endian).
    FormatS32Be => "s32be",
    /// Signed, two's complement, 32-bit integer formats (little-endian).
    FormatS32Le => "s32le",
    /// Signed, two's complement, 64-bit integer formats (big-endian).
    FormatS64Be => "s64be",
    /// Signed, two's complement, 64-bit integer formats (little-endian).
    FormatS64Le => "s64le",
    /// 32-bit, IEEE-754 floating point formats (big-endian).
    FormatF32Be => "f32be",
    /// 32-bit, IEEE-754 floating point formats (little-endian).
    FormatF32Le => "f32le",
    /// 64-bit, IEEE-754 floating point formats (big-endian).
    FormatF64Be => "f64be",
    /// 64-bit, IEEE-754 floating point formats (little-endian).
    FormatF64Le => "f64le",
    /// Repeat formats up to an unsigned 8-bit length.
    FormatRepeatLen8 => "repeat_len8",
    /// Repeat formats up to an unsigned 16-bit length.
    FormatRepeatLen16 => "repeat_len16",
    /// Repeat formats up to an unsigned 32-bit length.
    FormatRepeatLen32 => "repeat_len32",
    /// Repeat formats up to an unsigned 64-bit length.
    FormatRepeatLen64 => "repeat_len64",
    /// Repeat a format until the length of the given parse scope is reached.
    FormatRepeatUntilEnd => "repeat_until_end",
    /// Limit the format to an unsigned 8-bit byte length.
    FormatLimit8 => "limit8",
    /// Limit the format to an unsigned 16-bit byte length.
    FormatLimit16 => "limit16",
    /// Limit the format to an unsigned 32-bit byte length.
    FormatLimit32 => "limit32",
    /// Limit the format to an unsigned 64-bit byte length.
    FormatLimit64 => "limit64",
    /// A format which returns the current position in the input stream.
    FormatStreamPos => "stream_pos",
    /// A format that links to another location in the binary data stream,
    /// relative to a base position.
    FormatLink => "link",
    /// A format that forces a reference to be read eagerly.
    FormatDeref => "deref",
    /// A format that always succeeds with some data.
    FormatSucceed => "succeed",
    /// A format that always fails to parse.
    FormatFail => "fail",
    /// Unwrap an option, or fail to parse.
    FormatUnwrap => "unwrap",
    /// Format representations.
    FormatRepr => "Repr",

    /// Reported errors.
    ReportedError => "reported_error",

    BoolEq  => "bool_eq",
    BoolNeq => "bool_neq",
    BoolNot => "bool_not",
    BoolAnd => "bool_and",
    BoolOr  => "bool_or",
    BoolXor => "bool_xor",

    U8Eq  => "u8_eq",
    U8Neq => "u8_neq",
    U8Gt  => "u8_gt",
    U8Lt  => "u8_lt",
    U8Gte => "u8_gte",
    U8Lte => "u8_lte",
    U8Add => "u8_add",
    U8Sub => "u8_sub",
    U8Mul => "u8_mul",
    U8Div => "u8_div",
    U8Not => "u8_not",
    U8Shl => "u8_shl",
    U8Shr => "u8_shr",
    U8And => "u8_and",
    U8Or  => "u8_or",
    U8Xor => "u8_xor",

    U16Eq  => "u16_eq",
    U16Neq => "u16_neq",
    U16Gt  => "u16_gt",
    U16Lt  => "u16_lt",
    U16Gte => "u16_gte",
    U16Lte => "u16_lte",
    U16Add => "u16_add",
    U16Sub => "u16_sub",
    U16Mul => "u16_mul",
    U16Div => "u16_div",
    U16Not => "u16_not",
    U16Shl => "u16_shl",
    U16Shr => "u16_shr",
    U16And => "u16_and",
    U16Or  => "u16_or",
    U16Xor => "u16_xor",

    U32Eq  => "u32_eq",
    U32Neq => "u32_neq",
    U32Gt  => "u32_gt",
    U32Lt  => "u32_lt",
    U32Gte => "u32_gte",
    U32Lte => "u32_lte",
    U32Add => "u32_add",
    U32Sub => "u32_sub",
    U32Mul => "u32_mul",
    U32Div => "u32_div",
    U32Not => "u32_not",
    U32Shl => "u32_shl",
    U32Shr => "u32_shr",
    U32And => "u32_and",
    U32Or  => "u32_or",
    U32Xor => "u32_xor",

    U64Eq  => "u64_eq",
    U64Neq => "u64_neq",
    U64Gt  => "u64_gt",
    U64Lt  => "u64_lt",
    U64Gte => "u64_gte",
    U64Lte => "u64_lte",
    U64Add => "u64_add",
    U64Sub => "u64_sub",
    U64Mul => "u64_mul",
    U64Div => "u64_div",
    U64Not => "u64_not",
    U64Shl => "u64_shl",
    U64Shr => "u64_shr",
    U64And => "u64_and",
    U64Or  => "u64_or",
    U64Xor => "u64_xor",

    S8Eq  => "s8_eq",
    S8Neq => "s8_neq",
    S8Gt  => "s8_gt",
    S8Lt  => "s8_lt",
    S8Gte => "s8_gte",
    S8Lte => "s8_lte",
    S8Neg => "s8_neg",
    S8Add => "s8_add",
    S8Sub => "s8_sub",
    S8Mul => "s8_mul",
    S8Div => "s8_div",
    S8Abs => "s8_abs",
    S8UAbs => "s8_unsigned_abs",

    S16Eq  => "s16_eq",
    S16Neq => "s16_neq",
    S16Gt  => "s16_gt",
    S16Lt  => "s16_lt",
    S16Gte => "s16_gte",
    S16Lte => "s16_lte",
    S16Neg => "s16_neg",
    S16Add => "s16_add",
    S16Sub => "s16_sub",
    S16Mul => "s16_mul",
    S16Div => "s16_div",
    S16Abs => "s16_abs",
    S16UAbs => "s16_unsigned_abs",

    S32Eq  => "s32_eq",
    S32Neq => "s32_neq",
    S32Gt  => "s32_gt",
    S32Lt  => "s32_lt",
    S32Gte => "s32_gte",
    S32Lte => "s32_lte",
    S32Neg => "s32_neg",
    S32Add => "s32_add",
    S32Sub => "s32_sub",
    S32Mul => "s32_mul",
    S32Div => "s32_div",
    S32Abs => "s32_abs",
    S32UAbs => "s32_unsigned_abs",

    S64Eq  => "s64_eq",
    S64Neq => "s64_neq",
    S64Gt  => "s64_gt",
    S64Lt  => "s64_lt",
    S64Gte => "s64_gte",
    S64Lte => "s64_lte",
    S64Neg => "s64_neg",
    S64Add => "s64_add",
    S64Sub => "s64_sub",
    S64Mul => "s64_mul",
    S64Div => "s64_div",
    S64Abs => "s64_abs",
    S64UAbs => "s64_unsigned_abs",

    OptionSome => "some",
    OptionNone => "none",
    OptionFold => "option_fold",

    Array8Find => "array8_find",
    Array16Find => "array16_find",
    Array32Find => "array32_find",
    Array64Find => "array64_find",

    Array8Index => "array8_index",
    Array16Index => "array16_index",
    Array32Index => "array32_index",
    Array64Index => "array64_index",

    PosAddU8  => "pos_add_u8",
    PosAddU16 => "pos_add_u16",
    PosAddU32 => "pos_add_u32",
    PosAddU64 => "pos_add_u64",
}

/// Formatting style for integers
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd)]
pub enum UIntStyle {
    Binary,
    Decimal,
    Hexadecimal,
    /// A [four-character code](https://en.wikipedia.org/wiki/FourCC) (big-endian)
    Ascii,
}

/// Constants
#[derive(Debug, Copy, Clone)]
pub enum Const {
    Bool(bool),
    U8(u8, UIntStyle),
    U16(u16, UIntStyle),
    U32(u32, UIntStyle),
    U64(u64, UIntStyle),
    S8(i8),
    S16(i16),
    S32(i32),
    S64(i64),
    F32(f32),
    F64(f64),
    Pos(usize),
    Ref(usize),
}

impl PartialEq for Const {
    fn eq(&self, other: &Self) -> bool {
        match (*self, *other) {
            (Const::Bool(a), Const::Bool(b)) => a == b,
            (Const::U8(a, _), Const::U8(b, _)) => a == b,
            (Const::U16(a, _), Const::U16(b, _)) => a == b,
            (Const::U32(a, _), Const::U32(b, _)) => a == b,
            (Const::U64(a, _), Const::U64(b, _)) => a == b,
            (Const::S8(a), Const::S8(b)) => a == b,
            (Const::S16(a), Const::S16(b)) => a == b,
            (Const::S32(a), Const::S32(b)) => a == b,
            (Const::S64(a), Const::S64(b)) => a == b,
            (Const::F32(a), Const::F32(b)) => a.total_cmp(&b).is_eq(),
            (Const::F64(a), Const::F64(b)) => a.total_cmp(&b).is_eq(),
            (Const::Pos(a), Const::Pos(b)) => a == b,
            (Const::Ref(a), Const::Ref(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Const {}

impl PartialOrd for Const {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Const {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (*self, *other) {
            (Const::Bool(a), Const::Bool(b)) => a.cmp(&b),
            (Const::U8(a, _), Const::U8(b, _)) => a.cmp(&b),
            (Const::U16(a, _), Const::U16(b, _)) => a.cmp(&b),
            (Const::U32(a, _), Const::U32(b, _)) => a.cmp(&b),
            (Const::U64(a, _), Const::U64(b, _)) => a.cmp(&b),
            (Const::S8(a), Const::S8(b)) => a.cmp(&b),
            (Const::S16(a), Const::S16(b)) => a.cmp(&b),
            (Const::S32(a), Const::S32(b)) => a.cmp(&b),
            (Const::S64(a), Const::S64(b)) => a.cmp(&b),
            (Const::F32(a), Const::F32(b)) => a.total_cmp(&b),
            (Const::F64(a), Const::F64(b)) => a.total_cmp(&b),
            (Const::Pos(a), Const::Pos(b)) => a.cmp(&b),
            (Const::Ref(a), Const::Ref(b)) => a.cmp(&b),
            _ => {
                fn discriminant(r#const: &Const) -> usize {
                    match r#const {
                        Const::Bool(_) => 0,
                        Const::U8(_, _) => 1,
                        Const::U16(_, _) => 2,
                        Const::U32(_, _) => 3,
                        Const::U64(_, _) => 4,
                        Const::S8(_) => 5,
                        Const::S16(_) => 6,
                        Const::S32(_) => 7,
                        Const::S64(_) => 8,
                        Const::F32(_) => 9,
                        Const::F64(_) => 10,
                        Const::Pos(_) => 11,
                        Const::Ref(_) => 12,
                    }
                }

                let tag1 = discriminant(self);
                let tag2 = discriminant(other);
                tag1.cmp(&tag2)
            }
        }
    }
}

pub trait ToBeBytes<const N: usize> {
    fn to_be_bytes(self) -> [u8; N];
}

macro_rules! impl_styled_uint {
    ($($ty:ty),*) => {
        $(
        impl ToBeBytes<{std::mem::size_of::<$ty>()}> for &$ty {
            fn to_be_bytes(self) -> [u8; std::mem::size_of::<$ty>()] {
                <$ty>::to_be_bytes(*self)
            }
        }

        impl UIntStyled<{std::mem::size_of::<$ty>()}> for &$ty {}
        )*
    };
}

impl_styled_uint!(u8, u16, u32, u64);

pub trait UIntStyled<const N: usize>:
    std::fmt::Display + Copy + std::fmt::LowerHex + std::fmt::Binary + ToBeBytes<N>
{
}

impl UIntStyle {
    pub fn format<T: UIntStyled<N>, const N: usize>(&self, number: T) -> String {
        match self {
            UIntStyle::Binary => format!("0b{number:b}"),
            UIntStyle::Decimal => number.to_string(),
            UIntStyle::Hexadecimal => format!("0x{number:x}"),
            UIntStyle::Ascii => {
                let bytes = number.to_be_bytes();
                if bytes.iter().all(|c| c.is_ascii() && !c.is_ascii_control()) {
                    let s = std::str::from_utf8(&bytes).unwrap(); // unwrap safe due to above check
                    format!("\"{s}\"")
                } else {
                    format!("0x{number:x}")
                }
            }
        }
    }

    pub fn merge(left: UIntStyle, right: UIntStyle) -> UIntStyle {
        use UIntStyle::*;

        match (left, right) {
            // If one is the default style, then return the other
            (Decimal, style) | (style, Decimal) => style,
            // When both styles are the same. Note: (Decimal, Decimal) is handled above
            (Binary, Binary) => Binary,
            (Hexadecimal, Hexadecimal) => Hexadecimal,
            (Ascii, Ascii) => Ascii,
            // Otherwise use the default style
            (_, _) => Decimal,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_drop() {
        assert!(!std::mem::needs_drop::<Term<'_>>());
        assert!(!std::mem::needs_drop::<Term<'_>>());
    }

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn term_size() {
        assert_eq!(std::mem::size_of::<Term>(), 56);
    }
}
