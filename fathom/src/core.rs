//! Core language.

use std::borrow::Cow;
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UintType {
    U8,
    U16,
    U32,
    U64,
}

impl UintType {
    pub fn to_signed(self) -> SintType {
        match self {
            UintType::U8 => SintType::S8,
            UintType::U16 => SintType::S16,
            UintType::U32 => SintType::S32,
            UintType::U64 => SintType::S64,
        }
    }
}

impl From<UintType> for Prim {
    fn from(uint_type: UintType) -> Self {
        Self::IntType(uint_type.into())
    }
}

impl UintType {
    pub const ALL: [Self; 4] = [Self::U8, Self::U16, Self::U32, Self::U64];

    pub fn name_uppercase(self) -> &'static str {
        match self {
            UintType::U8 => "U8",
            UintType::U16 => "U16",
            UintType::U32 => "U32",
            UintType::U64 => "U64",
        }
    }

    pub fn name_lowercase(self) -> &'static str {
        match self {
            UintType::U8 => "u8",
            UintType::U16 => "u16",
            UintType::U32 => "u32",
            UintType::U64 => "u64",
        }
    }

    pub fn name_no_prefix(self) -> &'static str {
        match self {
            UintType::U8 => "8",
            UintType::U16 => "16",
            UintType::U32 => "32",
            UintType::U64 => "64",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SintType {
    S8,
    S16,
    S32,
    S64,
}

impl SintType {
    pub fn to_unsigned(self) -> UintType {
        match self {
            SintType::S8 => UintType::U8,
            SintType::S16 => UintType::U16,
            SintType::S32 => UintType::U32,
            SintType::S64 => UintType::U64,
        }
    }

    pub fn name_uppercase(self) -> &'static str {
        match self {
            Self::S8 => "S8",
            Self::S16 => "S16",
            Self::S32 => "S32",
            Self::S64 => "S64",
        }
    }

    pub fn name_lowercase(self) -> &'static str {
        match self {
            Self::S8 => "s8",
            Self::S16 => "s16",
            Self::S32 => "s32",
            Self::S64 => "s64",
        }
    }

    pub fn name_no_prefix(self) -> &'static str {
        match self {
            Self::S8 => "8",
            Self::S16 => "16",
            Self::S32 => "32",
            Self::S64 => "64",
        }
    }
}

impl From<SintType> for Prim {
    fn from(sint_type: SintType) -> Self {
        Self::IntType(sint_type.into())
    }
}

impl SintType {
    pub const ALL: [Self; 4] = [Self::S8, Self::S16, Self::S32, Self::S64];
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntType {
    Unsigned(UintType),
    Signed(SintType),
}

impl From<UintType> for IntType {
    fn from(uint_type: UintType) -> Self {
        Self::Unsigned(uint_type)
    }
}

impl From<SintType> for IntType {
    fn from(sint_type: SintType) -> Self {
        Self::Signed(sint_type)
    }
}

impl From<IntType> for Prim {
    fn from(int_type: IntType) -> Self {
        Self::IntType(int_type)
    }
}

impl IntType {
    pub const ALL: [Self; 8] = {
        [
            Self::Unsigned(UintType::U8),
            Self::Unsigned(UintType::U16),
            Self::Unsigned(UintType::U32),
            Self::Unsigned(UintType::U64),
            Self::Signed(SintType::S8),
            Self::Signed(SintType::S16),
            Self::Signed(SintType::S32),
            Self::Signed(SintType::S64),
        ]
    };

    fn name_uppercase(self) -> &'static str {
        match self {
            Self::Unsigned(uint_type) => uint_type.name_uppercase(),
            Self::Signed(int_type) => int_type.name_uppercase(),
        }
    }

    fn name_lowercase(self) -> &'static str {
        match self {
            Self::Unsigned(uint_type) => uint_type.name_lowercase(),
            Self::Signed(int_type) => int_type.name_lowercase(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatType {
    F32,
    F64,
}

impl From<FloatType> for Prim {
    fn from(float_type: FloatType) -> Self {
        Self::FloatType(float_type)
    }
}

impl FloatType {
    pub const ALL: [Self; 2] = [Self::F32, Self::F64];

    fn name_uppercase(self) -> &'static str {
        match self {
            Self::F32 => "F32",
            Self::F64 => "F64",
        }
    }

    fn name_lowercase(self) -> &'static str {
        match self {
            Self::F32 => "f32",
            Self::F64 => "f64",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Endianness {
    Little,
    Big,
}

impl Endianness {
    pub const ALL: [Self; 2] = [Self::Little, Self::Big];

    fn name_lowercase(self) -> &'static str {
        match self {
            Endianness::Little => "le",
            Endianness::Big => "be",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Prim {
    /// Void type.
    VoidType,
    /// Type of booleans.
    BoolType,
    /// Type of two's complement machine integers.
    IntType(IntType),
    /// Type of IEEE-754 floating point numbers.
    FloatType(FloatType),
    /// Type of optional data.
    OptionType,
    /// Type of dynamically sized arrays.
    VecType,
    /// Type of fixed size arrays
    ArrayType(UintType),
    /// Type of stream positions.
    PosType,
    /// Type of stream references.
    RefType,

    /// Type of format descriptions.
    FormatType,
    /// Integer formats.
    FormatInt(IntType, Endianness),
    /// IEEE-754 floating point formats.
    FormatFloat(FloatType, Endianness),
    /// Repeat formats up to an unsigned length.
    FormatRepeat(UintType),
    /// Repeat a format until the length of the given parse scope is reached.
    FormatRepeatUntilEnd,
    /// Limit the format to an unsigned length.
    FormatLimit(UintType),
    /// A format which returns the current position in the input stream.
    FormatStreamPos,
    /// A format that links to another location in the binary data stream,
    /// relative to a base position.
    FormatLink,
    /// A format that forces a reference to be read eagerly.
    FormatDeref,
    /// A format that always succeeds with some data.
    FormatSucceed,
    /// A format that always fails to parse.
    FormatFail,
    /// Unwrap an option, or fail to parse.
    FormatUnwrap,
    /// Format representations.
    FormatRepr,

    /// Void eliminator.
    Absurd,

    // TODO: replace with prelude functions.
    BoolEq,
    BoolNeq,
    BoolNot,
    BoolAnd,
    BoolOr,
    BoolXor,

    // Int operations.
    IntEq(IntType),
    IntNeq(IntType),
    IntLt(IntType),
    IntLte(IntType),
    IntGt(IntType),
    IntGte(IntType),
    IntAdd(IntType),
    IntSub(IntType),
    IntMul(IntType),
    IntDiv(IntType),

    IntNot(UintType),
    IntShl(UintType),
    IntShr(UintType),
    IntAnd(UintType),
    IntOr(UintType),
    IntXor(UintType),

    IntNeg(SintType),
    IntAbs(SintType),
    IntUAbs(SintType),

    OptionSome,
    OptionNone,
    OptionFold,

    ArrayFind(UintType),
    ArrayIndex(UintType),

    PosAdd(UintType),

    /// Reported errors.
    ReportedError,
}

impl Prim {
    pub fn name(self) -> Cow<'static, str> {
        match self {
            Prim::VoidType => "Void".into(),
            Prim::BoolType => "Bool".into(),
            Prim::IntType(int_type) => int_type.name_uppercase().into(),
            Prim::FloatType(float_type) => float_type.name_uppercase().into(),
            Prim::OptionType => "Option".into(),
            Prim::VecType => "Array".into(),
            Prim::ArrayType(uint_type) => format!("Array{}", uint_type.name_no_prefix()).into(),
            Prim::PosType => "Pos".into(),
            Prim::RefType => "Ref".into(),
            Prim::FormatType => "Format".into(),
            Prim::FormatInt(IntType::Unsigned(UintType::U8), _) => "u8".into(),
            Prim::FormatInt(IntType::Signed(SintType::S8), _) => "s8".into(),
            Prim::FormatInt(int_type, endianness) => format!(
                "{}{}",
                int_type.name_lowercase(),
                endianness.name_lowercase()
            )
            .into(),
            Prim::FormatFloat(float_type, endianness) => format!(
                "{}{}",
                float_type.name_lowercase(),
                endianness.name_lowercase()
            )
            .into(),
            Prim::FormatRepeat(uint_type) => {
                format!("repeat_len{}", uint_type.name_no_prefix()).into()
            }
            Prim::FormatRepeatUntilEnd => "repeat_until_end".into(),
            Prim::FormatLimit(uint_type) => format!("limit{}", uint_type.name_no_prefix()).into(),
            Prim::FormatStreamPos => "stream_pos".into(),
            Prim::FormatLink => "link".into(),
            Prim::FormatDeref => "deref".into(),
            Prim::FormatSucceed => "succeed".into(),
            Prim::FormatFail => "fail".into(),
            Prim::FormatUnwrap => "unwrap".into(),
            Prim::FormatRepr => "Repr".into(),
            Prim::Absurd => "absurd".into(),
            Prim::BoolEq => "bool_eq".into(),
            Prim::BoolNeq => "bool_neq".into(),
            Prim::BoolNot => "bool_not".into(),
            Prim::BoolAnd => "bool_and".into(),
            Prim::BoolOr => "bool_or".into(),
            Prim::BoolXor => "bool_xor".into(),
            Prim::IntEq(int_type) => format!("{}_eq", int_type.name_lowercase()).into(),
            Prim::IntNeq(int_type) => format!("{}_neq", int_type.name_lowercase()).into(),
            Prim::IntLt(int_type) => format!("{}_lt", int_type.name_lowercase()).into(),
            Prim::IntLte(int_type) => format!("{}_lte", int_type.name_lowercase()).into(),
            Prim::IntGt(int_type) => format!("{}_gt", int_type.name_lowercase()).into(),
            Prim::IntGte(int_type) => format!("{}_gte", int_type.name_lowercase()).into(),
            Prim::IntAdd(int_type) => format!("{}_add", int_type.name_lowercase()).into(),
            Prim::IntSub(int_type) => format!("{}_sub", int_type.name_lowercase()).into(),
            Prim::IntMul(int_type) => format!("{}_mul", int_type.name_lowercase()).into(),
            Prim::IntDiv(int_type) => format!("{}_div", int_type.name_lowercase()).into(),
            Prim::IntNot(int_type) => format!("{}_not", int_type.name_lowercase()).into(),
            Prim::IntShl(int_type) => format!("{}_shl", int_type.name_lowercase()).into(),
            Prim::IntShr(int_type) => format!("{}_shr", int_type.name_lowercase()).into(),
            Prim::IntAnd(int_type) => format!("{}_and", int_type.name_lowercase()).into(),
            Prim::IntOr(int_type) => format!("{}_or", int_type.name_lowercase()).into(),
            Prim::IntXor(int_type) => format!("{}_xor", int_type.name_lowercase()).into(),
            Prim::IntNeg(int_type) => format!("{}_neg", int_type.name_lowercase()).into(),
            Prim::IntAbs(int_type) => format!("{}_abs", int_type.name_lowercase()).into(),
            Prim::IntUAbs(int_type) => format!("{}_unsigned_abs", int_type.name_lowercase()).into(),
            Prim::OptionSome => "some".into(),
            Prim::OptionNone => "none".into(),
            Prim::OptionFold => "option_fold".into(),
            Prim::ArrayFind(uint_type) => {
                format!("array{}_find", uint_type.name_no_prefix()).into()
            }
            Prim::ArrayIndex(uint_type) => {
                format!("array{}_index", uint_type.name_no_prefix()).into()
            }
            Prim::PosAdd(uint_type) => format!("pos_add_{}", uint_type.name_lowercase()).into(),
            Prim::ReportedError => "reported_error".into(),
        }
    }
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
    Uint(u64, UintType, UIntStyle),
    Sint(i64, SintType),
    F32(f32),
    F64(f64),
    Pos(usize),
    Ref(usize),
}

impl Const {
    pub fn uint(num: impl Into<u64>, uint_type: UintType, style: UIntStyle) -> Self {
        Self::Uint(num.into(), uint_type, style)
    }

    pub fn u8(num: u8) -> Self {
        Self::uint(num, UintType::U8, UIntStyle::Decimal)
    }

    pub fn u16(num: u16) -> Self {
        Self::uint(num, UintType::U16, UIntStyle::Decimal)
    }

    pub fn u32(num: u32) -> Self {
        Self::uint(num, UintType::U32, UIntStyle::Decimal)
    }

    pub fn u64(num: u64) -> Self {
        Self::uint(num, UintType::U64, UIntStyle::Decimal)
    }

    pub fn sint(num: impl Into<i64>, sint_type: SintType) -> Self {
        Self::Sint(num.into(), sint_type)
    }

    pub fn s8(int: i8) -> Self {
        Self::sint(int, SintType::S8)
    }

    pub fn s16(int: i16) -> Self {
        Self::sint(int, SintType::S16)
    }

    pub fn s32(int: i32) -> Self {
        Self::sint(int, SintType::S32)
    }

    pub fn s64(int: i64) -> Self {
        Self::sint(int, SintType::S64)
    }
}

impl PartialEq for Const {
    fn eq(&self, other: &Self) -> bool {
        match (*self, *other) {
            (Const::Bool(a), Const::Bool(b)) => a == b,
            (Const::Uint(ta, a, _), Const::Uint(tb, b, _)) => ta == tb && a == b,
            (Const::Sint(ta, a), Const::Sint(tb, b)) => ta == tb && a == b,
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
            (Const::Uint(ta, a, _), Const::Uint(tb, b, _)) => ta.cmp(&tb).then(a.cmp(&b)),
            (Const::Sint(ta, a), Const::Sint(tb, b)) => ta.cmp(&tb).then(a.cmp(&b)),
            (Const::F32(a), Const::F32(b)) => a.total_cmp(&b),
            (Const::F64(a), Const::F64(b)) => a.total_cmp(&b),
            (Const::Pos(a), Const::Pos(b)) => a.cmp(&b),
            (Const::Ref(a), Const::Ref(b)) => a.cmp(&b),
            _ => {
                fn discriminant(r#const: &Const) -> usize {
                    match r#const {
                        Const::Bool(_) => 0,
                        Const::Uint(..) => 1,
                        Const::Sint(..) => 2,
                        Const::F32(_) => 3,
                        Const::F64(_) => 4,
                        Const::Pos(_) => 5,
                        Const::Ref(_) => 6,
                    }
                }

                let tag1 = discriminant(self);
                let tag2 = discriminant(other);
                tag1.cmp(&tag2)
            }
        }
    }
}

impl UIntStyle {
    pub fn format(&self, number: u64, uint_type: UintType) -> String {
        match self {
            UIntStyle::Binary => format!("0b{number:b}"),
            UIntStyle::Decimal => number.to_string(),
            UIntStyle::Hexadecimal => format!("0x{number:x}"),
            UIntStyle::Ascii => {
                let num_bytes = match uint_type {
                    UintType::U8 => 1,
                    UintType::U16 => 2,
                    UintType::U32 => 4,
                    UintType::U64 => 8,
                };
                let bytes = number.to_be_bytes();
                let bytes = &bytes[num_bytes..];
                if bytes.iter().all(|c| c.is_ascii() && !c.is_ascii_control()) {
                    let s = std::str::from_utf8(bytes).unwrap(); // unwrap safe due to above check
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
