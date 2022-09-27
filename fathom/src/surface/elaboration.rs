//! Elaboration of the surface language into the core language.
//!
//! This module is where user-facing type checking happens, along with
//! translating the convenient surface language into a simpler, more explicit
//! core language.
//!
//! The algorithm is structured _bidirectionally_, ie. divided into _checking_
//! and _synthesis_ modes. By supplying type annotations as early as possible
//! using the checking mode, we can improve the locality of type errors, and
//! provide enough _control_ to the algorithm to allow for elaboration even in
//! the presence of ‘fancy’ types.
//!
//! For places where bidirectional typing is not enough, _unification_ is used
//! in an attempt to infer unknown terms and types based on how they are used.
//!
//! ## Resources
//!
//! - [Bidirectional Typing Rules: A Tutorial](https://davidchristiansen.dk/tutorials/bidirectional.pdf)
//! - [Bidirectional Types Checking – Compose NYC 2019](https://www.youtube.com/watch?v=utyBNDj7s2w)
//! - [Lecture Notes on Bidirectional Type Checking](https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/15-bidirectional.pdf)
//! - [elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo/)

use scoped_arena::Scope;
use std::cell::RefCell;
use std::str::FromStr;
use std::sync::Arc;

use crate::alloc::SliceVec;
use crate::core::semantics::{self, ArcValue, Head, Telescope, Value};
use crate::core::{self, binary, Const, Prim, UIntStyle};
use crate::env::{self, EnvLen, Level, SharedEnv, SliceEnv, UniqueEnv};
use crate::source::{ByteRange, Span, Spanned};
use crate::surface::elaboration::reporting::Message;
use crate::surface::{distillation, pretty, BinOp, FormatField, Item, Module, Pattern, Term};
use crate::{StringId, StringInterner};

mod order;
mod reporting;
mod unification;

/// Top-level item environment.
pub struct ItemEnv<'arena> {
    /// Names of items.
    names: UniqueEnv<StringId>,
    /// Types of items.
    types: UniqueEnv<ArcValue<'arena>>,
    /// Expressions of items.
    exprs: UniqueEnv<ArcValue<'arena>>,
}

impl<'arena> ItemEnv<'arena> {
    /// Construct a new, empty environment.
    pub fn new() -> ItemEnv<'arena> {
        ItemEnv {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
            exprs: UniqueEnv::new(),
        }
    }

    pub fn push_definition(
        &mut self,
        name: StringId,
        r#type: ArcValue<'arena>,
        expr: ArcValue<'arena>,
    ) {
        self.names.push(name);
        self.types.push(r#type);
        self.exprs.push(expr);
    }

    pub fn get_name(&self, name: StringId) -> Option<(Level, ArcValue<'arena>)> {
        itertools::izip!(env::levels(), self.names.iter(), self.types.iter())
            .find_map(|(var, n, r#type)| (name == *n).then(|| (var, r#type.clone())))
    }
}

/// Local variable environment.
///
/// This is used for keeping track of [local variables] that are bound by the
/// program, for example by function parameters, let bindings, or pattern
/// matching.
///
/// This environment behaves as a stack. As scopes are entered, it is important
/// to remember to call either [`LocalEnv::push_def`] or [`LocalEnv::push_param`].
/// On scope exit, it is important to remember to call [`LocalEnv::pop`].
/// Multiple bindings can be removed at once with [`LocalEnv::truncate`].
///
/// [local variables]: core::Term::LocalVar
pub struct LocalEnv<'arena> {
    /// Names of local variables.
    names: UniqueEnv<Option<StringId>>,
    /// Types of local variables.
    types: UniqueEnv<ArcValue<'arena>>,
    /// Information about the local binders. Used when inserting new
    /// metavariables during [evaluation][semantics::EvalEnv::eval].
    infos: UniqueEnv<core::LocalInfo>,
    /// Expressions that will be substituted for local variables during
    /// [evaluation][semantics::EvalEnv::eval].
    exprs: SharedEnv<ArcValue<'arena>>,
}

impl<'arena> LocalEnv<'arena> {
    /// Construct a new, empty environment.
    pub fn new() -> LocalEnv<'arena> {
        LocalEnv {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
            infos: UniqueEnv::new(),
            exprs: SharedEnv::new(),
        }
    }

    pub fn default(
        interner: &RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    ) -> LocalEnv<'arena> {
        use crate::core::Prim::*;
        use crate::core::Term;

        const VAR0: Term<'_> = Term::LocalVar(Span::Empty, env::Index::last());
        const VAR1: Term<'_> = Term::LocalVar(Span::Empty, env::Index::last().prev());
        const VAR2: Term<'_> = Term::LocalVar(Span::Empty, env::Index::last().prev().prev());
        const VAR3: Term<'_> = Term::LocalVar(Span::Empty, env::Index::last().prev().prev().prev());
        const UNIVERSE: Term<'_> = Term::Universe(Span::Empty);
        const FORMAT_TYPE: Term<'_> = Term::Prim(Span::Empty, FormatType);
        const BOOL_TYPE: Term<'_> = Term::Prim(Span::Empty, BoolType);
        const U8_TYPE: Term<'_> = Term::Prim(Span::Empty, U8Type);
        const U16_TYPE: Term<'_> = Term::Prim(Span::Empty, U16Type);
        const U32_TYPE: Term<'_> = Term::Prim(Span::Empty, U32Type);
        const U64_TYPE: Term<'_> = Term::Prim(Span::Empty, U64Type);
        const S8_TYPE: Term<'_> = Term::Prim(Span::Empty, S8Type);
        const S16_TYPE: Term<'_> = Term::Prim(Span::Empty, S16Type);
        const S32_TYPE: Term<'_> = Term::Prim(Span::Empty, S32Type);
        const S64_TYPE: Term<'_> = Term::Prim(Span::Empty, S64Type);
        const ARRAY8_TYPE: Term<'_> = Term::Prim(Span::Empty, Array8Type);
        const ARRAY16_TYPE: Term<'_> = Term::Prim(Span::Empty, Array16Type);
        const ARRAY32_TYPE: Term<'_> = Term::Prim(Span::Empty, Array32Type);
        const ARRAY64_TYPE: Term<'_> = Term::Prim(Span::Empty, Array64Type);
        const POS_TYPE: Term<'_> = Term::Prim(Span::Empty, PosType);

        let mut env = LocalEnvBuilder::new(interner, scope);

        env.define_prim(VoidType, &UNIVERSE);
        env.define_prim(BoolType, &UNIVERSE);
        env.define_prim(U8Type, &UNIVERSE);
        env.define_prim(U16Type, &UNIVERSE);
        env.define_prim(U32Type, &UNIVERSE);
        env.define_prim(U64Type, &UNIVERSE);
        env.define_prim(S8Type, &UNIVERSE);
        env.define_prim(S16Type, &UNIVERSE);
        env.define_prim(S32Type, &UNIVERSE);
        env.define_prim(S64Type, &UNIVERSE);
        env.define_prim(F32Type, &UNIVERSE);
        env.define_prim(F64Type, &UNIVERSE);
        env.define_prim_fun(OptionType, [&UNIVERSE], &UNIVERSE);
        env.define_prim_fun(ArrayType, [&UNIVERSE], &UNIVERSE);
        env.define_prim_fun(Array8Type, [&U8_TYPE, &UNIVERSE], &UNIVERSE);
        env.define_prim_fun(Array16Type, [&U16_TYPE, &UNIVERSE], &UNIVERSE);
        env.define_prim_fun(Array32Type, [&U32_TYPE, &UNIVERSE], &UNIVERSE);
        env.define_prim_fun(Array64Type, [&U64_TYPE, &UNIVERSE], &UNIVERSE);
        env.define_prim(PosType, &UNIVERSE);
        env.define_prim_fun(RefType, [&FORMAT_TYPE], &UNIVERSE);
        env.define_prim(FormatType, &UNIVERSE);

        env.define_prim(FormatU8, &FORMAT_TYPE);
        env.define_prim(FormatU16Be, &FORMAT_TYPE);
        env.define_prim(FormatU16Le, &FORMAT_TYPE);
        env.define_prim(FormatU32Be, &FORMAT_TYPE);
        env.define_prim(FormatU32Le, &FORMAT_TYPE);
        env.define_prim(FormatU64Be, &FORMAT_TYPE);
        env.define_prim(FormatU64Le, &FORMAT_TYPE);
        env.define_prim(FormatS8, &FORMAT_TYPE);
        env.define_prim(FormatS16Be, &FORMAT_TYPE);
        env.define_prim(FormatS16Le, &FORMAT_TYPE);
        env.define_prim(FormatS32Be, &FORMAT_TYPE);
        env.define_prim(FormatS32Le, &FORMAT_TYPE);
        env.define_prim(FormatS64Be, &FORMAT_TYPE);
        env.define_prim(FormatS64Le, &FORMAT_TYPE);
        env.define_prim(FormatF32Be, &FORMAT_TYPE);
        env.define_prim(FormatF32Le, &FORMAT_TYPE);
        env.define_prim(FormatF64Be, &FORMAT_TYPE);
        env.define_prim(FormatF64Le, &FORMAT_TYPE);
        env.define_prim_fun(FormatArray8, [&U8_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatArray16, [&U16_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatArray32, [&U32_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatArray64, [&U64_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatRepeatUntilEnd, [&FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatLimit8, [&U8_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatLimit16, [&U16_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatLimit32, [&U32_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatLimit64, [&U64_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatLink, [&POS_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim(
            FormatDeref,
            &core::Term::FunType(
                Span::Empty,
                env.name("A"),
                &FORMAT_TYPE,
                &Term::FunType(
                    Span::Empty,
                    None,
                    &Term::FunApp(Span::Empty, &Term::Prim(Span::Empty, RefType), &VAR0),
                    &FORMAT_TYPE,
                ),
            ),
        );
        env.define_prim(FormatStreamPos, &FORMAT_TYPE);
        env.define_prim(
            FormatSucceed,
            &core::Term::FunType(
                Span::Empty,
                env.name("A"),
                &UNIVERSE,
                &Term::FunType(Span::Empty, None, &VAR0, &FORMAT_TYPE),
            ),
        );
        env.define_prim(FormatFail, &FORMAT_TYPE);
        env.define_prim(
            FormatUnwrap,
            // fun (A : Type) -> Option A   -> Format
            // fun (A : Type) -> Option A@0 -> Format
            &core::Term::FunType(
                Span::Empty,
                env.name("A"),
                &UNIVERSE,
                &Term::FunType(
                    Span::Empty,
                    None,
                    &Term::FunApp(Span::Empty, &Term::Prim(Span::Empty, OptionType), &VAR0),
                    &FORMAT_TYPE,
                ),
            ),
        );
        env.define_prim_fun(FormatRepr, [&FORMAT_TYPE], &UNIVERSE);

        env.define_prim_fun(BoolEq, [&BOOL_TYPE, &BOOL_TYPE], &BOOL_TYPE);
        env.define_prim_fun(BoolNeq, [&BOOL_TYPE, &BOOL_TYPE], &BOOL_TYPE);
        env.define_prim_fun(BoolNot, [&BOOL_TYPE], &BOOL_TYPE);
        env.define_prim_fun(BoolAnd, [&BOOL_TYPE, &BOOL_TYPE], &BOOL_TYPE);
        env.define_prim_fun(BoolOr, [&BOOL_TYPE, &BOOL_TYPE], &BOOL_TYPE);
        env.define_prim_fun(BoolXor, [&BOOL_TYPE, &BOOL_TYPE], &BOOL_TYPE);

        env.define_prim_fun(U8Eq, [&U8_TYPE, &U8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U8Neq, [&U8_TYPE, &U8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U8Lt, [&U8_TYPE, &U8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U8Gt, [&U8_TYPE, &U8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U8Lte, [&U8_TYPE, &U8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U8Gte, [&U8_TYPE, &U8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U8Add, [&U8_TYPE, &U8_TYPE], &U8_TYPE);
        env.define_prim_fun(U8Sub, [&U8_TYPE, &U8_TYPE], &U8_TYPE);
        env.define_prim_fun(U8Mul, [&U8_TYPE, &U8_TYPE], &U8_TYPE);
        env.define_prim_fun(U8Div, [&U8_TYPE, &U8_TYPE], &U8_TYPE);
        env.define_prim_fun(U8Not, [&U8_TYPE], &U8_TYPE);
        env.define_prim_fun(U8Shl, [&U8_TYPE, &U8_TYPE], &U8_TYPE);
        env.define_prim_fun(U8Shr, [&U8_TYPE, &U8_TYPE], &U8_TYPE);
        env.define_prim_fun(U8And, [&U8_TYPE, &U8_TYPE], &U8_TYPE);
        env.define_prim_fun(U8Or, [&U8_TYPE, &U8_TYPE], &U8_TYPE);
        env.define_prim_fun(U8Xor, [&U8_TYPE, &U8_TYPE], &U8_TYPE);

        env.define_prim_fun(U16Eq, [&U16_TYPE, &U16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U16Neq, [&U16_TYPE, &U16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U16Lt, [&U16_TYPE, &U16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U16Gt, [&U16_TYPE, &U16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U16Lte, [&U16_TYPE, &U16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U16Gte, [&U16_TYPE, &U16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U16Add, [&U16_TYPE, &U16_TYPE], &U16_TYPE);
        env.define_prim_fun(U16Sub, [&U16_TYPE, &U16_TYPE], &U16_TYPE);
        env.define_prim_fun(U16Mul, [&U16_TYPE, &U16_TYPE], &U16_TYPE);
        env.define_prim_fun(U16Div, [&U16_TYPE, &U16_TYPE], &U16_TYPE);
        env.define_prim_fun(U16Not, [&U16_TYPE], &U16_TYPE);
        env.define_prim_fun(U16Shl, [&U16_TYPE, &U8_TYPE], &U16_TYPE);
        env.define_prim_fun(U16Shr, [&U16_TYPE, &U8_TYPE], &U16_TYPE);
        env.define_prim_fun(U16And, [&U16_TYPE, &U16_TYPE], &U16_TYPE);
        env.define_prim_fun(U16Or, [&U16_TYPE, &U16_TYPE], &U16_TYPE);
        env.define_prim_fun(U16Xor, [&U16_TYPE, &U16_TYPE], &U16_TYPE);

        env.define_prim_fun(U32Eq, [&U32_TYPE, &U32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U32Neq, [&U32_TYPE, &U32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U32Lt, [&U32_TYPE, &U32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U32Gt, [&U32_TYPE, &U32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U32Lte, [&U32_TYPE, &U32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U32Gte, [&U32_TYPE, &U32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U32Add, [&U32_TYPE, &U32_TYPE], &U32_TYPE);
        env.define_prim_fun(U32Sub, [&U32_TYPE, &U32_TYPE], &U32_TYPE);
        env.define_prim_fun(U32Mul, [&U32_TYPE, &U32_TYPE], &U32_TYPE);
        env.define_prim_fun(U32Div, [&U32_TYPE, &U32_TYPE], &U32_TYPE);
        env.define_prim_fun(U32Not, [&U32_TYPE], &U32_TYPE);
        env.define_prim_fun(U32Shl, [&U32_TYPE, &U8_TYPE], &U32_TYPE);
        env.define_prim_fun(U32Shr, [&U32_TYPE, &U8_TYPE], &U32_TYPE);
        env.define_prim_fun(U32And, [&U32_TYPE, &U32_TYPE], &U32_TYPE);
        env.define_prim_fun(U32Or, [&U32_TYPE, &U32_TYPE], &U32_TYPE);
        env.define_prim_fun(U32Xor, [&U32_TYPE, &U32_TYPE], &U32_TYPE);

        env.define_prim_fun(U64Eq, [&U64_TYPE, &U64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U64Neq, [&U64_TYPE, &U64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U64Lt, [&U64_TYPE, &U64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U64Gt, [&U64_TYPE, &U64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U64Lte, [&U64_TYPE, &U64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U64Gte, [&U64_TYPE, &U64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(U64Add, [&U64_TYPE, &U64_TYPE], &U64_TYPE);
        env.define_prim_fun(U64Sub, [&U64_TYPE, &U64_TYPE], &U64_TYPE);
        env.define_prim_fun(U64Mul, [&U64_TYPE, &U64_TYPE], &U64_TYPE);
        env.define_prim_fun(U64Div, [&U64_TYPE, &U64_TYPE], &U64_TYPE);
        env.define_prim_fun(U64Not, [&U64_TYPE], &U64_TYPE);
        env.define_prim_fun(U64Shl, [&U64_TYPE, &U8_TYPE], &U64_TYPE);
        env.define_prim_fun(U64Shr, [&U64_TYPE, &U8_TYPE], &U64_TYPE);
        env.define_prim_fun(U64And, [&U64_TYPE, &U64_TYPE], &U64_TYPE);
        env.define_prim_fun(U64Or, [&U64_TYPE, &U64_TYPE], &U64_TYPE);
        env.define_prim_fun(U64Xor, [&U64_TYPE, &U64_TYPE], &U64_TYPE);

        env.define_prim_fun(S8Eq, [&S8_TYPE, &S8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S8Neq, [&S8_TYPE, &S8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S8Lt, [&S8_TYPE, &S8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S8Gt, [&S8_TYPE, &S8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S8Lte, [&S8_TYPE, &S8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S8Gte, [&S8_TYPE, &S8_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S8Neg, [&S8_TYPE], &S8_TYPE);
        env.define_prim_fun(S8Add, [&S8_TYPE, &S8_TYPE], &S8_TYPE);
        env.define_prim_fun(S8Sub, [&S8_TYPE, &S8_TYPE], &S8_TYPE);
        env.define_prim_fun(S8Mul, [&S8_TYPE, &S8_TYPE], &S8_TYPE);
        env.define_prim_fun(S8Div, [&S8_TYPE, &S8_TYPE], &S8_TYPE);
        env.define_prim_fun(S8Abs, [&S8_TYPE], &S8_TYPE);
        env.define_prim_fun(S8UAbs, [&S8_TYPE], &U8_TYPE);

        env.define_prim_fun(S16Eq, [&S16_TYPE, &S16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S16Neq, [&S16_TYPE, &S16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S16Lt, [&S16_TYPE, &S16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S16Gt, [&S16_TYPE, &S16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S16Lte, [&S16_TYPE, &S16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S16Gte, [&S16_TYPE, &S16_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S16Neg, [&S16_TYPE], &S16_TYPE);
        env.define_prim_fun(S16Add, [&S16_TYPE, &S16_TYPE], &S16_TYPE);
        env.define_prim_fun(S16Sub, [&S16_TYPE, &S16_TYPE], &S16_TYPE);
        env.define_prim_fun(S16Mul, [&S16_TYPE, &S16_TYPE], &S16_TYPE);
        env.define_prim_fun(S16Div, [&S16_TYPE, &S16_TYPE], &S16_TYPE);
        env.define_prim_fun(S16Abs, [&S16_TYPE], &S16_TYPE);
        env.define_prim_fun(S16UAbs, [&S16_TYPE], &U16_TYPE);

        env.define_prim_fun(S32Eq, [&S32_TYPE, &S32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S32Neq, [&S32_TYPE, &S32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S32Lt, [&S32_TYPE, &S32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S32Gt, [&S32_TYPE, &S32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S32Lte, [&S32_TYPE, &S32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S32Gte, [&S32_TYPE, &S32_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S32Neg, [&S32_TYPE], &S32_TYPE);
        env.define_prim_fun(S32Add, [&S32_TYPE, &S32_TYPE], &S32_TYPE);
        env.define_prim_fun(S32Sub, [&S32_TYPE, &S32_TYPE], &S32_TYPE);
        env.define_prim_fun(S32Mul, [&S32_TYPE, &S32_TYPE], &S32_TYPE);
        env.define_prim_fun(S32Div, [&S32_TYPE, &S32_TYPE], &S32_TYPE);
        env.define_prim_fun(S32Abs, [&S32_TYPE], &S32_TYPE);
        env.define_prim_fun(S32UAbs, [&S32_TYPE], &U32_TYPE);

        env.define_prim_fun(S64Eq, [&S64_TYPE, &S64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S64Neq, [&S64_TYPE, &S64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S64Lt, [&S64_TYPE, &S64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S64Gt, [&S64_TYPE, &S64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S64Lte, [&S64_TYPE, &S64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S64Gte, [&S64_TYPE, &S64_TYPE], &BOOL_TYPE);
        env.define_prim_fun(S64Neg, [&S64_TYPE], &S64_TYPE);
        env.define_prim_fun(S64Add, [&S64_TYPE, &S64_TYPE], &S64_TYPE);
        env.define_prim_fun(S64Sub, [&S64_TYPE, &S64_TYPE], &S64_TYPE);
        env.define_prim_fun(S64Mul, [&S64_TYPE, &S64_TYPE], &S64_TYPE);
        env.define_prim_fun(S64Div, [&S64_TYPE, &S64_TYPE], &S64_TYPE);
        env.define_prim_fun(S64Abs, [&S64_TYPE], &S64_TYPE);
        env.define_prim_fun(S64UAbs, [&S64_TYPE], &U64_TYPE);

        env.define_prim(
            OptionSome,
            // fun (A : Type) -> A   -> Option A
            // fun (A : Type) -> A@0 -> Option A@1
            &core::Term::FunType(
                Span::Empty,
                env.name("A"),
                &UNIVERSE,
                &Term::FunType(
                    Span::Empty,
                    None,
                    &VAR0,
                    &Term::FunApp(Span::Empty, &Term::Prim(Span::Empty, OptionType), &VAR1),
                ),
            ),
        );
        env.define_prim(
            OptionNone,
            // fun (A : Type) -> Option A
            // fun (A : Type) -> Option A@0
            &core::Term::FunType(
                Span::Empty,
                env.name("A"),
                &UNIVERSE,
                &Term::FunApp(Span::Empty, &Term::Prim(Span::Empty, OptionType), &VAR0),
            ),
        );
        env.define_prim(
            OptionFold,
            // fun (A : Type) (B : Type) -> B   -> (A   -> B  ) -> Option A   -> B
            // fun (A : Type) (B : Type) -> B@0 -> (A@2 -> B@2) -> Option A@3 -> B@3
            scope.to_scope(core::Term::FunType(
                Span::Empty,
                env.name("A"),
                &UNIVERSE,
                scope.to_scope(core::Term::FunType(
                    Span::Empty,
                    env.name("B"),
                    &UNIVERSE,
                    scope.to_scope(core::Term::FunType(
                        Span::Empty,
                        None,
                        &VAR0, // B@0
                        scope.to_scope(core::Term::FunType(
                            Span::Empty,
                            None,
                            &Term::FunType(Span::Empty, None, &VAR2, &VAR2), // A@2 -> B@2
                            scope.to_scope(core::Term::FunType(
                                Span::Empty,
                                None,
                                &Term::FunApp(
                                    Span::Empty,
                                    &Term::Prim(Span::Empty, OptionType),
                                    &VAR3,
                                ), // Option A@3
                                &VAR3, // B@3
                            )),
                        )),
                    )),
                )),
            )),
        );

        // fun (len : UN) (A : Type) -> (A   -> Bool) -> ArrayN len   A   -> Option A
        // fun (len : UN) (A : Type) -> (A@0 -> Bool) -> ArrayN len@2 A@1 -> Option A@2
        let find_type = |index_type, array_type| {
            scope.to_scope(core::Term::FunType(
                Span::Empty,
                env.name("len"),
                index_type,
                scope.to_scope(core::Term::FunType(
                    Span::Empty,
                    env.name("A"),
                    &UNIVERSE,
                    scope.to_scope(core::Term::FunType(
                        Span::Empty,
                        None,
                        &Term::FunType(Span::Empty, None, &VAR0, &BOOL_TYPE), // (A@0 -> Bool)
                        scope.to_scope(core::Term::FunType(
                            Span::Empty,
                            None,
                            // ArrayN len@2 A@1
                            scope.to_scope(Term::FunApp(
                                Span::Empty,
                                scope.to_scope(Term::FunApp(Span::Empty, array_type, &VAR2)),
                                &VAR1,
                            )),
                            &Term::FunApp(Span::Empty, &Term::Prim(Span::Empty, OptionType), &VAR2), // Option A@2
                        )),
                    )),
                )),
            ))
        };
        let array8_find_type = find_type(&U8_TYPE, &ARRAY8_TYPE);
        let array16_find_type = find_type(&U16_TYPE, &ARRAY16_TYPE);
        let array32_find_type = find_type(&U32_TYPE, &ARRAY32_TYPE);
        let array64_find_type = find_type(&U64_TYPE, &ARRAY64_TYPE);
        env.define_prim(Array8Find, array8_find_type);
        env.define_prim(Array16Find, array16_find_type);
        env.define_prim(Array32Find, array32_find_type);
        env.define_prim(Array64Find, array64_find_type);

        // fun (len : UN) -> (A : Type) -> (index : UN) -> ArrayN len   A   -> A
        // fun (len : UN) -> (A : Type) -> (index : UN) -> ArrayN len@2 A@1 -> A@2
        let array_index_type = |index_type, array_type| {
            scope.to_scope(core::Term::FunType(
                Span::Empty,
                env.name("len"),
                index_type,
                scope.to_scope(core::Term::FunType(
                    Span::Empty,
                    env.name("A"),
                    &UNIVERSE,
                    scope.to_scope(core::Term::FunType(
                        Span::Empty,
                        env.name("index"),
                        index_type,
                        scope.to_scope(core::Term::FunType(
                            Span::Empty,
                            None,
                            // ArrayN len@2 A@1
                            scope.to_scope(Term::FunApp(
                                Span::Empty,
                                scope.to_scope(Term::FunApp(Span::Empty, array_type, &VAR2)),
                                &VAR1,
                            )),
                            &VAR2, // A@2
                        )),
                    )),
                )),
            ))
        };
        let array8_index_type = array_index_type(&U8_TYPE, &ARRAY8_TYPE);
        let array16_index_type = array_index_type(&U16_TYPE, &ARRAY16_TYPE);
        let array32_index_type = array_index_type(&U32_TYPE, &ARRAY32_TYPE);
        let array64_index_type = array_index_type(&U64_TYPE, &ARRAY64_TYPE);
        env.define_prim(Array8Index, array8_index_type);
        env.define_prim(Array16Index, array16_index_type);
        env.define_prim(Array32Index, array32_index_type);
        env.define_prim(Array64Index, array64_index_type);

        env.define_prim_fun(PosAddU8, [&POS_TYPE, &U8_TYPE], &POS_TYPE);
        env.define_prim_fun(PosAddU16, [&POS_TYPE, &U16_TYPE], &POS_TYPE);
        env.define_prim_fun(PosAddU32, [&POS_TYPE, &U32_TYPE], &POS_TYPE);
        env.define_prim_fun(PosAddU64, [&POS_TYPE, &U64_TYPE], &POS_TYPE);

        env.build()
    }

    /// Get the length of the local environment.
    fn len(&self) -> EnvLen {
        self.names.len()
    }

    /// Push a local definition onto the context.
    fn push_def(
        &mut self,
        name: Option<StringId>,
        expr: ArcValue<'arena>,
        r#type: ArcValue<'arena>,
    ) {
        self.names.push(name);
        self.types.push(r#type);
        self.infos.push(core::LocalInfo::Definition);
        self.exprs.push(expr);
    }

    /// Push a local parameter onto the context.
    fn push_param(&mut self, name: Option<StringId>, r#type: ArcValue<'arena>) -> ArcValue<'arena> {
        // An expression that refers to itself once it is pushed onto the local
        // expression environment.
        let expr = Spanned::empty(Arc::new(Value::local_var(self.exprs.len().next_level())));

        self.names.push(name);
        self.types.push(r#type);
        self.infos.push(core::LocalInfo::Parameter);
        self.exprs.push(expr.clone());

        expr
    }

    /// Pop a local binder off the context.
    fn pop(&mut self) {
        self.names.pop();
        self.types.pop();
        self.infos.pop();
        self.exprs.pop();
    }

    /// Truncate the local environment.
    fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.types.truncate(len);
        self.infos.truncate(len);
        self.exprs.truncate(len);
    }
}

pub struct LocalEnvBuilder<'i, 'arena> {
    env: LocalEnv<'arena>,
    interner: &'i RefCell<StringInterner>,
    scope: &'arena Scope<'arena>,
}

impl<'i, 'arena> LocalEnvBuilder<'i, 'arena> {
    fn new(
        interner: &'i RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    ) -> LocalEnvBuilder<'i, 'arena> {
        let env = LocalEnv::new();
        LocalEnvBuilder {
            env,
            interner,
            scope,
        }
    }

    fn name(&self, name: &'static str) -> Option<StringId> {
        Some(self.interner.borrow_mut().get_or_intern_static(name))
    }

    fn define_prim(&mut self, prim: Prim, r#type: &core::Term<'arena>) {
        let name = self.name(prim.name());
        let meta_exprs = UniqueEnv::new();
        let item_exprs = UniqueEnv::new();
        let mut local_exprs = SharedEnv::new();

        let expr = Spanned::empty(Arc::new(Value::prim(prim, [])));
        let r#type =
            semantics::EvalEnv::new(&item_exprs, &mut local_exprs, &meta_exprs).eval(r#type);
        self.env.push_def(name, expr, r#type);
    }

    fn define_prim_fun<const ARITY: usize>(
        &mut self,
        prim: Prim,
        param_types: [&'arena core::Term<'arena>; ARITY],
        body_type: &'arena core::Term<'arena>,
    ) {
        self.define_prim(
            prim,
            (param_types.iter().rev()).fold(body_type, |r#type, param_type| {
                self.scope
                    .to_scope(core::Term::FunType(Span::Empty, None, param_type, r#type))
            }),
        );
    }

    fn build(self) -> LocalEnv<'arena> {
        self.env
    }
}

/// The reason why a metavariable was inserted.
#[derive(Debug, Copy, Clone)]
pub enum MetaSource {
    /// The type of a hole.
    HoleType(ByteRange, StringId),
    /// The expression of a hole.
    HoleExpr(ByteRange, StringId),
    /// The type of a placeholder
    PlaceholderType(ByteRange),
    /// The expression of a placeholder
    PlaceholderExpr(ByteRange),
    /// The type of a placeholder pattern.
    PlaceholderPatternType(ByteRange),
    /// The type of a named pattern.
    NamedPatternType(ByteRange, StringId),
    /// The overall type of a match expression
    MatchExprType(ByteRange),
    /// The type of a reported error.
    ReportedErrorType(ByteRange),
}

impl MetaSource {
    pub fn range(&self) -> ByteRange {
        match self {
            MetaSource::HoleType(range, _)
            | MetaSource::HoleExpr(range, _)
            | MetaSource::PlaceholderType(range)
            | MetaSource::PlaceholderExpr(range)
            | MetaSource::PlaceholderPatternType(range)
            | MetaSource::NamedPatternType(range, _)
            | MetaSource::MatchExprType(range)
            | MetaSource::ReportedErrorType(range) => *range,
        }
    }
}

/// Metavariable environment.
///
/// This is used for keeping track of the state of [metavariables] whose
/// definitions are intended to be found through the use of [unification].
///
/// [metavariables]: core::Term::MetaVar
pub struct MetaEnv<'arena> {
    /// The source of inserted metavariables, used when reporting [unsolved
    /// metavariables][Message::UnsolvedMetaVar].
    sources: UniqueEnv<MetaSource>,
    /// Types of metavariables.
    types: UniqueEnv</* TODO: lazy value */ ArcValue<'arena>>,
    /// Expressions that will be substituted for metavariables during
    /// [evaluation][semantics::EvalEnv::eval].
    ///
    /// These will be set to [`None`] when a metavariable is first
    /// [inserted][Context::push_unsolved_term], then will be set to [`Some`]
    /// if a solution is found during [`unification`].
    exprs: UniqueEnv<Option<ArcValue<'arena>>>,
}

impl<'arena> MetaEnv<'arena> {
    /// Construct a new, empty environment.
    pub fn new() -> MetaEnv<'arena> {
        MetaEnv {
            sources: UniqueEnv::new(),
            types: UniqueEnv::new(),
            exprs: UniqueEnv::new(),
        }
    }

    /// Push an unsolved metavariable onto the context.
    fn push(&mut self, source: MetaSource, r#type: ArcValue<'arena>) -> Level {
        // TODO: check that hole name is not already in use
        let var = self.exprs.len().next_level();

        self.sources.push(source);
        self.types.push(r#type);
        self.exprs.push(None);

        var
    }

    fn report<'this, 'interner: 'this, 'error: 'this>(
        &'this self,
        interner: &'interner RefCell<StringInterner>,
        scope: &'error Scope<'error>,
        mut item_names: UniqueEnv<StringId>,
        item_exprs: &'this SliceEnv<ArcValue<'this>>,
        mut local_names: UniqueEnv<Option<StringId>>,
    ) -> impl 'this + Iterator<Item = Message> {
        let entries = Iterator::zip(self.sources.iter(), self.exprs.iter());

        entries.filter_map(move |(&source, expr)| match (expr, source) {
            // Avoid producing messages for some unsolved metavariable sources:
            (None, MetaSource::HoleType(_, _)) => None, // should have an unsolved hole expression
            (None, MetaSource::PlaceholderType(_)) => None, // should have an unsolved placeholder expression
            (None, MetaSource::ReportedErrorType(_)) => None, // should already have an error reported

            // For other sources, report an unsolved problem message
            (None, source) => Some(Message::UnsolvedMetaVar { source }),
            // Yield messages of solved named holes
            (Some(expr), MetaSource::HoleExpr(range, name)) => {
                let term =
                    semantics::QuoteEnv::new(scope, item_exprs, local_names.len(), &self.exprs)
                        .quote(expr);
                let surface_term = distillation::Context::new(
                    interner,
                    scope,
                    &mut item_names,
                    &mut local_names,
                    &self.sources,
                )
                .check(&term);

                let pretty_context = pretty::Context::new(interner, scope);
                let doc = pretty_context.term(&surface_term).into_doc();
                let expr = doc.pretty(usize::MAX).to_string();

                Some(Message::HoleSolution { range, name, expr })
            }
            // Ignore solutions of anything else
            (Some(_), _) => None,
        })
    }
}

/// Elaboration context.
pub struct Context<'interner, 'arena, 'error> {
    /// Global string interner.
    interner: &'interner RefCell<StringInterner>,
    /// Scoped arena for storing elaborated terms.
    //
    // TODO: Make this local to the elaboration context, and reallocate
    //       elaborated terms to an external `Scope` during zonking, resetting
    //       this scope on completion.
    scope: &'arena Scope<'arena>,
    /// Scoped arena for storing surface terms generated during error reporting.
    error_scope: &'error Scope<'error>,

    // Commonly used values, cached to increase sharing.
    universe: ArcValue<'static>,
    format_type: ArcValue<'static>,
    bool_type: ArcValue<'static>,

    /// Item environment.
    item_env: ItemEnv<'arena>,
    /// Local environment.
    local_env: LocalEnv<'arena>,
    /// Meta environment.
    meta_env: MetaEnv<'arena>,
    /// A partial renaming to be used during [`unification`].
    renaming: unification::PartialRenaming,
    /// Diagnostic messages encountered during elaboration.
    messages: Vec<Message>,
}

impl<'interner, 'arena, 'error> Context<'interner, 'arena, 'error> {
    /// Construct a new elaboration context, backed by the supplied arena.
    pub fn new(
        interner: &'interner RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
        error_scope: &'error Scope<'error>,
    ) -> Context<'interner, 'arena, 'error> {
        Context {
            interner,
            scope,
            error_scope,

            universe: Spanned::empty(Arc::new(Value::Universe)),
            format_type: Spanned::empty(Arc::new(Value::prim(Prim::FormatType, []))),
            bool_type: Spanned::empty(Arc::new(Value::prim(Prim::BoolType, []))),

            item_env: ItemEnv::new(),
            local_env: LocalEnv::default(interner, scope),
            meta_env: MetaEnv::new(),
            renaming: unification::PartialRenaming::new(),
            messages: Vec::new(),
        }
    }

    /// Lookup an item name in the context.
    fn get_item_name(&self, name: StringId) -> Option<(env::Level, &ArcValue<'arena>)> {
        let item_var = self.item_env.names.elem_level(&name)?;
        let item_type = self.item_env.types.get_level(item_var)?;

        Some((item_var, item_type))
    }

    /// Lookup a local name in the context.
    fn get_local_name(&self, name: StringId) -> Option<(env::Index, &ArcValue<'arena>)> {
        let local_var = self.local_env.names.elem_index(&Some(name))?;
        let local_type = self.local_env.types.get_index(local_var)?;

        Some((local_var, local_type))
    }

    /// Push an unsolved term onto the context, to be updated later during unification.
    fn push_unsolved_term(
        &mut self,
        source: MetaSource,
        r#type: ArcValue<'arena>,
    ) -> core::Term<'arena> {
        core::Term::InsertedMeta(
            source.range().into(),
            self.meta_env.push(source, r#type),
            (self.scope).to_scope_from_iter(self.local_env.infos.iter().copied()),
        )
    }

    /// Push an unsolved type onto the context, to be updated later during unification.
    fn push_unsolved_type(&mut self, source: MetaSource) -> ArcValue<'arena> {
        let r#type = self.push_unsolved_term(source, self.universe.clone());
        self.eval_env().eval(&r#type)
    }

    fn push_message(&mut self, message: Message) {
        self.messages.push(message);
    }

    pub fn drain_messages(&mut self) -> impl '_ + Iterator<Item = Message> {
        let report_messages = self.meta_env.report(
            self.interner,
            self.error_scope,
            self.item_env.names.clone(),
            &self.item_env.exprs,
            self.local_env.names.clone(),
        );

        self.messages.drain(..).chain(report_messages)
    }

    pub fn eval_env(&mut self) -> semantics::EvalEnv<'arena, '_> {
        semantics::EvalEnv::new(
            &self.item_env.exprs,
            &mut self.local_env.exprs,
            &self.meta_env.exprs,
        )
    }

    pub fn elim_env(&self) -> semantics::ElimEnv<'arena, '_> {
        semantics::ElimEnv::new(&self.item_env.exprs, &self.meta_env.exprs)
    }

    pub fn quote_env<'out_arena>(
        &self,
        scope: &'out_arena Scope<'out_arena>,
    ) -> semantics::QuoteEnv<'arena, 'out_arena, '_> {
        semantics::QuoteEnv::new(
            scope,
            &self.item_env.exprs,
            self.local_env.len(),
            &self.meta_env.exprs,
        )
    }

    fn unification_context(&mut self) -> unification::Context<'arena, '_> {
        unification::Context::new(
            self.scope,
            &mut self.renaming,
            &self.item_env.exprs,
            self.local_env.len(),
            &mut self.meta_env.exprs,
        )
    }

    pub fn distillation_context<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
    ) -> distillation::Context<'interner, 'out_arena, '_> {
        distillation::Context::new(
            self.interner,
            scope,
            &mut self.item_env.names,
            &mut self.local_env.names,
            &self.meta_env.sources,
        )
    }

    pub fn binary_context<'data>(
        &self,
        buffer: binary::Buffer<'data>,
    ) -> binary::Context<'arena, '_, 'data> {
        binary::Context::new(&self.item_env.exprs, &self.meta_env.exprs, buffer)
    }

    fn pretty_print_value(&mut self, value: &ArcValue<'_>) -> String {
        let term = self.quote_env(self.error_scope).quote(value);
        let surface_term = self.distillation_context(self.error_scope).check(&term);
        let pretty_context = pretty::Context::new(self.interner, self.error_scope);
        let doc = pretty_context.term(&surface_term).into_doc();
        doc.pretty(usize::MAX).to_string()
    }

    /// Reports an error if there are duplicate fields found, returning a slice
    /// of the labels unique labels and an iterator over the unique fields.
    fn report_duplicate_labels<'fields, F>(
        &mut self,
        range: ByteRange,
        fields: &'fields [F],
        get_label: fn(&F) -> (ByteRange, StringId),
    ) -> (&'arena [StringId], impl Iterator<Item = &'fields F>) {
        let mut labels = SliceVec::new(self.scope, fields.len());
        // Will only allocate when duplicates are encountered
        let mut duplicate_indices = Vec::new();
        let mut duplicate_labels = Vec::new();

        for (index, field) in fields.iter().enumerate() {
            let (range, label) = get_label(field);
            if labels.contains(&label) {
                duplicate_indices.push(index);
                duplicate_labels.push((range, label));
            } else {
                labels.push(label)
            }
        }

        if !duplicate_labels.is_empty() {
            self.push_message(Message::DuplicateFieldLabels {
                range,
                labels: duplicate_labels,
            });
        }

        let filtered_fields = (fields.iter().enumerate())
            .filter_map(move |(index, field)| (!duplicate_indices.contains(&index)).then(|| field));

        (labels.into(), filtered_fields)
    }

    /// Parse a source string into number, assuming an ASCII encoding.
    fn parse_ascii<T>(&mut self, range: ByteRange, string_id: StringId) -> Option<T>
    where
        T: From<u8> + std::ops::Shl<Output = T> + std::ops::BitOr<Output = T>,
    {
        // TODO: Parse escape codes
        // TODO: Alternate byte orders
        // TODO: Non-ASCII encodings

        let interner = self.interner.borrow();
        let mut data = Some(T::from(0));
        let mut count: u8 = 0;

        for (offset, ch) in interner.resolve(string_id).unwrap().char_indices() {
            if !ch.is_ascii() {
                let ch_start = range.start() + 1 + offset;
                let ch_end = ch_start + ch.len_utf8();

                self.push_message(Message::NonAsciiStringLiteral {
                    invalid_range: ByteRange::new(range.file_id(), ch_start, ch_end),
                });
                data = None;
            }

            data = data.filter(|_| usize::from(count) < std::mem::size_of::<T>());
            data = data.map(|data| {
                // Yikes this is a tad ugly. Setting the bytes in reverse order...
                let offset = 8 * (std::mem::size_of::<T>() as u8 - (count + 1));
                data | (T::from(ch as u8) << T::from(offset))
            });
            count += 1;
        }

        if count as usize != std::mem::size_of::<T>() {
            self.push_message(Message::MismatchedStringLiteralByteLength {
                range,
                expected_len: std::mem::size_of::<T>(),
                found_len: count as usize,
            });
            data = None;
        }

        data
    }

    /// Parse a source string into a number.
    fn parse_number<T: FromStr>(&mut self, range: ByteRange, string_id: StringId) -> Option<T>
    where
        T::Err: std::fmt::Display,
    {
        // TODO: Custom parsing and improved errors
        match self.interner.borrow().resolve(string_id).unwrap().parse() {
            Ok(data) => Some(data),
            Err(error) => {
                let message = error.to_string();
                self.push_message(Message::InvalidNumericLiteral { range, message });
                None
            }
        }
    }

    /// Parse a source string into a number.
    fn parse_number_radix<T: FromStrRadix>(
        &mut self,
        range: ByteRange,
        string_id: StringId,
    ) -> Option<(T, UIntStyle)> {
        // TODO: Custom parsing and improved errors
        let s = self.interner.borrow();
        let s = s.resolve(string_id).unwrap();
        let (s, radix, style) = if let Some(s) = s.strip_prefix("0x") {
            (s, 16, UIntStyle::Hexadecimal)
        } else if let Some(s) = s.strip_prefix("0b") {
            (s, 2, UIntStyle::Binary)
        } else {
            (s, 10, UIntStyle::Decimal)
        };
        match T::from_str_radix(s, radix) {
            Ok(data) => Some((data, style)),
            Err(error) => {
                let message = error.to_string();
                self.push_message(Message::InvalidNumericLiteral { range, message });
                None
            }
        }
    }

    /// Conversion checking for `expr` under the types `type0` and `type1`.
    /// This will trigger unification, recording a unification error on failure.
    //
    // NOTE: We could eventually call this method `coerce` if we end up adding
    //       coercions to the core language.
    fn convert(
        &mut self,
        surface_range: ByteRange, // TODO: could be removed if we never encounter empty spans in the core term
        expr: core::Term<'arena>,
        type0: &ArcValue<'arena>,
        type1: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let span = expr.span();
        let range = match span {
            Span::Range(range) => range,
            Span::Empty => {
                self.push_message(Message::MissingSpan {
                    range: surface_range,
                });
                surface_range
            }
        };
        match self.unification_context().unify(type0, type1) {
            Ok(()) => expr,
            Err(error) => {
                let lhs = self.pretty_print_value(type0);
                let rhs = self.pretty_print_value(type1);
                self.push_message(Message::FailedToUnify {
                    range,
                    lhs,
                    rhs,
                    error,
                });
                core::Term::Prim(span, Prim::ReportedError)
            }
        }
    }

    /// Elaborate a module
    pub fn elab_module(&mut self, surface_module: &Module<'_, ByteRange>) -> core::Module<'arena> {
        let elab_order = order::elaboration_order(self, surface_module);
        let mut items = SliceVec::new(self.scope, elab_order.len());

        for item in elab_order.iter().copied().map(|i| &surface_module.items[i]) {
            match item {
                Item::Def(item) => {
                    let (expr, type_value) =
                        self.synth_fun_lit(item.range, item.patterns, item.expr, item.type_);
                    let expr_value = self.eval_env().eval(&expr);
                    let r#type = self.quote_env(self.scope).quote(&type_value);

                    self.item_env
                        .push_definition(item.label.1, type_value, expr_value);

                    items.push(core::Item::Def {
                        label: item.label.1,
                        r#type: self.scope.to_scope(r#type),
                        expr: self.scope.to_scope(expr),
                    });
                }
                Item::ReportedError(_) => {}
            }
        }

        core::Module {
            items: items.into(),
        }
    }

    /// Check that a pattern matches an expected type.
    fn check_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> CheckedPattern {
        match pattern {
            Pattern::Name(range, name) => CheckedPattern::Binder(*range, *name),
            Pattern::Placeholder(range) => CheckedPattern::Placeholder(*range),
            Pattern::StringLiteral(range, string) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self
                        .parse_ascii(*range, *string)
                        .map(|num| Const::U8(num, UIntStyle::Ascii)),
                    Some((Prim::U16Type, [])) => self
                        .parse_ascii(*range, *string)
                        .map(|num| Const::U16(num, UIntStyle::Ascii)),
                    Some((Prim::U32Type, [])) => self
                        .parse_ascii(*range, *string)
                        .map(|num| Const::U32(num, UIntStyle::Ascii)),
                    Some((Prim::U64Type, [])) => self
                        .parse_ascii(*range, *string)
                        .map(|num| Const::U64(num, UIntStyle::Ascii)),
                    // Some((Prim::Array8Type, [len, _])) => todo!(),
                    // Some((Prim::Array16Type, [len, _])) => todo!(),
                    // Some((Prim::Array32Type, [len, _])) => todo!(),
                    // Some((Prim::Array64Type, [len, _])) => todo!(),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        let expected_type = self.pretty_print_value(expected_type);
                        self.push_message(Message::StringLiteralNotSupported {
                            range: *range,
                            expected_type,
                        });
                        None
                    }
                };

                match constant {
                    Some(constant) => CheckedPattern::ConstLit(*range, constant),
                    None => CheckedPattern::ReportedError(*range),
                }
            }
            Pattern::NumberLiteral(range, number) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self
                        .parse_number_radix(*range, *number)
                        .map(|(num, style)| Const::U8(num, style)),
                    Some((Prim::U16Type, [])) => self
                        .parse_number_radix(*range, *number)
                        .map(|(num, style)| Const::U16(num, style)),
                    Some((Prim::U32Type, [])) => self
                        .parse_number_radix(*range, *number)
                        .map(|(num, style)| Const::U32(num, style)),
                    Some((Prim::U64Type, [])) => self
                        .parse_number_radix(*range, *number)
                        .map(|(num, style)| Const::U64(num, style)),
                    Some((Prim::S8Type, [])) => self.parse_number(*range, *number).map(Const::S8),
                    Some((Prim::S16Type, [])) => self.parse_number(*range, *number).map(Const::S16),
                    Some((Prim::S32Type, [])) => self.parse_number(*range, *number).map(Const::S32),
                    Some((Prim::S64Type, [])) => self.parse_number(*range, *number).map(Const::S64),
                    Some((Prim::F32Type, [])) => self.parse_number(*range, *number).map(Const::F32),
                    Some((Prim::F64Type, [])) => self.parse_number(*range, *number).map(Const::F64),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        let expected_type = self.pretty_print_value(expected_type);
                        self.push_message(Message::NumericLiteralNotSupported {
                            range: *range,
                            expected_type,
                        });
                        None
                    }
                };

                match constant {
                    Some(constant) => CheckedPattern::ConstLit(*range, constant),
                    None => CheckedPattern::ReportedError(*range),
                }
            }
            Pattern::BooleanLiteral(range, boolean) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::BoolType, [])) => match *boolean {
                        true => Some(Const::Bool(true)),
                        false => Some(Const::Bool(false)),
                    },
                    _ => {
                        self.push_message(Message::BooleanLiteralNotSupported { range: *range });
                        None
                    }
                };

                match constant {
                    Some(constant) => CheckedPattern::ConstLit(*range, constant),
                    None => CheckedPattern::ReportedError(*range),
                }
            }
        }
    }

    /// Synthesize the type of a pattern.
    fn synth_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
    ) -> (CheckedPattern, ArcValue<'arena>) {
        match pattern {
            Pattern::Name(range, name) => {
                let source = MetaSource::NamedPatternType(*range, *name);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::Binder(*range, *name), r#type)
            }
            Pattern::Placeholder(range) => {
                let source = MetaSource::PlaceholderPatternType(*range);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::Placeholder(*range), r#type)
            }
            Pattern::StringLiteral(range, _) => {
                self.push_message(Message::AmbiguousStringLiteral { range: *range });
                let source = MetaSource::ReportedErrorType(*range);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::ReportedError(*range), r#type)
            }
            Pattern::NumberLiteral(range, _) => {
                self.push_message(Message::AmbiguousNumericLiteral { range: *range });
                let source = MetaSource::ReportedErrorType(*range);
                let r#type = self.push_unsolved_type(source);
                (CheckedPattern::ReportedError(*range), r#type)
            }
            Pattern::BooleanLiteral(range, val) => {
                let r#const = Const::Bool(*val);
                let r#type = self.bool_type.clone();
                (CheckedPattern::ConstLit(*range, r#const), r#type)
            }
        }
    }

    /// Check that the type of an annotated pattern matches an expected type.
    fn check_ann_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
        r#type: Option<&Term<'_, ByteRange>>,
        expected_type: &ArcValue<'arena>,
    ) -> CheckedPattern {
        match r#type {
            None => self.check_pattern(pattern, expected_type),
            Some(r#type) => {
                let range = r#type.range();
                let r#type = self.check(r#type, &self.universe.clone());
                let r#type = self.eval_env().eval(&r#type);

                match self.unification_context().unify(&r#type, expected_type) {
                    Ok(()) => self.check_pattern(pattern, &r#type),
                    Err(error) => {
                        let lhs = self.pretty_print_value(&r#type);
                        let rhs = self.pretty_print_value(expected_type);
                        self.push_message(Message::FailedToUnify {
                            range,
                            lhs,
                            rhs,
                            error,
                        });
                        CheckedPattern::ReportedError(range)
                    }
                }
            }
        }
    }

    /// Synthesize the type of an annotated pattern.
    fn synth_ann_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
        r#type: Option<&Term<'_, ByteRange>>,
    ) -> (CheckedPattern, ArcValue<'arena>) {
        match r#type {
            None => self.synth_pattern(pattern),
            Some(r#type) => {
                let r#type = self.check(r#type, &self.universe.clone());
                let type_value = self.eval_env().eval(&r#type);
                (self.check_pattern(pattern, &type_value), type_value)
            }
        }
    }

    /// Push a local definition onto the context.
    /// The supplied `pattern` is expected to be irrefutable.
    fn push_local_def(
        &mut self,
        pattern: CheckedPattern,
        expr: ArcValue<'arena>,
        r#type: ArcValue<'arena>,
    ) -> Option<StringId> {
        let name = match pattern {
            CheckedPattern::Binder(_, name) => Some(name),
            CheckedPattern::Placeholder(_) => None,
            // FIXME: generate failing parameter expressions?
            CheckedPattern::ConstLit(range, _) => {
                self.push_message(Message::RefutablePattern {
                    pattern_range: range,
                });
                None
            }
            CheckedPattern::ReportedError(_) => None,
        };

        self.local_env.push_def(name, expr, r#type);

        name
    }

    /// Push a local parameter onto the context.
    /// The supplied `pattern` is expected to be irrefutable.
    fn push_local_param(
        &mut self,
        pattern: CheckedPattern,
        r#type: ArcValue<'arena>,
    ) -> (Option<StringId>, ArcValue<'arena>) {
        let name = match pattern {
            CheckedPattern::Binder(_, name) => Some(name),
            CheckedPattern::Placeholder(_) => None,
            // FIXME: generate failing parameter expressions?
            CheckedPattern::ConstLit(range, _) => {
                self.push_message(Message::RefutablePattern {
                    pattern_range: range,
                });
                None
            }
            CheckedPattern::ReportedError(_) => None,
        };

        let expr = self.local_env.push_param(name, r#type);

        (name, expr)
    }

    /// Check that a surface term conforms to the given type.
    ///
    /// Returns the elaborated term in the core language.
    pub fn check(
        &mut self,
        surface_term: &Term<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let expected_type = self.elim_env().force(expected_type);

        match (surface_term, expected_type.as_ref()) {
            (Term::Let(range, def_pattern, def_type, def_expr, body_expr), _) => {
                let (def_pattern, def_type_value) = self.synth_ann_pattern(def_pattern, *def_type);
                let def_type = self.quote_env(self.scope).quote(&def_type_value); // FIXME: avoid requote if possible?
                let def_expr = self.check(def_expr, &def_type_value);
                let def_expr_value = self.eval_env().eval(&def_expr);

                let def_name = self.push_local_def(def_pattern, def_expr_value, def_type_value); // TODO: split on constants
                let body_expr = self.check(body_expr, &expected_type);
                self.local_env.pop();

                core::Term::Let(
                    range.into(),
                    def_name,
                    self.scope.to_scope(def_type),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(body_expr),
                )
            }
            (Term::Match(range, scrutinee_expr, equations), _) => {
                self.check_match(*range, scrutinee_expr, equations, &expected_type)
            }
            (Term::FunLiteral(range, patterns, body_expr), _) => {
                self.check_fun_lit(*range, patterns, body_expr, &expected_type)
            }
            (Term::RecordLiteral(range, expr_fields), Value::RecordType(labels, types)) => {
                // TODO: improve handling of duplicate labels
                if expr_fields.len() != labels.len()
                    || Iterator::zip(expr_fields.iter(), labels.iter())
                        .any(|(expr_field, type_label)| expr_field.label.1 != *type_label)
                {
                    self.push_message(Message::MismatchedFieldLabels {
                        range: *range,
                        expr_labels: (expr_fields.iter())
                            .map(|expr_field| expr_field.label)
                            .collect(),
                        type_labels: labels.to_vec(),
                    });
                    return core::Term::Prim(range.into(), Prim::ReportedError);
                }

                let mut types = types.clone();
                let mut expr_fields = expr_fields.iter();
                let mut exprs = SliceVec::new(self.scope, types.len());

                while let Some((expr_field, (r#type, next_types))) =
                    Option::zip(expr_fields.next(), self.elim_env().split_telescope(types))
                {
                    let expr = self.check(&expr_field.expr, &r#type);
                    types = next_types(self.eval_env().eval(&expr));
                    exprs.push(expr);
                }

                core::Term::RecordLit(range.into(), labels, exprs.into())
            }
            (Term::UnitLiteral(range), Value::Universe) => {
                core::Term::RecordType(range.into(), &[], &[])
            }
            (Term::UnitLiteral(range), _)
                if matches!(
                    expected_type.match_prim_spine(),
                    Some((Prim::FormatType, [])),
                ) =>
            {
                core::Term::FormatRecord(range.into(), &[], &[])
            }
            (Term::ArrayLiteral(range, elem_exprs), _) => {
                use crate::core::semantics::Elim::FunApp as App;

                let (len_value, elem_type) = match expected_type.match_prim_spine() {
                    Some((Prim::ArrayType, [App(elem_type)])) => (None, elem_type),
                    Some((Prim::Array8Type, [App(len), App(elem_type)])) => (Some(len), elem_type),
                    Some((Prim::Array16Type, [App(len), App(elem_type)])) => (Some(len), elem_type),
                    Some((Prim::Array32Type, [App(len), App(elem_type)])) => (Some(len), elem_type),
                    Some((Prim::Array64Type, [App(len), App(elem_type)])) => (Some(len), elem_type),
                    Some((Prim::ReportedError, _)) => {
                        return core::Term::Prim(range.into(), Prim::ReportedError)
                    }
                    _ => {
                        let expected_type = self.pretty_print_value(&expected_type);
                        self.push_message(Message::ArrayLiteralNotSupported {
                            range: *range,
                            expected_type,
                        });
                        return core::Term::Prim(range.into(), Prim::ReportedError);
                    }
                };

                let len = match len_value.map(|val| val.as_ref()) {
                    None => Some(elem_exprs.len() as u64),
                    Some(Value::ConstLit(Const::U8(len, _))) => Some(*len as u64),
                    Some(Value::ConstLit(Const::U16(len, _))) => Some(*len as u64),
                    Some(Value::ConstLit(Const::U32(len, _))) => Some(*len as u64),
                    Some(Value::ConstLit(Const::U64(len, _))) => Some(*len as u64),
                    Some(Value::Stuck(Head::Prim(Prim::ReportedError), _)) => {
                        return core::Term::Prim(range.into(), Prim::ReportedError);
                    }
                    _ => None,
                };

                match len {
                    Some(len) if elem_exprs.len() as u64 == len => core::Term::ArrayLit(
                        range.into(),
                        self.scope.to_scope_from_iter(
                            (elem_exprs.iter()).map(|elem_expr| self.check(elem_expr, elem_type)),
                        ),
                    ),
                    _ => {
                        // Check the array elements anyway in order to report
                        // any errors inside the literal as well.
                        for elem_expr in *elem_exprs {
                            self.check(elem_expr, elem_type);
                        }

                        let expected_len = self.pretty_print_value(len_value.unwrap());
                        self.push_message(Message::MismatchedArrayLength {
                            range: *range,
                            found_len: elem_exprs.len(),
                            expected_len,
                        });

                        core::Term::Prim(range.into(), Prim::ReportedError)
                    }
                }
            }
            (Term::StringLiteral(range, string), _) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self
                        .parse_ascii(*range, *string)
                        .map(|num| Const::U8(num, UIntStyle::Ascii)),
                    Some((Prim::U16Type, [])) => self
                        .parse_ascii(*range, *string)
                        .map(|num| Const::U16(num, UIntStyle::Ascii)),
                    Some((Prim::U32Type, [])) => self
                        .parse_ascii(*range, *string)
                        .map(|num| Const::U32(num, UIntStyle::Ascii)),
                    Some((Prim::U64Type, [])) => self
                        .parse_ascii(*range, *string)
                        .map(|num| Const::U64(num, UIntStyle::Ascii)),
                    // Some((Prim::Array8Type, [len, _])) => todo!(),
                    // Some((Prim::Array16Type, [len, _])) => todo!(),
                    // Some((Prim::Array32Type, [len, _])) => todo!(),
                    // Some((Prim::Array64Type, [len, _])) => todo!(),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        let expected_type = self.pretty_print_value(&expected_type);
                        self.push_message(Message::StringLiteralNotSupported {
                            range: *range,
                            expected_type,
                        });
                        None
                    }
                };

                match constant {
                    Some(constant) => core::Term::ConstLit(range.into(), constant),
                    None => core::Term::Prim(range.into(), Prim::ReportedError),
                }
            }
            (Term::NumberLiteral(range, number), _) => {
                let constant = match expected_type.match_prim_spine() {
                    Some((Prim::U8Type, [])) => self
                        .parse_number_radix(*range, *number)
                        .map(|(num, style)| Const::U8(num, style)),
                    Some((Prim::U16Type, [])) => self
                        .parse_number_radix(*range, *number)
                        .map(|(num, style)| Const::U16(num, style)),
                    Some((Prim::U32Type, [])) => self
                        .parse_number_radix(*range, *number)
                        .map(|(num, style)| Const::U32(num, style)),
                    Some((Prim::U64Type, [])) => self
                        .parse_number_radix(*range, *number)
                        .map(|(num, style)| Const::U64(num, style)),
                    Some((Prim::S8Type, [])) => self.parse_number(*range, *number).map(Const::S8),
                    Some((Prim::S16Type, [])) => self.parse_number(*range, *number).map(Const::S16),
                    Some((Prim::S32Type, [])) => self.parse_number(*range, *number).map(Const::S32),
                    Some((Prim::S64Type, [])) => self.parse_number(*range, *number).map(Const::S64),
                    Some((Prim::F32Type, [])) => self.parse_number(*range, *number).map(Const::F32),
                    Some((Prim::F64Type, [])) => self.parse_number(*range, *number).map(Const::F64),
                    Some((Prim::ReportedError, _)) => None,
                    _ => {
                        let expected_type = self.pretty_print_value(&expected_type);
                        self.push_message(Message::NumericLiteralNotSupported {
                            range: *range,
                            expected_type,
                        });
                        return core::Term::Prim(range.into(), Prim::ReportedError);
                    }
                };

                match constant {
                    Some(constant) => core::Term::ConstLit(range.into(), constant),
                    None => core::Term::Prim(range.into(), Prim::ReportedError),
                }
            }
            (Term::ReportedError(range), _) => core::Term::Prim(range.into(), Prim::ReportedError),
            (_, _) => {
                let (core_term, synth_type) = self.synth(surface_term);
                self.convert(surface_term.range(), core_term, &synth_type, &expected_type)
            }
        }
    }

    /// Synthesize the type of the given surface term.
    ///
    /// Returns the elaborated term in the core language and its type.
    pub fn synth(
        &mut self,
        surface_term: &Term<'_, ByteRange>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        match surface_term {
            Term::Name(range, name) => {
                if let Some((term, r#type)) = self.get_local_name(*name) {
                    return (core::Term::LocalVar(range.into(), term), r#type.clone());
                }
                if let Some((term, r#type)) = self.get_item_name(*name) {
                    return (core::Term::ItemVar(range.into(), term), r#type.clone());
                }

                self.push_message(Message::UnboundName {
                    range: *range,
                    name: *name,
                });
                self.synth_reported_error(*range)
            }
            Term::Hole(range, name) => {
                let type_source = MetaSource::HoleType(*range, *name);
                let expr_source = MetaSource::HoleExpr(*range, *name);

                let r#type = self.push_unsolved_type(type_source);
                let expr = self.push_unsolved_term(expr_source, r#type.clone());

                (expr, r#type)
            }
            Term::Placeholder(range) => {
                let type_source = MetaSource::PlaceholderType(*range);
                let expr_source = MetaSource::PlaceholderExpr(*range);

                let r#type = self.push_unsolved_type(type_source);
                let expr = self.push_unsolved_term(expr_source, r#type.clone());

                (expr, r#type)
            }
            Term::Ann(range, expr, r#type) => {
                let r#type = self.check(r#type, &self.universe.clone());
                let type_value = self.eval_env().eval(&r#type);
                let expr = self.check(expr, &type_value);

                let ann_expr = core::Term::Ann(
                    range.into(),
                    self.scope.to_scope(expr),
                    self.scope.to_scope(r#type),
                );

                (ann_expr, type_value)
            }
            Term::Let(range, def_pattern, def_type, def_expr, body_expr) => {
                let (def_pattern, def_type_value) = self.synth_ann_pattern(def_pattern, *def_type);
                let def_type = self.quote_env(self.scope).quote(&def_type_value); // FIXME: avoid requote if possible?
                let def_expr = self.check(def_expr, &def_type_value);
                let def_expr_value = self.eval_env().eval(&def_expr);

                let def_name = self.push_local_def(def_pattern, def_expr_value, def_type_value);
                let (body_expr, body_type) = self.synth(body_expr);
                self.local_env.pop();

                let let_expr = core::Term::Let(
                    range.into(),
                    def_name,
                    self.scope.to_scope(def_type),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(body_expr),
                );

                (let_expr, body_type)
            }
            Term::Match(range, scrutinee_expr, equations) => {
                // Create a single metavariable representing the overall
                // type of the match expression, allowing us to unify this with
                // the types of the match equations together.
                let r#type = self.push_unsolved_type(MetaSource::MatchExprType(*range));
                let expr = self.check_match(*range, scrutinee_expr, equations, &r#type);
                (expr, r#type)
            }
            Term::Universe(range) => (core::Term::Universe(range.into()), self.universe.clone()),
            Term::Arrow(range, param_type, body_type) => {
                let universe = self.universe.clone();
                let param_type = self.check(param_type, &universe);
                let param_type_value = self.eval_env().eval(&param_type);

                self.local_env.push_param(None, param_type_value);
                let body_type = self.check(body_type, &universe);
                self.local_env.pop();

                let fun_type = core::Term::FunType(
                    range.into(),
                    None,
                    self.scope.to_scope(param_type),
                    self.scope.to_scope(body_type),
                );

                (fun_type, self.universe.clone())
            }
            Term::FunType(range, patterns, body_type) => {
                let initial_local_len = self.local_env.len();

                // Elaborate the parameters, collecting them in a stack
                let mut params = Vec::with_capacity(patterns.len());
                for (pattern, r#type) in *patterns {
                    let (pattern, type_value) = self.synth_ann_pattern(pattern, *r#type);
                    let r#type = self.quote_env(self.scope).quote(&type_value);
                    let (name, _) = self.push_local_param(pattern, type_value);
                    params.push((name, r#type));
                }

                let mut fun_type = self.check(body_type, &self.universe.clone());
                self.local_env.truncate(initial_local_len);

                // Construct the function type from the parameters in reverse
                for (name, r#type) in params.into_iter().rev() {
                    fun_type = core::Term::FunType(
                        range.into(), // FIXME
                        name,
                        self.scope.to_scope(r#type),
                        self.scope.to_scope(fun_type),
                    );
                }

                (fun_type, self.universe.clone())
            }
            Term::FunLiteral(range, patterns, body_expr) => {
                self.synth_fun_lit(*range, patterns, body_expr, None)
            }
            Term::App(range, head_expr, arg_exprs) => {
                let head_range = head_expr.range();
                let (mut head_expr, mut head_type) = self.synth(head_expr);

                for arg_expr in *arg_exprs {
                    head_type = self.elim_env().force(&head_type);
                    match (&head_expr, head_type.as_ref()) {
                        // Ensure that the head of the application is a function
                        (_, Value::FunType(_, param_type, body_type)) => {
                            // Check the argument and apply it to the body type
                            let param_range = arg_expr.range();
                            let arg_expr = self.check(arg_expr, param_type);
                            let arg_expr_value = self.eval_env().eval(&arg_expr);

                            head_expr = core::Term::FunApp(
                                ByteRange::merge(&head_range, &param_range).into(),
                                self.scope.to_scope(head_expr),
                                self.scope.to_scope(arg_expr),
                            );
                            head_type = self.elim_env().apply_closure(body_type, arg_expr_value);
                        }
                        // There's been an error when elaborating the head of
                        // the application, so avoid trying to elaborate any
                        // further to prevent cascading type errors.
                        (core::Term::Prim(_, Prim::ReportedError), _)
                        | (_, Value::Stuck(Head::Prim(Prim::ReportedError), _)) => {
                            return self.synth_reported_error(*range);
                        }
                        _ => {
                            // NOTE: We could try to infer that this is a function type,
                            // but this takes more work to prevent cascading type errors
                            let head_type = self.pretty_print_value(&head_type);
                            self.push_message(Message::UnexpectedArgument {
                                head_range,
                                head_type,
                                arg_range: arg_expr.range(),
                            });
                            return self.synth_reported_error(*range);
                        }
                    }
                }

                (head_expr, head_type)
            }
            Term::RecordType(range, type_fields) => {
                let universe = self.universe.clone();
                let initial_local_len = self.local_env.len();
                let (labels, type_fields) =
                    self.report_duplicate_labels(*range, type_fields, |f| f.label);
                let mut types = SliceVec::new(self.scope, labels.len());

                for type_field in type_fields {
                    let r#type = self.check(&type_field.type_, &universe);
                    let type_value = self.eval_env().eval(&r#type);
                    self.local_env
                        .push_param(Some(type_field.label.1), type_value);
                    types.push(r#type);
                }
                self.local_env.truncate(initial_local_len);

                let record_type = core::Term::RecordType(range.into(), labels, types.into());

                (record_type, universe)
            }
            Term::RecordLiteral(range, expr_fields) => {
                let (labels, expr_fields) =
                    self.report_duplicate_labels(*range, expr_fields, |f| f.label);
                let mut types = SliceVec::new(self.scope, labels.len());
                let mut exprs = SliceVec::new(self.scope, labels.len());

                for expr_field in expr_fields {
                    let (expr, r#type) = self.synth(&expr_field.expr);
                    types.push(self.quote_env(self.scope).quote(&r#type)); // NOTE: Unsure if these are correctly bound!
                    exprs.push(expr);
                }

                let types = Telescope::new(self.local_env.exprs.clone(), types.into());

                (
                    core::Term::RecordLit(range.into(), labels, exprs.into()),
                    Spanned::empty(Arc::new(Value::RecordType(labels, types))),
                )
            }
            Term::UnitLiteral(range) => (
                core::Term::RecordLit(range.into(), &[], &[]),
                Spanned::empty(Arc::new(Value::RecordType(
                    &[],
                    Telescope::new(SharedEnv::new(), &[]),
                ))),
            ),
            Term::Proj(range, head_expr, labels) => {
                let head_range = head_expr.range();
                let (mut head_expr, mut head_type) = self.synth(head_expr);

                'labels: for (label_range, proj_label) in *labels {
                    head_type = self.elim_env().force(&head_type);
                    match (&head_expr, head_type.as_ref()) {
                        // Ensure that the head of the projection is a record
                        (_, Value::RecordType(labels, types)) => {
                            let mut labels = labels.iter().copied();
                            let mut types = types.clone();

                            let head_expr_value = self.eval_env().eval(&head_expr);

                            // Look for a field matching the label of the current
                            // projection in the record type.
                            while let Some((label, (r#type, next_types))) =
                                Option::zip(labels.next(), self.elim_env().split_telescope(types))
                            {
                                if *proj_label == label {
                                    // The field was found. Update the head expression
                                    // and continue elaborating the next projection.
                                    head_expr = core::Term::RecordProj(
                                        ByteRange::merge(&head_range, label_range).into(),
                                        self.scope.to_scope(head_expr),
                                        *proj_label,
                                    );
                                    head_type = r#type;
                                    continue 'labels;
                                } else {
                                    // This is not the field we are looking for. Substitute the
                                    // value of this field in the rest of the types and continue
                                    // looking for the field.
                                    let head_expr = head_expr_value.clone();
                                    let expr = self.elim_env().record_proj(head_expr, label);
                                    types = next_types(expr);
                                }
                            }
                            // Couldn't find the field in the record type.
                            // Fallthrough with an error.
                        }
                        // There's been an error when elaborating the head of
                        // the projection, so avoid trying to elaborate any
                        // further to prevent cascading type errors.
                        (core::Term::Prim(_, Prim::ReportedError), _)
                        | (_, Value::Stuck(Head::Prim(Prim::ReportedError), _)) => {
                            return self.synth_reported_error(*range);
                        }
                        // The head expression was not a record type.
                        // Fallthrough with an error.
                        _ => {}
                    }

                    let head_type = self.pretty_print_value(&head_type);
                    self.push_message(Message::UnknownField {
                        head_range,
                        head_type,
                        label_range: *label_range,
                        label: *proj_label,
                    });
                    return self.synth_reported_error(*range);
                }

                (head_expr, head_type)
            }
            Term::ArrayLiteral(range, _) => {
                self.push_message(Message::AmbiguousArrayLiteral { range: *range });
                self.synth_reported_error(*range)
            }
            // TODO: Stuck macros + unification like in Klister?
            Term::StringLiteral(range, _) => {
                self.push_message(Message::AmbiguousStringLiteral { range: *range });
                self.synth_reported_error(*range)
            }
            // TODO: Stuck macros + unification like in Klister?
            Term::NumberLiteral(range, _) => {
                self.push_message(Message::AmbiguousNumericLiteral { range: *range });
                self.synth_reported_error(*range)
            }
            Term::BooleanLiteral(range, val) => {
                let expr = core::Term::ConstLit(range.into(), Const::Bool(*val));
                (expr, self.bool_type.clone())
            }
            Term::FormatRecord(range, format_fields) => {
                let (labels, formats) = self.check_format_fields(*range, format_fields);
                let format_record = core::Term::FormatRecord(range.into(), labels, formats);
                (format_record, self.format_type.clone())
            }
            Term::FormatCond(range, (_, name), format, pred) => {
                let format_type = self.format_type.clone();
                let format = self.check(format, &format_type);
                let format_value = self.eval_env().eval(&format);
                let repr_type = self.elim_env().format_repr(&format_value);

                self.local_env.push_param(Some(*name), repr_type);
                let bool_type = self.bool_type.clone();
                let pred_expr = self.check(pred, &bool_type);
                self.local_env.pop();

                let cond_format = core::Term::FormatCond(
                    range.into(),
                    *name,
                    self.scope.to_scope(format),
                    self.scope.to_scope(pred_expr),
                );

                (cond_format, format_type)
            }
            Term::FormatOverlap(range, format_fields) => {
                let (labels, formats) = self.check_format_fields(*range, format_fields);
                let overlap_format = core::Term::FormatOverlap(range.into(), labels, formats);

                (overlap_format, self.format_type.clone())
            }
            Term::BinOp(range, lhs, op, rhs) => self.synth_bin_op(*range, lhs, *op, rhs),
            Term::ReportedError(range) => self.synth_reported_error(*range),
        }
    }

    fn check_fun_lit(
        &mut self,
        range: ByteRange,
        patterns: &[(Pattern<ByteRange>, Option<&Term<'_, ByteRange>>)],
        body_expr: &Term<'_, ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        match patterns.split_first() {
            Some(((pattern, r#type), next_patterns)) => {
                let body_type = self.elim_env().force(expected_type);
                match body_type.as_ref() {
                    Value::FunType(_, param_type, next_body_type) => {
                        let range = ByteRange::merge(&pattern.range(), &body_expr.range()).unwrap();
                        let pattern = self.check_ann_pattern(pattern, *r#type, param_type);
                        let (name, arg_expr) = self.push_local_param(pattern, param_type.clone());

                        let body_type = self.elim_env().apply_closure(next_body_type, arg_expr);
                        let body_expr =
                            self.check_fun_lit(range, next_patterns, body_expr, &body_type);
                        self.local_env.pop();

                        core::Term::FunLit(range.into(), name, self.scope.to_scope(body_expr))
                    }
                    // Attempt to elaborate the the body of the function in synthesis
                    // mode if we are checking against a metavariable.
                    Value::Stuck(Head::MetaVar(_), _) => {
                        let range = ByteRange::merge(&pattern.range(), &body_expr.range()).unwrap();
                        let (expr, r#type) = self.synth_fun_lit(range, patterns, body_expr, None);
                        self.convert(range, expr, &r#type, expected_type)
                    }
                    Value::Stuck(Head::Prim(Prim::ReportedError), _) => {
                        core::Term::Prim(range.into(), Prim::ReportedError)
                    }
                    _ => {
                        self.push_message(Message::UnexpectedParameter {
                            param_range: pattern.range(),
                        });
                        // TODO: For improved error recovery, bind the rest of
                        // the parameters, and check the body of the function
                        // literal using the expected body type.
                        core::Term::Prim(range.into(), Prim::ReportedError)
                    }
                }
            }
            None => self.check(body_expr, expected_type),
        }
    }

    fn synth_fun_lit(
        &mut self,
        range: ByteRange,
        patterns: &[(Pattern<ByteRange>, Option<&Term<ByteRange>>)],
        body_expr: &Term<'_, ByteRange>,
        body_type: Option<&Term<'_, ByteRange>>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        let initial_local_len = self.local_env.len();

        // Elaborate the parameters, collecting them into a stack
        let mut params = Vec::with_capacity(patterns.len());
        for (i, (pattern, r#type)) in patterns.iter().enumerate() {
            let range = match i {
                0 => range, // The first pattern uses the range of the full function literal
                _ => ByteRange::merge(&pattern.range(), &body_expr.range()).unwrap(),
            };
            let (pattern, type_value) = self.synth_ann_pattern(pattern, *r#type);
            let r#type = self.quote_env(self.scope).quote(&type_value);
            let (name, _) = self.push_local_param(pattern, type_value);
            params.push((range, name, r#type));
        }

        let (mut fun_lit, mut fun_type) = match body_type {
            Some(body_type) => {
                let body_type = self.check(body_type, &self.universe.clone());
                let body_type_value = self.eval_env().eval(&body_type);
                (self.check(body_expr, &body_type_value), body_type)
            }
            None => {
                let (body_expr, body_type) = self.synth(body_expr);
                (body_expr, self.quote_env(self.scope).quote(&body_type))
            }
        };

        self.local_env.truncate(initial_local_len);

        // Construct the function literal and type from the parameters in reverse
        for (range, name, r#type) in params.into_iter().rev() {
            fun_lit = core::Term::FunLit(range.into(), name, self.scope.to_scope(fun_lit));
            fun_type = core::Term::FunType(
                Span::Empty,
                name,
                self.scope.to_scope(r#type),
                self.scope.to_scope(fun_type),
            );
        }

        (fun_lit, self.eval_env().eval(&fun_type))
    }

    fn synth_bin_op(
        &mut self,
        range: ByteRange,
        lhs: &Term<'_, ByteRange>,
        op: BinOp<ByteRange>,
        rhs: &Term<'_, ByteRange>,
    ) -> (core::Term<'arena>, ArcValue<'arena>) {
        use BinOp::*;
        use Prim::*;

        // de-sugar into function application
        let (lhs_expr, lhs_type) = self.synth(lhs);
        let (rhs_expr, rhs_type) = self.synth(rhs);
        let lhs_type = self.elim_env().force(&lhs_type);
        let rhs_type = self.elim_env().force(&rhs_type);
        let operand_types = Option::zip(lhs_type.match_prim_spine(), rhs_type.match_prim_spine());

        let (fun, body_type) = match (op, operand_types) {
            (Mul(_), Some(((U8Type, []), (U8Type, [])))) => (U8Mul, U8Type),
            (Mul(_), Some(((U16Type, []), (U16Type, [])))) => (U16Mul, U16Type),
            (Mul(_), Some(((U32Type, []), (U32Type, [])))) => (U32Mul, U32Type),
            (Mul(_), Some(((U64Type, []), (U64Type, [])))) => (U64Mul, U64Type),

            (Mul(_), Some(((S8Type, []), (S8Type, [])))) => (S8Mul, S8Type),
            (Mul(_), Some(((S16Type, []), (S16Type, [])))) => (S16Mul, S16Type),
            (Mul(_), Some(((S32Type, []), (S32Type, [])))) => (S32Mul, S32Type),
            (Mul(_), Some(((S64Type, []), (S64Type, [])))) => (S64Mul, S64Type),

            (Div(_), Some(((U8Type, []), (U8Type, [])))) => (U8Div, U8Type),
            (Div(_), Some(((U16Type, []), (U16Type, [])))) => (U16Div, U16Type),
            (Div(_), Some(((U32Type, []), (U32Type, [])))) => (U32Div, U32Type),
            (Div(_), Some(((U64Type, []), (U64Type, [])))) => (U64Div, U64Type),

            (Div(_), Some(((S8Type, []), (S8Type, [])))) => (S8Div, S8Type),
            (Div(_), Some(((S16Type, []), (S16Type, [])))) => (S16Div, S16Type),
            (Div(_), Some(((S32Type, []), (S32Type, [])))) => (S32Div, S32Type),
            (Div(_), Some(((S64Type, []), (S64Type, [])))) => (S64Div, S64Type),

            (Add(_), Some(((U8Type, []), (U8Type, [])))) => (U8Add, U8Type),
            (Add(_), Some(((U16Type, []), (U16Type, [])))) => (U16Add, U16Type),
            (Add(_), Some(((U32Type, []), (U32Type, [])))) => (U32Add, U32Type),
            (Add(_), Some(((U64Type, []), (U64Type, [])))) => (U64Add, U64Type),

            (Add(_), Some(((S8Type, []), (S8Type, [])))) => (S8Add, S8Type),
            (Add(_), Some(((S16Type, []), (S16Type, [])))) => (S16Add, S16Type),
            (Add(_), Some(((S32Type, []), (S32Type, [])))) => (S32Add, S32Type),
            (Add(_), Some(((S64Type, []), (S64Type, [])))) => (S64Add, S64Type),

            (Add(_), Some(((PosType, []), (U8Type, [])))) => (PosAddU8, PosType),
            (Add(_), Some(((PosType, []), (U16Type, [])))) => (PosAddU16, PosType),
            (Add(_), Some(((PosType, []), (U32Type, [])))) => (PosAddU32, PosType),
            (Add(_), Some(((PosType, []), (U64Type, [])))) => (PosAddU64, PosType),

            (Sub(_), Some(((U8Type, []), (U8Type, [])))) => (U8Sub, U8Type),
            (Sub(_), Some(((U16Type, []), (U16Type, [])))) => (U16Sub, U16Type),
            (Sub(_), Some(((U32Type, []), (U32Type, [])))) => (U32Sub, U32Type),
            (Sub(_), Some(((U64Type, []), (U64Type, [])))) => (U64Sub, U64Type),

            (Sub(_), Some(((S8Type, []), (S8Type, [])))) => (S8Sub, S8Type),
            (Sub(_), Some(((S16Type, []), (S16Type, [])))) => (S16Sub, S16Type),
            (Sub(_), Some(((S32Type, []), (S32Type, [])))) => (S32Sub, S32Type),
            (Sub(_), Some(((S64Type, []), (S64Type, [])))) => (S64Sub, S64Type),

            (Eq(_), Some(((BoolType, []), (BoolType, [])))) => (U8Eq, BoolType),
            (Eq(_), Some(((U8Type, []), (U8Type, [])))) => (U8Eq, BoolType),
            (Eq(_), Some(((U16Type, []), (U16Type, [])))) => (U16Eq, BoolType),
            (Eq(_), Some(((U32Type, []), (U32Type, [])))) => (U32Eq, BoolType),
            (Eq(_), Some(((U64Type, []), (U64Type, [])))) => (U64Eq, BoolType),

            (Eq(_), Some(((S8Type, []), (S8Type, [])))) => (S8Eq, BoolType),
            (Eq(_), Some(((S16Type, []), (S16Type, [])))) => (S16Eq, BoolType),
            (Eq(_), Some(((S32Type, []), (S32Type, [])))) => (S32Eq, BoolType),
            (Eq(_), Some(((S64Type, []), (S64Type, [])))) => (S64Eq, BoolType),

            (Neq(_), Some(((BoolType, []), (BoolType, [])))) => (U8Neq, BoolType),
            (Neq(_), Some(((U8Type, []), (U8Type, [])))) => (U8Neq, BoolType),
            (Neq(_), Some(((U16Type, []), (U16Type, [])))) => (U16Neq, BoolType),
            (Neq(_), Some(((U32Type, []), (U32Type, [])))) => (U32Neq, BoolType),
            (Neq(_), Some(((U64Type, []), (U64Type, [])))) => (U64Neq, BoolType),

            (Neq(_), Some(((S8Type, []), (S8Type, [])))) => (S8Neq, BoolType),
            (Neq(_), Some(((S16Type, []), (S16Type, [])))) => (S16Neq, BoolType),
            (Neq(_), Some(((S32Type, []), (S32Type, [])))) => (S32Neq, BoolType),
            (Neq(_), Some(((S64Type, []), (S64Type, [])))) => (S64Neq, BoolType),

            (Lt(_), Some(((U8Type, []), (U8Type, [])))) => (U8Lt, BoolType),
            (Lt(_), Some(((U16Type, []), (U16Type, [])))) => (U16Lt, BoolType),
            (Lt(_), Some(((U32Type, []), (U32Type, [])))) => (U32Lt, BoolType),
            (Lt(_), Some(((U64Type, []), (U64Type, [])))) => (U64Lt, BoolType),

            (Lt(_), Some(((S8Type, []), (S8Type, [])))) => (S8Lt, BoolType),
            (Lt(_), Some(((S16Type, []), (S16Type, [])))) => (S16Lt, BoolType),
            (Lt(_), Some(((S32Type, []), (S32Type, [])))) => (S32Lt, BoolType),
            (Lt(_), Some(((S64Type, []), (S64Type, [])))) => (S64Lt, BoolType),

            (Lte(_), Some(((U8Type, []), (U8Type, [])))) => (U8Lte, BoolType),
            (Lte(_), Some(((U16Type, []), (U16Type, [])))) => (U16Lte, BoolType),
            (Lte(_), Some(((U32Type, []), (U32Type, [])))) => (U32Lte, BoolType),
            (Lte(_), Some(((U64Type, []), (U64Type, [])))) => (U64Lte, BoolType),

            (Lte(_), Some(((S8Type, []), (S8Type, [])))) => (S8Lte, BoolType),
            (Lte(_), Some(((S16Type, []), (S16Type, [])))) => (S16Lte, BoolType),
            (Lte(_), Some(((S32Type, []), (S32Type, [])))) => (S32Lte, BoolType),
            (Lte(_), Some(((S64Type, []), (S64Type, [])))) => (S64Lte, BoolType),

            (Gt(_), Some(((U8Type, []), (U8Type, [])))) => (U8Gt, BoolType),
            (Gt(_), Some(((U16Type, []), (U16Type, [])))) => (U16Gt, BoolType),
            (Gt(_), Some(((U32Type, []), (U32Type, [])))) => (U32Gt, BoolType),
            (Gt(_), Some(((U64Type, []), (U64Type, [])))) => (U64Gt, BoolType),

            (Gt(_), Some(((S8Type, []), (S8Type, [])))) => (S8Gt, BoolType),
            (Gt(_), Some(((S16Type, []), (S16Type, [])))) => (S16Gt, BoolType),
            (Gt(_), Some(((S32Type, []), (S32Type, [])))) => (S32Gt, BoolType),
            (Gt(_), Some(((S64Type, []), (S64Type, [])))) => (S64Gt, BoolType),

            (Gte(_), Some(((U8Type, []), (U8Type, [])))) => (U8Gte, BoolType),
            (Gte(_), Some(((U16Type, []), (U16Type, [])))) => (U16Gte, BoolType),
            (Gte(_), Some(((U32Type, []), (U32Type, [])))) => (U32Gte, BoolType),
            (Gte(_), Some(((U64Type, []), (U64Type, [])))) => (U64Gte, BoolType),

            (Gte(_), Some(((S8Type, []), (S8Type, [])))) => (S8Gte, BoolType),
            (Gte(_), Some(((S16Type, []), (S16Type, [])))) => (S16Gte, BoolType),
            (Gte(_), Some(((S32Type, []), (S32Type, [])))) => (S32Gte, BoolType),
            (Gte(_), Some(((S64Type, []), (S64Type, [])))) => (S64Gte, BoolType),

            _ => {
                let lhs_pretty = self.pretty_print_value(&lhs_type);
                let rhs_pretty = self.pretty_print_value(&rhs_type);
                self.push_message(Message::BinOpMismatchedTypes {
                    range,
                    lhs_range: lhs.range(),
                    rhs_range: rhs.range(),
                    op,
                    lhs: lhs_pretty,
                    rhs: rhs_pretty,
                });
                return self.synth_reported_error(range);
            }
        };

        let fun_head = core::Term::Prim(op.range().into(), fun);
        let fun_app = core::Term::FunApp(
            range.into(),
            self.scope.to_scope(core::Term::FunApp(
                Span::merge(&lhs_expr.span(), &rhs_expr.span()),
                self.scope.to_scope(fun_head),
                self.scope.to_scope(lhs_expr),
            )),
            self.scope.to_scope(rhs_expr),
        );

        // TODO: Maybe it would be good to reuse lhs_type here if body_type is the same
        (
            fun_app,
            Spanned::empty(Arc::new(Value::prim(body_type, []))),
        )
    }

    fn synth_reported_error(&mut self, range: ByteRange) -> (core::Term<'arena>, ArcValue<'arena>) {
        let expr = core::Term::Prim(range.into(), Prim::ReportedError);
        let r#type = self.push_unsolved_type(MetaSource::ReportedErrorType(range));
        (expr, r#type)
    }

    /// Check a series of format fields
    fn check_format_fields(
        &mut self,
        range: ByteRange,
        format_fields: &[FormatField<'_, ByteRange>],
    ) -> (&'arena [StringId], &'arena [core::Term<'arena>]) {
        let universe = self.universe.clone();
        let format_type = self.format_type.clone();

        let initial_local_len = self.local_env.len();
        let (labels, format_fields) =
            self.report_duplicate_labels(range, format_fields, |f| match f {
                FormatField::Format { label, .. } | FormatField::Computed { label, .. } => *label,
            });
        let mut formats = SliceVec::new(self.scope, labels.len());

        for format_field in format_fields {
            match format_field {
                FormatField::Format {
                    label: (label_range, label),
                    format,
                    pred,
                } => {
                    let format = self.check(format, &format_type);
                    let format_value = self.eval_env().eval(&format);
                    let r#type = self.elim_env().format_repr(&format_value);

                    self.local_env.push_param(Some(*label), r#type);

                    match pred {
                        None => formats.push(format),
                        // Elaborate refined fields to conditional formats
                        Some(pred) => {
                            // Note: No need to push a param, as this was done above,
                            // in preparation for checking the the next format field.
                            let cond_expr = self.check(pred, &self.bool_type.clone());

                            let field_span = Span::merge(&label_range.into(), &cond_expr.span());
                            formats.push(core::Term::FormatCond(
                                field_span,
                                *label,
                                self.scope.to_scope(format),
                                self.scope.to_scope(cond_expr),
                            ));
                        }
                    }
                }
                FormatField::Computed {
                    label: (label_range, label),
                    type_: r#type,
                    expr,
                } => {
                    let (expr, r#type, type_value) = match r#type {
                        Some(r#type) => {
                            let r#type = self.check(r#type, &universe);
                            let type_value = self.eval_env().eval(&r#type);
                            (self.check(expr, &type_value), r#type, type_value)
                        }
                        None => {
                            let (expr, type_value) = self.synth(expr);
                            let r#type = self.quote_env(self.scope).quote(&type_value);
                            (expr, r#type, type_value)
                        }
                    };

                    let field_span = Span::merge(&label_range.into(), &expr.span());
                    let format = core::Term::FunApp(
                        Span::merge(&label_range.into(), &expr.span()),
                        self.scope.to_scope(core::Term::FunApp(
                            field_span,
                            self.scope
                                .to_scope(core::Term::Prim(field_span, Prim::FormatSucceed)),
                            self.scope.to_scope(r#type),
                        )),
                        self.scope.to_scope(expr),
                    );

                    // Assume that `Repr ${type_value} ${expr} = ${type_value}`
                    self.local_env.push_param(Some(*label), type_value);
                    formats.push(format);
                }
            }
        }

        self.local_env.truncate(initial_local_len);

        (labels, formats.into())
    }

    /// Elaborate a match expression in checking mode
    fn check_match(
        &mut self,
        range: ByteRange,
        scrutinee_expr: &Term<'_, ByteRange>,
        equations: &[(Pattern<ByteRange>, Term<'_, ByteRange>)],
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let match_info = MatchInfo {
            range,
            scrutinee: self.synth_scrutinee(scrutinee_expr),
            expected_type: self.elim_env().force(expected_type),
        };

        self.elab_match(&match_info, true, equations.iter())
    }

    fn synth_scrutinee(&mut self, scrutinee_expr: &Term<'_, ByteRange>) -> Scrutinee<'arena> {
        let (expr, r#type) = self.synth(scrutinee_expr);
        Scrutinee {
            range: scrutinee_expr.range(),
            expr: self.scope.to_scope(expr),
            r#type,
        }
    }

    /// Elaborate a pattern match into a case tree in the core language.
    ///
    /// The implementation is based on the algorithm described in Section 5 of
    /// [“The Implementation of Functional Programming Languages”][impl-fpl].
    ///
    /// [impl-fpl]: https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/
    fn elab_match<'a>(
        &mut self,
        match_info: &MatchInfo<'arena>,
        is_reachable: bool,
        mut equations: impl Iterator<Item = &'a (Pattern<ByteRange>, Term<'a, ByteRange>)>,
    ) -> core::Term<'arena> {
        match equations.next() {
            Some((pattern, body_expr)) => {
                match self.check_pattern(pattern, &match_info.scrutinee.r#type) {
                    // Named patterns are elaborated to let bindings, where the
                    // scrutinee is bound as a definition in the body expression.
                    // Subsequent patterns are unreachable.
                    CheckedPattern::Binder(range, name) => {
                        self.check_match_reachable(is_reachable, range);

                        let def_name = Some(name);
                        let def_expr = self.eval_env().eval(match_info.scrutinee.expr);
                        let def_type_value = match_info.scrutinee.r#type.clone();
                        let def_type = self.quote_env(self.scope).quote(&def_type_value);

                        self.local_env.push_def(def_name, def_expr, def_type_value);
                        let body_expr = self.check(body_expr, &match_info.expected_type);
                        self.local_env.pop();

                        self.elab_match_unreachable(match_info, equations);

                        core::Term::Let(
                            Span::merge(&range.into(), &body_expr.span()),
                            def_name,
                            self.scope.to_scope(def_type),
                            match_info.scrutinee.expr,
                            self.scope.to_scope(body_expr),
                        )
                    }
                    // Placeholder patterns just elaborate to the body
                    // expression. Subsequent patterns are unreachable.
                    CheckedPattern::Placeholder(range) => {
                        self.check_match_reachable(is_reachable, range);

                        let body_expr = self.check(body_expr, &match_info.expected_type);
                        self.elab_match_unreachable(match_info, equations);

                        body_expr
                    }
                    // If we see a constant pattern we should expect a run of
                    // constants, elaborating to a constant elimination.
                    CheckedPattern::ConstLit(range, r#const) => {
                        self.check_match_reachable(is_reachable, range);

                        let body_expr = self.check(body_expr, &match_info.expected_type);
                        let const_equation = (range, r#const, body_expr);

                        self.elab_match_const(match_info, is_reachable, const_equation, equations)
                    }
                    // If we hit an error, propagate it, while still checking
                    // the body expression and the subsequent branches.
                    CheckedPattern::ReportedError(range) => {
                        self.check(body_expr, &match_info.expected_type);
                        self.elab_match_unreachable(match_info, equations);
                        core::Term::Prim(range.into(), Prim::ReportedError)
                    }
                }
            }
            None => self.elab_match_absurd(is_reachable, match_info),
        }
    }

    /// Ensure that this part of a match expression is reachable, reporting
    /// a message if it is not.
    fn check_match_reachable(&mut self, is_reachable: bool, range: ByteRange) {
        if !is_reachable {
            self.push_message(Message::UnreachablePattern { range });
        }
    }

    /// Elaborate the equations, expecting a series of constant patterns
    fn elab_match_const<'a>(
        &mut self,
        match_info: &MatchInfo<'arena>,
        is_reachable: bool,
        (const_range, r#const, body_expr): (ByteRange, Const, core::Term<'arena>),
        mut equations: impl Iterator<Item = &'a (Pattern<ByteRange>, Term<'a, ByteRange>)>,
    ) -> core::Term<'arena> {
        // The full range of this series of patterns
        let mut full_span = Span::merge(&const_range.into(), &body_expr.span());
        // Temporary vector for accumulating branches
        let mut branches = vec![(r#const, body_expr)];

        // Elaborate a run of constant patterns.
        'patterns: while let Some((pattern, body_expr)) = equations.next() {
            // Update the range up to the end of the next body expression
            full_span = Span::merge(&full_span, &body_expr.range().into());

            // Default expression, defined if we arrive at a default case
            let default_expr;

            match self.check_pattern(pattern, &match_info.scrutinee.r#type) {
                // Accumulate constant pattern. Search for it in the accumulated
                // branches and insert it in order.
                CheckedPattern::ConstLit(range, r#const) => {
                    let body_expr = self.check(body_expr, &match_info.expected_type);

                    // Find insertion index of the branch
                    let insertion_index = branches.binary_search_by(|(probe_const, _)| {
                        Const::partial_cmp(probe_const, &r#const)
                            .expect("attempt to compare non-ordered value")
                    });

                    match insertion_index {
                        Ok(_) => self.push_message(Message::UnreachablePattern { range }),
                        Err(index) => {
                            // This has not yet been covered, so it should be reachable.
                            self.check_match_reachable(is_reachable, range);
                            branches.insert(index, (r#const, body_expr));
                        }
                    }

                    // No default case yet, continue looking for constant patterns.
                    continue 'patterns;
                }

                // Time to elaborate the default pattern. The default case of
                // `core::Term::ConstMatch` binds a variable, so both
                // the named and  placeholder patterns should bind this.
                CheckedPattern::Binder(range, name) => {
                    self.check_match_reachable(is_reachable, range);

                    (self.local_env).push_param(Some(name), match_info.scrutinee.r#type.clone());
                    default_expr = self.check(body_expr, &match_info.expected_type);
                    self.local_env.pop();
                }
                CheckedPattern::Placeholder(range) => {
                    self.check_match_reachable(is_reachable, range);

                    (self.local_env).push_param(None, match_info.scrutinee.r#type.clone());
                    default_expr = self.check(body_expr, &match_info.expected_type);
                    self.local_env.pop();
                }
                CheckedPattern::ReportedError(range) => {
                    self.check(body_expr, &match_info.expected_type);
                    default_expr = core::Term::Prim(range.into(), Prim::ReportedError);
                }
            };

            // A default pattern was found, check any unreachable patterns.
            self.elab_match_unreachable(match_info, equations);

            return core::Term::ConstMatch(
                full_span,
                match_info.scrutinee.expr,
                self.scope.to_scope_from_iter(branches.into_iter()),
                Some(self.scope.to_scope(default_expr)),
            );
        }

        // Finished all the constant patterns without encountering a default
        // case. This should have been an exhaustive match, so check to see if
        // all the cases were covered.
        let default_expr = match match_info.scrutinee.r#type.match_prim_spine() {
            // No need for a default case if all the values were covered
            Some((Prim::BoolType, [])) if branches.len() >= 2 => None,
            _ => Some(self.elab_match_absurd(is_reachable, match_info)),
        };

        core::Term::ConstMatch(
            full_span,
            match_info.scrutinee.expr,
            self.scope.to_scope_from_iter(branches.into_iter()),
            default_expr.map(|expr| self.scope.to_scope(expr) as &_),
        )
    }

    /// Elaborate unreachable match cases. This is useful for that these cases
    /// are correctly typed, even if they are never actually needed.
    fn elab_match_unreachable<'a>(
        &mut self,
        match_info: &MatchInfo<'arena>,
        equations: impl Iterator<Item = &'a (Pattern<ByteRange>, Term<'a, ByteRange>)>,
    ) {
        self.elab_match(match_info, false, equations);
    }

    /// All the equations have been consumed.
    fn elab_match_absurd(
        &mut self,
        is_reachable: bool,
        match_info: &MatchInfo<'arena>,
    ) -> core::Term<'arena> {
        // Report if we can still reach this point
        if is_reachable {
            // TODO: this should be admitted if the scrutinee type is uninhabited
            self.push_message(Message::NonExhaustiveMatchExpr {
                match_expr_range: match_info.range,
                scrutinee_expr_range: match_info.scrutinee.range,
            });
        }
        core::Term::Prim(match_info.range.into(), Prim::ReportedError)
    }
}

trait FromStrRadix: Sized {
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, std::num::ParseIntError>;
}

macro_rules! impl_from_str_radix {
    ($t:ty) => {
        impl FromStrRadix for $t {
            fn from_str_radix(src: &str, radix: u32) -> Result<Self, std::num::ParseIntError> {
                // calls base implementation, not trait function
                Self::from_str_radix(src, radix)
            }
        }
    };
}

impl_from_str_radix!(u8);
impl_from_str_radix!(u16);
impl_from_str_radix!(u32);
impl_from_str_radix!(u64);

/// Simple patterns that have had some initial elaboration performed on them
#[derive(Debug)]
enum CheckedPattern {
    /// Pattern that binds local variable
    Binder(ByteRange, StringId),
    /// Placeholder patterns that match everything
    Placeholder(ByteRange),
    /// Constant literals
    ConstLit(ByteRange, Const),
    /// Error sentinel
    ReportedError(ByteRange),
}

/// Scrutinee of a match expression
struct Scrutinee<'arena> {
    range: ByteRange,
    expr: &'arena core::Term<'arena>,
    r#type: ArcValue<'arena>,
}

struct MatchInfo<'arena> {
    /// The full range of the match expression
    range: ByteRange,
    /// The expression being matched on
    scrutinee: Scrutinee<'arena>,
    /// The expected type of the match arms
    expected_type: ArcValue<'arena>,
}
