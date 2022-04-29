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
use crate::core::semantics::{self, ArcValue, Closure, Head, Telescope, Value};
use crate::core::{self, binary, Const, Prim, UIntStyle};
use crate::env::{self, EnvLen, GlobalVar, SharedEnv, UniqueEnv};
use crate::source::ByteRange;
use crate::surface::elaboration::reporting::Message;
use crate::surface::{distillation, Pattern, Term};
use crate::{StringId, StringInterner};

mod reporting;
mod unification;

/// Rigid environment.
///
/// This is used for keeping track of [rigid variables] that are bound by the
/// program, for example by function parameters, let bindings, or pattern
/// matching.
///
/// This environment behaves as a stack. As scopes are entered, it is important
/// to remember to call either [`RigidEnv::push_def`] or [`RigidEnv::push_param`].
/// On scope exit, it is important to remember to call [`RigidEnv::pop`].
/// Multiple bindings can be removed at once with [`RigidEnv::truncate`].
///
/// [rigid variables]: core::Term::RigidVar
pub struct RigidEnv<'arena> {
    /// Names of rigid variables.
    names: UniqueEnv<Option<StringId>>,
    /// Types of rigid variables.
    types: UniqueEnv<ArcValue<'arena>>,
    /// Information about the binders. Used when inserting new flexible
    /// variables during [evaluation][semantics::EvalContext::eval].
    infos: UniqueEnv<core::EntryInfo>,
    /// Expressions that will be substituted for rigid variables during
    /// [evaluation][semantics::EvalContext::eval].
    exprs: SharedEnv<ArcValue<'arena>>,
}

impl<'arena> RigidEnv<'arena> {
    /// Construct a new, empty environment.
    pub fn new() -> RigidEnv<'arena> {
        RigidEnv {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
            infos: UniqueEnv::new(),
            exprs: SharedEnv::new(),
        }
    }

    pub fn default(
        interner: &RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    ) -> RigidEnv<'arena> {
        use crate::core::Prim::*;
        use crate::core::Term;

        const VAR0: core::Term<'_> = core::Term::RigidVar(env::LocalVar::last());
        const UNIVERSE: core::Term<'_> = core::Term::Universe;
        const FORMAT_TYPE: core::Term<'_> = core::Term::Prim(Prim::FormatType);
        const BOOL_TYPE: core::Term<'_> = core::Term::Prim(Prim::BoolType);
        const U8_TYPE: core::Term<'_> = core::Term::Prim(Prim::U8Type);
        const U16_TYPE: core::Term<'_> = core::Term::Prim(Prim::U16Type);
        const U32_TYPE: core::Term<'_> = core::Term::Prim(Prim::U32Type);
        const U64_TYPE: core::Term<'_> = core::Term::Prim(Prim::U64Type);
        const S8_TYPE: core::Term<'_> = core::Term::Prim(Prim::S8Type);
        const S16_TYPE: core::Term<'_> = core::Term::Prim(Prim::S16Type);
        const S32_TYPE: core::Term<'_> = core::Term::Prim(Prim::S32Type);
        const S64_TYPE: core::Term<'_> = core::Term::Prim(Prim::S64Type);
        const POS_TYPE: core::Term<'_> = core::Term::Prim(Prim::PosType);

        let mut env = RigidEnvBuilder::new(interner, scope);

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

        env.define_prim_fun(Array8Type, [&U8_TYPE, &UNIVERSE], &UNIVERSE);
        env.define_prim_fun(Array16Type, [&U16_TYPE, &UNIVERSE], &UNIVERSE);
        env.define_prim_fun(Array32Type, [&U32_TYPE, &UNIVERSE], &UNIVERSE);
        env.define_prim_fun(Array64Type, [&U64_TYPE, &UNIVERSE], &UNIVERSE);

        env.define_prim(PosType, &UNIVERSE);
        env.define_prim(RefType, &core::Term::FunType(None, &FORMAT_TYPE, &UNIVERSE));

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
        env.define_prim_fun(FormatLink, [&POS_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim(
            FormatDeref,
            &core::Term::FunType(
                env.name("Elem"),
                &FORMAT_TYPE,
                &Term::FunType(
                    None,
                    &Term::FunElim(&Term::Prim(RefType), &VAR0),
                    &FORMAT_TYPE,
                ),
            ),
        );
        env.define_prim(FormatStreamPos, &FORMAT_TYPE);
        env.define_prim(
            FormatSucceed,
            &core::Term::FunType(
                env.name("A"),
                &UNIVERSE,
                &Term::FunType(None, &VAR0, &FORMAT_TYPE),
            ),
        );
        env.define_prim(FormatFail, &FORMAT_TYPE);
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

        env.define_prim_fun(PosAddU8, [&POS_TYPE, &U8_TYPE], &POS_TYPE);
        env.define_prim_fun(PosAddU16, [&POS_TYPE, &U16_TYPE], &POS_TYPE);
        env.define_prim_fun(PosAddU32, [&POS_TYPE, &U32_TYPE], &POS_TYPE);
        env.define_prim_fun(PosAddU64, [&POS_TYPE, &U64_TYPE], &POS_TYPE);

        env.build()
    }

    /// Get the length of the rigid environment.
    fn len(&self) -> EnvLen {
        self.names.len()
    }

    /// Push a rigid definition onto the context.
    fn push_def(
        &mut self,
        name: Option<StringId>,
        expr: ArcValue<'arena>,
        r#type: ArcValue<'arena>,
    ) {
        self.names.push(name);
        self.types.push(r#type);
        self.infos.push(core::EntryInfo::Definition);
        self.exprs.push(expr);
    }

    /// Push a rigid parameter onto the context.
    fn push_param(&mut self, name: Option<StringId>, r#type: ArcValue<'arena>) -> ArcValue<'arena> {
        // An expression that refers to itself once it is pushed onto the rigid
        // expression environment.
        let expr = Arc::new(Value::rigid_var(self.exprs.len().next_global()));

        self.names.push(name);
        self.types.push(r#type);
        self.infos.push(core::EntryInfo::Parameter);
        self.exprs.push(expr.clone());

        expr
    }

    /// Pop a rigid binder off the context.
    fn pop(&mut self) {
        self.names.pop();
        self.types.pop();
        self.infos.pop();
        self.exprs.pop();
    }

    /// Truncate the rigid environment.
    fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.types.truncate(len);
        self.infos.truncate(len);
        self.exprs.truncate(len);
    }
}

pub struct RigidEnvBuilder<'i, 'arena> {
    env: RigidEnv<'arena>,
    interner: &'i RefCell<StringInterner>,
    scope: &'arena Scope<'arena>,
}

impl<'i, 'arena> RigidEnvBuilder<'i, 'arena> {
    fn new(
        interner: &'i RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    ) -> RigidEnvBuilder<'i, 'arena> {
        let env = RigidEnv::new();
        RigidEnvBuilder {
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
        let flexible_exprs = UniqueEnv::new();
        let r#type =
            semantics::EvalContext::new(&mut SharedEnv::new(), &flexible_exprs).eval(r#type);
        self.env
            .push_def(name, Arc::new(Value::prim(prim, [])), r#type);
    }

    fn define_prim_fun<const ARITY: usize>(
        &mut self,
        prim: Prim,
        input_tys: [&'arena core::Term<'arena>; ARITY],
        output_ty: &'arena core::Term<'arena>,
    ) {
        self.define_prim(
            prim,
            (input_tys.iter().rev()).fold(output_ty, |output_ty, input_ty| {
                self.scope
                    .to_scope(core::Term::FunType(None, input_ty, output_ty))
            }),
        );
    }

    fn build(self) -> RigidEnv<'arena> {
        self.env
    }
}

/// The reason why a flexible variable was inserted.
#[derive(Debug, Copy, Clone)]
pub enum FlexSource {
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
    /// The output type of a match expression
    MatchOutputType(ByteRange),
    /// The input type of a function.
    FunInputType(ByteRange),
    /// The output type of a function.
    FunOutputType(ByteRange),
    /// The type of a reported error.
    ReportedErrorType(ByteRange),
}

/// Flexible environment.
///
/// This is used for keeping track of the state of [flexible variables] whose
/// definitions are intended to be found through the use of [unification].
///
/// [flexible variables]: core::Term::FlexibleVar
pub struct FlexibleEnv<'arena> {
    /// The source of inserted flexible variables, used when reporting [unsolved
    /// flexible variables][Message::UnsolvedFlexibleVar].
    sources: UniqueEnv<FlexSource>,
    /// Types of flexible variables.
    types: UniqueEnv</* TODO: lazy value */ ArcValue<'arena>>,
    /// Expressions that will be substituted for flexible variables during
    /// [evaluation][semantics::EvalContext::eval].
    ///
    /// These will be set to [`None`] when a flexible variable is first
    /// [inserted][Context::push_flexible_term], then will be set to [`Some`]
    /// if a solution is found during [`unification`].
    exprs: UniqueEnv<Option<ArcValue<'arena>>>,
}

impl<'arena> FlexibleEnv<'arena> {
    /// Construct a new, empty environment.
    pub fn new() -> FlexibleEnv<'arena> {
        FlexibleEnv {
            sources: UniqueEnv::new(),
            types: UniqueEnv::new(),
            exprs: UniqueEnv::new(),
        }
    }

    /// Push an unsolved flexible binder onto the context.
    fn push(&mut self, source: FlexSource, r#type: ArcValue<'arena>) -> GlobalVar {
        // TODO: check that hole name is not already in use
        let var = self.exprs.len().next_global();

        self.sources.push(source);
        self.types.push(r#type);
        self.exprs.push(None);

        var
    }

    /// Report on any unsolved flexible variables, or solved named holes.
    fn report<'this>(&'this self) -> impl 'this + Iterator<Item = Message> {
        Iterator::zip(self.sources.iter(), self.exprs.iter()).filter_map(|(&source, expr)| {
            match (expr, source) {
                // Avoid producing messages for some unsolved flexible sources:
                (None, FlexSource::HoleType(_, _)) => None, // should have an unsolved hole expression
                (None, FlexSource::PlaceholderType(_)) => None, // should have an unsolved placeholder expression
                (None, FlexSource::ReportedErrorType(_)) => None, // should already have an error reported
                // For other sources, report an unsolved problem message
                (None, source) => Some(Message::UnsolvedFlexibleVar { source }),
                // Yield messages of solved named holes
                (Some(_), FlexSource::HoleExpr(range, name)) => {
                    Some(Message::HoleSolution { range, name })
                }
                // Ignore solutions of anything else
                (Some(_), _) => None,
            }
        })
    }
}

#[derive(Debug)]
enum CheckedPattern {
    Name(ByteRange, StringId),
    Placeholder(ByteRange),
    Const(ByteRange, Const),
    ReportedError(ByteRange),
}

/// Elaboration context.
pub struct Context<'interner, 'arena> {
    /// Global string interner.
    interner: &'interner RefCell<StringInterner>,
    /// Scoped arena for storing elaborated terms.
    //
    // TODO: Make this local to the elaboration context, and reallocate
    //       elaborated terms to an external `Scope` during zonking, resetting
    //       this scope on completion.
    scope: &'arena Scope<'arena>,
    /// Rigid environment.
    rigid_env: RigidEnv<'arena>,
    /// Flexible environment.
    flexible_env: FlexibleEnv<'arena>,
    /// A partial renaming to be used during [`unification`].
    renaming: unification::PartialRenaming,
    /// Diagnostic messages encountered during elaboration.
    messages: Vec<Message>,
}

impl<'interner, 'arena> Context<'interner, 'arena> {
    /// Construct a new elaboration context, backed by the supplied arena.
    pub fn new(
        interner: &'interner RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    ) -> Context<'interner, 'arena> {
        Context {
            interner,
            scope,
            rigid_env: RigidEnv::default(interner, scope),
            flexible_env: FlexibleEnv::new(),
            renaming: unification::PartialRenaming::new(),
            messages: Vec::new(),
        }
    }

    /// Lookup a name in the context.
    fn get_name(&self, name: StringId) -> Option<(env::LocalVar, &ArcValue<'arena>)> {
        let rigid_types = Iterator::zip(env::local_vars(), self.rigid_env.types.iter().rev());

        Iterator::zip(self.rigid_env.names.iter().copied().rev(), rigid_types)
            .find_map(|(n, (var, r#type))| (Some(name) == n).then(|| (var, r#type)))
    }

    /// Push an unsolved flexible binder onto the context.
    fn push_flexible_term(
        &mut self,
        source: FlexSource,
        r#type: ArcValue<'arena>,
    ) -> core::Term<'arena> {
        let rigid_infos = (self.scope).to_scope_from_iter(self.rigid_env.infos.iter().copied());
        core::Term::FlexibleInsertion(self.flexible_env.push(source, r#type), rigid_infos)
    }

    /// Push an unsolved flexible binder onto the context.
    fn push_flexible_value(
        &mut self,
        source: FlexSource,
        r#type: ArcValue<'arena>,
    ) -> ArcValue<'arena> {
        let term = self.push_flexible_term(source, r#type); // TODO: Could avoid allocating the rigid infos
        self.eval_context().eval(&term)
    }

    fn push_message(&mut self, message: impl Into<Message>) {
        self.messages.push(message.into());
    }

    pub fn drain_messages<'this>(&'this mut self) -> impl 'this + Iterator<Item = Message> {
        self.messages.drain(..).chain(self.flexible_env.report())
    }

    pub fn eval_context(&mut self) -> semantics::EvalContext<'arena, '_> {
        semantics::EvalContext::new(&mut self.rigid_env.exprs, &self.flexible_env.exprs)
    }

    pub fn elim_context(&self) -> semantics::ElimContext<'arena, '_> {
        semantics::ElimContext::new(&self.flexible_env.exprs)
    }

    pub fn quote_context<'out_arena>(
        &self,
        scope: &'out_arena Scope<'out_arena>,
    ) -> semantics::QuoteContext<'arena, 'out_arena, '_> {
        semantics::QuoteContext::new(scope, self.rigid_env.len(), &self.flexible_env.exprs)
    }

    fn unification_context(&mut self) -> unification::Context<'arena, '_> {
        unification::Context::new(
            &self.scope,
            &mut self.renaming,
            self.rigid_env.len(),
            &mut self.flexible_env.exprs,
        )
    }

    pub fn distillation_context<'out_arena>(
        &mut self,
        scope: &'out_arena Scope<'out_arena>,
    ) -> distillation::Context<'interner, 'out_arena, '_> {
        distillation::Context::new(
            self.interner,
            scope,
            &mut self.rigid_env.names,
            &self.flexible_env.sources,
        )
    }

    pub fn binary_context(&self) -> binary::Context<'arena, '_> {
        binary::Context::new(&self.flexible_env.exprs)
    }

    /// Reports an error if there are duplicate fields found, returning a slice
    /// of the labels unique labels and an iterator over the unique fields.
    fn report_duplicate_labels<'fields, 'a>(
        &mut self,
        range: ByteRange,
        fields: &'fields [((ByteRange, StringId), Term<'a, ByteRange>)],
    ) -> (
        &'arena [StringId],
        impl Iterator<Item = &'fields ((ByteRange, StringId), Term<'a, ByteRange>)>,
    ) {
        let mut labels = SliceVec::new(self.scope, fields.len());
        // Will only allocate when duplicates are encountered
        let mut duplicate_indices = Vec::new();
        let mut duplicate_labels = Vec::new();

        for (index, ((range, label), _)) in fields.iter().enumerate() {
            if labels.contains(label) {
                duplicate_indices.push(index);
                duplicate_labels.push((*range, *label));
            } else {
                labels.push(*label)
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
                    invalid_range: ByteRange::new(ch_start, ch_end),
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
        let (s, radix, style) = if s.starts_with("0x") {
            (&s[2..], 16, UIntStyle::Hexadecimal)
        } else if s.starts_with("0b") {
            (&s[2..], 2, UIntStyle::Binary)
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
        range: ByteRange, // NOTE: could be removed if source info is added to `core::Term`
        expr: core::Term<'arena>,
        type0: &ArcValue<'arena>,
        type1: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        match self.unification_context().unify(type0, type1) {
            Ok(()) => expr,
            Err(error) => {
                self.push_message(Message::FailedToUnify { range, error });
                core::Term::Prim(Prim::ReportedError)
            }
        }
    }

    /// Check that a pattern matches an expected type.
    fn check_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
        expected_type: &ArcValue<'arena>,
    ) -> (CheckedPattern, ArcValue<'arena>) {
        match pattern {
            Pattern::Name(range, name) => {
                (CheckedPattern::Name(*range, *name), expected_type.clone())
            }
            Pattern::Placeholder(range) => {
                (CheckedPattern::Placeholder(*range), expected_type.clone())
            }
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
                        self.push_message(Message::StringLiteralNotSupported { range: *range });
                        None
                    }
                };

                match constant {
                    Some(constant) => (
                        CheckedPattern::Const(*range, constant),
                        expected_type.clone(),
                    ),
                    None => {
                        let source = FlexSource::ReportedErrorType(*range);
                        let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));

                        (CheckedPattern::ReportedError(*range), r#type)
                    }
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
                        self.push_message(Message::NumericLiteralNotSupported { range: *range });
                        None
                    }
                };

                match constant {
                    Some(constant) => (
                        CheckedPattern::Const(*range, constant),
                        expected_type.clone(),
                    ),
                    None => {
                        let source = FlexSource::ReportedErrorType(*range);
                        let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));

                        (CheckedPattern::ReportedError(*range), r#type)
                    }
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
                    Some(constant) => (
                        CheckedPattern::Const(*range, constant),
                        expected_type.clone(),
                    ),
                    None => {
                        let source = FlexSource::ReportedErrorType(*range);
                        let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));

                        (CheckedPattern::ReportedError(*range), r#type)
                    }
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
                let source = FlexSource::NamedPatternType(*range, *name);
                let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));
                (CheckedPattern::Name(*range, *name), r#type)
            }
            Pattern::Placeholder(range) => {
                let source = FlexSource::PlaceholderPatternType(*range);
                let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));
                (CheckedPattern::Placeholder(*range), r#type)
            }
            Pattern::StringLiteral(range, _) => {
                self.push_message(Message::AmbiguousStringLiteral { range: *range });
                let source = FlexSource::ReportedErrorType(*range);
                let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));
                (CheckedPattern::ReportedError(*range), r#type)
            }
            Pattern::NumberLiteral(range, _) => {
                self.push_message(Message::AmbiguousNumericLiteral { range: *range });
                let source = FlexSource::ReportedErrorType(*range);
                let r#type = self.push_flexible_value(source, Arc::new(Value::Universe));
                (CheckedPattern::ReportedError(*range), r#type)
            }
            Pattern::BooleanLiteral(range, val) => {
                let r#const = Const::Bool(*val);
                let r#type = Arc::new(Value::prim(Prim::BoolType, []));
                (CheckedPattern::Const(*range, r#const), r#type)
            }
        }
    }

    /// Check that the type of an annotated pattern matches an expected type.
    fn check_ann_pattern(
        &mut self,
        pattern: &Pattern<ByteRange>,
        r#type: Option<&Term<'_, ByteRange>>,
        expected_type: &ArcValue<'arena>,
    ) -> (CheckedPattern, ArcValue<'arena>) {
        match r#type {
            None => self.check_pattern(pattern, &expected_type),
            Some(r#type) => {
                let universe = Arc::new(Value::Universe);
                let range = r#type.range();
                let r#type = self.check(r#type, &universe);
                let r#type = self.eval_context().eval(&r#type);

                match self.unification_context().unify(&r#type, expected_type) {
                    Ok(()) => self.check_pattern(pattern, &r#type),
                    Err(error) => {
                        self.push_message(Message::FailedToUnify { range, error });

                        let source = FlexSource::ReportedErrorType(range);
                        let input_type = self.push_flexible_value(source, universe);

                        (CheckedPattern::ReportedError(range), input_type)
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
                let r#type = self.check(r#type, &Arc::new(Value::Universe));
                let type_value = self.eval_context().eval(&r#type);
                self.check_pattern(pattern, &type_value)
            }
        }
    }

    /// Push a rigid definition onto the context.
    /// The supplied `pattern` is expected to be irrefutable.
    fn push_rigid_def(
        &mut self,
        pattern: CheckedPattern,
        expr: ArcValue<'arena>,
        r#type: ArcValue<'arena>,
    ) -> Option<StringId> {
        let name = match pattern {
            CheckedPattern::Name(_, name) => Some(name),
            CheckedPattern::Placeholder(_) => None,
            // FIXME: generate failing output expressions?
            CheckedPattern::Const(range, _) => {
                self.push_message(Message::RefutablePattern {
                    pattern_range: range,
                });
                None
            }
            CheckedPattern::ReportedError(_) => None,
        };

        self.rigid_env.push_def(name, expr, r#type);

        name
    }

    /// Push a rigid parameter onto the context.
    /// The supplied `pattern` is expected to be irrefutable.
    fn push_rigid_param(
        &mut self,
        pattern: CheckedPattern,
        r#type: ArcValue<'arena>,
    ) -> (Option<StringId>, ArcValue<'arena>) {
        let name = match pattern {
            CheckedPattern::Name(_, name) => Some(name),
            CheckedPattern::Placeholder(_) => None,
            // FIXME: generate failing output expressions?
            CheckedPattern::Const(range, _) => {
                self.push_message(Message::RefutablePattern {
                    pattern_range: range,
                });
                None
            }
            CheckedPattern::ReportedError(_) => None,
        };

        let expr = self.rigid_env.push_param(name, r#type);

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
        let expected_type = self.elim_context().force(expected_type);

        match (surface_term, expected_type.as_ref()) {
            (Term::Let(_, def_pattern, def_type, def_expr, output_expr), _) => {
                let (def_pattern, def_type_value) = self.synth_ann_pattern(def_pattern, *def_type);
                let def_type = self.quote_context(self.scope).quote(&def_type_value); // FIXME: avoid requote if possible?
                let def_expr = self.check(def_expr, &def_type_value);
                let def_expr_value = self.eval_context().eval(&def_expr);

                let def_name = self.push_rigid_def(def_pattern, def_expr_value, def_type_value); // TODO: split on constants
                let output_expr = self.check(output_expr, &expected_type);
                self.rigid_env.pop();

                core::Term::Let(
                    def_name,
                    self.scope.to_scope(def_type),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(output_expr),
                )
            }
            (Term::Match(range, scrutinee_expr, equations), _) => {
                let scrutinee_range = scrutinee_expr.range();
                let (scrutinee_expr, scrutinee_type) = self.synth(scrutinee_expr);

                self.check_match(
                    true,
                    *range,
                    scrutinee_range,
                    self.scope.to_scope(scrutinee_expr),
                    &scrutinee_type,
                    equations,
                    &expected_type,
                )
            }
            (
                Term::FunLiteral(_, input_pattern, input_type, output_expr),
                Value::FunType(_, expected_input_type, output_type),
            ) => {
                let (input_name, input_type) =
                    self.check_ann_pattern(input_pattern, *input_type, expected_input_type);
                let (input_name, input_expr) = self.push_rigid_param(input_name, input_type);
                let output_type = self.elim_context().apply_closure(output_type, input_expr);
                let output_expr = self.check(output_expr, &output_type);

                self.rigid_env.pop();

                core::Term::FunIntro(input_name, self.scope.to_scope(output_expr))
            }
            (Term::RecordLiteral(range, expr_fields), Value::RecordType(labels, types)) => {
                // TODO: improve handling of duplicate labels
                if expr_fields.len() != labels.len()
                    || Iterator::zip(expr_fields.iter(), labels.iter())
                        .any(|(((_, expr_label), _), type_label)| expr_label != type_label)
                {
                    self.push_message(Message::MismatchedFieldLabels {
                        range: *range,
                        expr_labels: (expr_fields.iter())
                            .map(|(ranged_label, _)| *ranged_label)
                            .collect(),
                        type_labels: labels.iter().copied().collect(),
                    });
                    return core::Term::Prim(Prim::ReportedError);
                }

                let mut types = types.clone();
                let mut expr_fields = expr_fields.iter();
                let mut exprs = SliceVec::new(self.scope, types.len());

                while let Some(((_, expr), (r#type, next_types))) = Option::zip(
                    expr_fields.next(),
                    self.elim_context().split_telescope(types),
                ) {
                    let expr = self.check(expr, &r#type);
                    types = next_types(self.eval_context().eval(&expr));
                    exprs.push(expr);
                }

                core::Term::RecordIntro(labels, exprs.into())
            }
            (Term::UnitLiteral(_), Value::Universe) => core::Term::RecordType(&[], &[]),
            (Term::UnitLiteral(_), _)
                if matches!(
                    expected_type.match_prim_spine(),
                    Some((Prim::FormatType, [])),
                ) =>
            {
                core::Term::FormatRecord(&[], &[])
            }
            (Term::ArrayLiteral(range, elem_exprs), _) => {
                use crate::core::semantics::Elim::Fun;

                let (len, elem_type) = match expected_type.match_prim_spine() {
                    Some((Prim::Array8Type, [Fun(len), Fun(elem_type)])) => (len, elem_type),
                    Some((Prim::Array16Type, [Fun(len), Fun(elem_type)])) => (len, elem_type),
                    Some((Prim::Array32Type, [Fun(len), Fun(elem_type)])) => (len, elem_type),
                    Some((Prim::Array64Type, [Fun(len), Fun(elem_type)])) => (len, elem_type),
                    Some((Prim::ReportedError, _)) => return core::Term::Prim(Prim::ReportedError),
                    _ => {
                        self.push_message(Message::ArrayLiteralNotSupported { range: *range });
                        return core::Term::Prim(Prim::ReportedError);
                    }
                };

                match len.as_ref() {
                    Value::Const(Const::U8(len, _)) if elem_exprs.len() as u64 == *len as u64 => {}
                    Value::Const(Const::U16(len, _)) if elem_exprs.len() as u64 == *len as u64 => {}
                    Value::Const(Const::U32(len, _)) if elem_exprs.len() as u64 == *len as u64 => {}
                    Value::Const(Const::U64(len, _)) if elem_exprs.len() as u64 == *len as u64 => {}
                    Value::Stuck(Head::Prim(Prim::ReportedError), _) => {
                        return core::Term::Prim(Prim::ReportedError);
                    }
                    _ => {
                        // Check the array elements anyway in order to report
                        // any errors inside the literal as well.
                        for elem_expr in *elem_exprs {
                            self.check(elem_expr, elem_type);
                        }

                        self.push_message(Message::MismatchedArrayLength {
                            range: *range,
                            found_len: elem_exprs.len(),
                        });

                        return core::Term::Prim(Prim::ReportedError);
                    }
                }

                let elem_exprs = self.scope.to_scope_from_iter(
                    (elem_exprs.iter()).map(|elem_expr| self.check(elem_expr, elem_type)),
                );

                core::Term::ArrayIntro(elem_exprs)
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
                        self.push_message(Message::StringLiteralNotSupported { range: *range });
                        None
                    }
                };

                match constant {
                    Some(constant) => core::Term::Const(constant),
                    None => core::Term::Prim(Prim::ReportedError),
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
                        self.push_message(Message::NumericLiteralNotSupported { range: *range });
                        return core::Term::Prim(Prim::ReportedError);
                    }
                };

                match constant {
                    Some(constant) => core::Term::Const(constant),
                    None => core::Term::Prim(Prim::ReportedError),
                }
            }
            (Term::ReportedError(_), _) => core::Term::Prim(Prim::ReportedError),
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
            Term::Name(range, name) => match self.get_name(*name) {
                Some((term, r#type)) => (core::Term::RigidVar(term), r#type.clone()),
                None => {
                    self.push_message(Message::UnboundName {
                        range: *range,
                        name: *name,
                    });
                    self.synth_reported_error(*range)
                }
            },
            Term::Hole(range, name) => {
                let type_source = FlexSource::HoleType(*range, *name);
                let expr_source = FlexSource::HoleExpr(*range, *name);

                let r#type = self.push_flexible_value(type_source, Arc::new(Value::Universe));
                let expr = self.push_flexible_term(expr_source, r#type.clone());

                (expr, r#type)
            }
            Term::Placeholder(range) => {
                let type_source = FlexSource::PlaceholderType(*range);
                let expr_source = FlexSource::PlaceholderExpr(*range);

                let r#type = self.push_flexible_value(type_source, Arc::new(Value::Universe));
                let expr = self.push_flexible_term(expr_source, r#type.clone());

                (expr, r#type)
            }
            Term::Ann(_, expr, r#type) => {
                let r#type = self.check(r#type, &Arc::new(Value::Universe)); // FIXME: avoid temporary Arc
                let type_value = self.eval_context().eval(&r#type);
                let expr = self.check(expr, &type_value);

                let ann_expr =
                    core::Term::Ann(self.scope.to_scope(expr), self.scope.to_scope(r#type));

                (ann_expr, type_value)
            }
            Term::Let(_, def_pattern, def_type, def_expr, output_expr) => {
                let (def_pattern, def_type_value) = self.synth_ann_pattern(def_pattern, *def_type);
                let def_type = self.quote_context(self.scope).quote(&def_type_value); // FIXME: avoid requote if possible?
                let def_expr = self.check(def_expr, &def_type_value);
                let def_expr_value = self.eval_context().eval(&def_expr);

                let def_name = self.push_rigid_def(def_pattern, def_expr_value, def_type_value);
                let (output_expr, output_type) = self.synth(output_expr);
                self.rigid_env.pop();

                let let_expr = core::Term::Let(
                    def_name,
                    self.scope.to_scope(def_type),
                    self.scope.to_scope(def_expr),
                    self.scope.to_scope(output_expr),
                );

                (let_expr, output_type)
            }
            Term::Match(range, scrutinee_expr, equations) => {
                let scrutinee_range = scrutinee_expr.range();
                let (scrutinee_expr, scrutinee_type) = self.synth(scrutinee_expr);

                // Create a single flexible variable representing the type of
                // the match expression's output expressions, allowing us to
                // unify them together.
                let source = FlexSource::MatchOutputType(*range);
                let universe = Arc::new(Value::Universe);
                let output_type = self.push_flexible_value(source, universe);

                let match_expr = self.check_match(
                    true,
                    *range,
                    scrutinee_range,
                    self.scope.to_scope(scrutinee_expr),
                    &scrutinee_type,
                    equations,
                    &output_type,
                );

                (match_expr, output_type)
            }
            Term::Universe(_) => (core::Term::Universe, Arc::new(Value::Universe)),
            Term::Arrow(_, input_type, output_type) => {
                let universe = Arc::new(Value::Universe); // FIXME: avoid temporary Arc
                let input_type = self.check(input_type, &universe);
                let input_type_value = self.eval_context().eval(&input_type);

                self.rigid_env.push_param(None, input_type_value);
                let output_type = self.check(output_type, &universe);
                self.rigid_env.pop();

                let fun_type = core::Term::FunType(
                    None,
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                );

                (fun_type, universe)
            }
            Term::FunType(_, input_pattern, input_type, output_type) => {
                let universe = Arc::new(Value::Universe); // FIXME: avoid temporary Arc
                let (input_pattern, input_type_value) =
                    self.synth_ann_pattern(input_pattern, *input_type);
                let input_type = self.quote_context(self.scope).quote(&input_type_value); // FIXME: avoid requote if possible?

                let (input_name, _) = self.push_rigid_param(input_pattern, input_type_value);
                let output_type = self.check(output_type, &universe);
                self.rigid_env.pop();

                let fun_type = core::Term::FunType(
                    input_name,
                    self.scope.to_scope(input_type),
                    self.scope.to_scope(output_type),
                );

                (fun_type, universe)
            }
            Term::FunLiteral(_, input_pattern, input_type, output_expr) => {
                let (input_pattern, input_type) =
                    self.synth_ann_pattern(input_pattern, *input_type);

                let (input_name, _) = self.push_rigid_param(input_pattern, input_type.clone());
                let (output_expr, output_type) = self.synth(output_expr);
                let output_type = self.quote_context(self.scope).quote(&output_type);
                self.rigid_env.pop();

                (
                    core::Term::FunIntro(input_name, self.scope.to_scope(output_expr)),
                    Arc::new(Value::FunType(
                        input_name,
                        input_type,
                        Closure::new(
                            self.rigid_env.exprs.clone(),
                            self.scope.to_scope(output_type),
                        ),
                    )),
                )
            }
            Term::FunElim(range, head_expr, input_expr) => {
                let head_range = head_expr.range();
                let (head_expr, head_type) = self.synth(head_expr);

                // Ensure that the head type is a function type
                let head_type = self.elim_context().force(&head_type);
                let (head_expr, input_type, output_type) = match head_type.as_ref() {
                    // The simple case - it's easy to see that it is a function type!
                    Value::FunType(_, input_type, output_type) => {
                        (head_expr, input_type.clone(), output_type.clone())
                    }
                    Value::Stuck(Head::Prim(Prim::ReportedError), _) => {
                        return self.synth_reported_error(*range);
                    }
                    // It's not immediately obvious that the head type is a
                    // function type, so instead we construct a function type
                    // with flexible variables standing-in for the input and
                    // output types, and then we attempt to unify the head type
                    // against it.
                    _ => {
                        let universe = Arc::new(Value::Universe);
                        // Create a flexible input type
                        let input_source = FlexSource::FunInputType(head_range);
                        let input_type = self.push_flexible_value(input_source, universe.clone());

                        // Create a flexible output type, with the input bound
                        self.rigid_env.push_param(None, input_type.clone());
                        let output_source = FlexSource::FunOutputType(head_range);
                        let output_type = self.push_flexible_term(output_source, universe);
                        self.rigid_env.pop();

                        // Create a function type between the flexible variables.
                        let output_type = Closure::new(
                            self.rigid_env.exprs.clone(),
                            self.scope.to_scope(output_type),
                        );
                        let fun_type = Arc::new(Value::FunType(
                            None,
                            input_type.clone(),
                            output_type.clone(),
                        ));

                        // Unify the type of the head expression with the function type
                        let head_expr = self.convert(head_range, head_expr, &head_type, &fun_type);

                        (head_expr, input_type, output_type)
                    }
                };

                // Check the input expression and apply it to the output type
                let input_expr = self.check(input_expr, &input_type);
                let input_expr_value = self.eval_context().eval(&input_expr);
                let output_type = self
                    .elim_context()
                    .apply_closure(&output_type, input_expr_value);

                // Construct the final elimination
                let fun_elim = core::Term::FunElim(
                    self.scope.to_scope(head_expr),
                    self.scope.to_scope(input_expr),
                );

                (fun_elim, output_type)
            }
            Term::RecordType(range, type_fields) => {
                let universe = Arc::new(Value::Universe);
                let initial_rigid_len = self.rigid_env.len();
                let (labels, type_fields) = self.report_duplicate_labels(*range, type_fields);
                let mut types = SliceVec::new(self.scope, labels.len());

                for ((_, label), r#type) in type_fields {
                    let r#type = self.check(r#type, &universe);
                    let type_value = self.eval_context().eval(&r#type);
                    self.rigid_env.push_param(Some(*label), type_value);
                    types.push(r#type);
                }

                self.rigid_env.truncate(initial_rigid_len);

                (core::Term::RecordType(labels, types.into()), universe)
            }
            Term::RecordLiteral(range, expr_fields) => {
                let (labels, expr_fields) = self.report_duplicate_labels(*range, expr_fields);
                let mut types = SliceVec::new(self.scope, labels.len());
                let mut exprs = SliceVec::new(self.scope, labels.len());

                for (_, expr) in expr_fields {
                    let (expr, r#type) = self.synth(expr);
                    types.push(self.quote_context(self.scope).quote(&r#type)); // NOTE: Unsure if these are correctly bound!
                    exprs.push(expr);
                }

                let types = Telescope::new(self.rigid_env.exprs.clone(), types.into());

                (
                    core::Term::RecordIntro(labels, exprs.into()),
                    Arc::new(Value::RecordType(labels, types)),
                )
            }
            Term::UnitLiteral(_) => (
                core::Term::RecordIntro(&[], &[]),
                Arc::new(Value::RecordType(
                    &[],
                    Telescope::new(SharedEnv::new(), &[]),
                )),
            ),
            Term::RecordElim(range, head_expr, (label_range, label)) => {
                let head_range = head_expr.range();
                let (head_expr, head_type) = self.synth(head_expr);
                let head_expr_value = self.eval_context().eval(&head_expr);

                let head_type = self.elim_context().force(&head_type);
                match head_type.as_ref() {
                    Value::RecordType(labels, types) => {
                        let mut labels = labels.iter();
                        let mut types = types.clone();

                        while let Some((type_label, (r#type, next_types))) =
                            Option::zip(labels.next(), self.elim_context().split_telescope(types))
                        {
                            if label == type_label {
                                let head_expr = self.scope.to_scope(head_expr);
                                let expr = core::Term::RecordElim(head_expr, *label);
                                return (expr, r#type);
                            } else {
                                let head_expr = head_expr_value.clone();
                                let expr = self.elim_context().apply_record(head_expr, *type_label);
                                types = next_types(expr);
                            }
                        }
                    }
                    Value::Stuck(Head::Prim(Prim::ReportedError), _) => {
                        return self.synth_reported_error(*range);
                    }
                    _ => {}
                }

                self.push_message(Message::UnknownField {
                    head_range,
                    label_range: *label_range,
                    label: *label,
                });
                self.synth_reported_error(*range)
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
            Term::BooleanLiteral(_range, val) => {
                let bool_type = Arc::new(Value::prim(Prim::BoolType, []));
                (core::Term::Const(Const::Bool(*val)), bool_type)
            }
            Term::FormatRecord(range, format_fields) => {
                let format_type = Arc::new(Value::prim(Prim::FormatType, []));
                let (labels, formats) = self.check_format_fields(*range, format_fields);

                (core::Term::FormatRecord(labels, formats), format_type)
            }
            Term::FormatOverlap(range, format_fields) => {
                let format_type = Arc::new(Value::prim(Prim::FormatType, []));
                let (labels, formats) = self.check_format_fields(*range, format_fields);

                (core::Term::FormatOverlap(labels, formats), format_type)
            }
            Term::ReportedError(range) => self.synth_reported_error(*range),
        }
    }

    fn synth_reported_error(&mut self, range: ByteRange) -> (core::Term<'arena>, ArcValue<'arena>) {
        let type_source = FlexSource::ReportedErrorType(range);
        let r#type = self.push_flexible_value(type_source, Arc::new(Value::Universe));
        (core::Term::Prim(Prim::ReportedError), r#type)
    }

    /// Check a series of format fields
    fn check_format_fields(
        &mut self,
        range: ByteRange,
        format_fields: &[((ByteRange, StringId), Term<'_, ByteRange>)],
    ) -> (&'arena [StringId], &'arena [core::Term<'arena>]) {
        let format_type = Arc::new(Value::prim(Prim::FormatType, []));
        let initial_rigid_len = self.rigid_env.len();
        let (labels, format_fields) = self.report_duplicate_labels(range, format_fields);
        let mut formats = SliceVec::new(self.scope, labels.len());

        for ((_, label), format) in format_fields {
            let format = self.check(format, &format_type);
            let format_value = self.eval_context().eval(&format);
            let r#type = self.elim_context().apply_repr(&format_value);
            self.rigid_env.push_param(Some(*label), r#type);
            formats.push(format);
        }

        self.rigid_env.truncate(initial_rigid_len);

        (labels, formats.into())
    }

    /// Elaborate a pattern match into a case tree in the core language.
    ///
    /// The implementation is based on the algorithm described in Section 5 of
    /// [“The Implementation of Functional Programming Languages”].
    ///
    /// [“The Implementation of Functional Programming Languages”]: https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/
    fn check_match(
        &mut self,
        is_reachable: bool,
        match_range: ByteRange,
        scrutinee_range: ByteRange,
        scrutinee_expr: &'arena core::Term<'arena>,
        scrutinee_type: &ArcValue<'arena>,
        mut equations: &[(Pattern<ByteRange>, Term<'_, ByteRange>)],
        expected_type: &ArcValue<'arena>,
    ) -> core::Term<'arena> {
        match equations.split_first() {
            Some(((pattern, output_expr), next_equations)) => {
                let (def_pattern, def_type_value) = self.check_pattern(pattern, &scrutinee_type);
                let def_type = self.quote_context(self.scope).quote(&def_type_value); // FIXME: avoid requote if possible?

                // Warn about unreachable patterns, only when checking the pattern was a success
                if !is_reachable && !matches!(def_pattern, CheckedPattern::ReportedError(_)) {
                    self.push_message(Message::UnreachablePattern {
                        range: pattern.range(),
                    });
                }

                match def_pattern {
                    CheckedPattern::Name(_, name) => {
                        let def_name = Some(name);
                        let def_expr = self.eval_context().eval(&scrutinee_expr);
                        self.rigid_env.push_def(def_name, def_expr, def_type_value);
                        let output_expr = self.check(output_expr, &expected_type);
                        self.rigid_env.pop();

                        // These patterns are unreachable, but check them anyway!
                        self.check_match(
                            false,
                            match_range,
                            scrutinee_range,
                            scrutinee_expr,
                            scrutinee_type,
                            next_equations,
                            expected_type,
                        );

                        core::Term::Let(
                            def_name,
                            self.scope.to_scope(def_type),
                            scrutinee_expr,
                            self.scope.to_scope(output_expr),
                        )
                    }
                    CheckedPattern::Placeholder(_) => {
                        let output_expr = self.check(output_expr, &expected_type);

                        // These patterns are unreachable, but check them anyway!
                        self.check_match(
                            false,
                            match_range,
                            scrutinee_range,
                            scrutinee_expr,
                            scrutinee_type,
                            next_equations,
                            expected_type,
                        );

                        output_expr
                    }
                    CheckedPattern::Const(_, _) => {
                        // Temporary vector for accumulating branches
                        let mut branches = Vec::new();
                        let num_constructors = match scrutinee_type.match_prim_spine() {
                            Some((Prim::BoolType, [])) => Some(2),
                            _ => None,
                        };

                        // Collect a run of constant patterns
                        while let Some(((pattern, output_expr), next_equations)) =
                            equations.split_first()
                        {
                            let (def_pattern, _) = self.check_pattern(pattern, &scrutinee_type);
                            match def_pattern {
                                // Accumulate constant pattern
                                CheckedPattern::Const(_, r#const) => {
                                    let output_term = self.check(output_expr, expected_type);
                                    // Find insertion index
                                    let res = branches.binary_search_by(
                                        |(probe_const, _term): &(Const, _)| {
                                            probe_const
                                                .partial_cmp(&r#const)
                                                .expect("attempt to compare non-ordered value")
                                        },
                                    );
                                    match res {
                                        Ok(_index) => {
                                            // this is a duplicate branch
                                            self.push_message(Message::UnreachablePattern {
                                                range: pattern.range(),
                                            });
                                        }
                                        Err(index) => {
                                            branches.insert(index, (r#const, output_term))
                                        }
                                    }

                                    equations = next_equations;
                                }

                                // Time for the default pattern
                                CheckedPattern::Name(_, _)
                                | CheckedPattern::Placeholder(_)
                                | CheckedPattern::ReportedError(_) => {
                                    // Push the default parameter of the constant elimination
                                    self.push_rigid_param(def_pattern, scrutinee_type.clone());

                                    // Check the default expression and any other
                                    // unreachable equations following that.
                                    let default_expr = self.check_match(
                                        true,
                                        match_range,
                                        scrutinee_range,
                                        scrutinee_expr,
                                        scrutinee_type,
                                        equations,
                                        expected_type,
                                    );

                                    self.rigid_env.pop();

                                    return core::Term::ConstElim(
                                        scrutinee_expr,
                                        self.scope.to_scope_from_iter(branches.into_iter()),
                                        Some(self.scope.to_scope(default_expr)),
                                    );
                                }
                            }
                        }

                        if num_constructors == Some(branches.len()) {
                            // The absence of a default constructor is ok as the match was exhaustive.
                            return core::Term::ConstElim(
                                scrutinee_expr,
                                self.scope.to_scope_from_iter(branches.into_iter()),
                                None,
                            );
                        }

                        if is_reachable {
                            // TODO: this should be admitted if the scrutinee type is uninhabited
                            self.push_message(Message::NonExhaustiveMatchExpr {
                                match_expr_range: match_range,
                                scrutinee_expr_range: scrutinee_range,
                            });
                        }
                        core::Term::Prim(Prim::ReportedError)
                    }
                    CheckedPattern::ReportedError(_) => {
                        // Check for any further errors in the first equation's output expression.
                        self.check(output_expr, expected_type);
                        self.check_match(
                            false,
                            match_range,
                            scrutinee_range,
                            scrutinee_expr,
                            scrutinee_type,
                            next_equations,
                            expected_type,
                        );

                        core::Term::Prim(Prim::ReportedError)
                    }
                }
            }
            None => {
                if is_reachable {
                    // TODO: this should be admitted if the scrutinee type is uninhabited
                    self.push_message(Message::NonExhaustiveMatchExpr {
                        match_expr_range: match_range,
                        scrutinee_expr_range: scrutinee_range,
                    });
                }
                core::Term::Prim(Prim::ReportedError)
            }
        }
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
