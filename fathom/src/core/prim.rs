use fxhash::FxHashMap;
use scoped_arena::Scope;
use std::cell::RefCell;
use std::sync::Arc;

use crate::core::semantics::{ArcValue, Elim, ElimEnv, Value};
use crate::core::{self, Const, Plicity, Prim, UIntStyle};
use crate::env::{self, SharedEnv, UniqueEnv};
use crate::source::{Span, Spanned, StringId, StringInterner};

/// Environment of primitives
pub struct Env<'arena> {
    // TODO: Provide a way to reflect these as top-level items in a module for
    //       improved documentation and error messages.
    entries: FxHashMap<StringId, (Prim, ArcValue<'arena>)>,
}

impl<'arena> Env<'arena> {
    /// Lookup a primitive name in the context.
    pub fn get_name(&self, name: StringId) -> Option<(Prim, &ArcValue<'arena>)> {
        let (prim, r#type) = self.entries.get(&name)?;

        Some((*prim, r#type))
    }

    pub fn default(
        interner: &RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    ) -> Env<'arena> {
        // TODO: Clean this up somehow!

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

        let mut env = EnvBuilder::new(interner, scope);

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
                Plicity::Explicit,
                env.name("A"),
                &FORMAT_TYPE,
                &Term::FunType(
                    Span::Empty,
                    Plicity::Explicit,
                    None,
                    &Term::FunApp(
                        Span::Empty,
                        Plicity::Explicit,
                        &Term::Prim(Span::Empty, RefType),
                        &VAR0,
                    ),
                    &FORMAT_TYPE,
                ),
            ),
        );
        env.define_prim(FormatStreamPos, &FORMAT_TYPE);
        env.define_prim(
            FormatSucceed,
            &core::Term::FunType(
                Span::Empty,
                Plicity::Explicit,
                env.name("A"),
                &UNIVERSE,
                &Term::FunType(Span::Empty, Plicity::Explicit, None, &VAR0, &FORMAT_TYPE),
            ),
        );
        env.define_prim(FormatFail, &FORMAT_TYPE);
        env.define_prim(
            FormatUnwrap,
            // fun (A : Type) -> Option A   -> Format
            // fun (A : Type) -> Option A@0 -> Format
            &core::Term::FunType(
                Span::Empty,
                Plicity::Explicit,
                env.name("A"),
                &UNIVERSE,
                &Term::FunType(
                    Span::Empty,
                    Plicity::Explicit,
                    None,
                    &Term::FunApp(
                        Span::Empty,
                        Plicity::Explicit,
                        &Term::Prim(Span::Empty, OptionType),
                        &VAR0,
                    ),
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
                Plicity::Explicit,
                env.name("A"),
                &UNIVERSE,
                &Term::FunType(
                    Span::Empty,
                    Plicity::Explicit,
                    None,
                    &VAR0,
                    &Term::FunApp(
                        Span::Empty,
                        Plicity::Explicit,
                        &Term::Prim(Span::Empty, OptionType),
                        &VAR1,
                    ),
                ),
            ),
        );
        env.define_prim(
            OptionNone,
            // fun (A : Type) -> Option A
            // fun (A : Type) -> Option A@0
            &core::Term::FunType(
                Span::Empty,
                Plicity::Explicit,
                env.name("A"),
                &UNIVERSE,
                &Term::FunApp(
                    Span::Empty,
                    Plicity::Explicit,
                    &Term::Prim(Span::Empty, OptionType),
                    &VAR0,
                ),
            ),
        );
        env.define_prim(
            OptionFold,
            // fun (A : Type) (B : Type) -> B   -> (A   -> B  ) -> Option A   -> B
            // fun (A : Type) (B : Type) -> B@0 -> (A@2 -> B@2) -> Option A@3 -> B@3
            scope.to_scope(core::Term::FunType(
                Span::Empty,
                Plicity::Explicit,
                env.name("A"),
                &UNIVERSE,
                scope.to_scope(core::Term::FunType(
                    Span::Empty,
                    Plicity::Explicit,
                    env.name("B"),
                    &UNIVERSE,
                    scope.to_scope(core::Term::FunType(
                        Span::Empty,
                        Plicity::Explicit,
                        None,
                        &VAR0, // B@0
                        scope.to_scope(core::Term::FunType(
                            Span::Empty,
                            Plicity::Explicit,
                            None,
                            &Term::FunType(Span::Empty, Plicity::Explicit, None, &VAR2, &VAR2), // A@2 -> B@2
                            scope.to_scope(core::Term::FunType(
                                Span::Empty,
                                Plicity::Explicit,
                                None,
                                &Term::FunApp(
                                    Span::Empty,
                                    Plicity::Explicit,
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
                Plicity::Explicit,
                env.name("len"),
                index_type,
                scope.to_scope(core::Term::FunType(
                    Span::Empty,
                    Plicity::Explicit,
                    env.name("A"),
                    &UNIVERSE,
                    scope.to_scope(core::Term::FunType(
                        Span::Empty,
                        Plicity::Explicit,
                        None,
                        &Term::FunType(Span::Empty, Plicity::Explicit, None, &VAR0, &BOOL_TYPE), // (A@0 -> Bool)
                        scope.to_scope(core::Term::FunType(
                            Span::Empty,
                            Plicity::Explicit,
                            None,
                            // ArrayN len@2 A@1
                            scope.to_scope(Term::FunApp(
                                Span::Empty,
                                Plicity::Explicit,
                                scope.to_scope(Term::FunApp(
                                    Span::Empty,
                                    Plicity::Explicit,
                                    array_type,
                                    &VAR2,
                                )),
                                &VAR1,
                            )),
                            &Term::FunApp(
                                Span::Empty,
                                Plicity::Explicit,
                                &Term::Prim(Span::Empty, OptionType),
                                &VAR2,
                            ), // Option A@2
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
                Plicity::Explicit,
                env.name("len"),
                index_type,
                scope.to_scope(core::Term::FunType(
                    Span::Empty,
                    Plicity::Explicit,
                    env.name("A"),
                    &UNIVERSE,
                    scope.to_scope(core::Term::FunType(
                        Span::Empty,
                        Plicity::Explicit,
                        env.name("index"),
                        index_type,
                        scope.to_scope(core::Term::FunType(
                            Span::Empty,
                            Plicity::Explicit,
                            None,
                            // ArrayN len@2 A@1
                            scope.to_scope(Term::FunApp(
                                Span::Empty,
                                Plicity::Explicit,
                                scope.to_scope(Term::FunApp(
                                    Span::Empty,
                                    Plicity::Explicit,
                                    array_type,
                                    &VAR2,
                                )),
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
}

struct EnvBuilder<'interner, 'arena> {
    entries: FxHashMap<StringId, (Prim, ArcValue<'arena>)>,
    interner: &'interner RefCell<StringInterner>,
    scope: &'arena Scope<'arena>,
    meta_exprs: UniqueEnv<Option<ArcValue<'arena>>>,
    item_exprs: UniqueEnv<ArcValue<'arena>>,
    local_exprs: SharedEnv<ArcValue<'arena>>,
}

impl<'interner, 'arena> EnvBuilder<'interner, 'arena> {
    fn new(
        interner: &'interner RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
    ) -> EnvBuilder<'interner, 'arena> {
        EnvBuilder {
            entries: FxHashMap::with_hasher(fxhash::FxBuildHasher::default()),
            interner,
            scope,
            meta_exprs: UniqueEnv::new(),
            item_exprs: UniqueEnv::new(),
            local_exprs: SharedEnv::new(),
        }
    }

    fn name(&self, name: &'static str) -> Option<StringId> {
        Some(self.interner.borrow_mut().get_or_intern_static(name))
    }

    fn define_prim(&mut self, prim: Prim, r#type: &core::Term<'arena>) {
        let name = self.interner.borrow_mut().get_or_intern_static(prim.name());
        let r#type = ElimEnv::new(&self.item_exprs, &self.meta_exprs)
            .eval_env(&mut self.local_exprs)
            .eval(r#type);
        self.entries.insert(name, (prim, r#type));
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
                self.scope.to_scope(core::Term::FunType(
                    Span::Empty,
                    Plicity::Explicit,
                    None,
                    param_type,
                    r#type,
                ))
            }),
        );
    }

    fn build(self) -> Env<'arena> {
        Env {
            entries: self.entries,
        }
    }
}

/// Primitive evaluation step.
pub type Step = for<'arena> fn(&ElimEnv<'arena, '_>, &[Elim<'arena>]) -> Option<ArcValue<'arena>>;

macro_rules! step {
    ($env:pat, [$($param:pat),*] => $body:expr) => {
        |$env, spine| match spine {
            [$(Elim::FunApp(Plicity::Explicit, $param)),*] => Some($body),
            _ => return None,
        }
    };
}

// TODO: Should we merge the spans of the param idents to produce the body span?
macro_rules! const_step {
    ([$($param:ident : $Input:ident),*] => $body:expr) => {
        step!(_, [$($param),*] => match ($($param.as_ref(),)*) {
            ($(Value::ConstLit(Const::$Input($param, ..)),)*) => Spanned::empty(Arc::new(Value::ConstLit($body))),
            _ => return None,
        })
    };
    ([$($param:ident , $style:ident : $Input:ident),*] => $body:expr) => {
        step!(_, [$($param),*] => match ($($param.as_ref(),)*) {
            ($(Value::ConstLit(Const::$Input($param, $style)),)*) => Spanned::empty(Arc::new(Value::ConstLit($body))),
            _ => return None,
        })
    };
}

#[rustfmt::skip]
pub fn repr(prim: Prim) -> Step {
    match prim {
        Prim::FormatU8 => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::U8Type, [])))),
        Prim::FormatU16Be => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::U16Type, [])))),
        Prim::FormatU16Le => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::U16Type, [])))),
        Prim::FormatU32Be => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::U32Type, [])))),
        Prim::FormatU32Le => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::U32Type, [])))),
        Prim::FormatU64Be => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::U64Type, [])))),
        Prim::FormatU64Le => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::U64Type, [])))),
        Prim::FormatS8 => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::S8Type, [])))),
        Prim::FormatS16Be => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::S16Type, [])))),
        Prim::FormatS16Le => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::S16Type, [])))),
        Prim::FormatS32Be => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::S32Type, [])))),
        Prim::FormatS32Le => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::S32Type, [])))),
        Prim::FormatS64Be => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::S64Type, [])))),
        Prim::FormatS64Le => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::S64Type, [])))),
        Prim::FormatF32Be => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::F32Type, [])))),
        Prim::FormatF32Le => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::F32Type, [])))),
        Prim::FormatF64Be => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::F64Type, [])))),
        Prim::FormatF64Le => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::F64Type, [])))),
        Prim::FormatArray8 => step!(env, [len, elem] => Spanned::empty(Arc::new(Value::prim(Prim::Array8Type, [len.clone(), env.format_repr(elem)])))),
        Prim::FormatArray16 => step!(env, [len, elem] => Spanned::empty(Arc::new(Value::prim(Prim::Array16Type, [len.clone(), env.format_repr(elem)])))),
        Prim::FormatArray32 => step!(env, [len, elem] => Spanned::empty(Arc::new(Value::prim(Prim::Array32Type, [len.clone(), env.format_repr(elem)])))),
        Prim::FormatArray64 => step!(env, [len, elem] => Spanned::empty(Arc::new(Value::prim(Prim::Array64Type, [len.clone(), env.format_repr(elem)])))),
        Prim::FormatLimit8 => step!(env, [_, elem] => env.format_repr(elem)),
        Prim::FormatLimit16 => step!(env, [_, elem] => env.format_repr(elem)),
        Prim::FormatLimit32 => step!(env, [_, elem] => env.format_repr(elem)),
        Prim::FormatLimit64 => step!(env, [_, elem] => env.format_repr(elem)),
        Prim::FormatRepeatUntilEnd => step!(env, [elem] => Spanned::empty(Arc::new(Value::prim(Prim::ArrayType, [env.format_repr(elem)])))),
        Prim::FormatLink => step!(_, [_, elem] => Spanned::empty(Arc::new(Value::prim(Prim::RefType, [elem.clone()])))),
        Prim::FormatDeref => step!(env, [elem, _] => env.format_repr(elem)),
        Prim::FormatStreamPos => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::PosType, [])))),
        Prim::FormatSucceed => step!(_, [elem, _] => elem.clone()),
        Prim::FormatFail => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::VoidType, [])))),
        Prim::FormatUnwrap => step!(_, [elem, _] => elem.clone()),
        Prim::ReportedError => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::ReportedError, [])))),
        _ => |_, _| None,
    }
}

/// Returns an evaluation step for a primitive, if there is one defined.
#[rustfmt::skip]
pub fn step(prim: Prim) -> Step {
    use std::ops::{BitAnd, BitOr, BitXor, Not};
    use std::convert::TryFrom;

    match prim {
        Prim::FormatRepr => step!(env, [format] => env.format_repr(format)),

        Prim::BoolEq => const_step!([x: Bool, y: Bool] => Const::Bool(x == y)),
        Prim::BoolNeq => const_step!([x: Bool, y: Bool] => Const::Bool(x != y)),
        Prim::BoolNot => const_step!([x: Bool] => Const::Bool(bool::not(*x))),
        Prim::BoolAnd => const_step!([x: Bool, y: Bool] => Const::Bool(*x && *y)),
        Prim::BoolOr => const_step!([x: Bool, y: Bool] => Const::Bool(*x || *y)),
        Prim::BoolXor => const_step!([x: Bool, y: Bool] => Const::Bool(*x ^ *y)),

        Prim::U8Eq => const_step!([x: U8, y: U8] => Const::Bool(x == y)),
        Prim::U8Neq => const_step!([x: U8, y: U8] => Const::Bool(x != y)),
        Prim::U8Gt => const_step!([x: U8, y: U8] => Const::Bool(x > y)),
        Prim::U8Lt => const_step!([x: U8, y: U8] => Const::Bool(x < y)),
        Prim::U8Gte => const_step!([x: U8, y: U8] => Const::Bool(x >= y)),
        Prim::U8Lte => const_step!([x: U8, y: U8] => Const::Bool(x <= y)),
        Prim::U8Add => const_step!([x, xst: U8, y, yst: U8] => Const::U8(u8::checked_add(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U8Sub => const_step!([x, xst: U8, y, yst: U8] => Const::U8(u8::checked_sub(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U8Mul => const_step!([x, xst: U8, y, yst: U8] => Const::U8(u8::checked_mul(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U8Div => const_step!([x, xst: U8, y, yst: U8] => Const::U8(u8::checked_div(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U8Not => const_step!([x, style: U8] => Const::U8(u8::not(*x), *style)),
        Prim::U8Shl => const_step!([x, xst: U8, y, _yst: U8] => Const::U8(u8::checked_shl(*x, u32::from(*y))?, *xst)),
        Prim::U8Shr => const_step!([x, xst: U8, y, _yst: U8] => Const::U8(u8::checked_shr(*x, u32::from(*y))?, *xst)),
        Prim::U8And => const_step!([x, xst: U8, y, yst: U8] => Const::U8(u8::bitand(*x, *y), UIntStyle::merge(*xst, *yst))),
        Prim::U8Or => const_step!([x, xst: U8, y, yst: U8] => Const::U8(u8::bitor(*x, *y), UIntStyle::merge(*xst, *yst))),
        Prim::U8Xor => const_step!([x, xst: U8, y, yst: U8] => Const::U8(u8::bitxor(*x, *y), UIntStyle::merge(*xst, *yst))),

        Prim::U16Eq => const_step!([x: U16, y: U16] => Const::Bool(x == y)),
        Prim::U16Neq => const_step!([x: U16, y: U16] => Const::Bool(x != y)),
        Prim::U16Gt => const_step!([x: U16, y: U16] => Const::Bool(x > y)),
        Prim::U16Lt => const_step!([x: U16, y: U16] => Const::Bool(x < y)),
        Prim::U16Gte => const_step!([x: U16, y: U16] => Const::Bool(x >= y)),
        Prim::U16Lte => const_step!([x: U16, y: U16] => Const::Bool(x <= y)),
        Prim::U16Add => const_step!([x, xst: U16, y, yst: U16] => Const::U16(u16::checked_add(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U16Sub => const_step!([x, xst: U16, y, yst: U16] => Const::U16(u16::checked_sub(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U16Mul => const_step!([x, xst: U16, y, yst: U16] => Const::U16(u16::checked_mul(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U16Div => const_step!([x, xst: U16, y, yst: U16] => Const::U16(u16::checked_div(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U16Not => const_step!([x: U16] => Const::U16(u16::not(*x), UIntStyle::Decimal)),
        Prim::U16Shl => const_step!([x, xst: U16, y, _yst: U8] => Const::U16(u16::checked_shl(*x, u32::from(*y))?, *xst)),
        Prim::U16Shr => const_step!([x, xst: U16, y, _yst: U8] => Const::U16(u16::checked_shr(*x, u32::from(*y))?, *xst)),
        Prim::U16And => const_step!([x, xst: U16, y, yst: U16] => Const::U16(u16::bitand(*x, *y), UIntStyle::merge(*xst, *yst))),
        Prim::U16Or => const_step!([x, xst: U16, y, yst: U16] => Const::U16(u16::bitor(*x, *y), UIntStyle::merge(*xst, *yst))),
        Prim::U16Xor => const_step!([x, xst: U16, y, yst: U16] => Const::U16(u16::bitxor(*x, *y), UIntStyle::merge(*xst, *yst))),

        Prim::U32Eq => const_step!([x: U32, y: U32] => Const::Bool(x == y)),
        Prim::U32Neq => const_step!([x: U32, y: U32] => Const::Bool(x != y)),
        Prim::U32Gt => const_step!([x: U32, y: U32] => Const::Bool(x > y)),
        Prim::U32Lt => const_step!([x: U32, y: U32] => Const::Bool(x < y)),
        Prim::U32Gte => const_step!([x: U32, y: U32] => Const::Bool(x >= y)),
        Prim::U32Lte => const_step!([x: U32, y: U32] => Const::Bool(x <= y)),
        Prim::U32Add => const_step!([x, xst: U32, y, yst: U32] => Const::U32(u32::checked_add(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U32Sub => const_step!([x, xst: U32, y, yst: U32] => Const::U32(u32::checked_sub(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U32Mul => const_step!([x, xst: U32, y, yst: U32] => Const::U32(u32::checked_mul(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U32Div => const_step!([x, xst: U32, y, yst: U32] => Const::U32(u32::checked_div(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U32Not => const_step!([x: U32] => Const::U32(u32::not(*x), UIntStyle::Decimal)),
        Prim::U32Shl => const_step!([x, xst: U32, y, _yst: U8] => Const::U32(u32::checked_shl(*x, u32::from(*y))?, *xst)),
        Prim::U32Shr => const_step!([x, xst: U32, y, _yst: U8] => Const::U32(u32::checked_shr(*x, u32::from(*y))?, *xst)),
        Prim::U32And => const_step!([x, xst: U32, y, yst: U32] => Const::U32(u32::bitand(*x, *y), UIntStyle::merge(*xst, *yst))),
        Prim::U32Or => const_step!([x, xst: U32, y, yst: U32] => Const::U32(u32::bitor(*x, *y), UIntStyle::merge(*xst, *yst))),
        Prim::U32Xor => const_step!([x, xst: U32, y, yst: U32] => Const::U32(u32::bitxor(*x, *y), UIntStyle::merge(*xst, *yst))),

        Prim::U64Eq => const_step!([x: U64, y: U64] => Const::Bool(x == y)),
        Prim::U64Neq => const_step!([x: U64, y: U64] => Const::Bool(x != y)),
        Prim::U64Gt => const_step!([x: U64, y: U64] => Const::Bool(x > y)),
        Prim::U64Lt => const_step!([x: U64, y: U64] => Const::Bool(x < y)),
        Prim::U64Gte => const_step!([x: U64, y: U64] => Const::Bool(x >= y)),
        Prim::U64Lte => const_step!([x: U64, y: U64] => Const::Bool(x <= y)),
        Prim::U64Add => const_step!([x, xst: U64, y, yst: U64] => Const::U64(u64::checked_add(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U64Sub => const_step!([x, xst: U64, y, yst: U64] => Const::U64(u64::checked_sub(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U64Mul => const_step!([x, xst: U64, y, yst: U64] => Const::U64(u64::checked_mul(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U64Div => const_step!([x, xst: U64, y, yst: U64] => Const::U64(u64::checked_div(*x, *y)?, UIntStyle::merge(*xst, *yst))),
        Prim::U64Not => const_step!([x: U64] => Const::U64(u64::not(*x), UIntStyle::Decimal)),
        Prim::U64Shl => const_step!([x, xst: U64, y, _yst: U8] => Const::U64(u64::checked_shl(*x, u32::from(*y))?, *xst)),
        Prim::U64Shr => const_step!([x, xst: U64, y, _yst: U8] => Const::U64(u64::checked_shr(*x, u32::from(*y))?, *xst)),
        Prim::U64And => const_step!([x, xst: U64, y, yst: U64] => Const::U64(u64::bitand(*x, *y), UIntStyle::merge(*xst, *yst))),
        Prim::U64Or => const_step!([x, xst: U64, y, yst: U64] => Const::U64(u64::bitor(*x, *y), UIntStyle::merge(*xst, *yst))),
        Prim::U64Xor => const_step!([x, xst: U64, y, yst: U64] => Const::U64(u64::bitxor(*x, *y), UIntStyle::merge(*xst, *yst))),

        Prim::S8Eq => const_step!([x: S8, y: S8] => Const::Bool(x == y)),
        Prim::S8Neq => const_step!([x: S8, y: S8] => Const::Bool(x != y)),
        Prim::S8Gt => const_step!([x: S8, y: S8] => Const::Bool(x > y)),
        Prim::S8Lt => const_step!([x: S8, y: S8] => Const::Bool(x < y)),
        Prim::S8Gte => const_step!([x: S8, y: S8] => Const::Bool(x >= y)),
        Prim::S8Lte => const_step!([x: S8, y: S8] => Const::Bool(x <= y)),
        Prim::S8Neg => const_step!([x: S8] => Const::S8(i8::checked_neg(*x)?)),
        Prim::S8Add => const_step!([x: S8, y: S8] => Const::S8(i8::checked_add(*x, *y)?)),
        Prim::S8Sub => const_step!([x: S8, y: S8] => Const::S8(i8::checked_sub(*x, *y)?)),
        Prim::S8Mul => const_step!([x: S8, y: S8] => Const::S8(i8::checked_mul(*x, *y)?)),
        Prim::S8Div => const_step!([x: S8, y: S8] => Const::S8(i8::checked_div(*x, *y)?)),
        Prim::S8Abs => const_step!([x: S8] => Const::S8(i8::abs(*x))),
        Prim::S8UAbs => const_step!([x: S8] => Const::U8(i8::unsigned_abs(*x), UIntStyle::Decimal)),

        Prim::S16Eq => const_step!([x: S16, y: S16] => Const::Bool(x == y)),
        Prim::S16Neq => const_step!([x: S16, y: S16] => Const::Bool(x != y)),
        Prim::S16Gt => const_step!([x: S16, y: S16] => Const::Bool(x > y)),
        Prim::S16Lt => const_step!([x: S16, y: S16] => Const::Bool(x < y)),
        Prim::S16Gte => const_step!([x: S16, y: S16] => Const::Bool(x >= y)),
        Prim::S16Lte => const_step!([x: S16, y: S16] => Const::Bool(x <= y)),
        Prim::S16Neg => const_step!([x: S16] => Const::S16(i16::checked_neg(*x)?)),
        Prim::S16Add => const_step!([x: S16, y: S16] => Const::S16(i16::checked_add(*x, *y)?)),
        Prim::S16Sub => const_step!([x: S16, y: S16] => Const::S16(i16::checked_sub(*x, *y)?)),
        Prim::S16Mul => const_step!([x: S16, y: S16] => Const::S16(i16::checked_mul(*x, *y)?)),
        Prim::S16Div => const_step!([x: S16, y: S16] => Const::S16(i16::checked_div(*x, *y)?)),
        Prim::S16Abs => const_step!([x: S16] => Const::S16(i16::abs(*x))),
        Prim::S16UAbs => const_step!([x: S16] => Const::U16(i16::unsigned_abs(*x), UIntStyle::Decimal)),

        Prim::S32Eq => const_step!([x: S32, y: S32] => Const::Bool(x == y)),
        Prim::S32Neq => const_step!([x: S32, y: S32] => Const::Bool(x != y)),
        Prim::S32Gt => const_step!([x: S32, y: S32] => Const::Bool(x > y)),
        Prim::S32Lt => const_step!([x: S32, y: S32] => Const::Bool(x < y)),
        Prim::S32Gte => const_step!([x: S32, y: S32] => Const::Bool(x >= y)),
        Prim::S32Lte => const_step!([x: S32, y: S32] => Const::Bool(x <= y)),
        Prim::S32Neg => const_step!([x: S32] => Const::S32(i32::checked_neg(*x)?)),
        Prim::S32Add => const_step!([x: S32, y: S32] => Const::S32(i32::checked_add(*x, *y)?)),
        Prim::S32Sub => const_step!([x: S32, y: S32] => Const::S32(i32::checked_sub(*x, *y)?)),
        Prim::S32Mul => const_step!([x: S32, y: S32] => Const::S32(i32::checked_mul(*x, *y)?)),
        Prim::S32Div => const_step!([x: S32, y: S32] => Const::S32(i32::checked_div(*x, *y)?)),
        Prim::S32Abs => const_step!([x: S32] => Const::S32(i32::abs(*x))),
        Prim::S32UAbs => const_step!([x: S32] => Const::U32(i32::unsigned_abs(*x), UIntStyle::Decimal)),

        Prim::S64Eq => const_step!([x: S64, y: S64] => Const::Bool(x == y)),
        Prim::S64Neq => const_step!([x: S64, y: S64] => Const::Bool(x != y)),
        Prim::S64Gt => const_step!([x: S64, y: S64] => Const::Bool(x > y)),
        Prim::S64Lt => const_step!([x: S64, y: S64] => Const::Bool(x < y)),
        Prim::S64Gte => const_step!([x: S64, y: S64] => Const::Bool(x >= y)),
        Prim::S64Lte => const_step!([x: S64, y: S64] => Const::Bool(x <= y)),
        Prim::S64Neg => const_step!([x: S64] => Const::S64(i64::checked_neg(*x)?)),
        Prim::S64Add => const_step!([x: S64, y: S64] => Const::S64(i64::checked_add(*x, *y)?)),
        Prim::S64Sub => const_step!([x: S64, y: S64] => Const::S64(i64::checked_sub(*x, *y)?)),
        Prim::S64Mul => const_step!([x: S64, y: S64] => Const::S64(i64::checked_mul(*x, *y)?)),
        Prim::S64Div => const_step!([x: S64, y: S64] => Const::S64(i64::checked_div(*x, *y)?)),
        Prim::S64Abs => const_step!([x: S64] => Const::S64(i64::abs(*x))),
        Prim::S64UAbs => const_step!([x: S64] => Const::U64(i64::unsigned_abs(*x), UIntStyle::Decimal)),

        Prim::OptionFold => step!(env, [_, _, on_none, on_some, option] => {
            match option.match_prim_spine()? {
                (Prim::OptionSome, [Elim::FunApp(Plicity::Explicit, value)]) => env.fun_app(Plicity::Explicit,on_some.clone(), value.clone()),
                (Prim::OptionNone, []) => on_none.clone(),
                _ => return None,
            }
        }),

        Prim::Array8Find | Prim::Array16Find | Prim::Array32Find | Prim::Array64Find => {
            step!(env, [_, _, pred, array] => match array.as_ref() {
                Value::ArrayLit(elems) => {
                    for elem in elems {
                        match env.fun_app(Plicity::Explicit, pred.clone(), elem.clone()).as_ref() {
                            Value::ConstLit(Const::Bool(true)) => {
                                // TODO: Is elem.span right here?
                                return Some(Spanned::new(elem.span(), Arc::new(Value::prim(Prim::OptionSome, [elem.clone()]))))
                            },
                            Value::ConstLit(Const::Bool(false)) => {}
                            _ => return None,
                        }
                    }
                    Spanned::empty(Arc::new(Value::prim(Prim::OptionNone, [])))
                }
                _ => return None,
            })
        }

        Prim::Array8Index | Prim::Array16Index | Prim::Array32Index | Prim::Array64Index => {
            step!(_, [_, _, index, array] => match array.as_ref() {
                Value::ArrayLit(elems) => {
                    let index = match (index).as_ref() {
                        Value::ConstLit(Const::U8(index, _)) => Some(usize::from(*index)),
                        Value::ConstLit(Const::U16(index, _)) => Some(usize::from(*index)),
                        Value::ConstLit(Const::U32(index, _)) => usize::try_from(*index).ok(),
                        Value::ConstLit(Const::U64(index, _)) => usize::try_from(*index).ok(),
                        _ => return None,
                    }?;
                    elems.get(index).cloned()?
                }
                _ => return None,
            })
        }

        Prim::PosAddU8 => const_step!([x: Pos, y: U8] => Const::Pos(usize::checked_add(*x, usize::from(*y))?)),
        Prim::PosAddU16 => const_step!([x: Pos, y: U16] => Const::Pos(usize::checked_add(*x, usize::from(*y))?)),
        Prim::PosAddU32 => const_step!([x: Pos, y: U32] => Const::Pos(usize::checked_add(*x, usize::try_from(*y).ok()?)?)),
        Prim::PosAddU64 => const_step!([x: Pos, y: U64] => Const::Pos(usize::checked_add(*x, usize::try_from(*y).ok()?)?)),

        _ => |_, _| None,
    }
}
