use std::sync::Arc;

use fxhash::FxHashMap;
use scoped_arena::Scope;

use super::Builder;
use crate::core::semantics::{ArcValue, Elim, ElimEnv, Head, Value};
use crate::core::{self, Const, Plicity, Prim, UIntStyle};
use crate::env::{self, SharedEnv, UniqueEnv};
use crate::source::{Span, Spanned};
use crate::symbol::Symbol;

/// Environment of primitives
pub struct Env<'arena> {
    // TODO: Provide a way to reflect these as top-level items in a module for
    //       improved documentation and error messages.
    entries: FxHashMap<Symbol, (Prim, ArcValue<'arena>)>,
}

impl<'arena> Env<'arena> {
    /// Lookup a primitive name in the context.
    pub fn get_name(&self, name: Symbol) -> Option<(Prim, &ArcValue<'arena>)> {
        let (prim, r#type) = self.entries.get(&name)?;

        Some((*prim, r#type))
    }

    pub fn default(scope: &'arena Scope<'arena>) -> Env<'arena> {
        // TODO: Clean this up somehow!

        use crate::core::Prim::*;
        use crate::core::Term;

        const VAR0: Term<'_> = Term::LocalVar(Span::Empty, env::Index::last());
        const VAR1: Term<'_> = Term::LocalVar(Span::Empty, env::Index::last().prev());
        const VAR2: Term<'_> = Term::LocalVar(Span::Empty, env::Index::last().prev().prev());
        const VAR3: Term<'_> = Term::LocalVar(Span::Empty, env::Index::last().prev().prev().prev());
        const UNIVERSE: Term<'_> = Term::Universe(Span::Empty);
        const VOID_TYPE: Term<'_> = Term::Prim(Span::Empty, VoidType);
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
        const REF_TYPE: Term<'_> = Term::Prim(Span::Empty, RefType);
        const OPTION_TYPE: Term<'_> = Term::Prim(Span::Empty, OptionType);

        let mut env = EnvBuilder::new(scope);
        let builder = env.builder();

        // comments force rustfmt not to mess with grouping
        for prim in [
            VoidType, BoolType, PosType, FormatType, //
            U8Type, U16Type, U32Type, U64Type, //
            S8Type, S16Type, S32Type, S64Type, //
            F32Type, F64Type,
        ] {
            env.define_prim(prim, &UNIVERSE);
        }

        for prim in [OptionType, ArrayType] {
            env.define_prim_fun(prim, [&UNIVERSE], &UNIVERSE);
        }

        for (prim, arg) in [
            (Array8Type, &U8_TYPE),
            (Array16Type, &U16_TYPE),
            (Array32Type, &U32_TYPE),
            (Array64Type, &U64_TYPE),
        ] {
            env.define_prim_fun(prim, [arg, &UNIVERSE], &UNIVERSE);
        }

        env.define_prim_fun(RefType, [&FORMAT_TYPE], &UNIVERSE);

        // rustfmt messes with grouping regardless of comments for some reason :(
        for prim in [
            FormatU8,
            FormatS8,
            FormatU16Be,
            FormatU16Le,
            FormatU32Be,
            FormatU32Le,
            FormatU64Be,
            FormatU64Le,
            FormatS16Be,
            FormatS16Le,
            FormatS32Be,
            FormatS32Le,
            FormatS64Be,
            FormatS64Le,
            FormatF32Be,
            FormatF32Le,
            FormatF64Be,
            FormatF64Le,
        ] {
            env.define_prim(prim, &FORMAT_TYPE);
        }

        for (prim1, prim2, arg) in [
            (FormatRepeatLen8, FormatLimit8, &U8_TYPE),
            (FormatRepeatLen16, FormatLimit16, &U16_TYPE),
            (FormatRepeatLen32, FormatLimit32, &U32_TYPE),
            (FormatRepeatLen64, FormatLimit64, &U64_TYPE),
        ] {
            env.define_prim_fun(prim1, [arg, &FORMAT_TYPE], &FORMAT_TYPE);
            env.define_prim_fun(prim2, [arg, &FORMAT_TYPE], &FORMAT_TYPE);
        }

        env.define_prim_fun(FormatRepeatUntilEnd, [&FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatLink, [&POS_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim(
            FormatDeref,
            &builder.fun_types(
                [
                    (Plicity::Implicit, env.name("f"), FORMAT_TYPE),
                    (Plicity::Explicit, None, builder.fun_apps(REF_TYPE, [VAR0])),
                ],
                FORMAT_TYPE,
            ),
        );
        env.define_prim(FormatStreamPos, &FORMAT_TYPE);
        env.define_prim(
            FormatSucceed,
            &builder.fun_types(
                [
                    (Plicity::Implicit, env.name("A"), UNIVERSE),
                    (Plicity::Explicit, None, VAR0),
                ],
                FORMAT_TYPE,
            ),
        );
        env.define_prim(FormatFail, &FORMAT_TYPE);
        env.define_prim(
            FormatUnwrap,
            // fun (@A : Type) -> Option A   -> Format
            // fun (@A : Type) -> Option A@0 -> Format
            &builder.fun_types(
                [
                    (Plicity::Implicit, env.name("A"), UNIVERSE),
                    (
                        Plicity::Explicit,
                        None,
                        builder.fun_apps(OPTION_TYPE, [VAR0]),
                    ),
                ],
                FORMAT_TYPE,
            ),
        );
        env.define_prim_fun(FormatRepr, [&FORMAT_TYPE], &UNIVERSE);

        // fun (@A : Type) -> Void -> A
        env.define_prim(
            Absurd,
            &builder.fun_types(
                [
                    (Plicity::Implicit, env.name("A"), UNIVERSE),
                    (Plicity::Explicit, None, VOID_TYPE),
                ],
                VAR1,
            ),
        );

        env.define_prim_fun(BoolNot, [&BOOL_TYPE], &BOOL_TYPE);
        for prim in [BoolEq, BoolNeq, BoolAnd, BoolOr, BoolXor] {
            env.define_prim_fun(prim, [&BOOL_TYPE, &BOOL_TYPE], &BOOL_TYPE);
        }

        struct UintPrims {
            r#type: &'static Term<'static>,
            not: Prim,
            relops: [Prim; 6],
            binops: [Prim; 7],
            shifts: [Prim; 2],
        }

        const U8_PRIMS: UintPrims = UintPrims {
            r#type: &U8_TYPE,
            not: U8Not,
            relops: [U8Eq, U8Neq, U8Lt, U8Gt, U8Lte, U8Gte],
            binops: [U8Add, U8Sub, U8Mul, U8Div, U8And, U8Or, U8Xor],
            shifts: [U8Shl, U8Shr],
        };

        const U16_PRIMS: UintPrims = UintPrims {
            r#type: &U16_TYPE,
            not: U16Not,
            relops: [U16Eq, U16Neq, U16Lt, U16Gt, U16Lte, U16Gte],
            binops: [U16Add, U16Sub, U16Mul, U16Div, U16And, U16Or, U16Xor],
            shifts: [U16Shl, U16Shr],
        };

        const U32_PRIMS: UintPrims = UintPrims {
            r#type: &U32_TYPE,
            not: U32Not,
            relops: [U32Eq, U32Neq, U32Lt, U32Gt, U32Lte, U32Gte],
            binops: [U32Add, U32Sub, U32Mul, U32Div, U32And, U32Or, U32Xor],
            shifts: [U32Shl, U32Shr],
        };

        const U64_PRIMS: UintPrims = UintPrims {
            r#type: &U64_TYPE,
            not: U64Not,
            relops: [U64Eq, U64Neq, U64Lt, U64Gt, U64Lte, U64Gte],
            binops: [U64Add, U64Sub, U64Mul, U64Div, U64And, U64Or, U64Xor],
            shifts: [U64Shl, U64Shr],
        };

        for schema in [U8_PRIMS, U16_PRIMS, U32_PRIMS, U64_PRIMS] {
            let r#type = schema.r#type;
            env.define_prim_fun(schema.not, [r#type], r#type);
            for prim in schema.relops {
                env.define_prim_fun(prim, [r#type, r#type], &BOOL_TYPE);
            }
            for prim in schema.binops {
                env.define_prim_fun(prim, [r#type, r#type], r#type);
            }
            for prim in schema.shifts {
                env.define_prim_fun(prim, [r#type, &U8_TYPE], r#type);
            }
        }

        struct SintPrims {
            signed_type: &'static Term<'static>,
            unsigned_type: &'static Term<'static>,
            relops: [Prim; 6],
            binops: [Prim; 4],
            neg: Prim,
            abs: Prim,
            uabs: Prim,
        }

        const S8_PRIMS: SintPrims = SintPrims {
            signed_type: &S8_TYPE,
            unsigned_type: &U8_TYPE,
            relops: [S8Eq, S8Neq, S8Lt, S8Gt, S8Lte, S8Gte],
            binops: [S8Add, S8Sub, S8Mul, S8Div],
            neg: S8Neg,
            abs: S8Abs,
            uabs: S8UAbs,
        };

        const S16_PRIMS: SintPrims = SintPrims {
            signed_type: &S16_TYPE,
            unsigned_type: &U16_TYPE,
            relops: [S16Eq, S16Neq, S16Lt, S16Gt, S16Lte, S16Gte],
            binops: [S16Add, S16Sub, S16Mul, S16Div],
            neg: S16Neg,
            abs: S16Abs,
            uabs: S16UAbs,
        };

        const S32_PRIMS: SintPrims = SintPrims {
            signed_type: &S32_TYPE,
            unsigned_type: &U32_TYPE,
            relops: [S32Eq, S32Neq, S32Lt, S32Gt, S32Lte, S32Gte],
            binops: [S32Add, S32Sub, S32Mul, S32Div],
            neg: S32Neg,
            abs: S32Abs,
            uabs: S32UAbs,
        };

        const S64_PRIMS: SintPrims = SintPrims {
            signed_type: &S64_TYPE,
            unsigned_type: &U64_TYPE,
            relops: [S64Eq, S64Neq, S64Lt, S64Gt, S64Lte, S64Gte],
            binops: [S64Add, S64Sub, S64Mul, S64Div],
            neg: S64Neg,
            abs: S64Abs,
            uabs: S64UAbs,
        };

        for schema in [S8_PRIMS, S16_PRIMS, S32_PRIMS, S64_PRIMS] {
            let r#type = schema.signed_type;

            for prim in schema.relops {
                env.define_prim_fun(prim, [r#type, r#type], &BOOL_TYPE);
            }
            for prim in schema.binops {
                env.define_prim_fun(prim, [r#type, r#type], r#type);
            }
            env.define_prim_fun(schema.neg, [r#type], r#type);
            env.define_prim_fun(schema.abs, [r#type], r#type);
            env.define_prim_fun(schema.uabs, [r#type], schema.unsigned_type);
        }

        env.define_prim(
            OptionSome,
            // fun (@A : Type) -> A   -> Option A
            // fun (@A : Type) -> A@0 -> Option A@1
            &builder.fun_types(
                [
                    (Plicity::Implicit, env.name("A"), UNIVERSE),
                    (Plicity::Explicit, None, VAR0),
                ],
                builder.fun_app(Span::Empty, Plicity::Explicit, OPTION_TYPE, VAR1),
            ),
        );
        env.define_prim(
            OptionNone,
            // fun (@A : Type) -> Option A
            // fun (@A : Type) -> Option A@0
            &core::Term::FunType(
                Span::Empty,
                Plicity::Implicit,
                env.name("A"),
                &UNIVERSE,
                &Term::FunApp(Span::Empty, Plicity::Explicit, &OPTION_TYPE, &VAR0),
            ),
        );
        env.define_prim(
            OptionFold,
            // fun (@A : Type) (@B : Type) -> B   -> (A   -> B  ) -> Option A   -> B
            // fun (@A : Type) (@B : Type) -> B@0 -> (A@2 -> B@2) -> Option A@3 -> B@3
            &builder.fun_types(
                [
                    (Plicity::Implicit, env.name("A"), UNIVERSE),
                    (Plicity::Implicit, env.name("B"), UNIVERSE),
                    (Plicity::Explicit, None, VAR0),
                    (Plicity::Explicit, None, builder.fun_types([VAR2], VAR2)),
                    (
                        Plicity::Explicit,
                        None,
                        builder.fun_apps(OPTION_TYPE, [VAR3]),
                    ),
                ],
                VAR3,
            ),
        );

        // fun (@len : UN) (@A : Type) -> (A   -> Bool) -> ArrayN len   A   -> Option A
        // fun (@len : UN) (@A : Type) -> (A@0 -> Bool) -> ArrayN len@2 A@1 -> Option
        // A@2
        fn find_type<'arena>(
            env: &EnvBuilder<'arena>,
            index_type: Term<'arena>,
            array_type: Term<'arena>,
        ) -> &'arena Term<'arena> {
            let builder = env.builder();
            env.scope.to_scope(builder.fun_types(
                [
                    (Plicity::Implicit, env.name("len"), index_type),
                    (Plicity::Implicit, env.name("A"), UNIVERSE),
                    (
                        Plicity::Explicit,
                        None,
                        builder.fun_types([VAR0], BOOL_TYPE),
                    ),
                    (
                        Plicity::Explicit,
                        None,
                        builder.fun_apps(array_type, [VAR2, VAR1]),
                    ),
                ],
                Term::FunApp(Span::Empty, Plicity::Explicit, &OPTION_TYPE, &VAR2),
            ))
        }

        // fun (@len : UN) (@A : Type) (index : UN) -> ArrayN len   A   -> A
        // fun (@len : UN) (@A : Type) (index : UN) -> ArrayN len@2 A@1 -> A@2
        fn array_index_type<'arena>(
            env: &EnvBuilder<'arena>,
            index_type: Term<'arena>,
            array_type: Term<'arena>,
        ) -> &'arena Term<'arena> {
            let builder = env.builder();
            env.scope.to_scope(builder.fun_types(
                [
                    (Plicity::Implicit, env.name("len"), index_type.clone()),
                    (Plicity::Implicit, env.name("A"), UNIVERSE),
                    (Plicity::Explicit, env.name("index"), index_type),
                    (
                        Plicity::Explicit,
                        None,
                        builder.fun_apps(array_type, [VAR2, VAR1]),
                    ),
                ],
                VAR2,
            ))
        }

        for (prim1, prim2, index_type, array_type) in [
            (Array8Find, Array8Index, U8_TYPE, ARRAY8_TYPE),
            (Array16Find, Array16Index, U16_TYPE, ARRAY16_TYPE),
            (Array32Find, Array32Index, U32_TYPE, ARRAY32_TYPE),
            (Array64Find, Array64Index, U64_TYPE, ARRAY64_TYPE),
        ] {
            let find_type = find_type(&env, index_type.clone(), array_type.clone());
            let array_index_type = array_index_type(&env, index_type, array_type);
            env.define_prim(prim1, find_type);
            env.define_prim(prim2, array_index_type);
        }

        env.define_prim_fun(PosAddU8, [&POS_TYPE, &U8_TYPE], &POS_TYPE);
        env.define_prim_fun(PosAddU16, [&POS_TYPE, &U16_TYPE], &POS_TYPE);
        env.define_prim_fun(PosAddU32, [&POS_TYPE, &U32_TYPE], &POS_TYPE);
        env.define_prim_fun(PosAddU64, [&POS_TYPE, &U64_TYPE], &POS_TYPE);

        env.build()
    }
}

struct EnvBuilder<'arena> {
    entries: FxHashMap<Symbol, (Prim, ArcValue<'arena>)>,
    scope: &'arena Scope<'arena>,
    meta_exprs: UniqueEnv<Option<ArcValue<'arena>>>,
    item_exprs: UniqueEnv<ArcValue<'arena>>,
    local_exprs: SharedEnv<ArcValue<'arena>>,
}

impl<'arena> EnvBuilder<'arena> {
    fn new(scope: &'arena Scope<'arena>) -> EnvBuilder<'arena> {
        EnvBuilder {
            entries: FxHashMap::with_hasher(fxhash::FxBuildHasher::default()),
            scope,
            meta_exprs: UniqueEnv::new(),
            item_exprs: UniqueEnv::new(),
            local_exprs: SharedEnv::new(),
        }
    }

    fn builder(&self) -> Builder<'arena> {
        Builder::new(self.scope)
    }

    fn name(&self, name: &'static str) -> Option<Symbol> {
        Some(Symbol::intern_static(name))
    }

    fn define_prim(&mut self, prim: Prim, r#type: &core::Term<'arena>) {
        let name = Symbol::intern_static(prim.name());
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
            [$(Elim::FunApp(_, $param)),*] => Some($body),
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
        Prim::FormatRepeatLen8 => step!(env, [len, elem] => Spanned::empty(Arc::new(Value::prim(Prim::Array8Type, [len.clone(), env.format_repr(elem)])))),
        Prim::FormatRepeatLen16 => step!(env, [len, elem] => Spanned::empty(Arc::new(Value::prim(Prim::Array16Type, [len.clone(), env.format_repr(elem)])))),
        Prim::FormatRepeatLen32 => step!(env, [len, elem] => Spanned::empty(Arc::new(Value::prim(Prim::Array32Type, [len.clone(), env.format_repr(elem)])))),
        Prim::FormatRepeatLen64 => step!(env, [len, elem] => Spanned::empty(Arc::new(Value::prim(Prim::Array64Type, [len.clone(), env.format_repr(elem)])))),
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
        #[allow(unreachable_code)]
        Prim::Absurd => step!(_, [_, _] => panic!("Constructed an element of `Void`")),

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
                (Prim::OptionSome, [_, Elim::FunApp(Plicity::Explicit, value)]) => {
                    env.fun_app(Plicity::Explicit, on_some.clone(), value.clone())
                },
                (Prim::OptionNone, [_]) => on_none.clone(),
                _ => return None,
            }
        }),

        Prim::Array8Find | Prim::Array16Find | Prim::Array32Find | Prim::Array64Find => {
            step!(env, [_, elem_type, pred, array] => match array.as_ref() {
                Value::ArrayLit(elems) => {
                    for elem in elems {
                        match env.fun_app(
                            Plicity::Explicit,
                            pred.clone(), elem.clone()).as_ref() {
                            Value::ConstLit(Const::Bool(true)) => {
                                return Some(Spanned::empty(Arc::new(Value::Stuck(
                                    Head::Prim(Prim::OptionSome),
                                    vec![
                                        Elim::FunApp(Plicity::Implicit, elem_type.clone()),
                                        Elim::FunApp(Plicity::Explicit, elem.clone()),
                                    ],
                                ))));
                            },
                            Value::ConstLit(Const::Bool(false)) => {}
                            _ => return None,
                        }
                    }
                    Spanned::empty(Arc::new(Value::Stuck(
                        Head::Prim(Prim::OptionNone),
                        vec![Elim::FunApp(Plicity::Implicit, elem_type.clone())],
                    )))
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
