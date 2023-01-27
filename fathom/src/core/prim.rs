use std::cell::RefCell;
use std::sync::Arc;

use fxhash::FxHashMap;
use scoped_arena::Scope;

use crate::core::semantics::{ArcValue, Elim, ElimEnv, Head, Value};
use crate::core::{self, Const, IntType, Plicity, Prim, SintType, UIntStyle, UintType};
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
        const VOID_TYPE: Term<'_> = Term::Prim(Span::Empty, VoidType);
        const U8_TYPE: Term<'_> =
            Term::Prim(Span::Empty, IntType(core::IntType::Unsigned(UintType::U8)));
        const FORMAT_TYPE: Term<'_> = Term::Prim(Span::Empty, FormatType);
        const BOOL_TYPE: Term<'_> = Term::Prim(Span::Empty, BoolType);
        const POS_TYPE: Term<'_> = Term::Prim(Span::Empty, PosType);

        let mut env = EnvBuilder::new(interner, scope);

        env.define_prim(VoidType, &UNIVERSE);
        env.define_prim(BoolType, &UNIVERSE);

        for int_type in core::IntType::ALL {
            env.define_prim(Prim::IntType(int_type), &UNIVERSE);
        }

        for float_type in core::FloatType::ALL {
            env.define_prim(Prim::FloatType(float_type), &UNIVERSE);
        }

        env.define_prim_fun(OptionType, [&UNIVERSE], &UNIVERSE);

        env.define_prim_fun(VecType, [&UNIVERSE], &UNIVERSE);
        for uint_type in core::UintType::ALL {
            let type_term = scope.to_scope(Term::Prim(Span::Empty, uint_type.into()));
            env.define_prim_fun(ArrayType(uint_type), [type_term, &UNIVERSE], &UNIVERSE);
        }

        env.define_prim(PosType, &UNIVERSE);
        env.define_prim_fun(RefType, [&FORMAT_TYPE], &UNIVERSE);
        env.define_prim(FormatType, &UNIVERSE);

        for endianness in core::Endianness::ALL {
            for int_type in core::IntType::ALL {
                env.define_prim(FormatInt(int_type, endianness), &FORMAT_TYPE);
            }

            for float_type in core::FloatType::ALL {
                env.define_prim(FormatFloat(float_type, endianness), &FORMAT_TYPE);
            }
        }

        for uint_type in core::UintType::ALL {
            let type_term = scope.to_scope(Term::Prim(Span::Empty, uint_type.into()));
            env.define_prim_fun(
                FormatRepeat(uint_type),
                [type_term, &FORMAT_TYPE],
                &FORMAT_TYPE,
            );
            env.define_prim_fun(
                FormatLimit(uint_type),
                [type_term, &FORMAT_TYPE],
                &FORMAT_TYPE,
            );
        }

        env.define_prim_fun(FormatRepeatUntilEnd, [&FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim_fun(FormatLink, [&POS_TYPE, &FORMAT_TYPE], &FORMAT_TYPE);
        env.define_prim(
            FormatDeref,
            &core::Term::FunType(
                Span::Empty,
                Plicity::Implicit,
                env.name("f"),
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
                Plicity::Implicit,
                env.name("A"),
                &UNIVERSE,
                &Term::FunType(Span::Empty, Plicity::Explicit, None, &VAR0, &FORMAT_TYPE),
            ),
        );
        env.define_prim(FormatFail, &FORMAT_TYPE);
        env.define_prim(
            FormatUnwrap,
            // fun (@A : Type) -> Option A   -> Format
            // fun (@A : Type) -> Option A@0 -> Format
            &core::Term::FunType(
                Span::Empty,
                Plicity::Implicit,
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

        // fun (@A : Type) -> Void -> A
        env.define_prim(
            Absurd,
            &core::Term::FunType(
                Span::Empty,
                Plicity::Implicit,
                env.name("A"),
                &UNIVERSE,
                &core::Term::FunType(Span::Empty, Plicity::Explicit, None, &VOID_TYPE, &VAR1),
            ),
        );

        env.define_prim_fun(BoolNot, [&BOOL_TYPE], &BOOL_TYPE);
        for op in [BoolEq, BoolNeq, BoolAnd, BoolOr, BoolXor] {
            env.define_prim_fun(op, [&BOOL_TYPE, &BOOL_TYPE], &BOOL_TYPE);
        }

        for int_type in core::IntType::ALL {
            let type_term = scope.to_scope(Term::Prim(Span::Empty, int_type.into()));
            env.define_prim_fun(IntEq(int_type), [type_term, type_term], &BOOL_TYPE);
            env.define_prim_fun(IntNeq(int_type), [type_term, type_term], &BOOL_TYPE);
            env.define_prim_fun(IntNeq(int_type), [type_term, type_term], &BOOL_TYPE);
            env.define_prim_fun(IntLt(int_type), [type_term, type_term], &BOOL_TYPE);
            env.define_prim_fun(IntLte(int_type), [type_term, type_term], &BOOL_TYPE);
            env.define_prim_fun(IntGt(int_type), [type_term, type_term], &BOOL_TYPE);
            env.define_prim_fun(IntGte(int_type), [type_term, type_term], &BOOL_TYPE);
            env.define_prim_fun(IntAdd(int_type), [type_term, type_term], type_term);
            env.define_prim_fun(IntSub(int_type), [type_term, type_term], type_term);
            env.define_prim_fun(IntMul(int_type), [type_term, type_term], type_term);
            env.define_prim_fun(IntDiv(int_type), [type_term, type_term], type_term);

            if let core::IntType::Unsigned(uint_type) = int_type {
                env.define_prim_fun(IntNot(uint_type), [type_term], type_term);

                env.define_prim_fun(IntShl(uint_type), [type_term, &U8_TYPE], type_term);
                env.define_prim_fun(IntShr(uint_type), [type_term, &U8_TYPE], type_term);

                env.define_prim_fun(IntAnd(uint_type), [type_term, type_term], type_term);
                env.define_prim_fun(IntOr(uint_type), [type_term, type_term], type_term);
                env.define_prim_fun(IntXor(uint_type), [type_term, type_term], type_term);
            }

            if let core::IntType::Signed(sint_type) = int_type {
                let uint_type_term =
                    scope.to_scope(Term::Prim(Span::Empty, sint_type.to_unsigned().into()));

                env.define_prim_fun(IntNeg(sint_type), [type_term], type_term);
                env.define_prim_fun(IntAbs(sint_type), [type_term], type_term);
                env.define_prim_fun(IntUAbs(sint_type), [type_term], uint_type_term);
            }
        }

        env.define_prim(
            OptionSome,
            // fun (@A : Type) -> A   -> Option A
            // fun (@A : Type) -> A@0 -> Option A@1
            &core::Term::FunType(
                Span::Empty,
                Plicity::Implicit,
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
            // fun (@A : Type) -> Option A
            // fun (@A : Type) -> Option A@0
            &core::Term::FunType(
                Span::Empty,
                Plicity::Implicit,
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
            // fun (@A : Type) (@B : Type) -> B   -> (A   -> B  ) -> Option A   -> B
            // fun (@A : Type) (@B : Type) -> B@0 -> (A@2 -> B@2) -> Option A@3 -> B@3
            scope.to_scope(core::Term::FunType(
                Span::Empty,
                Plicity::Implicit,
                env.name("A"),
                &UNIVERSE,
                scope.to_scope(core::Term::FunType(
                    Span::Empty,
                    Plicity::Implicit,
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
                            // A@2 -> B@2
                            &Term::FunType(Span::Empty, Plicity::Explicit, None, &VAR2, &VAR2),
                            scope.to_scope(core::Term::FunType(
                                Span::Empty,
                                Plicity::Explicit,
                                None,
                                // Option A@3
                                &Term::FunApp(
                                    Span::Empty,
                                    Plicity::Explicit,
                                    &Term::Prim(Span::Empty, OptionType),
                                    &VAR3,
                                ),
                                &VAR3, // B@3
                            )),
                        )),
                    )),
                )),
            )),
        );

        // fun (@len : UN) (@A : Type) -> (A   -> Bool) -> ArrayN len   A   -> Option A
        // fun (@len : UN) (@A : Type) -> (A@0 -> Bool) -> ArrayN len@2 A@1 -> Option
        // A@2
        fn find_type<'arena>(
            env: &mut EnvBuilder<'_, '_>,
            scope: &'arena Scope<'arena>,
            index_type: &'arena Term<'arena>,
            array_type: &'arena Term<'arena>,
        ) -> &'arena Term<'arena> {
            scope.to_scope(core::Term::FunType(
                Span::Empty,
                Plicity::Implicit,
                env.name("len"),
                index_type,
                scope.to_scope(core::Term::FunType(
                    Span::Empty,
                    Plicity::Implicit,
                    env.name("A"),
                    &UNIVERSE,
                    scope.to_scope(core::Term::FunType(
                        Span::Empty,
                        Plicity::Explicit,
                        None,
                        // (A@0 -> Bool)
                        &Term::FunType(Span::Empty, Plicity::Explicit, None, &VAR0, &BOOL_TYPE),
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
                            // Option A@2
                            &Term::FunApp(
                                Span::Empty,
                                Plicity::Explicit,
                                &Term::Prim(Span::Empty, OptionType),
                                &VAR2,
                            ),
                        )),
                    )),
                )),
            ))
        }

        // fun (@len : UN) (@A : Type) (index : UN) -> ArrayN len   A   -> A
        // fun (@len : UN) (@A : Type) (index : UN) -> ArrayN len@2 A@1 -> A@2
        fn array_index_type<'arena>(
            env: &mut EnvBuilder<'_, '_>,
            scope: &'arena Scope<'arena>,
            index_type: &'arena Term<'arena>,
            array_type: &'arena Term<'arena>,
        ) -> &'arena Term<'arena> {
            scope.to_scope(core::Term::FunType(
                Span::Empty,
                Plicity::Implicit,
                env.name("len"),
                index_type,
                scope.to_scope(core::Term::FunType(
                    Span::Empty,
                    Plicity::Implicit,
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
        }

        for uint_type in core::UintType::ALL {
            let uint_type_term = scope.to_scope(Term::Prim(Span::Empty, uint_type.into()));
            let array_type_term = scope.to_scope(Term::Prim(Span::Empty, ArrayType(uint_type)));
            let array_find_type_term = find_type(&mut env, scope, uint_type_term, array_type_term);
            let array_index_type_term =
                array_index_type(&mut env, scope, uint_type_term, array_type_term);
            env.define_prim(ArrayFind(uint_type), array_find_type_term);
            env.define_prim(ArrayIndex(uint_type), array_index_type_term);
            env.define_prim_fun(PosAdd(uint_type), [&POS_TYPE, uint_type_term], &POS_TYPE);
        }

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
        let name = self.interner.borrow_mut().get_or_intern(prim.name());
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
pub type Step =
    Box<dyn for<'arena> FnOnce(&ElimEnv<'arena, '_>, &[Elim<'arena>]) -> Option<ArcValue<'arena>>>;

macro_rules! step {
    ($env:pat, [$($param:pat),*] => $body:expr) => {
        Box::new(move |$env, spine| match spine {
            [$(Elim::FunApp(_, $param)),*] => Some($body),
            _ => return None,
        })
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
            ($(Value::ConstLit(Const::$Input($param, _, $style)),)*) => Spanned::empty(Arc::new(Value::ConstLit($body))),
            _ => return None,
        })
    };
}

#[rustfmt::skip]
pub fn repr(prim: Prim) -> Step {
    match prim {
        Prim::FormatInt(int_type, _endianness) => step!(_, [] => Spanned::empty(Arc::new(Value::prim(int_type.into(), [])))),
        Prim::FormatFloat(float_type, _endianness) => step!(_, [] => Spanned::empty(Arc::new(Value::prim(float_type.into(), [])))),
        Prim::FormatRepeat(uint_type) => step!(env, [len, elem] => Spanned::empty(Arc::new(Value::prim(Prim::ArrayType(uint_type), [len.clone(), env.format_repr(elem)])))),
        Prim::FormatLimit(_uint_type) => step!(env, [_, elem] => env.format_repr(elem)),
        Prim::FormatRepeatUntilEnd => step!(env, [elem] => Spanned::empty(Arc::new(Value::prim(Prim::VecType, [env.format_repr(elem)])))),
        Prim::FormatLink => step!(_, [_, elem] => Spanned::empty(Arc::new(Value::prim(Prim::RefType, [elem.clone()])))),
        Prim::FormatDeref => step!(env, [elem, _] => env.format_repr(elem)),
        Prim::FormatStreamPos => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::PosType, [])))),
        Prim::FormatSucceed => step!(_, [elem, _] => elem.clone()),
        Prim::FormatFail => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::VoidType, [])))),
        Prim::FormatUnwrap => step!(_, [elem, _] => elem.clone()),
        Prim::ReportedError => step!(_, [] => Spanned::empty(Arc::new(Value::prim(Prim::ReportedError, [])))),
        _ => Box::new(|_, _| None),
    }
}

macro_rules! int_relop {
    ($int_type:expr, $x:ident, $y:ident, $op:expr) => {
        match $int_type {
            IntType::Unsigned(_) => const_step!([$x: Uint, $y: Uint] => $op),
            IntType::Signed(_) => const_step!([$x: Sint, $y: Sint] => $op),
        }
    };
}

macro_rules! int_checked_binop {
    ($int_type:expr, $x:ident, $xst:ident, $y:ident, $yst:ident, $op:ident) => {
        match $int_type {
            IntType::Unsigned(ty @ UintType::U8) => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u8::$op(u8::try_from(*$x).ok()?, u8::try_from(*$y).ok()?)?, ty, UIntStyle::merge(*$xst, *$yst))),
            IntType::Unsigned(ty @ UintType::U16) => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u16::$op(u16::try_from(*$x).ok()?, u16::try_from(*$y).ok()?)?, ty, UIntStyle::merge(*$xst, *$yst))),
            IntType::Unsigned(ty @ UintType::U32) => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u32::$op(u32::try_from(*$x).ok()?, u32::try_from(*$y).ok()?)?, ty, UIntStyle::merge(*$xst, *$yst))),
            IntType::Unsigned(ty @ UintType::U64) => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u64::$op(u64::try_from(*$x).ok()?, u64::try_from(*$y).ok()?)?, ty, UIntStyle::merge(*$xst, *$yst))),

            IntType::Signed(ty @ SintType::S8) => const_step!([$x: Sint, $y: Sint] => Const::sint(i8::$op(i8::try_from(*$x).ok()?, i8::try_from(*$y).ok()?)?, ty)),
            IntType::Signed(ty @ SintType::S16) => const_step!([$x: Sint, $y: Sint] => Const::sint(i16::$op(i16::try_from(*$x).ok()?, i16::try_from(*$y).ok()?)?, ty)),
            IntType::Signed(ty @ SintType::S32) => const_step!([$x: Sint, $y: Sint] => Const::sint(i32::$op(i32::try_from(*$x).ok()?, i32::try_from(*$y).ok()?)?, ty)),
            IntType::Signed(ty @ SintType::S64) => const_step!([$x: Sint, $y: Sint] => Const::sint(i64::$op(i64::try_from(*$x).ok()?, i64::try_from(*$y).ok()?)?, ty)),
        }
    };
}

macro_rules! uint_checked_binop {
    ($uint_type:expr, $x:ident, $xst:ident, $y:ident, $yst:ident, $op:ident) => {
        match $uint_type {
            UintType::U8 => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u8::$op(u8::try_from(*$x).ok()?, u8::try_from(*$y).ok()? as _)?, $uint_type, UIntStyle::merge(*$xst, *$yst))),
            UintType::U16 => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u16::$op(u16::try_from(*$x).ok()?, u16::try_from(*$y).ok()? as _)?, $uint_type, UIntStyle::merge(*$xst, *$yst))),
            UintType::U32 => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u32::$op(u32::try_from(*$x).ok()?, u32::try_from(*$y).ok()? as _)?, $uint_type, UIntStyle::merge(*$xst, *$yst))),
            UintType::U64 => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u64::$op(u64::try_from(*$x).ok()?, u64::try_from(*$y).ok()? as _)?, $uint_type, UIntStyle::merge(*$xst, *$yst))),
        }
    };
}

macro_rules! uint_binop {
    ($uint_type:expr, $x:ident, $xst:ident, $y:ident, $yst:ident, $op:ident) => {
        match $uint_type {
            UintType::U8 => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u8::$op(u8::try_from(*$x).ok()?, u8::try_from(*$y).ok()?), $uint_type, UIntStyle::merge(*$xst, *$yst))),
            UintType::U16 => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u16::$op(u16::try_from(*$x).ok()?, u16::try_from(*$y).ok()?), $uint_type, UIntStyle::merge(*$xst, *$yst))),
            UintType::U32 => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u32::$op(u32::try_from(*$x).ok()?, u32::try_from(*$y).ok()?), $uint_type, UIntStyle::merge(*$xst, *$yst))),
            UintType::U64 => const_step!([$x, $xst: Uint, $y, $yst: Uint] => Const::uint(u64::$op(u64::try_from(*$x).ok()?, u64::try_from(*$y).ok()?), $uint_type, UIntStyle::merge(*$xst, *$yst))),
        }
    };
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

        Prim::IntEq(int_type) => int_relop!(int_type, x, y, Const::Bool(x == y)),
        Prim::IntNeq(int_type) => int_relop!(int_type, x, y, Const::Bool(x != y)),
        Prim::IntLt(int_type) => int_relop!(int_type, x, y, Const::Bool(x < y)),
        Prim::IntLte(int_type) => int_relop!(int_type, x, y, Const::Bool(x <= y)),
        Prim::IntGt(int_type) => int_relop!(int_type, x, y, Const::Bool(x > y)),
        Prim::IntGte(int_type) => int_relop!(int_type, x, y, Const::Bool(x >= y)),

        Prim::IntAdd(int_type) => int_checked_binop!(int_type, x, xst, y, yst, checked_add),
        Prim::IntSub(int_type) => int_checked_binop!(int_type, x, xst, y, yst, checked_sub),
        Prim::IntMul(int_type) => int_checked_binop!(int_type, x, xst, y, yst, checked_mul),
        Prim::IntDiv(int_type) => int_checked_binop!(int_type, x, xst, y, yst, checked_div),

        Prim::IntShl(uint_type) => uint_checked_binop!(uint_type, x, xst, y, yst, checked_shl),
        Prim::IntShr(uint_type) => uint_checked_binop!(uint_type, x, xst, y, yst, checked_shr),

        Prim::IntAnd(int_type) => uint_binop!(int_type, x, xst, y, yst, bitand),
        Prim::IntOr(int_type) => uint_binop!(int_type, x, xst, y, yst, bitor),
        Prim::IntXor(int_type) => uint_binop!(int_type, x, xst, y, yst, bitxor),

        Prim::IntNot(uint_type) => match uint_type {
            UintType::U8 => const_step!([x, style: Uint] => Const::uint(u8::not(u8::try_from(*x).ok()?), uint_type, *style)),
            UintType::U16 => const_step!([x, style: Uint] => Const::uint(u16::not(u16::try_from(*x).ok()?), uint_type, *style)),
            UintType::U32 => const_step!([x, style: Uint] => Const::uint(u32::not(u32::try_from(*x).ok()?), uint_type, *style)),
            UintType::U64 => const_step!([x, style: Uint] => Const::uint(u64::not(*x), uint_type, *style)),
        }

        Prim::IntAbs(sint_type) => match sint_type {
            SintType::S8 => const_step!([x: Sint] => Const::sint(i8::abs(i8::try_from(*x).ok()?), sint_type)),
            SintType::S16 => const_step!([x: Sint] => Const::sint(i16::abs(i16::try_from(*x).ok()?), sint_type)),
            SintType::S32 => const_step!([x: Sint] => Const::sint(i32::abs(i32::try_from(*x).ok()?), sint_type)),
            SintType::S64 => const_step!([x: Sint] => Const::sint(i64::abs(*x), sint_type)),
        }

        Prim::IntUAbs(sint_type) => match sint_type {
            SintType::S8 => const_step!([x: Sint] => Const::u8(i8::unsigned_abs(i8::try_from(*x).ok()?))),
            SintType::S16 => const_step!([x: Sint] => Const::u16(i16::unsigned_abs(i16::try_from(*x).ok()?))),
            SintType::S32 => const_step!([x: Sint] => Const::u32(i32::unsigned_abs(i32::try_from(*x).ok()?))),
            SintType::S64 => const_step!([x: Sint] => Const::u64(i64::unsigned_abs(*x))),
        }

        Prim::OptionFold => step!(env, [_, _, on_none, on_some, option] => {
            match option.match_prim_spine()? {
                (Prim::OptionSome, [_, Elim::FunApp(Plicity::Explicit, value)]) => {
                    env.fun_app(Plicity::Explicit, on_some.clone(), value.clone())
                },
                (Prim::OptionNone, [_]) => on_none.clone(),
                _ => return None,
            }
        }),

        Prim::ArrayFind(_) => {
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

        Prim::ArrayIndex(_) => {
            step!(_, [_, _, index, array] => match array.as_ref() {
                Value::ArrayLit(elems) => {
                    let index = match (index).as_ref() {
                        Value::ConstLit(Const::Uint(index, ..)) => usize::try_from(*index).ok(),
                        _ => return None,
                    }?;
                    elems.get(index).cloned()?
                }
                _ => return None,
            })
        }

        Prim::PosAdd(_) => const_step!([x: Pos, y: Uint] => Const::Pos(usize::checked_add(*x, usize::try_from(*y).ok()?)?)),

        _ => Box::new(|_, _| None),
    }
}
