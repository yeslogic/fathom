use im::HashMap;
use moniker::FreeVar;

use semantics::PrimEnv;
use syntax::core::{RcTerm, RcType, RcValue};

/// The type checking environment
///
/// A default environment with entries for built-in types is provided via the
/// implementation of the `Default` trait.
///
/// We use persistent data structures internally so that we can copy the
/// environment as we enter into scopes, without having to deal with the
/// error-prone tedium of working with mutable context.
#[derive(Clone, Debug)]
pub struct TcEnv {
    /// Primitive definitions
    pub primitives: PrimEnv,
    /// Global annotation/definition pairs
    pub globals: HashMap<&'static str, (Option<RcValue>, RcType)>,
    /// The type annotations of the binders we have passed over
    pub declarations: HashMap<FreeVar<String>, RcType>,
    /// Any definitions we have passed over
    pub definitions: HashMap<FreeVar<String>, RcTerm>,
}

impl Default for TcEnv {
    fn default() -> TcEnv {
        use moniker::{Binder, Embed, FreeVar, Scope};
        use num_bigint::BigInt;
        use std::{i16, i32, i64, i8, u16, u32, u64, u8};

        use syntax::core::{Literal, Value};

        fn int_ty<T: Into<BigInt>>(min: Option<T>, max: Option<T>) -> RcValue {
            RcValue::from(Value::IntType(
                min.map(|x| RcValue::from(Value::Literal(Literal::Int(x.into())))),
                max.map(|x| RcValue::from(Value::Literal(Literal::Int(x.into())))),
            ))
        }

        let universe0 = RcValue::from(Value::universe(0));
        let true_value = RcValue::from(Value::Literal(Literal::Bool(true)));
        let false_value = RcValue::from(Value::Literal(Literal::Bool(false)));
        let bool_ty = RcValue::from(Value::global("Bool"));
        let nat_ty = RcValue::from(Value::IntType(
            Some(RcValue::from(Value::Literal(Literal::Int(0.into())))),
            None,
        ));
        let arrow = |params: Vec<RcType>, ret: RcType| {
            params.into_iter().rev().fold(ret, |body, ann| {
                RcValue::from(Value::Pi(Scope::new(
                    (Binder(FreeVar::fresh_unnamed()), Embed(ann)),
                    body,
                )))
            })
        };

        TcEnv {
            primitives: PrimEnv::default(),
            globals: hashmap!{
                "Bool" => (None, universe0.clone()),
                "true" => (Some(true_value), bool_ty.clone()),
                "false" => (Some(false_value), bool_ty.clone()),
                "String" => (None, universe0.clone()),
                "Char" => (None, universe0.clone()),

                "U8" => (Some(int_ty(Some(u8::MIN), Some(u8::MAX))), universe0.clone()),
                "U16" => (Some(int_ty(Some(u16::MIN), Some(u16::MAX))), universe0.clone()),
                "U32" => (Some(int_ty(Some(u32::MIN), Some(u32::MAX))), universe0.clone()),
                "U64" => (Some(int_ty(Some(u64::MIN), Some(u64::MAX))), universe0.clone()),
                "S8" => (Some(int_ty(Some(i8::MIN), Some(i8::MAX))), universe0.clone()),
                "S16" => (Some(int_ty(Some(i16::MIN), Some(i16::MAX))), universe0.clone()),
                "S32" => (Some(int_ty(Some(i32::MIN), Some(i32::MAX))), universe0.clone()),
                "S64" => (Some(int_ty(Some(i64::MIN), Some(i64::MAX))), universe0.clone()),

                "F32" => (None, universe0.clone()),
                "F64" => (None, universe0.clone()),
                "Array" => (None, arrow(vec![nat_ty, universe0.clone()], universe0.clone())),

                // TODO: Replace these with more general compute types
                "U16Le" => (None, universe0.clone()),
                "U32Le" => (None, universe0.clone()),
                "U64Le" => (None, universe0.clone()),
                "S16Le" => (None, universe0.clone()),
                "S32Le" => (None, universe0.clone()),
                "S64Le" => (None, universe0.clone()),
                "F32Le" => (None, universe0.clone()),
                "F64Le" => (None, universe0.clone()),
                "U16Be" => (None, universe0.clone()),
                "U32Be" => (None, universe0.clone()),
                "U64Be" => (None, universe0.clone()),
                "S16Be" => (None, universe0.clone()),
                "S32Be" => (None, universe0.clone()),
                "S64Be" => (None, universe0.clone()),
                "F32Be" => (None, universe0.clone()),
                "F64Be" => (None, universe0.clone()),
            },
            declarations: hashmap!{},
            definitions: hashmap!{},
        }
    }
}
