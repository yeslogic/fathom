use im::HashMap;
use moniker::{Binder, FreeVar};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::fmt;
use std::rc::Rc;

use crate::semantics::InternalError;
use crate::syntax::core::{self, Literal, RcTerm, RcType, RcValue, Term, Value};
use crate::syntax::translation::ResugarEnv;
use crate::syntax::{FloatFormat, IntFormat};

// Some helper traits for marshalling between Rust and Pikelet values
//
// I'm not super happy with the API at the moment, so these are currently private

trait IntoValue {
    fn into_value(self) -> RcValue;
}

macro_rules! impl_into_value {
    ($T:ty, $Variant:ident) => {
        impl IntoValue for $T {
            fn into_value(self) -> RcValue {
                RcValue::from(Value::Literal(Literal::$Variant(self)))
            }
        }
    };
    ($T:ty, $Variant:ident, $format:expr) => {
        impl IntoValue for $T {
            fn into_value(self) -> RcValue {
                RcValue::from(Value::Literal(Literal::$Variant(self, $format)))
            }
        }
    };
}

impl_into_value!(String, String);
impl_into_value!(char, Char);
impl_into_value!(bool, Bool);
impl_into_value!(BigInt, Int, IntFormat::Dec);
impl_into_value!(f32, F32, FloatFormat::Dec);
impl_into_value!(f64, F64, FloatFormat::Dec);

trait TryFromValueRef {
    fn try_from_value_ref(src: &Value) -> Result<&Self, InternalError>;
}

impl TryFromValueRef for String {
    fn try_from_value_ref(src: &Value) -> Result<&Self, InternalError> {
        match *src {
            Value::Literal(Literal::String(ref val)) => Ok(val),
            _ => unimplemented!("type error"), // TODO
        }
    }
}

impl TryFromValueRef for char {
    fn try_from_value_ref(src: &Value) -> Result<&Self, InternalError> {
        match *src {
            Value::Literal(Literal::Char(ref val)) => Ok(val),
            _ => unimplemented!("type error"), // TODO
        }
    }
}

impl TryFromValueRef for bool {
    fn try_from_value_ref(src: &Value) -> Result<&Self, InternalError> {
        match *src {
            Value::Literal(Literal::Bool(ref val)) => Ok(val),
            _ => unimplemented!("type error"), // TODO
        }
    }
}

impl TryFromValueRef for BigInt {
    fn try_from_value_ref(src: &Value) -> Result<&Self, InternalError> {
        match *src {
            Value::Literal(Literal::Int(ref val, _)) => Ok(val),
            _ => unimplemented!("type error"), // TODO
        }
    }
}

impl TryFromValueRef for f32 {
    fn try_from_value_ref(src: &Value) -> Result<&Self, InternalError> {
        match *src {
            Value::Literal(Literal::F32(ref val, _)) => Ok(val),
            _ => unimplemented!("type error"), // TODO
        }
    }
}

impl TryFromValueRef for f64 {
    fn try_from_value_ref(src: &Value) -> Result<&Self, InternalError> {
        match *src {
            Value::Literal(Literal::F64(ref val, _)) => Ok(val),
            _ => unimplemented!("type error"), // TODO
        }
    }
}

/// External functions
#[derive(Clone)]
pub struct Extern {
    /// The primitive definition to be used during normalization
    // FIXME: Closures should capture an environment, rather than us passing in a context here
    pub interpretation:
        for<'a> fn(&'a Context, &'a [RcValue]) -> Result<Option<RcValue>, InternalError>,
}

impl fmt::Debug for Extern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Extern")
            .field("interpretation", &"|context, params| { .. }")
            .finish()
    }
}

fn default_extern_definitions() -> HashMap<&'static str, Extern> {
    /// Define a primitive function
    macro_rules! prim {
        (fn($($param_name:ident : $PType:ty),*) -> $RType:ty $body:block) => {{
            fn interpretation(_: &Context, params: &[RcValue]) -> Result<Option<RcValue>, InternalError> {
                match params {
                    [$(ref $param_name),*] if $($param_name.is_nf())&&* => {
                        $(let $param_name = <$PType>::try_from_value_ref($param_name)?;)*
                        Ok(Some(<$RType>::into_value($body)))
                    }
                    _ => Ok(None),
                }
            }

            Extern {
                interpretation,
            }
        }};
    }

    fn array_index(_: &Context, params: &[RcValue]) -> Result<Option<RcValue>, InternalError> {
        match params {
            [ref index, ref elems] if index.is_nf() && elems.is_nf() => match *elems.inner {
                Value::Array(ref elems) => {
                    let index = BigInt::try_from_value_ref(index)?;
                    Ok(Some(elems[index.to_usize().unwrap()].clone()))
                },
                _ => unimplemented!("type error"), // TODO
            },
            _ => Ok(None),
        }
    }

    fn array_map(context: &Context, params: &[RcValue]) -> Result<Option<RcValue>, InternalError> {
        match params {
            [ref f, ref elems] if elems.is_nf() => match *elems.inner {
                Value::Array(ref elems) => {
                    let elems = elems
                        .iter()
                        .map(|elem| {
                            super::nf_term(
                                context,
                                &RcTerm::from(Term::App(
                                    RcTerm::from(Term::from(&**f)),
                                    RcTerm::from(Term::from(&**elem)),
                                )),
                            )
                        })
                        .collect::<Result<_, InternalError>>()?;

                    Ok(Some(RcValue::from(Value::Array(elems))))
                },
                _ => unimplemented!("type error"), // TODO
            },
            _ => Ok(None),
        }
    }

    fn array_find_map(
        context: &Context,
        params: &[RcValue],
    ) -> Result<Option<RcValue>, InternalError> {
        match params {
            [ref f, ref elems] if elems.is_nf() => match *elems.inner {
                Value::Array(ref elems) => {
                    for elem in elems {
                        let result = super::nf_term(
                            context,
                            &RcTerm::from(Term::App(
                                RcTerm::from(Term::from(&**f)),
                                RcTerm::from(Term::from(&**elem)),
                            )),
                        )?;

                        if let Some((_, value)) = context.some(&result) {
                            return Ok(Some(value.clone()));
                        }
                    }

                    unimplemented!("no match found"); // TODO
                },
                _ => unimplemented!("type error"), // TODO
            },
            _ => Ok(None),
        }
    }

    fn array_eq(context: &Context, params: &[RcValue]) -> Result<Option<RcValue>, InternalError> {
        match params {
            [ref pred, ref elems1, ref elems2] if elems1.is_nf() && elems2.is_nf() => {
                match (&*elems1.inner, &*elems2.inner) {
                    (&Value::Array(ref elems1), &Value::Array(ref elems2)) => {
                        if elems1.len() != elems2.len() {
                            return Ok(Some(RcValue::from(Value::Literal(Literal::Bool(false)))));
                        }

                        for (elem1, elem2) in Iterator::zip(elems1.iter(), elems2.iter()) {
                            let value = super::nf_term(
                                context,
                                &RcTerm::from(Term::App(
                                    RcTerm::from(Term::App(
                                        RcTerm::from(Term::from(&**pred)),
                                        RcTerm::from(Term::from(&**elem1)),
                                    )),
                                    RcTerm::from(Term::from(&**elem2)),
                                )),
                            )?;

                            match *value.inner {
                                Value::Literal(Literal::Bool(true)) => {},
                                Value::Literal(Literal::Bool(false)) => return Ok(Some(value)),
                                _ => unimplemented!("type error"), // TODO
                            }
                        }

                        Ok(Some(RcValue::from(Value::Literal(Literal::Bool(true)))))
                    },
                    _ => unimplemented!("type error"), // TODO
                }
            },
            _ => Ok(None),
        }
    }

    im::hashmap! {
        "string-eq" => prim!(fn(x: String, y: String) -> bool { x == y }),
        "bool-eq" => prim!(fn(x: bool, y: bool) -> bool { x == y }),
        "char-eq" => prim!(fn(x: char, y: char) -> bool { x == y }),
        "int-eq" => prim!(fn(x: BigInt, y: BigInt) -> bool { x == y }),
        "f32-eq" => prim!(fn(x: f32, y: f32) -> bool { f32::eq(x, y) }),
        "f64-eq" => prim!(fn(x: f64, y: f64) -> bool { f64::eq(x, y) }),

        "string-ne" => prim!(fn(x: String, y: String) -> bool { x != y }),
        "bool-ne" => prim!(fn(x: bool, y: bool) -> bool { x != y }),
        "char-ne" => prim!(fn(x: char, y: char) -> bool { x != y }),
        "f32-ne" => prim!(fn(x: f32, y: f32) -> bool { f32::ne(x, y) }),
        "f64-ne" => prim!(fn(x: f64, y: f64) -> bool { f64::ne(x, y) }),

        "string-le" => prim!(fn(x: String, y: String) -> bool { x <= y }),
        "bool-le" => prim!(fn(x: bool, y: bool) -> bool { x <= y }),
        "char-le" => prim!(fn(x: char, y: char) -> bool { x <= y }),
        "f32-le" => prim!(fn(x: f32, y: f32) -> bool { x <= y }),
        "f64-le" => prim!(fn(x: f64, y: f64) -> bool { x <= y }),

        "string-lt" => prim!(fn(x: String, y: String) -> bool { x < y }),
        "bool-lt" => prim!(fn(x: bool, y: bool) -> bool { x < y }),
        "char-lt" => prim!(fn(x: char, y: char) -> bool { x < y }),
        "f32-lt" => prim!(fn(x: f32, y: f32) -> bool { x < y }),
        "f64-lt" => prim!(fn(x: f64, y: f64) -> bool { x < y }),

        "string-gt" => prim!(fn(x: String, y: String) -> bool { x > y }),
        "bool-gt" => prim!(fn(x: bool, y: bool) -> bool { x > y }),
        "char-gt" => prim!(fn(x: char, y: char) -> bool { x > y }),
        "f32-gt" => prim!(fn(x: f32, y: f32) -> bool { x > y }),
        "f64-gt" => prim!(fn(x: f64, y: f64) -> bool { x > y }),

        "string-ge" => prim!(fn(x: String, y: String) -> bool { x >= y }),
        "bool-ge" => prim!(fn(x: bool, y: bool) -> bool { x >= y }),
        "char-ge" => prim!(fn(x: char, y: char) -> bool { x >= y }),
        "f32-ge" => prim!(fn(x: f32, y: f32) -> bool { x >= y }),
        "f64-ge" => prim!(fn(x: f64, y: f64) -> bool { x >= y }),

        "int-add" => prim!(fn(x: BigInt, y: BigInt) -> BigInt { x + y }),
        "f32-add" => prim!(fn(x: f32, y: f32) -> f32 { x + y }),
        "f64-add" => prim!(fn(x: f64, y: f64) -> f64 { x + y }),

        "int-sub" => prim!(fn(x: BigInt, y: BigInt) -> BigInt { x - y }),
        "f32-sub" => prim!(fn(x: f32, y: f32) -> f32 { x - y }),
        "f64-sub" => prim!(fn(x: f64, y: f64) -> f64 { x - y }),

        "int-mul" => prim!(fn(x: BigInt, y: BigInt) -> BigInt { x * y }),
        "f32-mul" => prim!(fn(x: f32, y: f32) -> f32 { x * y }),
        "f64-mul" => prim!(fn(x: f64, y: f64) -> f64 { x * y }),

        "int-div" => prim!(fn(x: BigInt, y: BigInt) -> BigInt { x / y }),
        "f32-div" => prim!(fn(x: f32, y: f32) -> f32 { x / y }),
        "f64-div" => prim!(fn(x: f64, y: f64) -> f64 { x / y }),

        "int-neg" => prim!(fn(val: BigInt) -> BigInt { -val }),
        "f32-neg" => prim!(fn(val: f32) -> f32 { -val }),
        "f64-neg" => prim!(fn(val: f64) -> f64 { -val }),

        "bool-not" => prim!(fn(val: bool) -> bool { !val }),

        "char-to-string" => prim!(fn(val: char) -> String { val.to_string() }),
        "int-to-string" => prim!(fn(val: BigInt) -> String { val.to_string() }),
        "f32-to-string" => prim!(fn(val: f32) -> String { val.to_string() }),
        "f64-to-string" => prim!(fn(val: f64) -> String { val.to_string() }),

        "string-append" => prim!(fn(x: String, y: String) -> String { x.clone() + y }), // FIXME: Clone

        "array-index" => Extern { interpretation: array_index },
        "array-map" => Extern { interpretation: array_map },
        "array-find-map" => Extern { interpretation: array_find_map },
        "array-eq" => Extern { interpretation: array_eq },
    }
}

#[derive(Clone, Debug)]
pub struct Globals {
    ty_bool: RcType,
    ty_string: RcType,
    ty_char: RcType,
    ty_unit: RcType,
    ty_pos: RcType,

    ty_u8: RcType,
    ty_u16: RcType,
    ty_u32: RcType,
    ty_u64: RcType,
    ty_u16le: RcType,
    ty_u32le: RcType,
    ty_u64le: RcType,
    ty_u16be: RcType,
    ty_u32be: RcType,
    ty_u64be: RcType,

    ty_s8: RcType,
    ty_s16: RcType,
    ty_s32: RcType,
    ty_s64: RcType,
    ty_s16le: RcType,
    ty_s32le: RcType,
    ty_s64le: RcType,
    ty_s16be: RcType,
    ty_s32be: RcType,
    ty_s64be: RcType,

    ty_f32: RcType,
    ty_f64: RcType,
    ty_f32le: RcType,
    ty_f64le: RcType,
    ty_f32be: RcType,
    ty_f64be: RcType,

    var_offset8: FreeVar<String>,
    var_offset16le: FreeVar<String>,
    var_offset32le: FreeVar<String>,
    var_offset64le: FreeVar<String>,
    var_offset16be: FreeVar<String>,
    var_offset32be: FreeVar<String>,
    var_offset64be: FreeVar<String>,

    var_array: FreeVar<String>,
    var_compute: FreeVar<String>,
    var_compute_array: FreeVar<String>,
    var_reserved: FreeVar<String>,
    var_link: FreeVar<String>,
    var_option: FreeVar<String>,
    var_some: FreeVar<String>,
    var_none: FreeVar<String>,
}

#[derive(Debug, Clone, PartialEq, moniker::BoundTerm)]
pub enum Definition {
    Alias(core::RcTerm),
    IntersectionType(core::IntersectionType),
    StructType(core::StructType),
    UnionType(core::UnionType),
}

/// The type checking environment
///
/// A default environment with entries for built-in types is provided via the
/// implementation of the `Default` trait.
///
/// We use persistent data structures internally so that we can copy the
/// environment as we enter into scopes, without having to deal with the
/// error-prone tedium of working with mutable context.
#[derive(Clone, Debug)]
pub struct Context {
    /// The resugar environment
    ///
    /// We'll keep this up to date as we type check to make it easier to do
    /// resugaring on any errors that we encounter
    resugar_env: ResugarEnv,
    /// The globals
    globals: Rc<Globals>,
    /// External definitions
    extern_definitions: HashMap<&'static str, Extern>,
    /// The type annotations of the binders we have passed over
    declarations: HashMap<FreeVar<String>, RcType>,
    /// Any definitions we have passed over
    definitions: HashMap<FreeVar<String>, Definition>,
}

impl Default for Context {
    fn default() -> Context {
        use moniker::{Embed, Nest, Scope, Var};
        use num_bigint::BigInt;
        use std::{i16, i32, i64, i8, u16, u32, u64, u8};

        use crate::syntax::core::{Head, Neutral, RcNeutral, RcTerm, Term};

        let var_bool = FreeVar::fresh_named("Bool");
        let var_true = FreeVar::fresh_named("true");
        let var_false = FreeVar::fresh_named("false");
        let var_string = FreeVar::fresh_named("String");
        let var_char = FreeVar::fresh_named("Char");
        let var_unit_ty = FreeVar::fresh_named("Unit");
        let var_unit = FreeVar::fresh_named("unit");
        let var_pos = FreeVar::fresh_named("Pos");

        let var_u8 = FreeVar::fresh_named("U8");
        let var_u16 = FreeVar::fresh_named("U16");
        let var_u32 = FreeVar::fresh_named("U32");
        let var_u64 = FreeVar::fresh_named("U64");
        let var_u16le = FreeVar::fresh_named("U16Le");
        let var_u32le = FreeVar::fresh_named("U32Le");
        let var_u64le = FreeVar::fresh_named("U64Le");
        let var_u16be = FreeVar::fresh_named("U16Be");
        let var_u32be = FreeVar::fresh_named("U32Be");
        let var_u64be = FreeVar::fresh_named("U64Be");

        let var_s8 = FreeVar::fresh_named("S8");
        let var_s16 = FreeVar::fresh_named("S16");
        let var_s32 = FreeVar::fresh_named("S32");
        let var_s64 = FreeVar::fresh_named("S64");
        let var_s16le = FreeVar::fresh_named("S16Le");
        let var_s32le = FreeVar::fresh_named("S32Le");
        let var_s64le = FreeVar::fresh_named("S64Le");
        let var_s16be = FreeVar::fresh_named("S16Be");
        let var_s32be = FreeVar::fresh_named("S32Be");
        let var_s64be = FreeVar::fresh_named("S64Be");

        let var_f32 = FreeVar::fresh_named("F32");
        let var_f64 = FreeVar::fresh_named("F64");
        let var_f32le = FreeVar::fresh_named("F32Le");
        let var_f64le = FreeVar::fresh_named("F64Le");
        let var_f32be = FreeVar::fresh_named("F32Be");
        let var_f64be = FreeVar::fresh_named("F64Be");

        let var_offset8 = FreeVar::fresh_named("Offset8");
        let var_offset16le = FreeVar::fresh_named("Offset16Le");
        let var_offset32le = FreeVar::fresh_named("Offset32Le");
        let var_offset64le = FreeVar::fresh_named("Offset64Le");
        let var_offset16be = FreeVar::fresh_named("Offset16Be");
        let var_offset32be = FreeVar::fresh_named("Offset32Be");
        let var_offset64be = FreeVar::fresh_named("Offset64Be");

        let var_array = FreeVar::fresh_named("Array");
        let var_compute = FreeVar::fresh_named("Compute");
        let var_compute_array = FreeVar::fresh_named("ComputeArray");
        let var_reserved = FreeVar::fresh_named("Reserved");
        let var_link = FreeVar::fresh_named("Link");
        let var_option = FreeVar::fresh_named("Option");
        let var_some = FreeVar::fresh_named("some");
        let var_none = FreeVar::fresh_named("none");

        fn int_ty<T: Into<BigInt>>(min: T, max: T) -> RcType {
            RcValue::from(Value::IntType(
                Some(RcValue::from(Value::Literal(Literal::Int(
                    min.into(),
                    IntFormat::Dec,
                )))),
                Some(RcValue::from(Value::Literal(Literal::Int(
                    max.into(),
                    IntFormat::Dec,
                )))),
            ))
        }

        let mut context = Context {
            resugar_env: ResugarEnv::new(),
            globals: Rc::new(Globals {
                ty_bool: RcValue::from(Value::var(Var::Free(var_bool.clone()))),
                ty_string: RcValue::from(Value::var(Var::Free(var_string.clone()))),
                ty_char: RcValue::from(Value::var(Var::Free(var_char.clone()))),
                ty_unit: RcValue::from(Value::var(Var::Free(var_unit_ty.clone()))),
                ty_pos: RcValue::from(Value::var(Var::Free(var_pos.clone()))),

                ty_u8: int_ty(u8::MIN, u8::MAX),
                ty_u16: int_ty(u16::MIN, u16::MAX),
                ty_u32: int_ty(u32::MIN, u32::MAX),
                ty_u64: int_ty(u64::MIN, u64::MAX),
                ty_u16le: RcValue::from(Value::var(Var::Free(var_u16le.clone()))),
                ty_u32le: RcValue::from(Value::var(Var::Free(var_u32le.clone()))),
                ty_u64le: RcValue::from(Value::var(Var::Free(var_u64le.clone()))),
                ty_u16be: RcValue::from(Value::var(Var::Free(var_u16be.clone()))),
                ty_u32be: RcValue::from(Value::var(Var::Free(var_u32be.clone()))),
                ty_u64be: RcValue::from(Value::var(Var::Free(var_u64be.clone()))),

                ty_s8: int_ty(i8::MIN, i8::MAX),
                ty_s16: int_ty(i16::MIN, i16::MAX),
                ty_s32: int_ty(i32::MIN, i32::MAX),
                ty_s64: int_ty(i64::MIN, i64::MAX),
                ty_s16le: RcValue::from(Value::var(Var::Free(var_s16le.clone()))),
                ty_s32le: RcValue::from(Value::var(Var::Free(var_s32le.clone()))),
                ty_s64le: RcValue::from(Value::var(Var::Free(var_s64le.clone()))),
                ty_s16be: RcValue::from(Value::var(Var::Free(var_s16be.clone()))),
                ty_s32be: RcValue::from(Value::var(Var::Free(var_s32be.clone()))),
                ty_s64be: RcValue::from(Value::var(Var::Free(var_s64be.clone()))),

                ty_f32: RcValue::from(Value::var(Var::Free(var_f32.clone()))),
                ty_f64: RcValue::from(Value::var(Var::Free(var_f64.clone()))),
                ty_f32le: RcValue::from(Value::var(Var::Free(var_f32le.clone()))),
                ty_f64le: RcValue::from(Value::var(Var::Free(var_f64le.clone()))),
                ty_f32be: RcValue::from(Value::var(Var::Free(var_f32be.clone()))),
                ty_f64be: RcValue::from(Value::var(Var::Free(var_f64be.clone()))),

                var_offset8: var_offset8.clone(),
                var_offset16le: var_offset16le.clone(),
                var_offset32le: var_offset32le.clone(),
                var_offset64le: var_offset64le.clone(),
                var_offset16be: var_offset16be.clone(),
                var_offset32be: var_offset32be.clone(),
                var_offset64be: var_offset64be.clone(),

                var_array: var_array.clone(),
                var_compute: var_compute.clone(),
                var_compute_array: var_compute_array.clone(),
                var_reserved: var_reserved.clone(),
                var_link: var_link.clone(),
                var_option: var_option.clone(),
                var_some: var_some.clone(),
                var_none: var_none.clone(),
            }),
            extern_definitions: default_extern_definitions(),
            declarations: HashMap::new(),
            definitions: HashMap::new(),
        };

        let universe0 = RcValue::from(Value::universe(0));
        let nat_ty = RcValue::from(Value::IntType(
            Some(RcValue::from(Value::Literal(Literal::Int(
                0.into(),
                IntFormat::Dec,
            )))),
            None,
        ));
        let bool_ty = context.globals.ty_bool.clone();
        let bool_lit = |value| RcTerm::from(Term::Literal(Literal::Bool(value)));
        let pos_ty = context.globals.ty_pos.clone();
        let ty_unit = context.globals.ty_unit.clone();
        let ty_unit_def = Definition::StructType(Scope::new(
            Nest::new(vec![]),
            Scope::new(Nest::new(vec![]), ()),
        ));
        let unit_def = Definition::Alias(RcTerm::from(Term::Struct(vec![])));
        let ty_u8 = RcTerm::from(Term::from(&*context.globals.ty_u8.clone()));
        let ty_u16 = RcTerm::from(Term::from(&*context.globals.ty_u16.clone()));
        let ty_u32 = RcTerm::from(Term::from(&*context.globals.ty_u32.clone()));
        let ty_u64 = RcTerm::from(Term::from(&*context.globals.ty_u64.clone()));
        let ty_s8 = RcTerm::from(Term::from(&*context.globals.ty_s8.clone()));
        let ty_s16 = RcTerm::from(Term::from(&*context.globals.ty_s16.clone()));
        let ty_s32 = RcTerm::from(Term::from(&*context.globals.ty_s32.clone()));
        let ty_s64 = RcTerm::from(Term::from(&*context.globals.ty_s64.clone()));

        // OffsetX : Pos -> Type
        let offset_ty = RcValue::from(Value::Pi(Scope::new(
            (Binder(FreeVar::fresh_unnamed()), Embed(pos_ty.clone())),
            RcValue::from(Value::Pi(Scope::new(
                (Binder(FreeVar::fresh_unnamed()), Embed(universe0.clone())),
                universe0.clone(),
            ))),
        )));

        // Array : int {0 ..} -> Type -> Type
        let array_ty = RcValue::from(Value::Pi(Scope::new(
            (Binder(FreeVar::fresh_unnamed()), Embed(nat_ty.clone())),
            RcValue::from(Value::Pi(Scope::new(
                (Binder(FreeVar::fresh_unnamed()), Embed(universe0.clone())),
                universe0.clone(),
            ))),
        )));

        // Reserved : Type -> Type
        let reserved_ty = RcValue::from(Value::Pi(Scope::new(
            (Binder(FreeVar::fresh_unnamed()), Embed(universe0.clone())),
            universe0.clone(),
        )));

        // Link : Pos -> int {0 ..} -> Type -> Type
        let link_ty = RcValue::from(Value::Pi(Scope::new(
            (Binder(FreeVar::fresh_unnamed()), Embed(pos_ty.clone())),
            RcValue::from(Value::Pi(Scope::new(
                (Binder(FreeVar::fresh_unnamed()), Embed(nat_ty.clone())),
                RcValue::from(Value::Pi(Scope::new(
                    (Binder(FreeVar::fresh_unnamed()), Embed(universe0.clone())),
                    universe0.clone(),
                ))),
            ))),
        )));

        // Compute : (A : Type) -> (Unit -> A) -> Type
        let compute_ty = {
            let var_a = FreeVar::fresh_named("A");

            // Unit -> A
            let fun_ty = RcValue::from(Value::Pi(Scope::new(
                (Binder(FreeVar::fresh_unnamed()), Embed(ty_unit.clone())),
                RcValue::from(Value::var(Var::Free(var_a.clone()))),
            )));

            RcValue::from(Value::Pi(Scope::new(
                (Binder(var_a.clone()), Embed(universe0.clone())),
                RcValue::from(Value::Pi(Scope::new(
                    (Binder(FreeVar::fresh_unnamed()), Embed(fun_ty)),
                    universe0.clone(),
                ))),
            )))
        };

        // TODO: gen_array : (len : int {0 ..}) (A : Type) -> (int {0 .. len} -> A) -> Array len A

        // ComputeArray : int {0 ..} -> (A : Type) -> (int {0 ..} -> A) -> Type
        let compute_array_ty = {
            let var_a = FreeVar::fresh_named("A");

            // int {0 .. len} -> A
            let gen_ty = RcValue::from(Value::Pi(Scope::new(
                (Binder(FreeVar::fresh_unnamed()), Embed(nat_ty.clone())),
                RcValue::from(Value::var(Var::Free(var_a.clone()))),
            )));

            RcValue::from(Value::Pi(Scope::new(
                (Binder(FreeVar::fresh_unnamed()), Embed(nat_ty.clone())),
                RcValue::from(Value::Pi(Scope::new(
                    (Binder(var_a.clone()), Embed(universe0.clone())),
                    RcValue::from(Value::Pi(Scope::new(
                        (Binder(FreeVar::fresh_unnamed()), Embed(gen_ty)),
                        universe0.clone(),
                    ))),
                ))),
            )))
        };

        // Option : Type -> Type
        let option_ty = RcValue::from(Value::Pi(Scope::new(
            (Binder(FreeVar::fresh_unnamed()), Embed(universe0.clone())),
            universe0.clone(),
        )));

        // some : (A : Type) -> A -> Option A
        let some_ty = {
            let var_a = FreeVar::fresh_named("A");
            let a_ty = RcValue::from(Value::var(Var::Free(var_a.clone())));

            // Option A
            let option_a_ty = RcValue::from(Value::Neutral(RcNeutral::from(Neutral::Head(
                Head::Var(Var::Free(var_option.clone())),
                vec![a_ty.clone()],
            ))));

            RcValue::from(Value::Pi(Scope::new(
                (Binder(var_a.clone()), Embed(universe0.clone())),
                RcValue::from(Value::Pi(Scope::new(
                    (Binder(FreeVar::fresh_unnamed()), Embed(a_ty)),
                    option_a_ty,
                ))),
            )))
        };

        // none : (A : Type) -> Option A
        let none_ty = {
            let var_a = FreeVar::fresh_named("A");
            let a_ty = RcValue::from(Value::var(Var::Free(var_a.clone())));

            // Option A
            let option_a_ty = RcValue::from(Value::Neutral(RcNeutral::from(Neutral::Head(
                Head::Var(Var::Free(var_option.clone())),
                vec![a_ty.clone()],
            ))));

            RcValue::from(Value::Pi(Scope::new(
                (Binder(var_a.clone()), Embed(universe0.clone())),
                option_a_ty,
            )))
        };

        context.insert_declaration(var_true.clone(), bool_ty.clone());
        context.insert_declaration(var_false.clone(), bool_ty.clone());
        context.insert_definition(var_true, Definition::Alias(bool_lit(true)));
        context.insert_definition(var_false, Definition::Alias(bool_lit(false)));
        context.insert_declaration(var_bool, universe0.clone());
        context.insert_declaration(var_string, universe0.clone());
        context.insert_declaration(var_char, universe0.clone());
        context.insert_declaration(var_pos, universe0.clone());

        context.insert_declaration(var_u8.clone(), universe0.clone());
        context.insert_declaration(var_u16.clone(), universe0.clone());
        context.insert_declaration(var_u32.clone(), universe0.clone());
        context.insert_declaration(var_u64.clone(), universe0.clone());
        context.insert_definition(var_u8, Definition::Alias(ty_u8.clone()));
        context.insert_definition(var_u16, Definition::Alias(ty_u16.clone()));
        context.insert_definition(var_u32, Definition::Alias(ty_u32.clone()));
        context.insert_definition(var_u64, Definition::Alias(ty_u64.clone()));
        context.insert_declaration(var_u16le, universe0.clone());
        context.insert_declaration(var_u32le, universe0.clone());
        context.insert_declaration(var_u64le, universe0.clone());
        context.insert_declaration(var_u16be, universe0.clone());
        context.insert_declaration(var_u32be, universe0.clone());
        context.insert_declaration(var_u64be, universe0.clone());

        context.insert_declaration(var_s8.clone(), universe0.clone());
        context.insert_declaration(var_s16.clone(), universe0.clone());
        context.insert_declaration(var_s32.clone(), universe0.clone());
        context.insert_declaration(var_s64.clone(), universe0.clone());
        context.insert_definition(var_s8, Definition::Alias(ty_s8.clone()));
        context.insert_definition(var_s16, Definition::Alias(ty_s16.clone()));
        context.insert_definition(var_s32, Definition::Alias(ty_s32.clone()));
        context.insert_definition(var_s64, Definition::Alias(ty_s64.clone()));
        context.insert_declaration(var_s16le, universe0.clone());
        context.insert_declaration(var_s32le, universe0.clone());
        context.insert_declaration(var_s64le, universe0.clone());
        context.insert_declaration(var_s16be, universe0.clone());
        context.insert_declaration(var_s32be, universe0.clone());
        context.insert_declaration(var_s64be, universe0.clone());

        context.insert_declaration(var_f32, universe0.clone());
        context.insert_declaration(var_f64, universe0.clone());
        context.insert_declaration(var_f32le, universe0.clone());
        context.insert_declaration(var_f64le, universe0.clone());
        context.insert_declaration(var_f32be, universe0.clone());
        context.insert_declaration(var_f64be, universe0.clone());

        context.insert_declaration(var_offset8, offset_ty.clone());
        context.insert_declaration(var_offset16le, offset_ty.clone());
        context.insert_declaration(var_offset32le, offset_ty.clone());
        context.insert_declaration(var_offset64le, offset_ty.clone());
        context.insert_declaration(var_offset16be, offset_ty.clone());
        context.insert_declaration(var_offset32be, offset_ty.clone());
        context.insert_declaration(var_offset64be, offset_ty.clone());

        context.insert_declaration(var_array, array_ty);
        context.insert_declaration(var_compute, compute_ty);
        context.insert_declaration(var_compute_array, compute_array_ty);
        context.insert_declaration(var_reserved, reserved_ty);
        context.insert_declaration(var_link, link_ty);

        context.insert_declaration(var_unit_ty.clone(), universe0.clone());
        context.insert_definition(var_unit_ty, ty_unit_def);
        context.insert_declaration(var_unit.clone(), ty_unit.clone());
        context.insert_definition(var_unit, unit_def);
        context.insert_declaration(var_option, option_ty);
        context.insert_declaration(var_some, some_ty);
        context.insert_declaration(var_none, none_ty);

        context
    }
}

fn free_var_app<'a>(free_var: &FreeVar<String>, ty: &'a RcType) -> Option<&'a [RcValue]> {
    match ty.free_var_app() {
        Some((fv, spine)) if fv == free_var => Some(spine),
        Some(_) | None => None,
    }
}

fn offset_app<'a>(free_var: &FreeVar<String>, ty: &'a RcType) -> Option<(u64, &'a RcType)> {
    free_var_app(free_var, ty).and_then(|spine| match spine {
        &[ref pos, ref elem_ty] => match **pos {
            Value::Literal(Literal::Pos(pos)) => Some((pos, elem_ty)),
            _ => None,
        },
        _ => None,
    })
}

impl Context {
    pub fn resugar_env(&self) -> &ResugarEnv {
        &self.resugar_env
    }

    pub fn mappings(&self) -> HashMap<String, FreeVar<String>> {
        self.declarations
            .iter()
            .filter_map(|(free_var, _)| {
                let pretty_name = free_var.pretty_name.as_ref()?;
                Some((pretty_name.clone(), free_var.clone()))
            })
            .collect()
    }

    pub fn bool(&self) -> &RcType {
        &self.globals.ty_bool
    }

    pub fn string(&self) -> &RcType {
        &self.globals.ty_string
    }

    pub fn char(&self) -> &RcType {
        &self.globals.ty_char
    }

    pub fn unit(&self) -> &RcType {
        &self.globals.ty_unit
    }

    pub fn pos(&self) -> &RcType {
        &self.globals.ty_pos
    }

    pub fn u8(&self) -> &RcType {
        &self.globals.ty_u8
    }

    pub fn u16(&self) -> &RcType {
        &self.globals.ty_u16
    }

    pub fn u32(&self) -> &RcType {
        &self.globals.ty_u32
    }

    pub fn u64(&self) -> &RcType {
        &self.globals.ty_u64
    }

    pub fn u16le(&self) -> &RcType {
        &self.globals.ty_u16le
    }

    pub fn u32le(&self) -> &RcType {
        &self.globals.ty_u32le
    }

    pub fn u64le(&self) -> &RcType {
        &self.globals.ty_u64le
    }

    pub fn u16be(&self) -> &RcType {
        &self.globals.ty_u16be
    }

    pub fn u32be(&self) -> &RcType {
        &self.globals.ty_u32be
    }

    pub fn u64be(&self) -> &RcType {
        &self.globals.ty_u64be
    }

    pub fn s8(&self) -> &RcType {
        &self.globals.ty_s8
    }

    pub fn s16(&self) -> &RcType {
        &self.globals.ty_s16
    }

    pub fn s32(&self) -> &RcType {
        &self.globals.ty_s32
    }

    pub fn s64(&self) -> &RcType {
        &self.globals.ty_s64
    }

    pub fn s16le(&self) -> &RcType {
        &self.globals.ty_s16le
    }

    pub fn s32le(&self) -> &RcType {
        &self.globals.ty_s32le
    }

    pub fn s64le(&self) -> &RcType {
        &self.globals.ty_s64le
    }

    pub fn s16be(&self) -> &RcType {
        &self.globals.ty_s16be
    }

    pub fn s32be(&self) -> &RcType {
        &self.globals.ty_s32be
    }

    pub fn s64be(&self) -> &RcType {
        &self.globals.ty_s64be
    }

    pub fn f32(&self) -> &RcType {
        &self.globals.ty_f32
    }

    pub fn f64(&self) -> &RcType {
        &self.globals.ty_f64
    }

    pub fn f32le(&self) -> &RcType {
        &self.globals.ty_f32le
    }

    pub fn f64le(&self) -> &RcType {
        &self.globals.ty_f64le
    }

    pub fn f32be(&self) -> &RcType {
        &self.globals.ty_f32be
    }

    pub fn f64be(&self) -> &RcType {
        &self.globals.ty_f64be
    }

    pub fn offset8<'a>(&self, ty: &'a RcType) -> Option<(u64, &'a RcType)> {
        offset_app(&self.globals.var_offset8, ty)
    }

    pub fn offset16le<'a>(&self, ty: &'a RcType) -> Option<(u64, &'a RcType)> {
        offset_app(&self.globals.var_offset16le, ty)
    }

    pub fn offset32le<'a>(&self, ty: &'a RcType) -> Option<(u64, &'a RcType)> {
        offset_app(&self.globals.var_offset32le, ty)
    }

    pub fn offset64le<'a>(&self, ty: &'a RcType) -> Option<(u64, &'a RcType)> {
        offset_app(&self.globals.var_offset64le, ty)
    }

    pub fn offset16be<'a>(&self, ty: &'a RcType) -> Option<(u64, &'a RcType)> {
        offset_app(&self.globals.var_offset16be, ty)
    }

    pub fn offset32be<'a>(&self, ty: &'a RcType) -> Option<(u64, &'a RcType)> {
        offset_app(&self.globals.var_offset32be, ty)
    }

    pub fn offset64be<'a>(&self, ty: &'a RcType) -> Option<(u64, &'a RcType)> {
        offset_app(&self.globals.var_offset64be, ty)
    }

    pub fn array<'a>(&self, ty: &'a RcType) -> Option<(&'a BigInt, &'a RcType)> {
        free_var_app(&self.globals.var_array, ty).and_then(|spine| match spine {
            &[ref len, ref elem_ty] => match **len {
                Value::Literal(Literal::Int(ref len, _)) => Some((len, elem_ty)),
                _ => None,
            },
            _ => None,
        })
    }

    pub fn compute<'a>(&self, ty: &'a RcType) -> Option<(&'a RcType, &'a RcValue)> {
        free_var_app(&self.globals.var_compute, ty).and_then(|spine| match spine {
            &[ref elem_ty, ref fun] => Some((elem_ty, fun)),
            _ => None,
        })
    }

    pub fn compute_array<'a>(
        &self,
        ty: &'a RcType,
    ) -> Option<(&'a BigInt, &'a RcType, &'a RcValue)> {
        free_var_app(&self.globals.var_compute_array, ty).and_then(|spine| match spine {
            &[ref len, ref elem_ty, ref fun] => match **len {
                Value::Literal(Literal::Int(ref len, _)) => Some((len, elem_ty, fun)),
                _ => None,
            },
            _ => None,
        })
    }

    pub fn reserved<'a>(&self, ty: &'a RcType) -> Option<&'a RcValue> {
        free_var_app(&self.globals.var_reserved, ty).and_then(|spine| match spine {
            &[ref elem_ty] => Some(elem_ty),
            _ => None,
        })
    }

    pub fn link<'a>(&self, ty: &'a RcType) -> Option<(u64, &'a BigInt, &'a RcType)> {
        free_var_app(&self.globals.var_link, ty).and_then(|spine| match spine {
            &[ref pos, ref len, ref elem_ty] => match (&**pos, &**len) {
                (&Value::Literal(Literal::Pos(pos)), &Value::Literal(Literal::Int(ref len, _))) => {
                    Some((pos, len, elem_ty))
                },
                _ => None,
            },
            _ => None,
        })
    }

    pub fn some<'a>(&self, value: &'a RcValue) -> Option<(&'a RcType, &'a RcValue)> {
        free_var_app(&self.globals.var_some, value).and_then(|spine| match spine {
            &[ref elem_ty, ref elem] => Some((elem_ty, elem)),
            _ => None,
        })
    }

    pub fn none<'a>(&self, value: &'a RcValue) -> Option<&'a RcType> {
        free_var_app(&self.globals.var_none, value).and_then(|spine| match spine {
            &[ref elem_ty] => Some(elem_ty),
            _ => None,
        })
    }

    pub fn get_declaration(&self, free_var: &FreeVar<String>) -> Option<&RcType> {
        self.declarations.get(free_var)
    }

    pub fn insert_declaration(&mut self, free_var: FreeVar<String>, ty: RcType) {
        self.resugar_env.on_binder(&Binder(free_var.clone()));
        self.declarations.insert(free_var, ty);
    }

    pub fn extend_declarations<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, RcType)>,
    {
        self.declarations.extend(iter)
    }

    pub fn get_extern_definition(&self, name: &str) -> Option<&Extern> {
        self.extern_definitions.get(name)
    }

    pub fn get_definition(&self, free_var: &FreeVar<String>) -> Option<&Definition> {
        self.definitions.get(free_var)
    }

    pub fn insert_definition(&mut self, free_var: FreeVar<String>, term: Definition) {
        self.resugar_env.on_binder(&Binder(free_var.clone()));
        self.definitions.insert(free_var, term);
    }

    pub fn extend_definitions<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, Definition)>,
    {
        self.definitions.extend(iter)
    }
}
