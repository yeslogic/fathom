//! Primitive operations

use im::HashMap;
use std::fmt;

use syntax::core::{Literal, RcValue, Spine, Value};

// Some helper traits for marshalling between Rust and Pikelet values
//
// I'm not super happy with the API at the moment, so these are currently private

trait IntoValue {
    fn into_value(self) -> RcValue;
}

trait TryFromValueRef {
    fn try_from_value_ref(src: &Value) -> Result<&Self, ()>;
}

macro_rules! impl_into_value {
    ($T:ty, $Variant:ident) => {
        impl IntoValue for $T {
            fn into_value(self) -> RcValue {
                RcValue::from(Value::Literal(Literal::$Variant(self)))
            }
        }
    };
}

impl_into_value!(String, String);
impl_into_value!(char, Char);
impl_into_value!(bool, Bool);
impl_into_value!(f32, F32);
impl_into_value!(f64, F64);

macro_rules! impl_try_from_value_ref {
    ($T:ty, $Variant:ident) => {
        impl TryFromValueRef for $T {
            fn try_from_value_ref(src: &Value) -> Result<&Self, ()> {
                match *src {
                    Value::Literal(Literal::$Variant(ref x)) => Ok(x),
                    _ => Err(()),
                }
            }
        }
    };
}

impl_try_from_value_ref!(String, String);
impl_try_from_value_ref!(char, Char);
impl_try_from_value_ref!(bool, Bool);
impl_try_from_value_ref!(f32, F32);
impl_try_from_value_ref!(f64, F64);

/// Primitive functions
#[derive(Clone)]
pub struct PrimFn {
    /// The number of arguments to pass to the primitive during normalization
    pub arity: usize,
    /// The primitive definition to be used during normalization
    // TODO: Return a `Result` with better errors
    pub interpretation: fn(Spine) -> Result<RcValue, ()>,
}

impl fmt::Debug for PrimFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("PrimFn")
            .field("arity", &self.arity)
            .field("interpretation", &"|params| { .. }")
            .finish()
    }
}

#[derive(Clone, Debug)]
pub struct PrimEnv {
    definitions: HashMap<String, PrimFn>,
}

impl PrimEnv {
    pub fn get(&self, name: &str) -> Option<&PrimFn> {
        self.definitions.get(name)
    }
}

impl Default for PrimEnv {
    fn default() -> PrimEnv {
        /// Boilerplate macro for counting the number of supplied token trees
        macro_rules! count {
            () => (0_usize);
            ( $x:tt $($xs:tt)* ) => (1_usize + count!($($xs)*));
        }

        /// Define a primitive function
        macro_rules! prim {
            (fn($($param_name:ident : $PType:ty),*) -> $RType:ty $body:block) => {{
                fn interpretation(params: Spine) -> Result<RcValue, ()> {
                    if params.len() == count!($($param_name)*) {
                        let mut arg_index = 0;
                        $(
                            arg_index += 1;
                            let $param_name = <$PType>::try_from_value_ref(&params[arg_index - 1])?;
                        )*
                        Ok(<$RType>::into_value($body))
                    } else {
                        Err(()) // TODO: Better errors
                    }
                }

                PrimFn {
                    arity: count!($($param_name)*),
                    interpretation,
                }
            }};
        }

        let definitions = hashmap!{
            "string-eq".to_string() => prim!(fn(x: String, y: String) -> bool { x == y }),
            "bool-eq".to_string() => prim!(fn(x: bool, y: bool) -> bool { x == y }),
            "char-eq".to_string() => prim!(fn(x: char, y: char) -> bool { x == y }),
            "f32-eq".to_string() => prim!(fn(x: f32, y: f32) -> bool { f32::eq(x, y) }),
            "f64-eq".to_string() => prim!(fn(x: f64, y: f64) -> bool { f64::eq(x, y) }),

            "string-ne".to_string() => prim!(fn(x: String, y: String) -> bool { x != y }),
            "bool-ne".to_string() => prim!(fn(x: bool, y: bool) -> bool { x != y }),
            "char-ne".to_string() => prim!(fn(x: char, y: char) -> bool { x != y }),
            "f32-ne".to_string() => prim!(fn(x: f32, y: f32) -> bool { f32::ne(x, y) }),
            "f64-ne".to_string() => prim!(fn(x: f64, y: f64) -> bool { f64::ne(x, y) }),

            "string-le".to_string() => prim!(fn(x: String, y: String) -> bool { x <= y }),
            "bool-le".to_string() => prim!(fn(x: bool, y: bool) -> bool { x <= y }),
            "char-le".to_string() => prim!(fn(x: char, y: char) -> bool { x <= y }),
            "f32-le".to_string() => prim!(fn(x: f32, y: f32) -> bool { x <= y }),
            "f64-le".to_string() => prim!(fn(x: f64, y: f64) -> bool { x <= y }),

            "string-lt".to_string() => prim!(fn(x: String, y: String) -> bool { x < y }),
            "bool-lt".to_string() => prim!(fn(x: bool, y: bool) -> bool { x < y }),
            "char-lt".to_string() => prim!(fn(x: char, y: char) -> bool { x < y }),
            "f32-lt".to_string() => prim!(fn(x: f32, y: f32) -> bool { x < y }),
            "f64-lt".to_string() => prim!(fn(x: f64, y: f64) -> bool { x < y }),

            "string-gt".to_string() => prim!(fn(x: String, y: String) -> bool { x > y }),
            "bool-gt".to_string() => prim!(fn(x: bool, y: bool) -> bool { x > y }),
            "char-gt".to_string() => prim!(fn(x: char, y: char) -> bool { x > y }),
            "f32-gt".to_string() => prim!(fn(x: f32, y: f32) -> bool { x > y }),
            "f64-gt".to_string() => prim!(fn(x: f64, y: f64) -> bool { x > y }),

            "string-ge".to_string() => prim!(fn(x: String, y: String) -> bool { x >= y }),
            "bool-ge".to_string() => prim!(fn(x: bool, y: bool) -> bool { x >= y }),
            "char-ge".to_string() => prim!(fn(x: char, y: char) -> bool { x >= y }),
            "f32-ge".to_string() => prim!(fn(x: f32, y: f32) -> bool { x >= y }),
            "f64-ge".to_string() => prim!(fn(x: f64, y: f64) -> bool { x >= y }),

            "f32-add".to_string() => prim!(fn(x: f32, y: f32) -> f32 { x + y }),
            "f64-add".to_string() => prim!(fn(x: f64, y: f64) -> f64 { x + y }),

            "f32-sub".to_string() => prim!(fn(x: f32, y: f32) -> f32 { x - y }),
            "f64-sub".to_string() => prim!(fn(x: f64, y: f64) -> f64 { x - y }),

            "f32-mul".to_string() => prim!(fn(x: f32, y: f32) -> f32 { x * y }),
            "f64-mul".to_string() => prim!(fn(x: f64, y: f64) -> f64 { x * y }),

            "f32-div".to_string() => prim!(fn(x: f32, y: f32) -> f32 { x / y }),
            "f64-div".to_string() => prim!(fn(x: f64, y: f64) -> f64 { x / y }),

            "char-to-string".to_string() => prim!(fn(val: char) -> String { val.to_string() }),
            "f32-to-string".to_string() => prim!(fn(val: f32) -> String { val.to_string() }),
            "f64-to-string".to_string() => prim!(fn(val: f64) -> String { val.to_string() }),

            "string-append".to_string() => prim!(fn(x: String, y: String) -> String { x.clone() + y }), // FIXME: Clone
        };

        PrimEnv { definitions }
    }
}
