use im::HashMap;
use moniker::FreeVar;
use std::fmt;

use syntax::core::{Literal, RcTerm, RcType, RcValue, Spine, Value};

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

/// External functions
#[derive(Clone)]
pub struct Extern {
    /// The number of arguments to pass to the primitive during normalization
    pub arity: usize,
    /// The primitive definition to be used during normalization
    // TODO: Return a `Result` with better errors
    pub interpretation: fn(Spine) -> Result<RcValue, ()>,
}

impl fmt::Debug for Extern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Extern")
            .field("arity", &self.arity)
            .field("interpretation", &"|params| { .. }")
            .finish()
    }
}

fn default_extern_definitions() -> HashMap<&'static str, Extern> {
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

            Extern {
                arity: count!($($param_name)*),
                interpretation,
            }
        }};
    }

    hashmap!{
        "string-eq" => prim!(fn(x: String, y: String) -> bool { x == y }),
        "bool-eq" => prim!(fn(x: bool, y: bool) -> bool { x == y }),
        "char-eq" => prim!(fn(x: char, y: char) -> bool { x == y }),
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

        "f32-add" => prim!(fn(x: f32, y: f32) -> f32 { x + y }),
        "f64-add" => prim!(fn(x: f64, y: f64) -> f64 { x + y }),

        "f32-sub" => prim!(fn(x: f32, y: f32) -> f32 { x - y }),
        "f64-sub" => prim!(fn(x: f64, y: f64) -> f64 { x - y }),

        "f32-mul" => prim!(fn(x: f32, y: f32) -> f32 { x * y }),
        "f64-mul" => prim!(fn(x: f64, y: f64) -> f64 { x * y }),

        "f32-div" => prim!(fn(x: f32, y: f32) -> f32 { x / y }),
        "f64-div" => prim!(fn(x: f64, y: f64) -> f64 { x / y }),

        "char-to-string" => prim!(fn(val: char) -> String { val.to_string() }),
        "f32-to-string" => prim!(fn(val: f32) -> String { val.to_string() }),
        "f64-to-string" => prim!(fn(val: f64) -> String { val.to_string() }),

        "string-append" => prim!(fn(x: String, y: String) -> String { x.clone() + y }), // FIXME: Clone
    }
}

fn default_global_declarations() -> HashMap<&'static str, RcType> {
    use moniker::{Binder, Embed, Scope};

    let universe0 = RcValue::from(Value::universe(0));
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

    hashmap!{
        "Bool" => universe0.clone(),
        "true" => bool_ty.clone(),
        "false" => bool_ty.clone(),
        "String" => universe0.clone(),
        "Char" => universe0.clone(),

        "U8" => universe0.clone(),
        "U16" => universe0.clone(),
        "U32" => universe0.clone(),
        "U64" => universe0.clone(),
        "S8" => universe0.clone(),
        "S16" => universe0.clone(),
        "S32" => universe0.clone(),
        "S64" => universe0.clone(),

        "F32" => universe0.clone(),
        "F64" => universe0.clone(),
        "Array" => arrow(vec![nat_ty, universe0.clone()], universe0.clone()),

        // TODO: Replace these with more general compute types
        "U16Le" => universe0.clone(),
        "U32Le" => universe0.clone(),
        "U64Le" => universe0.clone(),
        "S16Le" => universe0.clone(),
        "S32Le" => universe0.clone(),
        "S64Le" => universe0.clone(),
        "F32Le" => universe0.clone(),
        "F64Le" => universe0.clone(),
        "U16Be" => universe0.clone(),
        "U32Be" => universe0.clone(),
        "U64Be" => universe0.clone(),
        "S16Be" => universe0.clone(),
        "S32Be" => universe0.clone(),
        "S64Be" => universe0.clone(),
        "F32Be" => universe0.clone(),
        "F64Be" => universe0.clone(),
    }
}

fn default_global_definitions() -> HashMap<&'static str, RcValue> {
    use num_bigint::BigInt;
    use std::{i16, i32, i64, i8, u16, u32, u64, u8};

    fn int_ty<T: Into<BigInt>>(min: Option<T>, max: Option<T>) -> RcValue {
        RcValue::from(Value::IntType(
            min.map(|x| RcValue::from(Value::Literal(Literal::Int(x.into())))),
            max.map(|x| RcValue::from(Value::Literal(Literal::Int(x.into())))),
        ))
    }

    hashmap!{
        "true" => RcValue::from(Value::Literal(Literal::Bool(true))),
        "false" => RcValue::from(Value::Literal(Literal::Bool(false))),

        "U8" => int_ty(Some(u8::MIN), Some(u8::MAX)),
        "U16" => int_ty(Some(u16::MIN), Some(u16::MAX)),
        "U32" => int_ty(Some(u32::MIN), Some(u32::MAX)),
        "U64" => int_ty(Some(u64::MIN), Some(u64::MAX)),
        "S8" => int_ty(Some(i8::MIN), Some(i8::MAX)),
        "S16" => int_ty(Some(i16::MIN), Some(i16::MAX)),
        "S32" => int_ty(Some(i32::MIN), Some(i32::MAX)),
        "S64" => int_ty(Some(i64::MIN), Some(i64::MAX)),
    }
}

/// An environment that contains declarations
pub trait DeclarationEnv: Clone {
    fn get_global_declaration(&self, name: &str) -> Option<&RcType>;
    fn get_declaration(&self, free_var: &FreeVar<String>) -> Option<&RcType>;
    fn insert_declaration(&mut self, free_var: FreeVar<String>, ty: RcType);
    fn extend_declarations<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, RcType)>;
}

/// An environment that contains definitions
pub trait DefinitionEnv: Clone {
    fn get_extern_definition(&self, name: &str) -> Option<&Extern>;
    fn get_global_definition(&self, name: &str) -> Option<&RcValue>;
    fn get_definition(&self, free_var: &FreeVar<String>) -> Option<&RcTerm>;
    fn insert_definition(&mut self, free_var: FreeVar<String>, RcTerm);
    fn extend_definitions<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, RcTerm)>;
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
pub struct TcEnv {
    /// External definitions
    extern_definitions: HashMap<&'static str, Extern>,
    /// Global declarations
    global_declarations: HashMap<&'static str, RcType>,
    /// Global definitions
    global_definitions: HashMap<&'static str, RcValue>,
    /// The type annotations of the binders we have passed over
    declarations: HashMap<FreeVar<String>, RcType>,
    /// Any definitions we have passed over
    definitions: HashMap<FreeVar<String>, RcTerm>,
}

impl Default for TcEnv {
    fn default() -> TcEnv {
        TcEnv {
            extern_definitions: default_extern_definitions(),
            global_declarations: default_global_declarations(),
            global_definitions: default_global_definitions(),
            declarations: hashmap!{},
            definitions: hashmap!{},
        }
    }
}

impl DeclarationEnv for TcEnv {
    fn get_global_declaration(&self, name: &str) -> Option<&RcType> {
        self.global_declarations.get(name)
    }

    fn get_declaration(&self, free_var: &FreeVar<String>) -> Option<&RcType> {
        self.declarations.get(free_var)
    }

    fn insert_declaration(&mut self, free_var: FreeVar<String>, ty: RcType) {
        self.declarations.insert(free_var, ty);
    }

    fn extend_declarations<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, RcType)>,
    {
        self.declarations.extend(iter)
    }
}

impl DefinitionEnv for TcEnv {
    fn get_extern_definition(&self, name: &str) -> Option<&Extern> {
        self.extern_definitions.get(name)
    }

    fn get_global_definition(&self, name: &str) -> Option<&RcValue> {
        self.global_definitions.get(name)
    }

    fn get_definition(&self, free_var: &FreeVar<String>) -> Option<&RcTerm> {
        self.definitions.get(free_var)
    }

    fn insert_definition(&mut self, free_var: FreeVar<String>, term: RcTerm) {
        self.definitions.insert(free_var, term);
    }

    fn extend_definitions<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, RcTerm)>,
    {
        self.definitions.extend(iter)
    }
}
