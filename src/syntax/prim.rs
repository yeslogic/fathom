//! Primitive operations

use nameless::{FreeVar, Var};
use std::fmt;
use std::rc::Rc;

use syntax::core::{Literal, Type, Value};

// Some helper traits for marshalling between Rust and Pikelet values
//
// I'm not super happy with the API at the moment, so these are currently private

trait IntoValue {
    fn into_value(self) -> Rc<Value>;
}

trait TryFromValueRef {
    fn try_from_value_ref(src: &Value) -> Result<&Self, ()>;
}

trait HasType {
    fn ty() -> Rc<Type>;
}

macro_rules! impl_into_value {
    ($T:ty, $Variant:ident) => {
        impl IntoValue for $T {
            fn into_value(self) -> Rc<Value> {
                Rc::new(Value::Literal(Literal::$Variant(self)))
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

macro_rules! impl_has_ty {
    ($T:ty, $ty_name:expr) => {
        impl HasType for $T {
            fn ty() -> Rc<Type> {
                Rc::new(Value::from(Var::Free(FreeVar::user($ty_name))))
            }
        }
    };
}

impl_has_ty!(String, "String");
impl_has_ty!(char, "Char");
impl_has_ty!(bool, "Bool");
impl_has_ty!(f32, "F32");
impl_has_ty!(f64, "F64");

// TODO: Return a `Result` with better errors
pub type NormFn = fn(&[Rc<Value>]) -> Result<Rc<Value>, ()>;

/// Primitive functions
#[derive(Clone)]
pub struct PrimFn {
    /// A name to be used when translating the primitive to the target language
    /// during compilation
    pub name: String,
    /// The number of arguments to pass to the primitive during normalization
    pub arity: usize,
    /// The type of the primitive
    pub ann: Rc<Type>,
    /// The primitive definition to be used during normalization
    pub fun: NormFn,
}

impl fmt::Debug for PrimFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("PrimFn")
            .field("name", &self.name)
            .field("arity", &self.arity)
            .field("ann", &self.ann)
            .field("fun", &"|params| { .. }")
            .finish()
    }
}

/// Boilerplate macro for counting the number of supplied token trees
macro_rules! count {
    () => (0_usize);
    ( $x:tt $($xs:tt)* ) => (1_usize + count!($($xs)*));
}

/// Define a primitive function
macro_rules! def_prim {
    ($id:ident, $name:expr,fn($($param_name:ident : $PType:ty),*) -> $RType:ty $body:block) => {
        pub fn $id() -> PrimFn {
            use nameless::{Embed, FreeVar, Scope};

            fn fun(params: &[Rc<Value>]) -> Result<Rc<Value>, ()> {
                match params[..] {
                    [$(ref $param_name),*] => {
                        $(let $param_name = <$PType>::try_from_value_ref($param_name)?;)*
                        Ok(<$RType>::into_value($body))
                    },
                    _ => Err(()) // TODO: Better errors
                }
            }

            let name = $name.to_string();
            let arity = count!($($param_name)*);
            let mut ann = <$RType>::ty();
            $(ann = Rc::new(Value::Pi(Scope::new(
                (FreeVar::user(stringify!($param_name)), Embed(<$PType>::ty())),
                ann
            )));)+

            PrimFn { name, arity, ann, fun }
        }
    };
}

// Primitive functions
//
// These are included in the default context, which can be created by via the
// `Context::default` function.

def_prim!(string_eq, "prim-string-eq", fn(x: String, y: String) -> bool { x == y });
def_prim!(bool_eq, "prim-bool-eq", fn(x: bool, y: bool) -> bool { x == y });
def_prim!(char_eq, "prim-char-eq", fn(x: char, y: char) -> bool { x == y });
def_prim!(f32_eq, "prim-f32-eq", fn(x: f32, y: f32) -> bool { f32::eq(x, y) });
def_prim!(f64_eq, "prim-f64-eq", fn(x: f64, y: f64) -> bool { f64::eq(x, y) });

def_prim!(string_ne, "prim-string-ne", fn(x: String, y: String) -> bool { x != y });
def_prim!(bool_ne, "prim-bool-ne", fn(x: bool, y: bool) -> bool { x != y });
def_prim!(char_ne, "prim-char-ne", fn(x: char, y: char) -> bool { x != y });
def_prim!(f32_ne, "prim-f32-ne", fn(x: f32, y: f32) -> bool { f32::ne(x, y) });
def_prim!(f64_ne, "prim-f64-ne", fn(x: f64, y: f64) -> bool { f64::ne(x, y) });

def_prim!(string_le, "prim-string-le", fn(x: String, y: String) -> bool { x <= y });
def_prim!(bool_le, "prim-bool-le", fn(x: bool, y: bool) -> bool { x <= y });
def_prim!(char_le, "prim-char-le", fn(x: char, y: char) -> bool { x <= y });
def_prim!(f32_le, "prim-f32-le", fn(x: f32, y: f32) -> bool { x <= y });
def_prim!(f64_le, "prim-f64-le", fn(x: f64, y: f64) -> bool { x <= y });

def_prim!(string_lt, "prim-string-lt", fn(x: String, y: String) -> bool { x < y });
def_prim!(bool_lt, "prim-bool-lt", fn(x: bool, y: bool) -> bool { x < y });
def_prim!(char_lt, "prim-char-lt", fn(x: char, y: char) -> bool { x < y });
def_prim!(f32_lt, "prim-f32-lt", fn(x: f32, y: f32) -> bool { x < y });
def_prim!(f64_lt, "prim-f64-lt", fn(x: f64, y: f64) -> bool { x < y });

def_prim!(string_gt, "prim-string-gt", fn(x: String, y: String) -> bool { x > y });
def_prim!(bool_gt, "prim-bool-gt", fn(x: bool, y: bool) -> bool { x > y });
def_prim!(char_gt, "prim-char-gt", fn(x: char, y: char) -> bool { x > y });
def_prim!(f32_gt, "prim-f32-gt", fn(x: f32, y: f32) -> bool { x > y });
def_prim!(f64_gt, "prim-f64-gt", fn(x: f64, y: f64) -> bool { x > y });

def_prim!(string_ge, "prim-string-ge", fn(x: String, y: String) -> bool { x >= y });
def_prim!(bool_ge, "prim-bool-ge", fn(x: bool, y: bool) -> bool { x >= y });
def_prim!(char_ge, "prim-char-ge", fn(x: char, y: char) -> bool { x >= y });
def_prim!(f32_ge, "prim-f32-ge", fn(x: f32, y: f32) -> bool { x >= y });
def_prim!(f64_ge, "prim-f64-ge", fn(x: f64, y: f64) -> bool { x >= y });

def_prim!(f32_add, "prim-f32-add", fn(x: f32, y: f32) -> f32 { x + y });
def_prim!(f64_add, "prim-f64-add", fn(x: f64, y: f64) -> f64 { x + y });

def_prim!(f32_sub, "prim-f32-sub", fn(x: f32, y: f32) -> f32 { x - y });
def_prim!(f64_sub, "prim-f64-sub", fn(x: f64, y: f64) -> f64 { x - y });

def_prim!(f32_mul, "prim-f32-mul", fn(x: f32, y: f32) -> f32 { x * y });
def_prim!(f64_mul, "prim-f64-mul", fn(x: f64, y: f64) -> f64 { x * y });

def_prim!(f32_div, "prim-f32-div", fn(x: f32, y: f32) -> f32 { x / y });
def_prim!(f64_div, "prim-f64-div", fn(x: f64, y: f64) -> f64 { x / y });

def_prim!(char_to_string, "prim-char-to-string", fn(val: char) -> String { val.to_string() });
def_prim!(f32_to_string, "prim-f32-to-string", fn(val: f32) -> String { val.to_string() });
def_prim!(f64_to_string, "prim-f64-to-string", fn(val: f64) -> String { val.to_string() });

def_prim!(string_append, "prim-string-append", fn(x: String, y: String) -> String { x.clone() + y }); // FIXME: Clone
