use im::HashMap;
use moniker::{Binder, FreeVar};
use std::fmt;
use std::rc::Rc;

use syntax::core::{Definition, Literal, RcType, RcValue, Spine, Value};
use syntax::translation::ResugarEnv;

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

#[derive(Clone, Debug)]
pub struct Globals {
    pub bool: FreeVar<String>,
    pub true_: FreeVar<String>,
    pub false_: FreeVar<String>,
    pub string: FreeVar<String>,
    pub char: FreeVar<String>,
    pub u8: FreeVar<String>,
    pub u16: FreeVar<String>,
    pub u32: FreeVar<String>,
    pub u64: FreeVar<String>,
    pub s8: FreeVar<String>,
    pub s16: FreeVar<String>,
    pub s32: FreeVar<String>,
    pub s64: FreeVar<String>,
    pub f32: FreeVar<String>,
    pub f64: FreeVar<String>,
    pub array: FreeVar<String>,
    pub u16le: FreeVar<String>,
    pub u32le: FreeVar<String>,
    pub u64le: FreeVar<String>,
    pub s16le: FreeVar<String>,
    pub s32le: FreeVar<String>,
    pub s64le: FreeVar<String>,
    pub f32le: FreeVar<String>,
    pub f64le: FreeVar<String>,
    pub u16be: FreeVar<String>,
    pub u32be: FreeVar<String>,
    pub u64be: FreeVar<String>,
    pub s16be: FreeVar<String>,
    pub s32be: FreeVar<String>,
    pub s64be: FreeVar<String>,
    pub f32be: FreeVar<String>,
    pub f64be: FreeVar<String>,
}

impl Default for Globals {
    fn default() -> Globals {
        Globals {
            bool: FreeVar::fresh_named("Bool"),
            true_: FreeVar::fresh_named("true"),
            false_: FreeVar::fresh_named("false"),
            string: FreeVar::fresh_named("String"),
            char: FreeVar::fresh_named("Char"),
            u8: FreeVar::fresh_named("U8"),
            u16: FreeVar::fresh_named("U16"),
            u32: FreeVar::fresh_named("U32"),
            u64: FreeVar::fresh_named("U64"),
            s8: FreeVar::fresh_named("S8"),
            s16: FreeVar::fresh_named("S16"),
            s32: FreeVar::fresh_named("S32"),
            s64: FreeVar::fresh_named("S64"),
            f32: FreeVar::fresh_named("F32"),
            f64: FreeVar::fresh_named("F64"),
            array: FreeVar::fresh_named("Array"),
            // TODO: Replace these with more general compute types
            u16le: FreeVar::fresh_named("U16Le"),
            u32le: FreeVar::fresh_named("U32Le"),
            u64le: FreeVar::fresh_named("U64Le"),
            s16le: FreeVar::fresh_named("S16Le"),
            s32le: FreeVar::fresh_named("S32Le"),
            s64le: FreeVar::fresh_named("S64Le"),
            f32le: FreeVar::fresh_named("F32Le"),
            f64le: FreeVar::fresh_named("F64Le"),
            u16be: FreeVar::fresh_named("U16Be"),
            u32be: FreeVar::fresh_named("U32Be"),
            u64be: FreeVar::fresh_named("U64Be"),
            s16be: FreeVar::fresh_named("S16Be"),
            s32be: FreeVar::fresh_named("S32Be"),
            s64be: FreeVar::fresh_named("S64Be"),
            f32be: FreeVar::fresh_named("F32Be"),
            f64be: FreeVar::fresh_named("F64Be"),
        }
    }
}

pub trait GlobalEnv: Clone {
    fn resugar_env(&self) -> &ResugarEnv;
    fn globals(&self) -> &Globals;
}

/// An environment that contains declarations
pub trait DeclarationEnv: GlobalEnv {
    fn get_declaration(&self, free_var: &FreeVar<String>) -> Option<&RcType>;
    fn insert_declaration(&mut self, free_var: FreeVar<String>, ty: RcType);
    fn extend_declarations<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, RcType)>;
}

/// An environment that contains definitions
pub trait DefinitionEnv: GlobalEnv {
    fn get_extern_definition(&self, name: &str) -> Option<&Extern>;
    fn get_definition(&self, free_var: &FreeVar<String>) -> Option<&Definition>;
    fn insert_definition(&mut self, free_var: FreeVar<String>, Definition);
    fn extend_definitions<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, Definition)>;
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

impl TcEnv {
    pub fn mappings(&self) -> HashMap<String, FreeVar<String>> {
        self.declarations
            .iter()
            .filter_map(|(free_var, _)| {
                let pretty_name = free_var.pretty_name.as_ref()?;
                Some((pretty_name.clone(), free_var.clone()))
            }).collect()
    }
}

impl Default for TcEnv {
    fn default() -> TcEnv {
        use moniker::{Embed, Scope, Var};
        use num_bigint::BigInt;
        use std::{i16, i32, i64, i8, u16, u32, u64, u8};

        use syntax::core::{RcTerm, Term};

        let mut tc_env = TcEnv {
            resugar_env: ResugarEnv::new(),
            globals: Rc::new(Globals::default()),
            extern_definitions: default_extern_definitions(),
            declarations: HashMap::new(),
            definitions: HashMap::new(),
        };

        let var_bool = tc_env.globals.bool.clone();
        let var_true_ = tc_env.globals.true_.clone();
        let var_false_ = tc_env.globals.false_.clone();
        let var_string = tc_env.globals.string.clone();
        let var_char = tc_env.globals.char.clone();
        let var_u8 = tc_env.globals.u8.clone();
        let var_u16 = tc_env.globals.u16.clone();
        let var_u32 = tc_env.globals.u32.clone();
        let var_u64 = tc_env.globals.u64.clone();
        let var_s8 = tc_env.globals.s8.clone();
        let var_s16 = tc_env.globals.s16.clone();
        let var_s32 = tc_env.globals.s32.clone();
        let var_s64 = tc_env.globals.s64.clone();
        let var_f32 = tc_env.globals.f32.clone();
        let var_f64 = tc_env.globals.f64.clone();
        let var_array = tc_env.globals.array.clone();
        let var_u16le = tc_env.globals.u16le.clone();
        let var_u32le = tc_env.globals.u32le.clone();
        let var_u64le = tc_env.globals.u64le.clone();
        let var_s16le = tc_env.globals.s16le.clone();
        let var_s32le = tc_env.globals.s32le.clone();
        let var_s64le = tc_env.globals.s64le.clone();
        let var_f32le = tc_env.globals.f32le.clone();
        let var_f64le = tc_env.globals.f64le.clone();
        let var_u16be = tc_env.globals.u16be.clone();
        let var_u32be = tc_env.globals.u32be.clone();
        let var_u64be = tc_env.globals.u64be.clone();
        let var_s16be = tc_env.globals.s16be.clone();
        let var_s32be = tc_env.globals.s32be.clone();
        let var_s64be = tc_env.globals.s64be.clone();
        let var_f32be = tc_env.globals.f32be.clone();
        let var_f64be = tc_env.globals.f64be.clone();

        let universe0 = RcValue::from(Value::universe(0));
        let nat_ty = RcValue::from(Value::IntType(
            Some(RcValue::from(Value::Literal(Literal::Int(0.into())))),
            None,
        ));
        let bool_ty = RcValue::from(Value::from(Var::Free(var_bool.clone())));
        let bool_lit = |value| RcTerm::from(Term::Literal(Literal::Bool(value)));
        let array_ty = RcValue::from(Value::Pi(Scope::new(
            (Binder(FreeVar::fresh_unnamed()), Embed(nat_ty)),
            RcValue::from(Value::Pi(Scope::new(
                (Binder(FreeVar::fresh_unnamed()), Embed(universe0.clone())),
                universe0.clone(),
            ))),
        )));

        fn int_ty<T: Into<BigInt>>(min: T, max: T) -> RcTerm {
            RcTerm::from(Term::IntType(
                Some(RcTerm::from(Term::Literal(Literal::Int(min.into())))),
                Some(RcTerm::from(Term::Literal(Literal::Int(max.into())))),
            ))
        }

        tc_env.insert_declaration(var_bool, universe0.clone());
        tc_env.insert_declaration(var_true_.clone(), bool_ty.clone());
        tc_env.insert_declaration(var_false_.clone(), bool_ty.clone());
        tc_env.insert_declaration(var_string, universe0.clone());
        tc_env.insert_declaration(var_char, universe0.clone());
        tc_env.insert_declaration(var_u8.clone(), universe0.clone());
        tc_env.insert_declaration(var_u16.clone(), universe0.clone());
        tc_env.insert_declaration(var_u32.clone(), universe0.clone());
        tc_env.insert_declaration(var_u64.clone(), universe0.clone());
        tc_env.insert_declaration(var_s8.clone(), universe0.clone());
        tc_env.insert_declaration(var_s16.clone(), universe0.clone());
        tc_env.insert_declaration(var_s32.clone(), universe0.clone());
        tc_env.insert_declaration(var_s64.clone(), universe0.clone());
        tc_env.insert_declaration(var_f32, universe0.clone());
        tc_env.insert_declaration(var_f64, universe0.clone());
        tc_env.insert_declaration(var_array, array_ty);
        tc_env.insert_declaration(var_u16le, universe0.clone());
        tc_env.insert_declaration(var_u32le, universe0.clone());
        tc_env.insert_declaration(var_u64le, universe0.clone());
        tc_env.insert_declaration(var_s16le, universe0.clone());
        tc_env.insert_declaration(var_s32le, universe0.clone());
        tc_env.insert_declaration(var_s64le, universe0.clone());
        tc_env.insert_declaration(var_f32le, universe0.clone());
        tc_env.insert_declaration(var_f64le, universe0.clone());
        tc_env.insert_declaration(var_u16be, universe0.clone());
        tc_env.insert_declaration(var_u32be, universe0.clone());
        tc_env.insert_declaration(var_u64be, universe0.clone());
        tc_env.insert_declaration(var_s16be, universe0.clone());
        tc_env.insert_declaration(var_s32be, universe0.clone());
        tc_env.insert_declaration(var_s64be, universe0.clone());
        tc_env.insert_declaration(var_f32be, universe0.clone());
        tc_env.insert_declaration(var_f64be, universe0.clone());

        tc_env.insert_definition(var_true_, Definition::Alias(bool_lit(true)));
        tc_env.insert_definition(var_false_, Definition::Alias(bool_lit(false)));
        tc_env.insert_definition(var_u8, Definition::Alias(int_ty(u8::MIN, u8::MAX)));
        tc_env.insert_definition(var_u16, Definition::Alias(int_ty(u16::MIN, u16::MAX)));
        tc_env.insert_definition(var_u32, Definition::Alias(int_ty(u32::MIN, u32::MAX)));
        tc_env.insert_definition(var_u64, Definition::Alias(int_ty(u64::MIN, u64::MAX)));
        tc_env.insert_definition(var_s8, Definition::Alias(int_ty(i8::MIN, i8::MAX)));
        tc_env.insert_definition(var_s16, Definition::Alias(int_ty(i16::MIN, i16::MAX)));
        tc_env.insert_definition(var_s32, Definition::Alias(int_ty(i32::MIN, i32::MAX)));
        tc_env.insert_definition(var_s64, Definition::Alias(int_ty(i64::MIN, i64::MAX)));

        tc_env
    }
}

impl GlobalEnv for TcEnv {
    fn resugar_env(&self) -> &ResugarEnv {
        &self.resugar_env
    }

    fn globals(&self) -> &Globals {
        &self.globals
    }
}

impl DeclarationEnv for TcEnv {
    fn get_declaration(&self, free_var: &FreeVar<String>) -> Option<&RcType> {
        self.declarations.get(free_var)
    }

    fn insert_declaration(&mut self, free_var: FreeVar<String>, ty: RcType) {
        self.resugar_env.on_binder(&Binder(free_var.clone()));
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

    fn get_definition(&self, free_var: &FreeVar<String>) -> Option<&Definition> {
        self.definitions.get(free_var)
    }

    fn insert_definition(&mut self, free_var: FreeVar<String>, term: Definition) {
        self.resugar_env.on_binder(&Binder(free_var.clone()));
        self.definitions.insert(free_var, term);
    }

    fn extend_definitions<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, Definition)>,
    {
        self.definitions.extend(iter)
    }
}
