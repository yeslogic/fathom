use im::Vector;
use moniker::FreeVar;
use std::fmt;
use std::rc::Rc;

use syntax::core::{RcTerm, RcType, Term};
use syntax::pretty::{self, ToDoc};
use syntax::prim::{self, PrimFn};

/// An entry in the context
#[derive(Debug, Clone)]
pub enum Entry {
    /// A type claim
    Claim(FreeVar<String>, RcType),
    /// A value definition
    Definition(FreeVar<String>, Definition),
}

#[derive(Debug, Clone)]
pub enum Definition {
    Term(RcTerm),
    Prim(Rc<PrimFn>),
}

/// A list of binders that have been accumulated during type checking
///
/// We use a persistent vector internally so that we don't need to deal with the
/// error-prone tedium of dealing with a mutable context when entering and
/// exiting binders.
#[derive(Clone)]
pub struct Context {
    pub entries: Vector<Entry>,
}

impl Context {
    /// Create a new, empty context
    pub fn new() -> Context {
        Context {
            entries: Vector::new(),
        }
    }

    pub fn claim(&self, name: FreeVar<String>, ann: RcType) -> Context {
        let mut entries = self.entries.clone();
        entries.push_front(Entry::Claim(name, ann));
        Context { entries }
    }

    pub fn define_term(&self, name: FreeVar<String>, ann: RcType, term: RcTerm) -> Context {
        let mut entries = self.entries.clone();
        entries.push_front(Entry::Claim(name.clone(), ann));
        entries.push_front(Entry::Definition(name, Definition::Term(term)));
        Context { entries }
    }

    fn define_prim(&self, name: FreeVar<String>, prim: Rc<PrimFn>) -> Context {
        let mut entries = self.entries.clone();
        entries.push_front(Entry::Claim(name.clone(), prim.ann.clone()));
        entries.push_front(Entry::Definition(name, Definition::Prim(prim)));
        Context { entries }
    }

    pub fn lookup_claim(&self, name: &FreeVar<String>) -> Option<RcType> {
        self.entries
            .iter()
            .filter_map(|entry| match *entry {
                Entry::Claim(ref n, ref ty) if n == name => Some(ty.clone()),
                Entry::Claim(_, _) | Entry::Definition(_, _) => None,
            })
            .next()
    }

    pub fn lookup_definition(&self, name: &FreeVar<String>) -> Option<Definition> {
        self.entries
            .iter()
            .filter_map(|entry| match *entry {
                Entry::Definition(ref n, ref def) if n == name => Some(def.clone()),
                Entry::Definition(_, _) | Entry::Claim(_, _) => None,
            })
            .next()
    }
}

impl Default for Context {
    fn default() -> Context {
        use moniker::{Binder, Embed, GenId, Scope, Var};
        use num_bigint::BigInt;

        use syntax::core::{Literal, RcTerm, RcValue, Value};
        use syntax::Level;

        fn int_ty<T: Into<BigInt>>(min: Option<T>, max: Option<T>) -> RcTerm {
            RcTerm::from(Term::IntType(
                min.map(|x| RcTerm::from(Term::Literal(Literal::Int(x.into())))),
                max.map(|x| RcTerm::from(Term::Literal(Literal::Int(x.into())))),
            ))
        }

        let name = FreeVar::user;
        let fresh_binder = || Binder(FreeVar::from(GenId::fresh()));
        let free_var = |n| RcValue::from(Value::from(Var::Free(name(n))));
        let universe0 = RcValue::from(Value::Universe(Level(0)));
        let bool_lit = |val| RcTerm::from(Term::Literal(Literal::Bool(val)));
        let arrow = |params: Vec<RcType>, ret: RcType| {
            params.into_iter().rev().fold(ret, |body, ann| {
                RcValue::from(Value::Pi(Scope::new((fresh_binder(), Embed(ann)), body)))
            })
        };

        #[cfg_attr(rustfmt, rustfmt_skip)]
        Context::new()
            .claim(name("Bool"), universe0.clone())
            .define_term(name("true"), free_var("Bool"), bool_lit(true))
            .define_term(name("false"), free_var("Bool"), bool_lit(false))
            .claim(name("String"), universe0.clone())
            .claim(name("Char"), universe0.clone())

            .define_term(name("U8"), universe0.clone(), int_ty(Some(u8::min_value()), Some(u8::max_value())))
            .define_term(name("U16"), universe0.clone(), int_ty(Some(u16::min_value()), Some(u16::max_value())))
            .define_term(name("U32"), universe0.clone(), int_ty(Some(u32::min_value()), Some(u32::max_value())))
            .define_term(name("U64"), universe0.clone(), int_ty(Some(u64::min_value()), Some(u64::max_value())))
            .define_term(name("S8"), universe0.clone(), int_ty(Some(i8::min_value()), Some(i8::max_value())))
            .define_term(name("S16"), universe0.clone(), int_ty(Some(i16::min_value()), Some(i16::max_value())))
            .define_term(name("S32"), universe0.clone(), int_ty(Some(i32::min_value()), Some(i32::max_value())))
            .define_term(name("S64"), universe0.clone(), int_ty(Some(i64::min_value()), Some(i64::max_value())))

            .claim(name("F32"), universe0.clone())
            .claim(name("F64"), universe0.clone())
            .claim(name("Array"), arrow(
                vec![
                    RcValue::from(Value::IntType(
                        Some(RcValue::from(Value::Literal(Literal::Int(0.into())))),
                        None,
                    )),
                    universe0.clone(),
                ],
                universe0.clone(),
            ))

            // TODO: Replace these with more general compute types
            .claim(name("U16Le"), universe0.clone())
            .claim(name("U32Le"), universe0.clone())
            .claim(name("U64Le"), universe0.clone())
            .claim(name("S16Le"), universe0.clone())
            .claim(name("S32Le"), universe0.clone())
            .claim(name("S64Le"), universe0.clone())
            .claim(name("F32Le"), universe0.clone())
            .claim(name("F64Le"), universe0.clone())
            .claim(name("U16Be"), universe0.clone())
            .claim(name("U32Be"), universe0.clone())
            .claim(name("U64Be"), universe0.clone())
            .claim(name("S16Be"), universe0.clone())
            .claim(name("S32Be"), universe0.clone())
            .claim(name("S64Be"), universe0.clone())
            .claim(name("F32Be"), universe0.clone())
            .claim(name("F64Be"), universe0.clone())

            .define_prim(name("prim-string-eq"), Rc::new(prim::string_eq()))
            .define_prim(name("prim-bool-eq"), Rc::new(prim::bool_eq()))
            .define_prim(name("prim-char-eq"), Rc::new(prim::char_eq()))
            .define_prim(name("prim-f32-eq"), Rc::new(prim::f32_eq()))
            .define_prim(name("prim-f64-eq"), Rc::new(prim::f64_eq()))
            .define_prim(name("prim-string-ne"), Rc::new(prim::string_ne()))
            .define_prim(name("prim-bool-ne"), Rc::new(prim::bool_ne()))
            .define_prim(name("prim-char-ne"), Rc::new(prim::char_ne()))
            .define_prim(name("prim-f32-ne"), Rc::new(prim::f32_ne()))
            .define_prim(name("prim-f64-ne"), Rc::new(prim::f64_ne()))
            .define_prim(name("prim-string-le"), Rc::new(prim::string_le()))
            .define_prim(name("prim-bool-le"), Rc::new(prim::bool_le()))
            .define_prim(name("prim-char-le"), Rc::new(prim::char_le()))
            .define_prim(name("prim-f32-le"), Rc::new(prim::f32_le()))
            .define_prim(name("prim-f64-le"), Rc::new(prim::f64_le()))
            .define_prim(name("prim-string-lt"), Rc::new(prim::string_lt()))
            .define_prim(name("prim-bool-lt"), Rc::new(prim::bool_lt()))
            .define_prim(name("prim-char-lt"), Rc::new(prim::char_lt()))
            .define_prim(name("prim-f32-lt"), Rc::new(prim::f32_lt()))
            .define_prim(name("prim-f64-lt"), Rc::new(prim::f64_lt()))
            .define_prim(name("prim-string-gt"), Rc::new(prim::string_gt()))
            .define_prim(name("prim-bool-gt"), Rc::new(prim::bool_gt()))
            .define_prim(name("prim-char-gt"), Rc::new(prim::char_gt()))
            .define_prim(name("prim-f32-gt"), Rc::new(prim::f32_gt()))
            .define_prim(name("prim-f64-gt"), Rc::new(prim::f64_gt()))
            .define_prim(name("prim-string-ge"), Rc::new(prim::string_ge()))
            .define_prim(name("prim-bool-ge"), Rc::new(prim::bool_ge()))
            .define_prim(name("prim-char-ge"), Rc::new(prim::char_ge()))
            .define_prim(name("prim-f32-ge"), Rc::new(prim::f32_ge()))
            .define_prim(name("prim-f64-ge"), Rc::new(prim::f64_ge()))
            .define_prim(name("prim-f32-add"), Rc::new(prim::f32_add()))
            .define_prim(name("prim-f64-add"), Rc::new(prim::f64_add()))
            .define_prim(name("prim-f32-sub"), Rc::new(prim::f32_sub()))
            .define_prim(name("prim-f64-sub"), Rc::new(prim::f64_sub()))
            .define_prim(name("prim-f32-mul"), Rc::new(prim::f32_mul()))
            .define_prim(name("prim-f64-mul"), Rc::new(prim::f64_mul()))
            .define_prim(name("prim-f32-div"), Rc::new(prim::f32_div()))
            .define_prim(name("prim-f64-div"), Rc::new(prim::f64_div()))
            .define_prim(name("prim-char-to-string"), Rc::new(prim::char_to_string()))
            .define_prim(name("prim-f32-to-string"), Rc::new(prim::f32_to_string()))
            .define_prim(name("prim-f64-to-string"), Rc::new(prim::f64_to_string()))
            .define_prim(name("prim-string-append"), Rc::new(prim::string_append()))
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct FmtContextEntries<'a>(&'a Vector<Entry>);

        impl<'a> fmt::Debug for FmtContextEntries<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_list().entries(self.0).finish()
            }
        }

        f.debug_struct("Context")
            .field("entries", &FmtContextEntries(&self.entries))
            .finish()
    }
}
