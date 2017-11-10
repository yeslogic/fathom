//! The syntax of our data description language

use std::fmt;
use std::collections::BTreeMap;
use std::rc::Rc;

use name::{Name, Named};

pub mod binary;
pub mod host;

/// A variable that can either be free or bound
///
/// We use a locally nameless representation for variable binding.
///
/// # References
///
/// - [How I learned to stop worrying and love de Bruijn indices]
///   (http://disciple-devel.blogspot.com.au/2011/08/how-i-learned-to-stop-worrying-and-love.html)
/// - [The Locally Nameless Representation]
///   (https://www.chargueraud.org/research/2009/ln/main.pdf)
/// - [Locally nameless representation with cofinite quantification]
///   (http://www.chargueraud.org/softs/ln/)
/// - [A Locally-nameless Backend for Ott]
///   (http://www.di.ens.fr/~zappa/projects/ln_ott/)
/// - [Library STLC_Tutorial]
///   (https://www.cis.upenn.edu/~plclub/popl08-tutorial/code/coqdoc/STLC_Tutorial.html)
///
/// ## Libraries
///
/// There are a number of libraries out there for other languages that abstract
/// away handling locally nameless representations, but I've not yet figured out
/// how to port them to Rust yet:
///
/// - DBLib: Facilities for working with de Bruijn indices in Coq
///     - [Blog Post](http://gallium.inria.fr/blog/announcing-dblib/)
///     - [Github](https://github.com/coq-contribs/dblib)
/// - Bound: Bruijn indices for Haskell
///     - [Blog Post](https://www.schoolofhaskell.com/user/edwardk/bound)
///     - [Github](https://github.com/ekmett/bound/)
///     - [Hackage](https://hackage.haskell.org/package/bound)
/// - The Penn Locally Nameless Metatheory Library
///     - [Github](https://github.com/plclub/metalib)
#[derive(Clone, PartialEq, Eq)]
pub enum Var<N, B> {
    /// A free, unbound variable
    Free(N),
    /// A bound variable
    Bound(Named<N, B>),
}

impl<N: Name, B> Var<N, B> {
    pub fn abstract_name_at(&mut self, name: &N, level: B) {
        *self = match *self {
            Var::Free(ref n) if n == name => Var::Bound(Named(n.clone(), level)),
            Var::Free(_) | Var::Bound(_) => return,
        }
    }
}

impl<F: fmt::Debug, B: fmt::Debug> fmt::Debug for Var<F, B> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Free(ref x) => {
                write!(f, "Free(")?;
                x.fmt(f)?;
            }
            Var::Bound(ref i) => {
                write!(f, "Bound(")?;
                i.fmt(f)?;
            }
        }
        write!(f, ")")
    }
}

/// A field in a struct type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field<N, T> {
    pub name: N,
    pub value: T,
}

impl<N, T> Field<N, T> {
    pub fn new<M: Into<N>, U: Into<T>>(name: M, value: U) -> Field<N, T> {
        Field {
            name: name.into(),
            value: value.into(),
        }
    }

    /// Apply the function `f` to the field name and return the wrapped result
    pub fn map_name<M, F: FnMut(N) -> M>(self, mut f: F) -> Field<M, T> {
        Field {
            name: f(self.name),
            value: self.value,
        }
    }

    /// Apply the function `f` to the field value and return the wrapped result
    pub fn map_value<U, F: FnMut(T) -> U>(self, mut f: F) -> Field<N, U> {
        Field {
            name: self.name,
            value: f(self.value),
        }
    }
}

fn lookup_field<'a, N, T>(fields: &'a [Field<N, T>], name: &N) -> Option<&'a T>
where
    N: PartialEq,
{
    fields
        .iter()
        .find(|field| &field.name == name)
        .map(|field| &field.value)
}

/// A type definition
///
/// ```plain
/// Point = {
///     x : u16,
///     y : u16,
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition<N> {
    pub name: N,
    pub ty: binary::RcType<N>,
}

impl<N> Definition<N> {
    pub fn new<N1: Into<N>, T1: Into<binary::RcType<N>>>(name: N1, ty: T1) -> Definition<N> {
        Definition {
            name: name.into(),
            ty: ty.into(),
        }
    }
}

pub type Substitutions<N> = BTreeMap<N, binary::Type<N>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<N> {
    pub defs: Vec<Definition<N>>,
}

impl<N: Name> Program<N> {
    pub fn new(mut defs: Vec<Definition<N>>) -> Program<N> {
        // We maintain a list of the seen definition names. This will allow us to
        // recover the index of these variables as we abstract later definitions...
        let mut seen_names = Vec::new();

        for def in &mut defs {
            for (level, name) in seen_names.iter().rev().enumerate() {
                Rc::make_mut(&mut def.ty).abstract_name_at(name, level as u32);
            }

            // Record that the definition has been 'seen'
            seen_names.push(def.name.clone());
        }

        Program { defs }
    }

    pub fn substitute(&mut self, substs: &Substitutions<N>) {
        for def in &mut self.defs {
            Rc::make_mut(&mut def.ty).substitute(substs);
        }
    }
}

pub fn base_defs<N: Name + for<'a> From<&'a str>>() -> Substitutions<N> {
    use syntax::ast::binary::{Type, TypeConst};

    fn prim_array_ty<N: Name>(size: i64, conv_name: &'static str) -> binary::Type<N> {
        use source::Span;
        use syntax::ast::host::Expr;

        let array_ty = Type::array(Span::start(), Type::bit(), Expr::int(Span::start(), size));
        let conv_ty = host::Type::arrow(array_ty.repr(), host::Type::int());

        Type::interp(
            Span::start(),
            array_ty,
            Expr::prim(conv_name, conv_ty),
            host::Type::int(),
        )
    }

    btreemap! {
        // TODO: "true" = Expr::bool(true)
        // TODO: "false" = Expr::bool(false)
        "bit".into() => Type::Const(TypeConst::Bit),
        // Native endian primitives (Do we need these?)
        "u8".into() => prim_array_ty(8, "from_u8"),
        "u16".into() => prim_array_ty(16, "from_u16"),
        "u32".into() => prim_array_ty(32, "from_u32"),
        "u64".into() => prim_array_ty(64, "from_u64"),
        "i8".into() => prim_array_ty(8, "from_i8"),
        "i16".into() => prim_array_ty(16, "from_i16"),
        "i32".into() => prim_array_ty(32, "from_i32"),
        "i64".into() => prim_array_ty(64, "from_i64"),
        "f32".into() => prim_array_ty(32, "from_f32"),
        "f64".into() => prim_array_ty(64, "from_f64"),
        // Little endian primitives
        "u8le".into() => prim_array_ty(8, "from_u8le"),
        "u16le".into() => prim_array_ty(16, "from_u16le"),
        "u32le".into() => prim_array_ty(32, "from_u32le"),
        "u64le".into() => prim_array_ty(64, "from_u64le"),
        "i8le".into() => prim_array_ty(8, "from_i8le"),
        "i16le".into() => prim_array_ty(16, "from_i16le"),
        "i32le".into() => prim_array_ty(32, "from_i32le"),
        "i64le".into() => prim_array_ty(64, "from_i64le"),
        "f32le".into() => prim_array_ty(32, "from_f32le"),
        "f64le".into() => prim_array_ty(64, "from_f64le"),
        // Big endian primitives
        "u8be".into() => prim_array_ty(8, "from_u8be"),
        "u16be".into() => prim_array_ty(16, "from_u16be"),
        "u32be".into() => prim_array_ty(32, "from_u32be"),
        "u64be".into() => prim_array_ty(64, "from_u64be"),
        "i8be".into() => prim_array_ty(8, "from_i8be"),
        "i16be".into() => prim_array_ty(16, "from_i16be"),
        "i32be".into() => prim_array_ty(32, "from_i32be"),
        "i64be".into() => prim_array_ty(64, "from_i64be"),
        "f32be".into() => prim_array_ty(32, "from_f32be"),
        "f64be".into() => prim_array_ty(64, "from_f64be"),
    }
}
