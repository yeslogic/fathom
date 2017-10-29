//! The syntax of our data description language

use std::fmt;

pub mod binary;
pub mod context;
pub mod host;

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
    pub ty: Box<binary::Type<N>>,
}

impl<N> Definition<N> {
    pub fn new<N1: Into<N>, T1: Into<Box<binary::Type<N>>>>(name: N1, ty: T1) -> Definition<N> {
        Definition {
            name: name.into(),
            ty: ty.into(),
        }
    }
}

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

/// Trait alias for types that work well as names in the AST
pub trait Name: Clone + PartialEq + fmt::Debug + fmt::Display {}

impl<T: Clone + PartialEq + fmt::Debug + fmt::Display> Name for T {}

/// A variable with a name that is ignored for comparisons. This is useful for
/// improving error reporting when converting free varables to a named form.
///
/// # Type parameters
///
/// - `N`: The name that will be ignored for comparison purposes
/// - `T`: The type of the variable
#[derive(Clone, Eq, PartialOrd, Ord)]
pub struct Named<N, T>(pub N, pub T);

impl<N, T: PartialEq> PartialEq for Named<N, T> {
    fn eq(&self, other: &Named<N, T>) -> bool {
        self.1 == other.1
    }
}

impl<N, T: PartialEq> PartialEq<T> for Named<N, T> {
    fn eq(&self, other: &T) -> bool {
        &self.1 == other
    }
}

impl<N, T> From<(N, T)> for Named<N, T> {
    fn from(src: (N, T)) -> Named<N, T> {
        Named(src.0, src.1)
    }
}

impl<F: fmt::Debug, B: fmt::Debug> fmt::Debug for Named<F, B> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Named(")?;
        self.0.fmt(f)?;
        write!(f, ", ")?;
        self.1.fmt(f)?;
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

pub fn base_defs<N: Name + for<'a> From<&'a str>>() -> Vec<Definition<N>> {
    fn prim_array_ty<N: Name>(size: i64, conv_name: &'static str) -> binary::Type<N> {
        use source::Span;
        use syntax::binary::Type;
        use syntax::host::Expr;

        let array_ty = Type::array(Span::start(), Type::bit(), Expr::int(Span::start(), size));
        let conv_ty = host::Type::arrow(array_ty.repr().unwrap(), host::Type::int());

        Type::interp(
            Span::start(),
            array_ty,
            Expr::prim(conv_name, conv_ty),
            host::Type::int(),
        )
    }

    vec![
        // TODO: "true" = Expr::bool(true)
        // TODO: "false" = Expr::bool(false)

        // Native endian primitives (Do we need these?)
        Definition::new("u8", prim_array_ty(8, "from_u8")),
        Definition::new("u16", prim_array_ty(16, "from_u16")),
        Definition::new("u32", prim_array_ty(32, "from_u32")),
        Definition::new("u64", prim_array_ty(64, "from_u64")),
        Definition::new("i8", prim_array_ty(8, "from_i8")),
        Definition::new("i16", prim_array_ty(16, "from_i16")),
        Definition::new("i32", prim_array_ty(32, "from_i32")),
        Definition::new("i64", prim_array_ty(64, "from_i64")),
        Definition::new("f32", prim_array_ty(32, "from_f32")),
        Definition::new("f64", prim_array_ty(64, "from_f64")),
        // Little endian primitives
        Definition::new("u8le", prim_array_ty(8, "from_u8le")),
        Definition::new("u16le", prim_array_ty(16, "from_u16le")),
        Definition::new("u32le", prim_array_ty(32, "from_u32le")),
        Definition::new("u64le", prim_array_ty(64, "from_u64le")),
        Definition::new("i8le", prim_array_ty(8, "from_i8le")),
        Definition::new("i16le", prim_array_ty(16, "from_i16le")),
        Definition::new("i32le", prim_array_ty(32, "from_i32le")),
        Definition::new("i64le", prim_array_ty(64, "from_i64le")),
        Definition::new("f32le", prim_array_ty(32, "from_f32le")),
        Definition::new("f64le", prim_array_ty(64, "from_f64le")),
        // Big endian primitives
        Definition::new("u8be", prim_array_ty(8, "from_u8be")),
        Definition::new("u16be", prim_array_ty(16, "from_u16be")),
        Definition::new("u32be", prim_array_ty(32, "from_u32be")),
        Definition::new("u64be", prim_array_ty(64, "from_u64be")),
        Definition::new("i8be", prim_array_ty(8, "from_i8be")),
        Definition::new("i16be", prim_array_ty(16, "from_i16be")),
        Definition::new("i32be", prim_array_ty(32, "from_i32be")),
        Definition::new("i64be", prim_array_ty(64, "from_i64be")),
        Definition::new("f32be", prim_array_ty(32, "from_f32be")),
        Definition::new("f64be", prim_array_ty(64, "from_f64be")),
    ]
}
