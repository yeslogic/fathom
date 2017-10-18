//! The syntax of our data description language

use std::fmt;

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
    Bound(B),
}

impl<N, B> Var<N, B> {
    /// `true` if the variable is free
    fn is_closed(&self) -> bool {
        match *self {
            Var::Free(_) => true,
            Var::Bound(_) => false,
        }
    }

    pub fn abstract_with<F>(&mut self, f: &F)
    where
        F: Fn(&N) -> Option<B>,
    {
        *self = match *self {
            Var::Free(ref n) => match f(n) {
                None => return,
                Some(i) => Var::Bound(i),
            },
            Var::Bound(_) => return,
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
        writeln!(f, ")")
    }
}

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
