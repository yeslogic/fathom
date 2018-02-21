//! Variable binding

use std::fmt;
use name::{Name, Named};

/// A generated id
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GenId(u32);

impl GenId {
    /// Generate a new, globally unique id
    pub fn fresh() -> GenId {
        use std::sync::atomic::{AtomicUsize, Ordering};

        lazy_static! {
            static ref NEXT_ID : AtomicUsize = AtomicUsize::new(0);
        }

        // FIXME: check for integer overflow
        GenId(NEXT_ID.fetch_add(1, Ordering::SeqCst) as u32)
    }
}

impl fmt::Display for GenId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct ScopeIndex(pub u32);

impl ScopeIndex {
    pub fn succ(self) -> ScopeIndex {
        ScopeIndex(self.0 + 1)
    }

    pub fn shift(self, amount: u32) -> ScopeIndex {
        ScopeIndex(self.0 + amount)
    }
}

impl fmt::Debug for ScopeIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ScopeIndex(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct BindingIndex(pub u32);

impl fmt::Debug for BindingIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BindingIndex(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

/// A reference to a bound variable
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct BoundVar {
    /// The debruijn index of the scope that introduced the variable
    pub scope: ScopeIndex,
    /// The index of the binder within the scope that introduced this variable
    pub binding: BindingIndex,
}

impl BoundVar {
    pub fn new(scope: ScopeIndex, binding: BindingIndex) -> BoundVar {
        BoundVar { scope, binding }
    }
}

impl fmt::Debug for BoundVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BoundVar(")?;
        self.scope.0.fmt(f)?;
        write!(f, ", ")?;
        self.binding.0.fmt(f)?;
        write!(f, ")")
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
#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    /// A free, unbound variable
    Free(Name),
    /// A bound variable
    Bound(Named<Name, BoundVar>),
}

impl Var {
    pub fn free<N: Into<Name>>(name: N) -> Var {
        Var::Free(name.into())
    }

    pub fn bound<N: Into<Name>>(name: N, var: BoundVar) -> Var {
        Var::Bound(Named(name.into(), var))
    }

    pub fn abstract_names_at(&mut self, names: &[Name], scope: ScopeIndex) {
        *self = match *self {
            Var::Free(ref n) => match names.iter().position(|name| name == n) {
                Some(position) => {
                    let bv = BoundVar {
                        scope,
                        binding: BindingIndex(position as u32),
                    };
                    Var::Bound(Named(n.clone(), bv))
                }
                None => return,
            },
            Var::Bound(_) => return,
        };
    }
}
