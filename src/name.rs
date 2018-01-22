use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;
use std::ops::Deref;

/// An identifier that originates from user input
#[derive(PartialEq, PartialOrd, Eq, Ord)]
pub struct Ident(pub str);

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ident(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

/// An identifier that originates from user input
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct OwnedIdent(pub String);

impl Deref for OwnedIdent {
    type Target = Ident;

    fn deref(&self) -> &Ident {
        unsafe {
            use std::mem;
            mem::transmute::<&str, &Ident>(&self.0[..])
        }
    }
}

impl Borrow<Ident> for OwnedIdent {
    #[inline]
    fn borrow(&self) -> &Ident {
        self.deref()
    }
}

impl<'a> From<&'a str> for OwnedIdent {
    fn from(src: &'a str) -> OwnedIdent {
        OwnedIdent(String::from(src))
    }
}

impl From<String> for OwnedIdent {
    fn from(src: String) -> OwnedIdent {
        OwnedIdent(src)
    }
}

impl<'a> From<&'a Ident> for OwnedIdent {
    fn from(src: &'a Ident) -> OwnedIdent {
        OwnedIdent(String::from(&src.0))
    }
}

impl PartialEq<Ident> for OwnedIdent {
    fn eq(&self, other: &Ident) -> bool {
        self.0 == other.0
    }
}

impl<'a> PartialEq<&'a Ident> for OwnedIdent {
    fn eq(&self, other: &&Ident) -> bool {
        self.0 == other.0
    }
}

impl PartialEq<OwnedIdent> for Ident {
    fn eq(&self, other: &OwnedIdent) -> bool {
        self.0 == other.0
    }
}

impl fmt::Debug for OwnedIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "OwnedIdent(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for OwnedIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A variable with a name that is ignored for comparisons. This is useful for
/// improving error reporting when converting free varables to a named form.
#[derive(Clone, Eq, Ord)]
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

impl<N, T: PartialOrd> PartialOrd for Named<N, T> {
    fn partial_cmp(&self, other: &Named<N, T>) -> Option<Ordering> {
        self.1.partial_cmp(&other.1)
    }
}

impl<N, T: PartialOrd> PartialOrd<T> for Named<N, T> {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.1.partial_cmp(other)
    }
}

impl<N, T> From<(N, T)> for Named<N, T> {
    fn from(src: (N, T)) -> Named<N, T> {
        Named(src.0, src.1)
    }
}

impl<N: fmt::Debug, T: fmt::Debug> fmt::Debug for Named<N, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Named(")?;
        self.0.fmt(f)?;
        write!(f, ", ")?;
        self.1.fmt(f)?;
        write!(f, ")")
    }
}
