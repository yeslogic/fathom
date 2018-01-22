use std::cmp::Ordering;
use std::fmt;

/// An identifier that originates from user input
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct Ident(pub String);

impl<'a> From<&'a str> for Ident {
    fn from(src: &'a str) -> Ident {
        Ident(String::from(src))
    }
}

impl From<String> for Ident {
    fn from(src: String) -> Ident {
        Ident(src)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ident(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The name of a free variable
#[derive(Debug, Clone)]
pub enum Name {
    /// Names originating from user input
    User(Ident),
    /// Abstract names, `_`
    ///
    /// Comparing two abstract names will always return false because we cannot
    /// be sure what they actually refer to.
    Abstract,
}

impl Name {
    pub fn user<S: Into<Ident>>(name: S) -> Name {
        Name::User(name.into())
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Name) -> bool {
        match (self, other) {
            (&Name::User(ref lhs), &Name::User(ref rhs)) => lhs == rhs,
            (&Name::Abstract, &Name::Abstract) | (_, _) => false,
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Name::User(ref name) => write!(f, "{}", name),
            Name::Abstract => write!(f, "_"),
        }
    }
}

/// A variable with a name that is ignored for comparisons. This is useful for
/// improving error reporting when converting free varables to a named form.
#[derive(Debug, Clone, Eq, Ord)]
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
