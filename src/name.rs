use std::fmt;
use std::cmp::Ordering;

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
