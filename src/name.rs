use std::fmt;
use std::cmp::Ordering;

/// A variable with a name that is ignored for comparisons. This is useful for
/// improving error reporting when converting free varables to a named form.
#[derive(Clone, Eq, Ord)]
pub struct Named<T>(pub String, pub T);

impl<T: PartialEq> PartialEq for Named<T> {
    fn eq(&self, other: &Named<T>) -> bool {
        self.1 == other.1
    }
}

impl<T: PartialEq> PartialEq<T> for Named<T> {
    fn eq(&self, other: &T) -> bool {
        &self.1 == other
    }
}

impl<T: PartialOrd> PartialOrd for Named<T> {
    fn partial_cmp(&self, other: &Named<T>) -> Option<Ordering> {
        self.1.partial_cmp(&other.1)
    }
}

impl<T: PartialOrd> PartialOrd<T> for Named<T> {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.1.partial_cmp(other)
    }
}

impl<T> From<(String, T)> for Named<T> {
    fn from(src: (String, T)) -> Named<T> {
        Named(src.0, src.1)
    }
}

impl<T: fmt::Debug> fmt::Debug for Named<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Named(")?;
        self.0.fmt(f)?;
        write!(f, ", ")?;
        self.1.fmt(f)?;
        write!(f, ")")
    }
}
