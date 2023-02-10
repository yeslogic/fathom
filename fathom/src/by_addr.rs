use std::hash::{Hash, Hasher};
use std::ops::Deref;

/// A wrapper struct for references that compare and hash based on the numeric
/// address of the pointer, rather than the value pointed to by the reference.
#[derive(Debug)]
pub struct ByAddr<'a, T>(pub &'a T);

impl<'a, T> Copy for ByAddr<'a, T> {}

impl<'a, T> Clone for ByAddr<'a, T> {
    fn clone(&self) -> Self {
        Self(<&T>::clone(&self.0))
    }
}

impl<'a, T> PartialEq for ByAddr<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<'a, T> Eq for ByAddr<'a, T> {}

impl<'a, T> PartialOrd for ByAddr<'a, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a, T> Ord for ByAddr<'a, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let this_addr = self.0 as *const T as usize;
        let other_addr = other.0 as *const T as usize;
        this_addr.cmp(&other_addr)
    }
}

impl<'a, T> Hash for ByAddr<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let this_addr = self.0 as *const T as usize;
        this_addr.hash(state);
    }
}

impl<'a, T> Deref for ByAddr<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}
