//! Environments and variables.
//!
//! # Variables
//!
//! Nameless variables are used to avoid the expense of keeping track of name
//! substitutions during evaluation and conversion checking. We use a
//! combination of [de Bruijn indices][LocalVar] in terms and [de Bruijn
//! levels][GlobalVar] in values in order to avoid the expensive and error-prone
//! shifting operations that are often associated with nameless approaches to
//! environments. For more information on this approach, see section 3.1 of
//! [Abel's habilitation thesis](https://www.cse.chalmers.se/~abela/habil.pdf).
//!
//! # Environments
//!
//! A number of different environment representations are used. Where possible
//! we try to stick to flat, low-indirection environments like [`UniqueEnv`]
//! and [`SliceEnv`], but when we need to copy environments often, we use a
//! [`SharedEnv`] to increase the amount of sharing at the expense of locality.

use std::fmt;

/// Underlying variable representation.
type RawVar = u16;

/// A [de Bruijn index], which represents a variable counting the number of
/// binders between a variable occurrence and the binder that introduced the
/// variable.
///
/// For example:
///
/// | Representation    | Example (S combinator)  |
/// | ----------------- | ----------------------- |
/// | Named             | `λx. λy. λz. x z (y z)` |
/// | de Bruijn indices | `λ_. λ_. λ_. 2 0 (1 0)` |
///
/// This is a helpful representation because it allows us to easily compare
/// terms for [alpha-equivalence] based on their binding structure without
/// maintaining a list of name substitutions. For example we want `λx. x` to be
/// the same as `λy. y`. With de Bruijn indices these would both be described as
/// `λ 0`.
///
/// [de Bruijn index]: https://en.wikipedia.org/wiki/De_Bruijn_index
/// [alpha-equivalence]: https://ncatlab.org/nlab/show/alpha-equivalence
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalVar(RawVar);

impl LocalVar {
    /// The last variable to be bound in the environment.
    pub const fn last() -> LocalVar {
        LocalVar(0)
    }

    /// Returns the previously bound variable, relative to this one.
    pub const fn prev(self) -> LocalVar {
        LocalVar(self.0 + 1) // FIXME: check overflow?
    }
}

impl fmt::Debug for LocalVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LocalVar(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

/// An iterator over local variables, listed from the most recently bound.
pub fn local_vars() -> impl Iterator<Item = LocalVar> {
    (0..).map(LocalVar)
}

/// A de Bruijn level, which represents a variable by counting the number of
/// binders between the binder that introduced the variable and the start of the
/// environment. For example:
///
/// | Representation    | Example (S combinator)  |
/// | ----------------- | ----------------------- |
/// | Named             | `λx. λy. λz. x z (y z)` |
/// | de Bruijn levels  | `λ_. λ_. λ_. 0 2 (1 2)` |
///
/// Levels are used in [values][crate::core::semantics::Value] because they
/// are not tied to a specific binding depth, unlike [indices][LocalVar].
/// Because of this, we're able to sidestep the need for expensive variable
/// shifting during [normalisation][crate::core::semantics::EvalContext::normalise].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct GlobalVar(RawVar);

impl GlobalVar {
    /// The first variable to be bound in the environment.
    pub const fn first() -> LocalVar {
        LocalVar(0)
    }

    /// Returns the next bound variable, relative to this one.
    pub const fn next(self) -> GlobalVar {
        GlobalVar(self.0 + 1) // FIXME: check overflow?
    }
}

impl fmt::Debug for GlobalVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GlobalVar(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

/// An iterator over global variables, listed from the least recently bound.
pub fn global_vars() -> impl Iterator<Item = GlobalVar> {
    (0..).map(GlobalVar)
}

/// The length of an environment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnvLen(RawVar);

impl EnvLen {
    /// Construct a new, empty environment.
    pub fn new() -> EnvLen {
        EnvLen(0)
    }

    /// Reset the environment to the empty environment.
    pub fn clear(&mut self) {
        *self = EnvLen::new();
    }

    /// Convert a local variable to a global variable in the current environment.
    pub fn local_to_global(self, local: LocalVar) -> Option<GlobalVar> {
        Some(GlobalVar(self.0.checked_sub(local.0)?.checked_sub(1)?))
    }

    /// Convert a global variable to a local variable in the current environment.
    pub fn global_to_local(self, global: GlobalVar) -> Option<LocalVar> {
        Some(LocalVar(self.0.checked_sub(global.0)?.checked_sub(1)?))
    }

    /// The next global variable that will be bound in this environment.
    pub fn next_global(self) -> GlobalVar {
        GlobalVar(self.0)
    }

    /// Push an entry onto the environment.
    pub fn push(&mut self) {
        self.0 += 1; // FIXME: check overflow?
    }

    /// Pop an entry off the environment.
    pub fn pop(&mut self) {
        self.0 -= 1; // FIXME: check underflow?
    }

    /// Truncate the environment to the given length.
    pub fn truncate(&mut self, len: EnvLen) {
        *self = len;
    }
}

/// A uniquely owned environment.
#[derive(Debug, Clone)]
pub struct UniqueEnv<Entry> {
    entries: Vec<Entry>,
}

impl<Entry> UniqueEnv<Entry> {
    /// Construct a new, empty environment.
    pub fn new() -> UniqueEnv<Entry> {
        UniqueEnv {
            entries: Vec::new(),
        }
    }

    /// Clear the renaming. This is useful for reusing environment allocations.
    pub fn clear(&mut self) {
        self.entries.clear()
    }

    /// Resize the environment to the desired length, filling new entries with `entry`.
    pub fn resize(&mut self, new_len: EnvLen, entry: Entry)
    where
        Entry: Clone,
    {
        self.entries.resize(usize::from(new_len.0), entry)
    }

    /// Push an entry onto the environment.
    pub fn push(&mut self, entry: Entry) {
        assert!(self.entries.len() < usize::from(u16::MAX));
        self.entries.push(entry);
    }

    /// Pop an entry off the environment.
    pub fn pop(&mut self) {
        self.entries.pop();
    }

    /// Truncate the environment to the given length.
    pub fn truncate(&mut self, len: EnvLen) {
        self.entries.truncate(len.0 as usize);
    }
}

impl<Entry> std::ops::Deref for UniqueEnv<Entry> {
    type Target = SliceEnv<Entry>;

    fn deref(&self) -> &SliceEnv<Entry> {
        // SAFETY:
        // - `SliceEnv<Entry>` is equivalent to an `[Entry]` internally
        unsafe { std::mem::transmute::<&[_], &SliceEnv<_>>(&self.entries[..]) }
    }
}

impl<Entry> std::ops::DerefMut for UniqueEnv<Entry> {
    fn deref_mut(&mut self) -> &mut SliceEnv<Entry> {
        // SAFETY:
        // - `SliceEnv<Entry>` is equivalent to an `[Entry]` internally
        unsafe { std::mem::transmute::<&mut [_], &mut SliceEnv<_>>(&mut self.entries[..]) }
    }
}

/// An environment backed by a slice.
#[derive(Debug)]
pub struct SliceEnv<Entry> {
    entries: [Entry],
}

impl<Entry> SliceEnv<Entry> {
    /// The length of the environment.
    pub fn len(&self) -> EnvLen {
        // SAFETY:
        // - The only way to construct a `SliceEnv` is via `UniqueEnv`. We
        //   ensure that the length of the environment never exceeds the the
        //   maximum `RawVar`, so this should never overflow.
        EnvLen(self.entries.len() as RawVar)
    }

    /// Lookup an entry in the environment using global variable reference.
    pub fn get_global(&self, global_var: GlobalVar) -> Option<&Entry> {
        self.entries.get(usize::from(global_var.0))
    }

    /// Lookup an entry in the environment using a local variable reference.
    pub fn get_local(&self, local_var: LocalVar) -> Option<&Entry> {
        self.get_global(self.len().local_to_global(local_var)?)
    }

    /// Set an entry in the environment using global variable reference.
    pub fn set_global(&mut self, global_var: GlobalVar, entry: Entry) {
        self.entries[usize::from(global_var.0)] = entry;
    }

    /// Iterate over the elements in the environment.
    pub fn iter<'this>(&'this self) -> impl 'this + DoubleEndedIterator<Item = &'this Entry> {
        self.entries.iter()
    }
}

/// A persistent environment with structural sharing.
#[derive(Clone)]
pub struct SharedEnv<Entry> {
    // An `rpds::Vector` is used instead of an `im::Vector` as it's a bit
    // more compact. We assume this is important because we tend to clone
    // environments often, and they contribute to the overall size of values.
    //
    // TODO: validate these assumptions by benchmarking against other internal
    //       representations, for example:
    //
    // - `Vec<_>`
    // - `Arc<Vec<_>>`
    // - `im::Vector<_>`
    // - `Arc<im::Vector<_>>`
    entries: rpds::VectorSync<Entry>,
}

impl<Entry> SharedEnv<Entry> {
    /// Construct a new, empty environment.
    pub fn new() -> SharedEnv<Entry> {
        SharedEnv {
            entries: rpds::Vector::new_sync(),
        }
    }

    /// The length of the environment.
    pub fn len(&self) -> EnvLen {
        // SAFETY:
        // - We ensure that the length of the environment never exceeds the the
        //   maximum `RawVar`, so this should never overflow.
        EnvLen(self.entries.len() as RawVar)
    }

    /// Lookup an entry in the environment using global variable reference.
    pub fn get_global(&self, global_var: GlobalVar) -> Option<&Entry> {
        self.entries.get(usize::from(global_var.0))
    }

    /// Lookup an entry in the environment using a local variable reference.
    pub fn get_local(&self, local_var: LocalVar) -> Option<&Entry> {
        self.get_global(self.len().local_to_global(local_var)?)
    }

    /// Push an entry onto the environment.
    pub fn push(&mut self, entry: Entry) {
        assert!(self.entries.len() < usize::from(u16::MAX));
        self.entries.push_back_mut(entry);
    }

    /// Pop an entry off the environment.
    pub fn pop(&mut self) {
        self.entries.drop_last_mut();
    }

    /// Truncate the environment to the given length.
    pub fn truncate(&mut self, len: EnvLen) {
        (len.0..self.len().0).for_each(|_| self.pop());
    }

    /// Iterate over the elements in the environment.
    pub fn iter<'this>(&'this self) -> impl 'this + DoubleEndedIterator<Item = &'this Entry> {
        self.entries.iter()
    }
}

impl<Entry: fmt::Debug> fmt::Debug for SharedEnv<Entry> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SharedEnv")
            .field("entries", &DebugEntries(&self.entries))
            .finish()
    }
}

struct DebugEntries<'a, Entry>(&'a rpds::VectorSync<Entry>);

impl<'a, Entry: fmt::Debug> fmt::Debug for DebugEntries<'a, Entry> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.0.iter()).finish()
    }
}
