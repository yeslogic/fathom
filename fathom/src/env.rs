//! Environments and variables.
//!
//! # Variables
//!
//! Nameless variables are used to avoid the expense of keeping track of name
//! substitutions during evaluation and conversion checking. We use a
//! combination of [de Bruijn indices][Index] in terms and [de Bruijn
//! levels][Level] in values in order to avoid the expensive and error-prone
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
type RawVar = usize;

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
pub struct Index(RawVar);

impl Index {
    /// The last variable to be bound in the environment.
    pub const fn last() -> Index {
        Index(0)
    }

    /// Returns the previously bound variable, relative to this one.
    pub const fn prev(self) -> Index {
        Index(self.0 + 1)
    }

    /// An iterator over indices, listed from the most recently bound.
    pub fn iter() -> impl Iterator<Item = Self> {
        (0..).map(Self)
    }

    /// An iterator over indices, listed from `self`
    pub fn iter_from(self) -> impl Iterator<Item = Self> {
        (self.0..).map(Self)
    }
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Index(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
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
/// are not tied to a specific binding depth, unlike [indices][Index].
/// Because of this, we're able to sidestep the need for expensive variable
/// shifting during [normalization][crate::core::semantics::EvalEnv::normalize].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Level(RawVar);

impl Level {
    /// The first variable to be bound in the environment.
    pub const fn first() -> Level {
        Level(0)
    }

    /// Returns the next bound variable, relative to this one.
    pub const fn next(self) -> Level {
        Level(self.0 + 1)
    }

    /// An iterator over levels, listed from the least recently bound.
    pub fn iter() -> impl Iterator<Item = Self> {
        (0..).map(Self)
    }

    /// An iterator over levels, listed from `self`
    pub fn iter_from(self) -> impl Iterator<Item = Self> {
        (self.0..).map(Self)
    }
}

impl fmt::Debug for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Level(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
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

    /// Convert an index to a level in the current environment.
    pub fn index_to_level(self, index: Index) -> Option<Level> {
        Some(Level(self.0.checked_sub(index.0)?.checked_sub(1)?))
    }

    /// Convert a level to an index in the current environment.
    pub fn level_to_index(self, level: Level) -> Option<Index> {
        Some(Index(self.0.checked_sub(level.0)?.checked_sub(1)?))
    }

    /// The next level that will be bound in this environment.
    pub fn next_level(self) -> Level {
        Level(self.0)
    }

    /// Push an entry onto the environment.
    pub fn push(&mut self) {
        self.0 += 1;
    }

    /// Pop an entry off the environment.
    pub fn pop(&mut self) {
        self.0 -= 1;
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

    /// Resize the environment to the desired length, filling new entries with
    /// `entry`.
    pub fn resize(&mut self, new_len: EnvLen, entry: Entry)
    where
        Entry: Clone,
    {
        self.entries.resize(new_len.0, entry)
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
        self.entries.truncate(len.0);
    }

    pub fn reserve(&mut self, additional: usize) {
        self.entries.reserve(additional);
    }
}

impl<Entry> std::ops::Deref for UniqueEnv<Entry> {
    type Target = SliceEnv<Entry>;

    fn deref(&self) -> &SliceEnv<Entry> {
        self.entries[..].into()
    }
}

impl<Entry> std::ops::DerefMut for UniqueEnv<Entry> {
    fn deref_mut(&mut self) -> &mut SliceEnv<Entry> {
        (&mut self.entries[..]).into()
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
        // - The only way to construct a `SliceEnv` is via `UniqueEnv`. We ensure that
        //   the length of the environment never exceeds the the maximum `RawVar`, so
        //   this should never overflow.
        EnvLen(self.entries.len() as RawVar)
    }

    /// Lookup an entry in the environment using a level
    pub fn get_level(&self, level: Level) -> Option<&Entry> {
        self.entries.get(level.0)
    }

    /// Lookup an entry in the environment using an index
    pub fn get_index(&self, index: Index) -> Option<&Entry> {
        self.get_level(self.len().index_to_level(index)?)
    }

    /// Set an entry in the environment using a level
    pub fn set_level(&mut self, level: Level, entry: Entry) {
        self.entries[level.0] = entry;
    }

    /// Iterate over the elements in the environment.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Entry> {
        self.entries.iter()
    }
}

impl<Entry: PartialEq> SliceEnv<Entry> {
    pub fn elem_level(&self, entry: &Entry) -> Option<Level> {
        Iterator::zip(Level::iter(), self.iter()).find_map(|(var, e)| (entry == e).then_some(var))
    }

    pub fn elem_index(&self, entry: &Entry) -> Option<Index> {
        Iterator::zip(Index::iter(), self.iter().rev())
            .find_map(|(var, e)| (entry == e).then_some(var))
    }
}

impl<'a, Entry> From<&'a [Entry]> for &'a SliceEnv<Entry> {
    fn from(entries: &'a [Entry]) -> &'a SliceEnv<Entry> {
        // SAFETY:
        // - `SliceEnv<Entry>` is equivalent to an `[Entry]` internally
        unsafe { std::mem::transmute::<&[_], &SliceEnv<_>>(entries) }
    }
}

impl<'a, Entry> From<&'a mut [Entry]> for &'a mut SliceEnv<Entry> {
    fn from(entries: &'a mut [Entry]) -> &'a mut SliceEnv<Entry> {
        // SAFETY:
        // - `SliceEnv<Entry>` is equivalent to an `[Entry]` internally
        unsafe { std::mem::transmute::<&mut [_], &mut SliceEnv<_>>(entries) }
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
        // - We ensure that the length of the environment never exceeds the the maximum
        //   `RawVar`, so this should never overflow.
        EnvLen(self.entries.len() as RawVar)
    }

    /// Lookup an entry in the environment using a level
    pub fn get_level(&self, level: Level) -> Option<&Entry> {
        self.entries.get(level.0)
    }

    /// Lookup an entry in the environment using an index
    pub fn get_index(&self, index: Index) -> Option<&Entry> {
        self.get_level(self.len().index_to_level(index)?)
    }

    /// Push an entry onto the environment.
    pub fn push(&mut self, entry: Entry) {
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
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Entry> {
        self.entries.iter()
    }

    pub fn reserve(&mut self, _additional: usize) {
        // FIXME: `rpds::VectorSync<Entry>` has no method to reserve extra space
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
