use std::mem::MaybeUninit;
use std::ops::Deref;

// TODO: investigate if this could be replaced with an existing crate. For example:
//
// - https://lib.rs/crates/slicevec
// - https://lib.rs/crates/fixed-slice-vec
//
// Our requirements around drop-glue might be an issue.

/// A helpful type for allocating elements to a slice up to a maximum length.
/// This can be helpful if we have initialization code that might be difficult
/// to implement using [`scoped_arena::Scope::to_scope_from_iter`], for example:
///
/// - when pushing to multiple slices at once
/// - when element initialization code has the possibility of failure
pub struct SliceVec<'a, Elem> {
    next_index: usize,
    // SAFETY: The slice `self.elems[..self.next_index]` should only ever
    //         contain elements initialized with `MaybeUninit::new`.
    elems: &'a mut [MaybeUninit<Elem>],
}

impl<'a, Elem> SliceVec<'a, Elem> {
    /// Allocates a new slice builder to the scope.
    ///
    /// # Panics
    ///
    /// If the type has drop-glue to be executed.
    pub fn new(scope: &'a scoped_arena::Scope<'a>, max_len: usize) -> SliceVec<'a, Elem> {
        // NOTE: Ensure that that the element type does not have any drop glue.
        //       This would be problematic as we have no way of registering the
        //       drop glue of `Elem` with `scoped_arena::Scope`.
        assert!(!std::mem::needs_drop::<Elem>());

        let elems = std::iter::repeat_with(MaybeUninit::uninit).take(max_len);

        SliceVec {
            next_index: 0,
            elems: scope.to_scope_from_iter(elems),
        }
    }

    /// Push an element to the slice builder.
    ///
    /// # Panics
    ///
    /// If the pushing the element would exceed the maximum slice length
    /// supplied in [`SliceBuilder::new`].
    pub fn push(&mut self, elem: Elem) {
        self.elems[self.next_index] = MaybeUninit::new(elem);
        self.next_index += 1;
    }
}

impl<'a, Elem> Deref for SliceVec<'a, Elem> {
    type Target = [Elem];

    fn deref(&self) -> &[Elem] {
        // SAFETY: This is safe because we know that `self.elems[..self.next_index]`
        // only ever contains elements initialized with `MaybeUninit::new`.
        // We know this because:
        //
        // - `self.next_index` is always initialized to `0` in `SliceBuilder::new`
        // - `self.next_index` is only incremented in `SliceBuilder::push`,
        //    and in that case we make sure `self.elems[self.next_index]`
        //    has been initialized before hand.
        unsafe { slice_assume_init_ref(&self.elems[..self.next_index]) }
    }
}

impl<'a, Elem> Into<&'a [Elem]> for SliceVec<'a, Elem> {
    fn into(self) -> &'a [Elem] {
        // SAFETY: This is safe because we know that `self.elems[..self.next_index]`
        // only ever contains elements initialized with `MaybeUninit::new`.
        // We know this because:
        //
        // - `self.next_index` is always initialized to `0` in `SliceBuilder::new`
        // - `self.next_index` is only incremented in `SliceBuilder::push`,
        //    and in that case we make sure `self.elems[self.next_index]`
        //    has been initialized before hand.
        unsafe { slice_assume_init_ref(&self.elems[..self.next_index]) }
    }
}

// NOTE: This is the same implementation as `MaybeUninit::slice_assume_init_ref`,
// which is currently unstable (see https://github.com/rust-lang/rust/issues/63569).
pub unsafe fn slice_assume_init_ref<'a, T>(slice: &'a [MaybeUninit<T>]) -> &'a [T] {
    // SAFETY: casting slice to a `*const [T]` is safe since the caller guarantees that
    // `slice` is initialized, and`MaybeUninit` is guaranteed to have the same layout as `T`.
    // The pointer obtained is valid since it refers to memory owned by `slice` which is a
    // reference and thus guaranteed to be valid for reads.
    &*(slice as *const [MaybeUninit<T>] as *const [T])
}
