//! Test a pair struct.

extern U8 : Format;

extern S8 : Format;

/// A pair of bytes.
struct Pair {
    /// The first field.
    first : item U8,
    /// The second field.
    second : item S8,
}
