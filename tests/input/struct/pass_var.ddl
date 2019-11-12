//! Test referring to aliases in struct fields.

extern U8 : Format;

struct Pair {
    first: U8,
    second: U8,
}

MyPair = Pair;

struct PairPair {
    first: Pair,
    second: MyPair,
}
