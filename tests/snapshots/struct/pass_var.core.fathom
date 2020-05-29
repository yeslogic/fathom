//! Test referring to aliases in struct fields.

struct Pair {
    first : global U8,
    second : global U8,
}

MyPair = item Pair;

struct PairPair {
    first : item Pair,
    second : item MyPair,
}
