//! Test referring to aliases in struct fields.

extern U8 : Format;

struct Pair {
    first : item U8,
    second : item U8,
}

MyPair = item Pair;

struct PairPair {
    first : item Pair,
    second : item MyPair,
}
