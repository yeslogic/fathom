struct Pair {
    first : U8,
    second : U8,
}

MyPair = item Pair;

struct PairPair {
    first : item Pair,
    second : item MyPair,
}
