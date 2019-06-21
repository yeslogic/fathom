//~ PARSE: ok
//~ ELABORATE: fail

struct Pair {
    first: Bloop, //~ error: cannot find `Bloop` in this scope
    second: Bloop, //~ error: cannot find `Bloop` in this scope
}
