//~ PARSE: ok
//~ ELABORATE: fail

struct Empty {}
struct Empty {} //~ error: the name `Empty` is defined multiple times
struct Empty {} //~ error: the name `Empty` is defined multiple times
