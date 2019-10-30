//~ SKIP: compile non-trivial aliases to structure types

is_be =
    true;

Bar =
    if is_be { F32Be } else { F32Le };

struct Test {
    bar: Bar,
}
