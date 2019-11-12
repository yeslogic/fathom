extern Bool : Type;
extern true : Bool;
extern F32Be : Format;
extern F32Le : Format;

is_be =
    true;

Bar =
    if is_be { F32Be } else { F32Le };

struct Test {
    bar: Bar,
}
