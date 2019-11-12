extern Bool : Type;
extern true : Bool;
extern F32Be : Format;
extern F32Le : Format;

is_be =
    true;

struct Test {
    bar: if is_be { F32Be } else { F32Le },
}
