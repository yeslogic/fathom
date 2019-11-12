extern Bool : Type;

extern true : item Bool;

extern F32Be : Format;

extern F32Le : Format;

is_be = item true;

Bar = bool_elim item is_be { item F32Be, item F32Le };

struct Test {
    bar : item Bar,
}
