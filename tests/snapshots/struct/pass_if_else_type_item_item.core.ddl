is_be = true;

Bar = bool_elim item is_be { F32Be, F32Le };

struct Test {
    bar : item Bar,
}
