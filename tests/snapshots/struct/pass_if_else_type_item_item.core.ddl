is_be = item true;

Bar = bool_elim item is_be { item F32Be, item F32Le };

struct Test {
    bar : item Bar,
}
