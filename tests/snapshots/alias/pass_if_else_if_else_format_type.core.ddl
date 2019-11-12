extern Bool : Type;

extern true : item Bool;

extern false : item Bool;

extern F32Be : Format;

extern F64Be : Format;

Test =
    bool_elim
    item
    true
    {
    bool_elim
    item
    true
    {
    item
    F64Be,
    item
    F32Be
    },
    bool_elim
    item
    false
    {
    item
    F64Be,
    item
    F32Be
    }
    }
    : Format;
