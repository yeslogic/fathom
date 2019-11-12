extern Bool : Type;

extern true : item Bool;

extern false : item Bool;

test = bool_elim ! { item true, item false } : item Bool;
