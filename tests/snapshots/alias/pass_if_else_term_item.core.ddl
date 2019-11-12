extern Bool : Type;

extern true : item Bool;

extern false : item Bool;

foo = item true;

bar = bool_elim item foo { item true, item false } : item Bool;

baz = item bar;
