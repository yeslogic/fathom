foo = int 33 : item Int;

bar = int_elim item foo { 33 => item true, 42 => item false, item false } : item Bool;

baz = item bar;
