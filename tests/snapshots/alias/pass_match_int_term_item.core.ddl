foo = int 33 : item Int;

bar = int_elim item foo { 33 => true, 42 => false, false } : item Bool;

baz = item bar;
