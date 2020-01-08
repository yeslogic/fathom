foo = int 33 : global Int;

bar = int_elim item foo { 33 => global true, 42 => global false, global false } : global Bool;

baz = item bar;
