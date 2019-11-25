foo = int 33 : Int;

bar = int_elim item foo { 33 => true, 42 => false, false } : Bool;

baz = item bar;
