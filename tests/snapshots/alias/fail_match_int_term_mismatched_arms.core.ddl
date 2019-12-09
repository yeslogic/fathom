Foo = f32 33.4 : item F32;

test = int_elim int 23 : item Int { 23 => item true, ! } : item Bool;
