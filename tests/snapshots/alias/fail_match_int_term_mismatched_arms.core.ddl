Foo = f32 33.4 : global F32;

test = int_elim int 23 : global Int { 23 => global true, ! } : global Bool;
