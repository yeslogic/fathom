foo = int 43 : Int;

Test = int_elim item foo { 42 => F64, F32 } : Type;
