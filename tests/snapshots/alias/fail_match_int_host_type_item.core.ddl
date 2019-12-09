foo = int 43 : item Int;

Test = int_elim item foo { 42 => item F64, item F32 } : Type;
