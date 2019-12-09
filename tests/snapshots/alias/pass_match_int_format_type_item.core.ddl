foo = int 0 : item Int;

Test = int_elim item foo { 0 => item F64Le, item F64Be } : Format;
