% vim: ts=4 sw=4 et ft=prolog

run :-
    runtest(_),
    fail.

runtest(Name) :-
    ex(Name, D, Ext),
    format('Test: ~w~n', [Name]),
    atomic_list_concat(['../test_data/', Name, '-*.', Ext], Pat),
    expand_file_name(Pat, Files),
    ( \+ ground(D) ->
        static_error('description not ground')
    ; \+ (repr(D, T), check_desc(D, T)) ->
        static_error('ill-kinded description')
    ; Files = [] ->
        writeln('no data files')
    ;
        parse_files(D, Files)
    ).

parse_files(_, []).
parse_files(D, [File | Files]) :-
    parse_file(D, File),
    parse_files(D, Files).

parse_file(D, File) :-
    ( open(File, read, In, [type(binary)]) ->
        format('File: ~w~n', [File]),
        ( parse([], In, D, V) ->
            write('Result: '),
            write_expr(V),
            nl,
            ( \+ get_byte(In, -1) ->
                writeln('remaining input')
            ;
                true
            )
        ;
            writeln('parse failed')
        ),
        close(In)
    ;
        static_error('unable to open file')
    ).

write_expr(evar(X)) :-
    write(X).
write_expr(eint(K)) :-
    write(K).
write_expr(enil) :-
    write(nil).
write_expr(epair(X, E1, E2)) :-
    format('{ ~w: ', [X]),
    write_expr(E1),
    write_struct(E2).
write_expr(efield(E, X)) :-
    write_expr(E),
    format('.~w', [X]).
write_expr(econs(E1, E2)) :-
    write('['),
    write_expr(E1),
    write_array(E2).
write_expr(elength(E)) :-
    write_expr(E),
    write('.length').
write_expr(eindex(E1, E2)) :-
    write_expr(E1),
    write('['),
    write_expr(E2),
    write(']').
write_expr(eleft(E)) :-
    write('inl '),
    write_expr(E).
write_expr(eright(E)) :-
    write('inr '),
    write_expr(E).
write_expr(ecase(E, X, E1, E2)) :-
    write('case '),
    write_expr(E),
    format(' ~w ', [X]),
    write_expr(E1),
    write(' '),
    write_expr(E2).
write_expr(eplus(E1, E2)) :-
    write('('),
    write_expr(E1),
    write(' + '),
    write_expr(E2),
    write(')').
write_expr(etimes(E1, E2)) :-
    write('('),
    write_expr(E1),
    write(' * '),
    write_expr(E2),
    write(')').
write_expr(eminus(E)) :-
    write('-'),
    write_expr(E).

write_struct(enil) :-
    write(' }').
write_struct(epair(X, E1, E2)) :-
    format(', ~w: ', [X]),
    write_expr(E1),
    write_struct(E2).

write_array(enil) :-
    write(']').
write_array(econs(E1, E2)) :-
    write(', '),
    write_expr(E1),
    write_array(E2).


