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
    ; \+ check_desc(D) ->
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


%%%%%%%%%%%%%%
%  Examples  %
%%%%%%%%%%%%%%

inttype(B, int(B, eint(L), eint(U))) :-
    L is -(2**(B-1)),
    U is (2**(B-1))-1.

% DL-1

ex(xyz, D, 'bin') :-
    inttype(16, Int16),
    D = sigma(x, Int16,
        sigma(y, Int16,
        sigma(z, Int16,
        unit))).

ex(xy_z, D, 'bin') :-
    inttype(16, Int16),
    D = sigma(xy,
            sigma(x, Int16,
            sigma(y, Int16,
            unit)),
        sigma(z, Int16,
        unit)).

% DL-ARRAY

ex(sudoku1, D, 'bin') :-
    inttype(8, Int8),
    inttype(16, Int16),
    inttype(32, Int32),
    D = sigma(initcount, Int32,
        sigma(initvalues,
            array(
                sigma(x, Int8,
                sigma(y, Int8,
                sigma(n, Int16,
                unit))),
                evar(initcount)
            ),
        unit)).

ex(sudoku2, D, 'bin') :-
    D = sigma(initcount, int(32, eint(1), eint(80)),
        sigma(initvalues,
            array(
                sigma(x, int(8, eint(0), eint(8)),
                sigma(y, int(8, eint(0), eint(8)),
                sigma(n, int(16, eint(1), eint(9)),
                unit))),
                evar(initcount)
            ),
        unit)).

