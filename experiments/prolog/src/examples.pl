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

write_struct(enil) :-
    write(' }').
write_struct(epair(X, E1, E2)) :-
    format(', ~w: ', [X]),
    write_expr(E1),
    write_struct(E2).


%%%%%%%%%%%%%%
%  Examples  %
%%%%%%%%%%%%%%

inttype(B, int(B, eint(L), eint(U))) :-
    L is -(2**(B-1)),
    U is (2**(B-1))-1.

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
