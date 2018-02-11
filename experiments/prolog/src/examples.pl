% vim: ts=4 sw=4 et ft=prolog

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
            format('Result: ~w~n', [V]),
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

inttype(B, int(B, eint(L), eint(U))) :-
    L is -(2**(B-1)),
    U is (2**(B-1))-1.

%%%%%%%%%%%%%%
%  Examples  %
%%%%%%%%%%%%%%

ex(xyz, D, 'bin') :-
    inttype(16, Int16),
    D = sigma(x, Int16,
        sigma(y, Int16,
        sigma(z, Int16,
        unit))).

