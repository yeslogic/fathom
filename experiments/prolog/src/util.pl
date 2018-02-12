%%%%%%%%%%%
%  Names  %
%%%%%%%%%%%

name(X) :-
    atom(X).

%%%%%%%%%%%%%%%%%%%%%%
%  Structure access  %
%%%%%%%%%%%%%%%%%%%%%%

field_type(pair(Y, T1, T2), X, T) :-
    ( X = Y ->
        T = T1
    ;
        field_type(T2, X, T)
    ).

field_expr(epair(Y, E1, E2), X, E) :-
    ( X = Y ->
        E = E1
    ;
        field_expr(E2, X, E)
    ).


%%%%%%%%%%%%%%%%%%
%  Array access  %
%%%%%%%%%%%%%%%%%%

array_length(enil, 0).
array_length(econs(_, E), N) :-
    ( var(E) ->
        eval_error('array not ground')
    ;
        array_length(E, N1),
        N is N1 + 1
    ).

array_index(econs(E0, E1), N, E) :-
    ( N > 0 ->
        N1 is N - 1,
        array_index(E1, N1, E)
    ;
        E = E0
    ).


%%%%%%%%%%%
%  Input  %
%%%%%%%%%%%

read_bytes(In, B, K) :-
    uread_bytes(In, B, U),
    ( U >= 1 << (B - 1) ->
        K is U - 1 << B
    ;
        K = U
    ).

uread_bytes(In, B, K) :-
    ( B mod 8 =\= 0 ->
        nyi('integer size must be multiple of 8')
    ;
        uread_bits(In, B, K)
    ).

uread_bits(In, B, K) :-
    ( B > 0 ->
        get_byte(In, K0),
        B1 is B - 8,
        uread_bits(In, B1, K1),
        K is K0 + 256 * K1
    ;
        K = 0
    ).

%%%%%%%%%%%%
%  Errors  %
%%%%%%%%%%%%

static_error(Msg) :-
    error('Static error', Msg).

eval_error(Msg) :-
    error('Evaluation error', Msg).

nyi(Msg) :-
    error('Not yet implemented', Msg).

error(Label, Msg) :-
    format('~w: ~w~n', [Label, Msg]),
    fail.
