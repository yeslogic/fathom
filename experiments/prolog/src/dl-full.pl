%%%%%%%%%%%
%  Kinds  %
%%%%%%%%%%%

check_desc(D) :-
    repr([], [], D, _).

repr(_, G, int(B, EL, EU), int) :-
    integer(B),
    B > 0,
    has_type(G, EL, int),
    has_type(G, EU, int).
repr(_, G, uint(B, EL, EU), int) :-
    integer(B),
    B > 0,
    has_type(G, EL, int),
    has_type(G, EU, int).
repr(_, _, unit, unit).
repr(M, G, sigma(X, D1, D2), pair(X, T1, T2)) :-
    name(X),
    repr(M, G, D1, T1),
    repr(M, [X - T1 | G], D2, T2).
repr(M, G, array(D, E), array(T)) :-
    has_type(G, E, int),
    repr(M, G, D, T).
repr(M, G, union(D1, D2), sum(T1, T2)) :-
    repr(M, G, D1, T1),
    repr(M, G, D2, T2).


%%%%%%%%%%%
%  Types  %
%%%%%%%%%%%

has_type(G, evar(X), T) :-
    member(X - T, G).
has_type(_, eint(K), int) :-
    integer(K).
has_type(_, enil, unit).
has_type(G, epair(X, E1, E2), pair(X, T1, T2)) :-
    name(X),
    has_type(G, E1, T1),
    has_type(G, E2, T2).
has_type(G, efield(E, X), TX) :-
    has_type(G, E, T),
    field_type(T, X, TX).
has_type(_, enil, array(_)).
has_type(G, econs(E1, E2), array(T)) :-
    has_type(G, E1, T),
    has_type(G, E2, array(T)).
has_type(G, elength(E), int) :-
    has_type(G, E, array(_)).
has_type(G, eindex(E1, E2), T) :-
    has_type(G, E1, array(T)),
    has_type(G, E2, int).
has_type(G, eleft(E), sum(T, _)) :-
    has_type(G, E, T).
has_type(G, eright(E), sum(_, T)) :-
    has_type(G, E, T).
has_type(G, ecase(E, X, E1, E2), T) :-
    has_type(G, E, sum(T1, T2)),
    has_type([X - T1 | G], E1, T),
    has_type([X - T2 | G], E2, T).
has_type(G, eplus(E1, E2), int) :-
    has_type(G, E1, int),
    has_type(G, E2, int).
has_type(G, etimes(E1, E2), int) :-
    has_type(G, E1, int),
    has_type(G, E2, int).
has_type(G, eminus(E), int) :-
    has_type(G, E, int).


%%%%%%%%%%%%
%  Parser  %
%%%%%%%%%%%%

parse(Sub, In, int(B, EL, EU), eint(K)) :-
    eval(Sub, EL, eint(L)),
    eval(Sub, EU, eint(U)),
    read_bytes(In, B, K),
    ( L =< K, K =< U ->
        true
    ;
        parse_error('integer out of range')
    ).
parse(Sub, In, uint(B, EL, EU), eint(K)) :-
    eval(Sub, EL, eint(L)),
    eval(Sub, EU, eint(U)),
    uread_bytes(In, B, K),
    ( L =< K, K =< U ->
        true
    ;
        parse_error('integer out of range')
    ).
parse(_, _, unit, enil).
parse(Sub, In, sigma(X, D1, D2), epair(X, E1, E2)) :-
    parse(Sub, In, D1, E1),
    parse([X - E1 | Sub], In, D2, E2).
parse(Sub, In, array(D, EN), E) :-
    eval(Sub, EN, eint(N)),
    parse_array(Sub, In, D, N, E).
parse(Sub, In, union(D, _), eleft(E)) :-
    parse(Sub, In, D, E).
parse(Sub, In, union(_, D), eright(E)) :-
    parse(Sub, In, D, E).

parse_array(Sub, In, D, N, E) :-
    ( N > 0 ->
        parse(Sub, In, D, E0),
        N1 is N - 1,
        parse_array(Sub, In, D, N1, E1),
        E = econs(E0, E1)
    ;
        E = enil
    ).


%%%%%%%%%%%%%%%%
%  Evaluation  %
%%%%%%%%%%%%%%%%

eval(Sub, evar(X), V) :-
    member(X - V, Sub).
eval(_, eint(K), eint(K)).
eval(_, enil, enil).
eval(Sub, epair(E1, E2), epair(V1, V2)) :-
    eval(Sub, E1, V1),
    eval(Sub, E2, V2).
eval(Sub, efield(E, X), VX) :-
    eval(Sub, E, V),
    field_expr(V, X, VX).
eval(Sub, econs(E1, E2), econs(V1, V2)) :-
    eval(Sub, E1, V1),
    eval(Sub, E2, V2).
eval(Sub, elength(E), eint(N)) :-
    eval(Sub, E, V),
    array_length(V, N).
eval(Sub, eindex(E1, E2), V) :-
    eval(Sub, E1, V1),
    eval(Sub, E2, eint(N)),
    array_index(V1, N, V).
eval(Sub, eleft(E), eleft(V)) :-
    eval(Sub, E, V).
eval(Sub, eright(E), eright(V)) :-
    eval(Sub, E, V).
eval(Sub, ecase(E, X, E1, E2), V) :-
    eval(Sub, E, V0),
    eval_case(Sub, V0, X, E1, E2, V).
eval(Sub, eplus(E1, E2), eint(V)) :-
    eval(Sub, E1, eint(V1)),
    eval(Sub, E2, eint(V2)),
    V is V1 + V2.
eval(Sub, etimes(E1, E2), eint(V)) :-
    eval(Sub, E1, eint(V1)),
    eval(Sub, E2, eint(V2)),
    V is V1 * V2.
eval(Sub, eminus(E1), eint(V)) :-
    eval(Sub, E1, eint(V1)),
    V is -V1.

eval_case(Sub, eleft(VX), X, E, _, V) :-
    eval([X - VX | Sub], E, V).
eval_case(Sub, eright(VX), X, _, E, V) :-
    eval([X - VX | Sub], E, V).

