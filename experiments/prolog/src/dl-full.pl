:- consult(util).

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
repr(_, _, unit, unit).
repr(M, G, sigma(X, D1, D2), pair(X, T1, T2)) :-
    name(X),
    repr(M, G, D1, T1),
    repr(M, [X - T1 | G], D2, T2).
repr(M, G, array(D, E), array(T)) :-
    has_type(G, E, int),
    repr(M, G, D, T).


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
parse(_, _, unit, enil).
parse(Sub, In, sigma(X, D1, D2), epair(X, E1, E2)) :-
    parse(Sub, In, D1, E1),
    parse([X - E1 | Sub], In, D2, E2).
parse(Sub, In, array(D, EN), E) :-
    eval(Sub, EN, eint(N)),
    parse_array(Sub, In, D, N, E).

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

