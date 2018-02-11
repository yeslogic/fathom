% vim: ts=4 sw=4 et ft=prolog

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


