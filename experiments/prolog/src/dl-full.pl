%%%%%%%%%%%%%%%%%%%%
%  Representation  %
%%%%%%%%%%%%%%%%%%%%

repr(int(_, _, _), int).
repr(uint(_, _, _), int).
repr(unit, unit).
repr(sigma(X, D1, D2), pair(X, T1, T2)) :-
    repr(D1, T1),
    repr(D2, T2).
repr(array(D, _), array(T)) :-
    repr(D, T).
repr(union(D1, D2), sum(T1, T2)) :-
    repr(D1, T1),
    repr(D2, T2).
repr(absorb(_), unit).
repr(compute(_, D), T) :-
    repr(D, T).
repr(cond(_, D1, D2), sum(T1, T2)) :-
    repr(D1, T1),
    repr(D2, T2).
repr(mvar(A), tvar(A)).
repr(mu(A, D), mu(A, T)) :-
    repr(D, T).


%%%%%%%%%%%
%  Kinds  %
%%%%%%%%%%%

check_desc(D, T) :-
    check_desc([], [], D, T).

check_desc(_, G, int(B, EL, EU), _) :-
    integer(B),
    B > 0,
    has_type(G, EL, int),
    has_type(G, EU, int).
check_desc(_, G, uint(B, EL, EU), _) :-
    integer(B),
    B > 0,
    has_type(G, EL, int),
    has_type(G, EU, int).
check_desc(_, _, unit, _).
check_desc(M, G, sigma(X, D1, D2), pair(_, T1, T2)) :-
    name(X),
    check_desc(M, G, D1, T1),
    rsub(M, [], T1, TX),
    check_desc(M, [X - TX | G], D2, T2).
check_desc(M, G, array(D, E), array(T)) :-
    has_type(G, E, int),
    check_desc(M, G, D, T).
check_desc(M, G, union(D1, D2), sum(T1, T2)) :-
    check_desc(M, G, D1, T1),
    check_desc(M, G, D2, T2).
check_desc(M, G, absorb(D), _) :-
    repr(D, T),
    check_desc(M, G, D, T).
check_desc(M, G, compute(E, D), T) :-
    has_type(G, E, T),
    check_desc(M, G, D, T).
check_desc(M, G, cond(E, D1, D2), sum(T1, T2)) :-
    has_type(G, E, bool),
    check_desc(M, G, D1, T1),
    check_desc(M, G, D2, T2).
check_desc(M, _, mvar(A), _) :-
    member(A - _, M).
check_desc(M, G, mu(A, D), mu(_, T)) :-
    check_desc([A - mu(A, T) | M], G, D, T).


%%%%%%%%%%%
%  Types  %
%%%%%%%%%%%

has_type(G, evar(X), U) :-
    member(X - T, G),
    unroll(T, U).
has_type(_, eint(K), int) :-
    integer(K).
has_type(_, enil, unit).
has_type(G, epair(X, E1, E2), pair(X, T1, T2)) :-
    name(X),
    has_type(G, E1, T1),
    has_type(G, E2, T2).
has_type(G, efield(E, X), UX) :-
    has_type(G, E, T),
    field_type(T, X, TX),
    unroll(TX, UX).
has_type(_, enil, array(_)).
has_type(G, econs(E1, E2), array(T)) :-
    has_type(G, E1, T),
    has_type(G, E2, array(T)).
has_type(G, elength(E), int) :-
    has_type(G, E, array(_)).
has_type(G, eindex(E1, E2), U) :-
    has_type(G, E1, array(T)),
    has_type(G, E2, int),
    unroll(T, U).
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
has_type(_, etrue, bool).
has_type(_, efalse, bool).
has_type(G, elt(E1, E2), bool) :-
    has_type(G, E1, int),
    has_type(G, E2, int).
has_type(G, eleq(E1, E2), bool) :-
    has_type(G, E1, int),
    has_type(G, E2, int).
has_type(G, econd(E, E1, E2), T) :-
    has_type(G, E, bool),
    has_type(G, E1, T),
    has_type(G, E2, T).
has_type(G, enot(E), bool) :-
    has_type(G, E, bool).
has_type(G, eor(E1, E2), bool) :-
    has_type(G, E1, bool),
    has_type(G, E2, bool).
has_type(G, eand(E1, E2), bool) :-
    has_type(G, E1, bool),
    has_type(G, E2, bool).
has_type(G, E, mu(A, T)) :-
    tsub(mu(A, T), A, T, U),
    has_type(G, E, U).

unroll(T, U) :-
    ( T = mu(A, T1) ->
        tsub(T, A, T1, U)
    ;
        U = T
    ).


%%%%%%%%%%%%
%  Parser  %
%%%%%%%%%%%%

parse(Sub, In, int(B, EL, EU), eint(K)) :-
    eval(Sub, EL, eint(L)),
    eval(Sub, EU, eint(U)),
    read_bytes(In, B, K),
    L =< K, K =< U.
parse(Sub, In, uint(B, EL, EU), eint(K)) :-
    eval(Sub, EL, eint(L)),
    eval(Sub, EU, eint(U)),
    uread_bytes(In, B, K),
    L =< K, K =< U.
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
parse(Sub, In, absorb(D), nil) :-
    parse(Sub, In, D, _).
parse(Sub, _, compute(E, _), V) :-
    eval(Sub, E, V).
parse(Sub, In, cond(E, D1, D2), V) :-
    eval(Sub, E, VB),
    parse_cond(Sub, In, VB, D1, D2, V).
parse(Sub, In, mu(A, D), V) :-
    dsub(mu(A, D), A, D, U),
    parse(Sub, In, U, V).

parse_array(Sub, In, D, N, E) :-
    ( N > 0 ->
        parse(Sub, In, D, E0),
        N1 is N - 1,
        parse_array(Sub, In, D, N1, E1),
        E = econs(E0, E1)
    ;
        E = enil
    ).

parse_cond(Sub, In, etrue, D, _, V) :-
    parse(Sub, In, D, V).
parse_cond(Sub, In, efalse, _, D, V) :-
    parse(Sub, In, D, V).


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
eval(_, etrue, etrue).
eval(_, efalse, efalse).
eval(Sub, elt(E1, E2), R) :-
    eval(Sub, E1, eint(K1)),
    eval(Sub, E2, eint(K2)),
    ( K1 < K2 -> R = etrue ; R = efalse ).
eval(Sub, eleq(E1, E2), R) :-
    eval(Sub, E1, eint(K1)),
    eval(Sub, E2, eint(K2)),
    ( K1 =< K2 -> R = etrue ; R = efalse ).
eval(Sub, econd(E, E1, E2), V) :-
    eval(Sub, E, V0),
    eval_cond(Sub, V0, E1, E2, V).
eval(Sub, enot(E1), V) :-
    eval(Sub, E1, V1),
    expr_not(V1, V).
eval(Sub, eor(E1, E2), V) :-
    eval(Sub, E1, V1),
    eval(Sub, E2, V2),
    expr_or(V1, V2, V).
eval(Sub, eand(E1, E2), V) :-
    eval(Sub, E1, V1),
    eval(Sub, E2, V2),
    expr_and(V1, V2, V).

eval_case(Sub, eleft(VX), X, E, _, V) :-
    eval([X - VX | Sub], E, V).
eval_case(Sub, eright(VX), X, _, E, V) :-
    eval([X - VX | Sub], E, V).

eval_cond(Sub, etrue, E, _, V) :-
    eval(Sub, E, V).
eval_cond(Sub, efalse, _, E, V) :-
    eval(Sub, E, V).

expr_not(etrue, efalse).
expr_not(efalse, etrue).

expr_or(etrue, _, etrue).
expr_or(efalse, etrue, etrue).
expr_or(efalse, efalse, efalse).

expr_and(etrue, etrue, etrue).
expr_and(etrue, efalse, efalse).
expr_and(efalse, _, efalse).


%%%%%%%%%%%%%%%%%%%
%  Substitutions  %
%%%%%%%%%%%%%%%%%%%

% Recursively substitute free variables.
rsub(_, _, int, int).
rsub(_, _, unit, unit).
rsub(M, BVars, pair(X, T1, T2), pair(X, U1, U2)) :-
    rsub(M, BVars, T1, U1),
    rsub(M, BVars, T2, U2).
rsub(M, BVars, array(T), array(U)) :-
    rsub(M, BVars, T, U).
rsub(M, BVars, sum(T1, T2), sum(U1, U2)) :-
    rsub(M, BVars, T1, U1),
    rsub(M, BVars, T2, U2).
rsub(M, BVars, tvar(A), U) :-
    ( member(A, BVars) ->
        U = tvar(A)
    ;
        append(_, [A - T | M1], M),
        rsub(M1, [], T, U)
    ).
rsub(M, BVars, mu(A, T), mu(A, U)) :-
    rsub(M, [A | BVars], T, U).

% Substitute a free type variable.
tsub(_, _, int, int).
tsub(_, _, unit, unit).
tsub(S, A, pair(X, T1, T2), pair(X, U1, U2)) :-
    tsub(S, A, T1, U1),
    tsub(S, A, T2, U2).
tsub(S, A, array(T), array(U)) :-
    tsub(S, A, T, U).
tsub(S, A, sum(T1, T2), sum(U1, U2)) :-
    tsub(S, A, T1, U1),
    tsub(S, A, T2, U2).
tsub(S, A, tvar(B), U) :-
    ( A = B ->
        U = S
    ;
        U = tvar(B)
    ).
tsub(S, A, mu(B, T), mu(B, U)) :-
    ( A = B ->
        U = T
    ;
        tsub(S, A, T, U)
    ).

% Substitute a free description variable.
dsub(_, _, int(B, EL, EU), int(B, EL, EU)).
dsub(_, _, uint(B, EL, EU), uint(B, EL, EU)).
dsub(_, _, unit, unit).
dsub(S, A, sigma(X, D1, D2), sigma(X, U1, U2)) :-
    dsub(S, A, D1, U1),
    dsub(S, A, D2, U2).
dsub(S, A, array(D, E), array(U, E)) :-
    dsub(S, A, D, U).
dsub(S, A, union(D1, D2), union(U1, U2)) :-
    dsub(S, A, D1, U1),
    dsub(S, A, D2, U2).
dsub(S, A, absorb(D), absorb(U)) :-
    dsub(S, A, D, U).
dsub(S, A, compute(E, D), compute(E, U)) :-
    dsub(S, A, D, U).
dsub(S, A, cond(E, D1, D2), cond(E, U1, U2)) :-
    dsub(S, A, D1, U1),
    dsub(S, A, D2, U2).
dsub(S, A, mvar(B), U) :-
    ( A = B ->
        U = S
    ;
        U = mvar(B)
    ).
dsub(S, A, mu(B, D), mu(B, U)) :-
    ( A = B ->
        U = D
    ;
        dsub(S, A, D, U)
    ).

