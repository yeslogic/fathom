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

