%%%%%%%%%%%%%%
%  Examples  %
%%%%%%%%%%%%%%

inttype(B, int(B, eint(L), eint(U))) :-
    L is -(2**(B-1)),
    U is (2**(B-1))-1.

% DL-1

% struct { x: int16; y: int16; z: int16; }
ex(xyz, D, 'bin') :-
    inttype(16, Int16),
    D = sigma(x, Int16,
        sigma(y, Int16,
        sigma(z, Int16,
        unit))).

% struct { xy: struct { x: int16; y: int16; }; z: int16; }
ex(xy_z, D, 'bin') :-
    inttype(16, Int16),
    D = sigma(xy,
            sigma(x, Int16,
            sigma(y, Int16,
            unit)),
        sigma(z, Int16,
        unit)).

% DL-ARRAY

% struct {
%   initcount: int32;
%   initvalues: struct { x: int8; y: int8; n: int16; }[initcount];
% }
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

% struct {
%   initcount: int32 in 1..80;
%   initvalues:
%       struct {
%           x: int8 in 0..8;
%           y: int8 in 0..8;
%           n: int16 in 1..9;
%       }[initcount];
% }
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

% DL-ARITH+ARRAY

% struct {
%   size: int16 in 1..0x7f;
%   initcount: int16 in 1..(size*size - 1);
%   initvalues:
%       struct {
%           x: int8 in 0..(size - 1);
%           y: int8 in 0..(size - 1);
%           n: int16 in 1..size;
%       }[initcount];
% }
ex(sudoku3, D, 'bin') :-
    MaxCount = eplus(etimes(evar(size), evar(size)), eint(-1)),
    MaxPos = eplus(evar(size), eint(-1)),
    D = sigma(size, int(16, eint(1), eint(0x7f)),
        sigma(initcount, int(16, eint(1), MaxCount),
        sigma(initvalues,
            array(
                sigma(x, int(8, eint(0), MaxPos),
                sigma(y, int(8, eint(0), MaxPos),
                sigma(n, int(16, eint(1), evar(size)),
                unit))),
                evar(initcount)
            ),
        unit))).

% DL-UNSIGNED+ARRAY

% struct {
%   initcount: uint32 in 1..80;
%   initvalues:
%       struct {
%           x: uint8 in 0..8;
%           y: uint8 in 0..8;
%           n: uint16 in 1..9;
%       }[initcount];
% }
ex(sudoku4, D, 'bin') :-
    D = sigma(initcount, uint(32, eint(1), eint(80)),
        sigma(initvalues,
            array(
                sigma(x, uint(8, eint(0), eint(8)),
                sigma(y, uint(8, eint(0), eint(8)),
                sigma(n, uint(16, eint(1), eint(9)),
                unit))),
                evar(initcount)
            ),
        unit)).

% DL-REC

% rec ascii = union {
%               struct {
%                   ch: int8 in 0..127;
%                   next: ascii;
%               };
%               unit;
%           }
ex(ascii, D, 'txt') :-
    D = mu(ascii,
            union(
                sigma(ch, int(8, eint(0), eint(127)),
                sigma(next, mvar(ascii),
                unit)),
                unit
            )
        ).

% DL-REC+COND

% rec tree =
%   struct {
%       data: int8;
%       children: rec list =
%           struct {
%               tag: int8 in 0..1;
%               val: if tag <= 0 then unit else
%                       struct {
%                           child: tree;
%                           next: list;
%                       };
%           }
%   }
ex(tree, D, 'bin') :-
    inttype(8, Int8),
    D = mu(tree,
            sigma(data, Int8,
            sigma(children, mu(list,
                sigma(tag, int(8, eint(0), eint(1)),
                sigma(val, cond(eleq(evar(tag), eint(0)), unit,
                    sigma(child, mvar(tree),
                    sigma(next, mvar(list),
                    unit))),
                unit))),
            unit))).

% DL-REC+ARITH+UNSIGNED

% struct {
%   size: int32 in 1..256;
%   initvalues: rec t =
%       union {
%           int32 in -1..-1;
%           struct {
%               x: uint8 in 0..(size - 1);
%               y: uint8 in 0..(size - 1);
%               n: uint16 in 1..size;
%               next: t;
%           }
%       }
% }
ex(sudoku5, D, 'bin') :-
    PosT = uint(8, eint(0), eplus(evar(size), eint(-1))),
    D = sigma(size, int(32, eint(1), eint(256)),
        sigma(initvalues,
            mu(t,
                union(
                    int(32, eint(-1), eint(-1)),    % sentinel
                    sigma(x, PosT,
                    sigma(y, PosT,
                    sigma(n, uint(16, eint(1), evar(size)),
                    sigma(next, mvar(t),
                    unit))))
                )
            ),
        unit)).

