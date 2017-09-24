:- module abort.

% Copyright (C) 2005-2007, 2009 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module string, univ.

:- pred abort(string).
:- mode abort(in) is erroneous.

:- func exception_to_string(univ) = string.

%--------------------------------------------------------------------%

:- implementation.

:- import_module list, require, exception.

abort(Msg) :- error(Msg).

exception_to_string(Univ) = S :-
    ( if univ_to_type(Univ, software_error(Msg)) then
        S = Msg
    else
        S = string(univ_value(Univ))
    ).

%--------------------------------------------------------------------%

:- end_module abort.

