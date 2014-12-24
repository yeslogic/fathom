:- module parse.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module list, string, parsing_utils.

:- import_module ddl.

:- pred parse(string, parse_result(list(struct_def))).
:- mode parse(in, out) is det.

%--------------------------------------------------------------------%

:- implementation.

:- import_module maybe, unit.

parse(Str, Res) :-
    promise_equivalent_solutions [Res] (
	parsing_utils.parse(Str, skip_ws_comments, parse_ddl, Res)
    ).

:- pred parse_ddl(src::in, list(struct_def)::out, ps::in, ps::out) is semidet.

parse_ddl(Src, Structs, !PS) :-
    one_or_more(parse_struct_def, Src, Structs, !PS).

:- pred parse_struct_def(src::in, struct_def::out, ps::in, ps::out) is semidet.

parse_struct_def(Src, Struct, !PS) :-
    skip_ws_comments(Src, _, !PS),
    keyword("struct", Src, !PS),
    identifier(Src, Ident, !PS),
    punct(":", Src, _, !PS),
    parse_item_size(Src, Size, !PS),
    punct("{", Src, _, !PS),
    comma_separated_list(parse_field_def, Src, Fields, !PS),
    punct("}", Src, _, !PS),
    Struct = struct_def(Ident, yes(Size), Fields).

:- pred parse_item_size(src::in, item_size::out, ps::in, ps::out) is semidet.

parse_item_size(Src, Size, !PS) :-
    keyword("byte", Src, !PS),
    punct("[", Src, _, !PS),
    int_literal(Src, N, !PS),
    punct("]", Src, _, !PS),
    Size = item_size_bytes(N).

:- pred parse_field_def(src::in, field_def::out, ps::in, ps::out) is semidet.

parse_field_def(Src, Field, !PS) :-
    identifier(Src, Name, !PS),
    punct(":", Src, _, !PS),
    parse_item_size(Src, Size, !PS),
    ( if punct("=", Src, _, !PS) then
	parse_field_value(Src, V, !PS),
	( if punct("|", Src, _, !PS) then
	    separated_list("|", parse_field_value, Src, Vs, !PS),
	    Values = [V|Vs]
	else
	    Values = [V]
	)
    else
	Values = []
    ),
    Field = field_def(Name, Size, Values).

:- pred parse_field_value(src::in, field_value::out, ps::in, ps::out) is semidet.

parse_field_value(Src, Value, !PS) :-
    ( if punct("*", Src, _, !PS) then
	Value = field_value_any
    else if int_literal(Src, N, !PS) then
	Value = field_value_int(N)
    else
	next_char(Src, '\'', !PS),
	next_char(Src, C1, !PS),
	next_char(Src, C2, !PS),
	next_char(Src, C3, !PS),
	next_char(Src, C4, !PS),
	next_char(Src, '\'', !PS),
	skip_ws_comments(Src, _, !PS),
	Value = field_value_tag(C1, C2, C3, C4)
    ).

:- pred keyword(string::in, src::in, ps::in, ps::out) is semidet.

keyword(Keyword, Src, !PS) :-
    parsing_utils.keyword(identifier_chars, Keyword, Src, _, !PS).

:- pred identifier(src::in, string::out, ps::in, ps::out) is semidet.

identifier(Src, Ident, !PS) :-
    parsing_utils.identifier(identifier_chars_init, identifier_chars, Src, Ident, !PS).

:- func identifier_chars_init = string.

identifier_chars_init =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_".

:- func identifier_chars = string.

identifier_chars =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789".

:- pred skip_ws_comments(src::in, unit::out, ps::in, ps::out) is semidet.

skip_ws_comments(Src, Res, !PS) :-
    whitespace(Src, _, !PS),
    ( if
	next_char(Src, '/', !PS),
	next_char(Src, '/', !PS)
    then
        skip_to_eol(Src, _, !PS),
        skip_ws_comments(Src, Res, !PS)
    else
        Res = unit 
    ).

%--------------------------------------------------------------------%

:- end_module parse.

