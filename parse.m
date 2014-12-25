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
    keyword(kw_struct, Src, !PS),
    parse_struct_def_body(Src, Struct, !PS).

:- pred parse_struct_def_body(src::in, struct_def::out, ps::in, ps::out) is semidet.

parse_struct_def_body(Src, Struct, !PS) :-
    identifier(Src, Ident, !PS),
    punct(":", Src, _, !PS),
    parse_field_type(Src, Type, !PS), % FIXME
    Size = calc_field_type_size(Type),
    punct("{", Src, _, !PS),
    comma_separated_list(parse_field_def, Src, Fields, !PS),
    punct("}", Src, _, !PS),
    Struct = struct_def(Ident, Size, Fields).

:- pred parse_field_type(src::in, field_type::out, ps::in, ps::out) is semidet.

parse_field_type(Src, Type, !PS) :-
    parse_word_type(Src, WordType, !PS),
    ( if punct("[", Src, _, !PS) then
	int_literal(Src, N, !PS),
	punct("]", Src, _, !PS),
	parse_field_values(Src, Values, !PS),
	Type = field_type_array(N, field_type_word(WordType, []), Values)
    else
	parse_field_values(Src, Values, !PS),
	Type = field_type_word(WordType, Values)
    ).

:- pred parse_field_values(src::in, list(field_value)::out, ps::in, ps::out) is semidet.

parse_field_values(Src, Values, !PS) :-
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
    ).

:- pred parse_word_type(src::in, word_type::out, ps::in, ps::out) is semidet.

parse_word_type(Src, Type, !PS) :-
    ( if keyword(kw_byte, Src, !PS) then
	Type = uint8
    else if keyword(kw_uint8, Src, !PS) then
	Type = uint8
    else if keyword(kw_uint16, Src, !PS) then
	Type = uint16
    else if keyword(kw_uint32, Src, !PS) then
	Type = uint32
    else
	fail
    ).

:- pred parse_field_def(src::in, field_def::out, ps::in, ps::out) is semidet.

parse_field_def(Src, Field, !PS) :-
    ( if keyword(kw_struct, Src, !PS) then
	parse_struct_def_body(Src, Struct, !PS),
	Name = Struct ^ struct_name,
	Size = Struct ^ struct_size,
	Type = field_type_struct(Struct ^ struct_fields)
    else
	identifier(Src, Name, !PS),
	punct(":", Src, _, !PS),
	parse_field_type(Src, Type, !PS),
	Size = calc_field_type_size(Type)
    ),
    Field = field_def(Name, Size, Type).

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

%--------------------------------------------------------------------%

:- type keyword
    --->    kw_struct
    ;	    kw_byte
    ;	    kw_uint8
    ;	    kw_uint16
    ;	    kw_uint32.

:- pred keyword_string(keyword, string).
:- mode keyword_string(in, out) is det.
:- mode keyword_string(out, in) is semidet.

keyword_string(kw_struct, "struct").
keyword_string(kw_byte, "byte").
keyword_string(kw_uint8, "uint8").
keyword_string(kw_uint16, "uint16").
keyword_string(kw_uint32, "uint32").

:- pred keyword(keyword::in, src::in, ps::in, ps::out) is semidet.

keyword(Keyword, Src, !PS) :-
    keyword_string(Keyword, Str),
    parsing_utils.keyword(identifier_chars, Str, Src, _, !PS).

:- pred identifier(src::in, string::out, ps::in, ps::out) is semidet.

identifier(Src, Ident, !PS) :-
    parsing_utils.identifier(identifier_chars_init, identifier_chars, Src, Ident, !PS),
    not keyword_string(_, Ident).

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

