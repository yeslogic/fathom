:- module parse.

% Copyright (C) 2014-2015 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module string, parsing_utils.

:- import_module ddl.

:- pred parse(string, parse_result(ddl)).
:- mode parse(in, out) is det.

%--------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list, list, pair, maybe, unit, char, int.

parse(Str, Res) :-
    promise_equivalent_solutions [Res] (
	parsing_utils.parse(Str, skip_ws_comments, parse_ddl, Res)
    ).

:- pred parse_ddl(src::in, ddl::out, ps::in, ps::out) is semidet.

parse_ddl(Src, Structs, !PS) :-
    one_or_more(parse_ddl_def, Src, Structs, !PS),
    eof(Src, _, !PS).

:- pred parse_ddl_def(src::in, pair(string, ddl_def)::out, ps::in, ps::out) is semidet.

parse_ddl_def(Src, Ident-Def, !PS) :-
    skip_ws_comments(Src, _, !PS),
    identifier(Src, Ident, !PS),
    ( if punct("(", Src, _, !PS) then
	parse_args(Src, Args, !PS),
	punct(")", Src, _, !PS)
    else
	Args = []
    ),
    punct(":", Src, _, !PS),
    ( if keyword(kw_struct, Src, !PS) then
	punct("{", Src, _, !PS),
	comma_separated_list(parse_field_def, Src, Fields, !PS),
	punct("}", Src, _, !PS),
	Type = ddl_type_struct(Fields)
    else
	keyword(kw_union, Src, !PS),
	punct("{", Src, _, !PS),
	comma_separated_list(identifier, Src, Options, !PS),
	punct("}", Src, _, !PS),
	Type = ddl_type_union(Options)
    ),
    Def = ddl_def(Ident, Args, Type).

:- type multiplicity
    --->    mult_singular
    ;	    mult_array(expr)
    ;	    mult_zero_or_more.

:- pred parse_field_def(src::in, field_def::out, ps::in, ps::out) is semidet.

parse_field_def(Src, Field, !PS) :-
    ( if at_rule(at_if, Src, !PS) then
	skip_ws_comments(Src, _, !PS),
	parse_expr(Src, Expr, !PS),
	punct(":", Src, _, !PS),
	Cond = yes(Expr)
    else
	Cond = no
    ),
    identifier(Src, Name, !PS),
    parse_multiplicity(Src, Mult, !PS),
    punct(":", Src, _, !PS),
    parse_ddl_type(Src, Type0, !PS),
    Type = apply_multiplicity(Mult, Type0),
    Field = field_def(Name, Type, Cond).

:- func apply_multiplicity(multiplicity, ddl_type) = ddl_type.

apply_multiplicity(Mult, Type0) = Type :-
    (
	Mult = mult_singular,
	Type = Type0
    ;
	Mult = mult_array(SizeExpr),
	Type = ddl_type_array(SizeExpr, Type0)
    ;
	Mult = mult_zero_or_more,
	Type = ddl_type_zero_or_more(Type0)
    ).

:- pred parse_multiplicity(src::in, multiplicity::out, ps::in, ps::out) is semidet.

parse_multiplicity(Src, Mult, !PS) :-
    ( if punct("[", Src, _, !PS) then
	parse_expr(Src, SizeExpr0, !PS),
	punct("]", Src, _, !PS),
	Mult = mult_array(SizeExpr0)
    else if punct("*", Src, _, !PS) then
	Mult = mult_zero_or_more
    else
	Mult = mult_singular
    ).

:- pred parse_ddl_type(src::in, ddl_type::out, ps::in, ps::out) is semidet.

parse_ddl_type(Src, Type, !PS) :-
    ( if keyword(kw_struct, Src, !PS) then
	parse_multiplicity(Src, Mult, !PS),
	punct("{", Src, _, !PS),
	comma_separated_list(parse_field_def, Src, Fields, !PS),
	punct("}", Src, _, !PS),
	Type0 = ddl_type_struct(Fields),
	Type1 = apply_multiplicity(Mult, Type0)
    else if identifier(Src, Ident, !PS) then
	( if Ident = "tag_magic" then
	    punct("(", Src, _, !PS),
	    identifier(Src, TagName, !PS),
	    punct(")", Src, _, !PS),
	    Type0 = ddl_type_tag_magic(TagName)
	else if punct("(", Src, _, !PS) then
	    parse_args(Src, Args, !PS),
	    punct(")", Src, _, !PS),
	    Type0 = ddl_type_named(Ident, Args)
	else
	    Type0 = ddl_type_named(Ident, [])
	),
	parse_multiplicity(Src, Mult, !PS),
	Type1 = apply_multiplicity(Mult, Type0)
    else
	parse_word_type(Src, WordType, !PS),
	parse_multiplicity(Src, Mult, !PS),
	parse_word_values(Src, WordValues, !PS),
	Type0 = ddl_type_word(WordType, WordValues),
	Type1 = apply_multiplicity(Mult, Type0)
    ),
    ( if punct("&", Src, _, !PS) then
	parse_expr(Src, Expr, !PS),
	Type = ddl_type_sized(Type1, Expr)
    else
	Type = Type1
    ).

:- pred parse_args(src::in, list(string)::out, ps::in, ps::out) is semidet.

parse_args(Src, Args, !PS) :-
    identifier(Src, Ident, !PS),
    ( if punct(",", Src, _, !PS) then
	parse_args(Src, Args0, !PS),
	Args = [Ident|Args0]
    else
	Args = [Ident]
    ).

:- pred parse_expr(src::in, expr::out, ps::in, ps::out) is semidet.

parse_expr(Src, Expr, !PS) :-
    parse_term(Src, Expr0, !PS),
    parse_expr0(Expr0, Src, Expr, !PS).

:- pred parse_expr0(expr::in, src::in, expr::out, ps::in, ps::out) is semidet.

parse_expr0(Expr0, Src, Expr, !PS) :-
    ( if punct("+", Src, _, !PS) then
	parse_term(Src, Expr1, !PS),
	Expr2 = expr_op(expr_add, Expr0, Expr1),
	parse_expr0(Expr2, Src, Expr, !PS)
    else if punct("-", Src, _, !PS) then
	parse_term(Src, Expr1, !PS),
	Expr2 = expr_op(expr_sub, Expr0, Expr1),
	parse_expr0(Expr2, Src, Expr, !PS)
    else if punct("*", Src, _, !PS) then
	parse_term(Src, Expr1, !PS),
	Expr2 = expr_op(expr_mul, Expr0, Expr1),
	parse_expr0(Expr2, Src, Expr, !PS)
    else if punct("&", Src, _, !PS) then
	parse_term(Src, Expr1, !PS),
	Expr2 = expr_op(expr_and, Expr0, Expr1),
	parse_expr0(Expr2, Src, Expr, !PS)
    else
	Expr = Expr0
    ).

:- pred parse_term(src::in, expr::out, ps::in, ps::out) is semidet.

parse_term(Src, Expr, !PS) :-
    ( if hex_literal(Src, N, !PS) then
	Expr = expr_const(N)
    else if int_literal(Src, N, !PS) then
	Expr = expr_const(N)
    else
	identifier(Src, Ident, !PS),
	Expr = expr_field(Ident)
    ).

:- pred parse_word_values(src::in, word_values::out, ps::in, ps::out) is semidet.

parse_word_values(Src, Values, !PS) :-
    ( if punct("=", Src, _, !PS) then
	parse_word_value(Src, V, !PS),
	parse_word_values0([V], Src, Values, !PS)
    else
	( if parse_word_interp(Src, Interp, !PS) then
	    AnyInterp = yes(Interp)
	else
	    AnyInterp = no
	),
	Values = word_values(AnyInterp, [])
    ).

:- pred parse_word_values0(assoc_list(word_value, word_interp)::in, src::in, word_values::out, ps::in, ps::out) is semidet.

parse_word_values0(Vs, Src, Values, !PS) :-
    ( if punct("|", Src, _, !PS) then
	( if parse_word_value(Src, V, !PS) then
	    parse_word_values0([V|Vs], Src, Values, !PS)
	else
	    parse_word_interp(Src, Interp, !PS),
	    Values = word_values(yes(Interp), Vs)
	)
    else
	Values = word_values(no, Vs)
    ).

:- pred parse_word_value(src::in, pair(word_value, word_interp)::out, ps::in, ps::out) is semidet.

parse_word_value(Src, Value-Interp, !PS) :-
    ( if hex_literal(Src, N, !PS) then
	Value = word_value_int(N)
    else if int_literal(Src, N, !PS) then
	Value = word_value_int(N)
    else
	next_char(Src, '\'', !PS),
	next_char(Src, C1, !PS),
	next_char(Src, C2, !PS),
	next_char(Src, C3, !PS),
	next_char(Src, C4, !PS),
	next_char(Src, '\'', !PS),
	Value = word_value_tag(C1, C2, C3, C4)
    ),
    skip_ws_comments(Src, _, !PS),
    ( if parse_word_interp(Src, Interp0, !PS) then
	Interp = Interp0
    else
	Interp = word_interp_none
    ).

:- pred parse_word_interp(src::in, word_interp::out, ps::in, ps::out) is semidet.

parse_word_interp(Src, Interp, !PS) :-
    at_rule(at_offset, Src, !PS),
    ( if punct("(", Src, _, !PS) then
	parse_offset_base(Src, Base, !PS),
	punct(")", Src, _, !PS)
    else
	Base = offset_base_struct
    ),
    punct("=>", Src, _, !PS),
    parse_ddl_type(Src, Type, !PS),
    Interp = word_interp_offset(offset(Base, Type)).

:- pred parse_offset_base(src::in, offset_base::out, ps::in, ps::out) is semidet.

parse_offset_base(Src, Base, !PS) :-
    ( if at_rule(at_root, Src, !PS) then
	Base = offset_base_root
    else
	identifier(Src, Ident, !PS),
	Base = offset_base_named(Ident)
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
    else if keyword(kw_uint64, Src, !PS) then
	Type = uint64
    else if keyword(kw_int8, Src, !PS) then
	Type = int8
    else if keyword(kw_int16, Src, !PS) then
	Type = int16
    else if keyword(kw_int32, Src, !PS) then
	Type = int32
    else if keyword(kw_int64, Src, !PS) then
	Type = int64
    else
	fail
    ).

%--------------------------------------------------------------------%

:- type keyword
    --->    kw_union
    ;	    kw_struct
    ;	    kw_byte
    ;	    kw_uint8
    ;	    kw_uint16
    ;	    kw_uint32
    ;	    kw_uint64
    ;	    kw_int8
    ;	    kw_int16
    ;	    kw_int32
    ;	    kw_int64.

:- pred keyword_string(keyword, string).
:- mode keyword_string(in, out) is det.
:- mode keyword_string(out, in) is semidet.

keyword_string(kw_union, "union").
keyword_string(kw_struct, "struct").
keyword_string(kw_byte, "byte").
keyword_string(kw_uint8, "uint8").
keyword_string(kw_uint16, "uint16").
keyword_string(kw_uint32, "uint32").
keyword_string(kw_uint64, "uint64").
keyword_string(kw_int8, "int8").
keyword_string(kw_int16, "int16").
keyword_string(kw_int32, "int32").
keyword_string(kw_int64, "int64").

:- pred keyword(keyword::in, src::in, ps::in, ps::out) is semidet.

keyword(Keyword, Src, !PS) :-
    keyword_string(Keyword, Str),
    parsing_utils.keyword(identifier_chars, Str, Src, _, !PS).

%--------------------------------------------------------------------%

:- type at_rule
    --->    at_offset
    ;	    at_root
    ;	    at_if.

:- pred at_rule_string(at_rule, string).
:- mode at_rule_string(in, out) is det.

at_rule_string(at_offset, "offset").
at_rule_string(at_root, "root").
at_rule_string(at_if, "if").

:- pred at_rule(at_rule::in, src::in, ps::in, ps::out) is semidet.

at_rule(AtRule, Src, !PS) :-
    at_rule_string(AtRule, Str),
    next_char(Src, '@', !PS),
    parsing_utils.keyword(identifier_chars, Str, Src, _, !PS).

%--------------------------------------------------------------------%

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

%--------------------------------------------------------------------%

:- pred hex_literal(src::in, int::out, ps::in, ps::out) is semidet.

hex_literal(Src, N, !PS) :-
    next_char(Src, '0', !PS),
    next_char(Src, 'x', !PS),
    hex_digit(Src, N0, !PS),
    hex_digits(N0, Src, N, !PS).

:- pred hex_digit(src::in, int::out, ps::in, ps::out) is semidet.

hex_digit(Src, N, !PS) :-
    next_char(Src, C, !PS),
    char.digit_to_int(C, N),
    N < 16.

:- pred hex_digits(int::in, src::in, int::out, ps::in, ps::out) is semidet.

hex_digits(N0, Src, N, !PS) :-
    ( if hex_digit(Src, N1, !PS) then
	hex_digits(N0 * 16 + N1, Src, N, !PS)
    else
	N = N0
    ).

%--------------------------------------------------------------------%

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

