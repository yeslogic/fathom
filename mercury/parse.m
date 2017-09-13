:- module parse.

% Copyright (C) 2014-2015, 2017 YesLogic Pty. Ltd.
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
    parse_ddl_type(Src, Type, !PS),
    Def = ddl_def(Ident, Args, Type).

:- pred parse_union_options(src::in, list(string)::out, ps::in, ps::out) is semidet.

parse_union_options(Src, Options, !PS) :-
    ( if identifier(Src, Name, !PS) then
        parse_union_options(Src, Options0, !PS),
        Options = [Name|Options0]
    else
        Options = []
    ).

:- type multiplicity
    --->    mult_singular
    ;       mult_array(expr_int)
    ;       mult_zero_or_more.

:- pred parse_field_defs(src::in, list(field_def)::out, ps::in, ps::out) is semidet.

parse_field_defs(Src, Fields, !PS) :-
    ( if at_rule(at_if, Src, !PS) then
        skip_ws_comments(Src, _, !PS),
        parse_expr_bool(Src, Expr, !PS),
        Cond = yes(Expr),
        ( if punct("{", Src, _, !PS) then
            % FIXME what about nested @if ??
            parse_field_defs(Src, Fields0, !PS),
            punct("}", Src, _, !PS),
            parse_field_defs(Src, Fields1, !PS),
            Fields = Fields0 ++ Fields1
        else
            punct(":", Src, _, !PS),
            parse_field_def(Cond, Src, FieldDef, !PS),
            parse_field_defs(Src, Fields0, !PS),
            Fields = [FieldDef|Fields0]
        )
    else
        ( if parse_field_def(no, Src, FieldDef, !PS) then
            parse_field_defs(Src, Fields0, !PS),
            Fields = [FieldDef|Fields0]
        else
            Fields = []
        )
    ).

:- pred parse_field_def(maybe(expr_bool)::in, src::in, field_def::out, ps::in, ps::out) is semidet.

parse_field_def(Cond, Src, Field, !PS) :-
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
        ( if SizeExpr0 = expr_int(SizeExpr1) then
            SizeExpr = SizeExpr1
        else
            fail_with_message("expr must be int", Src, SizeExpr, !PS)
        ),
        punct("]", Src, _, !PS),
        Mult = mult_array(SizeExpr)
    else if punct("*", Src, _, !PS) then
        Mult = mult_zero_or_more
    else
        Mult = mult_singular
    ).

:- pred parse_ddl_type(src::in, ddl_type::out, ps::in, ps::out) is semidet.

parse_ddl_type(Src, Type, !PS) :-
    parse_base_type(Src, Type1, !PS),
    ( if punct("&", Src, _, !PS) then
        parse_expr(Src, SizeExpr0, !PS),
        ( if SizeExpr0 = expr_int(SizeExpr1) then
            SizeExpr = SizeExpr1
        else
            fail_with_message("expr must be int", Src, SizeExpr, !PS)
        ),
        Type = ddl_type_sized(Type1, SizeExpr)
    else if punct("~", Src, _, !PS) then
        ( if Type1 = ddl_type_array(SizeExpr, ddl_type_word(uint8, word_values(no, []))) then
            parse_base_type(Src, Type2, !PS),
            Type = ddl_type_sized(Type2, SizeExpr)
        else
            fail_with_message("sized type must be uint8[]", Src, Type, !PS)
        )
    else
        Type = Type1
    ).

:- pred parse_base_type(src::in, ddl_type::out, ps::in, ps::out) is semidet.

parse_base_type(Src, Type, !PS) :-
    ( if keyword(kw_struct, Src, !PS) then
        parse_multiplicity(Src, Mult, !PS),
        punct("{", Src, _, !PS),
        parse_field_defs(Src, Fields, !PS),
        punct("}", Src, _, !PS),
        Type0 = ddl_type_struct(Fields),
        Type = apply_multiplicity(Mult, Type0)
    else if keyword(kw_union, Src, !PS) then
        punct("{", Src, _, !PS),
        parse_union_options(Src, Options, !PS),
        punct("}", Src, _, !PS),
        Type = ddl_type_union(Options)
    else if keyword(kw_switch, Src, !PS) then
        punct("{", Src, _, !PS),
        parse_switch(Src, Cases, Default, !PS),
        punct("}", Src, _, !PS),
        Type = ddl_type_switch(Cases, Default)
    else if identifier(Src, Ident, !PS) then
        ( if Ident = "tag_magic" then
            punct("(", Src, _, !PS),
            identifier(Src, TagName, !PS),
            punct(")", Src, _, !PS),
            Type0 = ddl_type_tag_magic(TagName)
        else if Ident = "string" then
            Type0 = ddl_type_string
        else if punct("(", Src, _, !PS) then
            parse_args(Src, Args, !PS),
            punct(")", Src, _, !PS),
            Type0 = ddl_type_named(Ident, Args)
        else
            Type0 = ddl_type_named(Ident, [])
        ),
        parse_multiplicity(Src, Mult, !PS),
        Type = apply_multiplicity(Mult, Type0)
    else
        parse_word_type(Src, WordType, !PS),
        parse_multiplicity(Src, Mult, !PS),
        parse_word_values(Src, WordValues, !PS),
        Type0 = ddl_type_word(WordType, WordValues),
        Type = apply_multiplicity(Mult, Type0)
    ).

:- pred parse_switch(src::in, assoc_list(expr_bool, ddl_type)::out, ddl_type::out, ps::in, ps::out) is semidet.

parse_switch(Src, Cases, Default, !PS) :-
    parse_ddl_type(Src, Type, !PS),
    ( if keyword(kw_when, Src, !PS) then
        parse_expr_bool(Src, Expr, !PS),
        skip_ws_comments(Src, _, !PS),
        parse_switch(Src, Cases0, Default, !PS),
        Cases = [Expr-Type|Cases0]
    else
        keyword(kw_otherwise, Src, !PS),
        Cases = [],
        Default = Type
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

:- pred parse_expr_bool(src::in, expr_bool::out, ps::in, ps::out) is semidet.

parse_expr_bool(Src, Expr, !PS) :-
    parse_expr(Src, Expr0, !PS),
    (
        Expr0 = expr_bool(Expr)
    ;
        Expr0 = expr_int(Expr1),
        Expr = expr_rel(expr_ne, Expr1, expr_const(0))
    ).

:- pred parse_expr(src::in, expr::out, ps::in, ps::out) is semidet.

parse_expr(Src, Expr, !PS) :-
    parse_expr_rel(Src, Expr0, !PS),
    parse_expr0(Expr0, Src, Expr, !PS).

:- pred parse_expr0(expr::in, src::in, expr::out, ps::in, ps::out) is semidet.

parse_expr0(Expr0, Src, Expr, !PS) :-
    ( if punct(",", Src, _, !PS) then
        Expr0 = expr_bool(ExprB0),
        parse_expr_rel(Src, Expr1, !PS),
        Expr1 = expr_bool(ExprB1),
        Expr2 = expr_bool_op(expr_and, ExprB0, ExprB1),
        parse_expr0(expr_bool(Expr2), Src, Expr, !PS)
    else if punct(";", Src, _, !PS) then
        Expr0 = expr_bool(ExprB0),
        parse_expr_rel(Src, Expr1, !PS),
        Expr1 = expr_bool(ExprB1),
        Expr2 = expr_bool_op(expr_or, ExprB0, ExprB1),
        parse_expr0(expr_bool(Expr2), Src, Expr, !PS)
    else
        Expr = Expr0
    ).

:- pred parse_expr_rel(src::in, expr::out, ps::in, ps::out) is semidet.

parse_expr_rel(Src, Expr, !PS) :-
    parse_expr_int(Src, Expr0, !PS),
    parse_expr_rel0(Expr0, Src, Expr, !PS).

:- pred parse_expr_rel0(expr_int::in, src::in, expr::out, ps::in, ps::out) is semidet.

parse_expr_rel0(Expr0, Src, Expr, !PS) :-
    % careful, order of punct calls is important!
    ( if punct("=<", Src, _, !PS) then
        parse_expr_int(Src, Expr1, !PS),
        Expr = expr_bool(expr_rel(expr_lte, Expr0, Expr1))
    else if punct(">=", Src, _, !PS) then
        parse_expr_int(Src, Expr1, !PS),
        Expr = expr_bool(expr_rel(expr_gte, Expr0, Expr1))
    else if punct("=", Src, _, !PS) then
        parse_expr_int(Src, Expr1, !PS),
        Expr = expr_bool(expr_rel(expr_eq, Expr0, Expr1))
    else if punct("<>", Src, _, !PS) then
        parse_expr_int(Src, Expr1, !PS),
        Expr = expr_bool(expr_rel(expr_ne, Expr0, Expr1))
    else if punct("<", Src, _, !PS) then
        parse_expr_int(Src, Expr1, !PS),
        Expr = expr_bool(expr_rel(expr_lt, Expr0, Expr1))
    else if punct(">", Src, _, !PS) then
        parse_expr_int(Src, Expr1, !PS),
        Expr = expr_bool(expr_rel(expr_gt, Expr0, Expr1))
    else
        Expr = expr_int(Expr0)
    ).

:- pred parse_expr_int(src::in, expr_int::out, ps::in, ps::out) is semidet.

parse_expr_int(Src, Expr, !PS) :-
    parse_term(Src, Expr0, !PS),
    parse_expr_int0(Expr0, Src, Expr, !PS).

:- pred parse_expr_int0(expr_int::in, src::in, expr_int::out, ps::in, ps::out) is semidet.

parse_expr_int0(Expr0, Src, Expr, !PS) :-
    ( if punct("+", Src, _, !PS) then
        parse_term(Src, Expr1, !PS),
        Expr2 = expr_int_op(expr_add, Expr0, Expr1),
        parse_expr_int0(Expr2, Src, Expr, !PS)
    else if punct("-", Src, _, !PS) then
        parse_term(Src, Expr1, !PS),
        Expr2 = expr_int_op(expr_sub, Expr0, Expr1),
        parse_expr_int0(Expr2, Src, Expr, !PS)
    else if punct("*", Src, _, !PS) then
        parse_term(Src, Expr1, !PS),
        Expr2 = expr_int_op(expr_mul, Expr0, Expr1),
        parse_expr_int0(Expr2, Src, Expr, !PS)
    else if punct("/", Src, _, !PS) then
        parse_term(Src, Expr1, !PS),
        Expr2 = expr_int_op(expr_div, Expr0, Expr1),
        parse_expr_int0(Expr2, Src, Expr, !PS)
    else if punct("&", Src, _, !PS) then
        parse_term(Src, Expr1, !PS),
        Expr2 = expr_int_op(expr_and_bits, Expr0, Expr1),
        parse_expr_int0(Expr2, Src, Expr, !PS)
    else
        Expr = Expr0
    ).

:- pred parse_term(src::in, expr_int::out, ps::in, ps::out) is semidet.

parse_term(Src, Expr, !PS) :-
    ( if hex_literal(Src, N, !PS) then
        Expr = expr_const(N)
    else if int_literal(Src, N, !PS) then
        Expr = expr_const(N)
    else
        identifier(Src, Ident, !PS),
        ( if
            string.remove_prefix("tag_", Ident, Tag),
            string.length(Tag) =< 4
        then
            % FIXME just a quick hack until a proper tag function
            Cs = string.to_char_list(string.pad_right(Tag, ' ', 4)),
            tag_to_int(Cs, 0, N),
            Expr = expr_const(N)
        else
            ( if punct("[", Src, _, !PS) then
                parse_expr_int(Src, Expr0, !PS),
                punct("]", Src, _, !PS),
                Expr = expr_index(Ident, Expr0)
            else
                Expr = expr_field(Ident)
            )
        )
    ).

:- pred tag_to_int(list(char), int, int).
:- mode tag_to_int(in, in, out) is det.

tag_to_int([], N, N).
tag_to_int([C|Cs], N0, N) :-
    N1 = N0*256 + char.to_int(C),
    tag_to_int(Cs, N1, N).

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
    ( if at_rule(at_offset, Src, !PS) then
        ( if punct("(", Src, _, !PS) then
            parse_offset_base(Src, Base, !PS),
            punct(")", Src, _, !PS)
        else
            Base = offset_base_struct
        )
    else
        at_rule(at_offset_scope, Src, !PS),
        punct("(", Src, _, !PS),
        parse_expr_int(Src, Expr, !PS),
        Base = offset_base_expr(Expr),
        punct(")", Src, _, !PS)
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
        Base = offset_base_stack(Ident)
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
    ;       kw_struct
    ;       kw_switch
    ;       kw_when
    ;       kw_otherwise
    ;       kw_byte
    ;       kw_uint8
    ;       kw_uint16
    ;       kw_uint32
    ;       kw_uint64
    ;       kw_int8
    ;       kw_int16
    ;       kw_int32
    ;       kw_int64.

:- pred keyword_string(keyword, string).
:- mode keyword_string(in, out) is det.
:- mode keyword_string(out, in) is semidet.

keyword_string(kw_union, "union").
keyword_string(kw_struct, "struct").
keyword_string(kw_switch, "switch").
keyword_string(kw_when, "when").
keyword_string(kw_otherwise, "otherwise").
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
    ;       at_offset_scope
    ;       at_root
    ;       at_if.

:- pred at_rule_string(at_rule, string).
:- mode at_rule_string(in, out) is det.

at_rule_string(at_offset, "offset").
at_rule_string(at_offset_scope, "offset_scope").
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

