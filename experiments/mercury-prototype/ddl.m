:- module ddl.

% Copyright (C) 2014-2015, 2017 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module list, assoc_list, maybe, bool, string, char.

:- type ddl == assoc_list(string, ddl_def).

:- type ddl_def
    --->    ddl_def(
                def_name :: string,
                def_args :: list(string),
                def_type :: ddl_type
            ).

:- type ddl_type
    --->    ddl_type_word(word_type, word_values)
    ;       ddl_type_array(expr_int, ddl_type)
    ;       ddl_type_zero_or_more(ddl_type)
    ;       ddl_type_sized(ddl_type, expr_int)
    ;       ddl_type_struct(list(field_def))
    ;       ddl_type_union(list(string))
    ;       ddl_type_switch(assoc_list(expr_bool, ddl_type), ddl_type)
    ;       ddl_type_named(string, list(string))
    ;       ddl_type_tag_magic(string)
    ;       ddl_type_string.

:- type field_def
    --->    field_def(
                field_name :: string,
                field_type :: ddl_type,
                field_cond :: maybe(expr_bool)
            ).

:- type expr
    --->    expr_bool(expr_bool)
    ;       expr_int(expr_int).

:- type expr_bool
    --->    expr_not(expr_bool)
    ;       expr_bool_op(expr_op_bool, expr_bool, expr_bool)
    ;       expr_rel(expr_op_rel, expr_int, expr_int).

:- type expr_int
    --->    expr_field(string)
    ;       expr_index(string, expr_int)
    ;       expr_const(int)
    ;       expr_int_op(expr_op_int, expr_int, expr_int).

:- type expr_op_bool
    --->    expr_and
    ;       expr_or.

:- type expr_op_rel
    --->    expr_eq
    ;       expr_ne
    ;       expr_lt
    ;       expr_gt
    ;       expr_lte
    ;       expr_gte.

:- type expr_op_int
    --->    expr_add
    ;       expr_sub
    ;       expr_mul
    ;       expr_div
    ;       expr_and_bits.

:- type word_values
    --->    word_values(
                word_values_any :: maybe(word_interp),
                word_values_enum :: assoc_list(word_value, word_interp)
            ).

:- type word_type
    --->    uint8
    ;       uint16
    ;       uint32
    ;       uint64
    ;       int8
    ;       int16
    ;       int32
    ;       int64.

:- type word_value
    --->    word_value_int(int)
    ;       word_value_tag(char, char, char, char).

:- type word_interp
    --->    word_interp_none
    ;       word_interp_error
    ;       word_interp_offset(offset).

:- type offset
    --->    offset(
                offset_base :: offset_base,
                offset_type :: ddl_type
            ).

:- type offset_base
    --->    offset_base_struct
    ;       offset_base_root
    ;       offset_base_stack(string)
    ;       offset_base_expr(expr_int).

:- type scope == assoc_list(string, scope_item).

:- type scope_item
    --->    scope_int(int)
    ;       scope_array(list(int)).

:- func word_type_size(word_type) = int.

:- func ddl_def_size(ddl, scope, ddl_def, list(int)) = int.

:- func union_options_size(ddl, scope, list(string)) = int.

:- func struct_fields_size(ddl, scope, list(field_def)) = int.

:- func ddl_type_size(ddl, scope, ddl_type) = int.

:- func eval_expr_bool(scope, expr_bool) = bool.

:- func eval_expr_int(scope, expr_int) = int.

:- func tag_num_to_string(int) = string.

:- func scope_resolve(scope, string) = scope_item.

:- func scope_resolve_int(scope, string) = int.

:- pred ddl_search(ddl::in, string::in, ddl_def::out) is semidet.

:- func ddl_lookup_det(ddl, string) = ddl_def.

:- pred simplify_type(scope, ddl_type, ddl_type).
:- mode simplify_type(in, in, out) is det.

%--------------------------------------------------------------------%

:- implementation.

:- import_module int, pair.

:- import_module abort.

word_type_size(uint8) = 1.
word_type_size(uint16) = 2.
word_type_size(uint32) = 4.
word_type_size(uint64) = 8.
word_type_size(int8) = 1.
word_type_size(int16) = 2.
word_type_size(int32) = 4.
word_type_size(int64) = 8.

ddl_def_size(DDL, Scope, Def, _Args) = Size :-
    Size = ddl_type_size(DDL, Scope, Def ^ def_type).

union_options_size(_DDL, _Scope, _Opts) = _Size :-
    abort("uh oh!").

struct_fields_size(_DDL, _Scope, []) = 0.
struct_fields_size(DDL, Scope, [F|Fs]) = Size :-
    Size0 = ddl_type_size(DDL, Scope, F ^ field_type),
    Size1 = struct_fields_size(DDL, Scope, Fs),
    Size = Size0 + Size1.

ddl_type_size(DDL, Scope, Type) = Size :-
    (
        Type = ddl_type_word(Word, _Values),
        Size = word_type_size(Word)
    ;
        Type = ddl_type_array(SizeExpr, Type0),
        Length = eval_expr_int(Scope, SizeExpr),
        Size = Length * ddl_type_size(DDL, Scope, Type0)
    ;
        Type = ddl_type_zero_or_more(_Type0),
        abort("FIXME zero_or_more has undefined size")
    ;
        Type = ddl_type_sized(_Type0, SizeExpr),
        Size = eval_expr_int(Scope, SizeExpr)
    ;
        Type = ddl_type_struct(Fields),
        Size = struct_fields_size(DDL, Scope, Fields)
    ;
        Type = ddl_type_union(Options),
        Size = union_options_size(DDL, Scope, Options)
    ;
        Type = ddl_type_switch(_, _),
        abort("FIXME switch size ???")
    ;
        Type = ddl_type_named(Name, Args0),
        Def = ddl_lookup_det(DDL, Name),
        Args = map(scope_resolve_int(Scope), Args0),
        Size = ddl_def_size(DDL, Scope, Def, Args)
    ;
        Type = ddl_type_tag_magic(Name),
        TagNum = scope_resolve_int(Scope, Name),
        TagStr = tag_num_to_string(TagNum),
        Def = ddl_lookup_det(DDL, TagStr),
        Size = ddl_def_size(DDL, Scope, Def, [])
    ;
        Type = ddl_type_string,
        abort("FIXME string has undefined size")
    ).

eval_expr_bool(Scope, Expr) = Res :-
    (
        Expr = expr_not(Expr0),
        Res0 = eval_expr_bool(Scope, Expr0),
        Res = not(Res0)
    ;
        Expr = expr_rel(Op, Lhs0, Rhs0),
        Lhs = eval_expr_int(Scope, Lhs0),
        Rhs = eval_expr_int(Scope, Rhs0),
        (
            Op = expr_eq,
            Res = ( if Lhs = Rhs then yes else no )
        ;
            Op = expr_ne,
            Res = ( if Lhs \= Rhs then yes else no )
        ;
            Op = expr_lt,
            Res = ( if Lhs < Rhs then yes else no )
        ;
            Op = expr_gt,
            Res = ( if Lhs > Rhs then yes else no )
        ;
            Op = expr_lte,
            Res = ( if Lhs =< Rhs then yes else no )
        ;
            Op = expr_gte,
            Res = ( if Lhs >= Rhs then yes else no )
        )
    ;
        Expr = expr_bool_op(Op, Lhs0, Rhs0),
        Lhs = eval_expr_bool(Scope, Lhs0),
        Rhs = eval_expr_bool(Scope, Rhs0),
        (
            Op = expr_and,
            Res = Lhs `and` Rhs
        ;
            Op = expr_or,
            Res = Lhs `or` Rhs
        )
    ).

eval_expr_int(Scope, Expr) = Res :-
    (
        Expr = expr_field(Name),
        Item = scope_resolve(Scope, Name),
        (
            Item = scope_int(Res)
        ;
            Item = scope_array(_),
            abort("scope item is array, expected int: "++Name)
        )
    ;
        Expr = expr_index(Name, Expr0),
        Index = eval_expr_int(Scope, Expr0),
        Item = scope_resolve(Scope, Name),
        (
            Item = scope_int(_),
            abort("scope item is int, expected array: "++Name)
        ;
            Item = scope_array(Array),
            Res = det_index0(Array, Index)
        )
    ;
        Expr = expr_const(Res)
    ;
        Expr = expr_int_op(Op, Lhs0, Rhs0),
        Lhs = eval_expr_int(Scope, Lhs0),
        Rhs = eval_expr_int(Scope, Rhs0),
        (
            Op = expr_add,
            Res = Lhs + Rhs
        ;
            Op = expr_sub,
            Res = Lhs - Rhs
        ;
            Op = expr_mul,
            Res = Lhs * Rhs
        ;
            Op = expr_div,
            Res = Lhs / Rhs
        ;
            Op = expr_and_bits,
            Res = Lhs /\ Rhs
        )
    ).

tag_num_to_string(N) = Tag :-
    B0 = (N >> 24) /\ 0xFF,
    B1 = (N >> 16) /\ 0xFF,
    B2 = (N >> 8) /\ 0xFF,
    B3 = N /\ 0xFF,
    ( if
        char.from_int(B0, C0),
        char.from_int(B1, C1),
        char.from_int(B2, C2),
        char.from_int(B3, C3)
    then
        Tag0 = string.from_char_list([C0,C1,C2,C3]),
        ( if Tag0 = "OS/2" then
            Tag = "OS2"
        else if Tag0 = "CFF " then
            Tag = "CFF"
        else
            Tag = Tag0
        )
    else
        abort("not a tag: "++int_to_string(N))
    ).

scope_resolve(Scope, Name) = Value :-
    ( if search(Scope, Name, Value0) then
        Value = Value0
    else
        abort("scope missing: "++Name)
    ).

scope_resolve_int(Scope, Name) = Value :-
    Item = scope_resolve(Scope, Name),
    (
        Item = scope_int(Value)
    ;
        Item = scope_array(_),
        abort("scope item is array, expected int: "++Name)
    ).

ddl_search(DDL, Name, Def) :-
    search(DDL, Name, Def).

ddl_lookup_det(DDL, Name) = Def :-
    ( if search(DDL, Name, Def0) then
        Def = Def0
    else
        abort("unknown type: "++Name)
    ).

%--------------------------------------------------------------------%

simplify_type(Scope, Type0, Type) :-
    (
        (
            Type0 = ddl_type_word(_, _) ;
            Type0 = ddl_type_array(_, _) ;
            Type0 = ddl_type_zero_or_more(_) ;
            Type0 = ddl_type_sized(_, _) ;
            Type0 = ddl_type_struct(_) ;
            Type0 = ddl_type_union(_) ;
            Type0 = ddl_type_named(_, _) ;
            Type0 = ddl_type_tag_magic(_) ;
            Type0 = ddl_type_string
        ),
        Type = Type0
    ;
        Type0 = ddl_type_switch(Cases, Default),
        eval_switch(Scope, Cases, Default, Type)
    ).

:- pred eval_switch(scope, assoc_list(expr_bool, ddl_type), ddl_type, ddl_type).
:- mode eval_switch(in, in, in, out) is det.

eval_switch(Scope, Cases, Default, Type) :-
    (
        Cases = [Expr-Type0|Cases0],
        ( if eval_expr_bool(Scope, Expr) = yes then
            Type = Type0
        else
            eval_switch(Scope, Cases0, Default, Type)
        )
    ;
        Cases = [],
        Type = Default
    ).

%--------------------------------------------------------------------%

:- end_module ddl.

