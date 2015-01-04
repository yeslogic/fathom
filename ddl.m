:- module ddl.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module list, assoc_list, maybe, string, char.

:- type ddl == assoc_list(string, ddl_def).

:- type ddl_def
    --->    ddl_def(
		def_name :: string,
		def_args :: list(string),
		def_body :: def_body
	    ).

:- type def_body
    --->    def_union(list(string))
    ;	    def_struct(list(field_def)).

:- type field_def
    --->    field_def(
		field_name :: string,
		field_type :: field_type
	    ).

:- type field_type
    --->    field_type_word(word_type, word_values)
    ;	    field_type_array(array_size, field_type)
    ;	    field_type_struct(list(field_def))
    ;	    field_type_union(list(string))
    ;	    field_type_named(string, list(string))
    ;	    field_type_tag_magic(string).

:- type array_size
    --->    array_size_fixed(int)
    ;	    array_size_expr(expr).

:- type expr
    --->    expr_field(string)
    ;	    expr_const(int)
    ;	    expr_op(expr_op, expr, expr).

:- type expr_op
    --->    expr_add
    ;	    expr_sub
    ;	    expr_mul.

:- type word_values
    --->    word_values(
		word_values_any :: maybe(word_interp),
		word_values_enum :: assoc_list(word_value, word_interp)
	    ).

:- type word_type
    --->    uint8
    ;	    uint16
    ;	    uint32
    ;	    int8
    ;	    int16
    ;	    int32.

:- type word_value
    --->    word_value_int(int)
    ;	    word_value_tag(char, char, char, char).

:- type word_interp
    --->    word_interp_none
    ;	    word_interp_error
    ;	    word_interp_offset(offset).

:- type offset
    --->    offset(
		offset_base :: offset_base,
		offset_type :: field_type
	    ).

:- type offset_base
    --->    offset_base_struct
    ;	    offset_base_root
    ;	    offset_base_named(string).

:- type scope == assoc_list(string, int).

:- func word_type_size(word_type) = int.

:- func ddl_def_size(ddl, scope, ddl_def, list(int)) = int.

:- func union_options_size(ddl, scope, list(string)) = int.

:- func struct_fields_size(ddl, scope, list(field_def)) = int.

:- func field_type_size(ddl, scope, field_type) = int.

:- func eval_expr(scope, expr) = int.

:- func tag_num_to_string(int) = string.

:- func scope_resolve(scope, string) = int.

:- pred ddl_search(ddl::in, string::in, ddl_def::out) is semidet.

:- func ddl_lookup_det(ddl, string) = ddl_def.

%--------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- import_module abort.

word_type_size(uint8) = 1.
word_type_size(uint16) = 2.
word_type_size(uint32) = 4.
word_type_size(int8) = 1.
word_type_size(int16) = 2.
word_type_size(int32) = 4.

ddl_def_size(DDL, Scope, Def, _Args) = Size :-
    (
	Def ^ def_body = def_union(Options),
	Size = union_options_size(DDL, Scope, Options)
    ;
	Def ^ def_body = def_struct(Fields),
	Size = struct_fields_size(DDL, Scope, Fields)
    ).

union_options_size(_DDL, _Scope, _Opts) = _Size :-
    abort("uh oh!").

struct_fields_size(_DDL, _Scope, []) = 0.
struct_fields_size(DDL, Scope, [F|Fs]) = Size :-
    Size0 = field_type_size(DDL, Scope, F ^ field_type),
    Size1 = struct_fields_size(DDL, Scope, Fs),
    Size = Size0 + Size1.

field_type_size(DDL, Scope, FieldType) = Size :-
    (
	FieldType = field_type_word(Word, _Values),
	Size = word_type_size(Word)
    ;
	FieldType = field_type_array(ArraySize, Type),
	(
	    ArraySize = array_size_fixed(Length)
	;
	    ArraySize = array_size_expr(Expr),
	    Length = eval_expr(Scope, Expr)
	),
	Size = Length * field_type_size(DDL, Scope, Type)
    ;
	FieldType = field_type_struct(Fields),
	Size = struct_fields_size(DDL, Scope, Fields)
    ;
	FieldType = field_type_union(Options),
	Size = union_options_size(DDL, Scope, Options)
    ;
	FieldType = field_type_named(Name, Args0),
	Def = ddl_lookup_det(DDL, Name),
	Args = map(scope_resolve(Scope), Args0),
	Size = ddl_def_size(DDL, Scope, Def, Args)
    ;
	FieldType = field_type_tag_magic(Name),
	TagNum = scope_resolve(Scope, Name),
	TagStr = tag_num_to_string(TagNum),
	Def = ddl_lookup_det(DDL, TagStr),
	Size = ddl_def_size(DDL, Scope, Def, [])
    ).

eval_expr(Scope, Expr) = Res :-
    (
	Expr = expr_field(Name),
	Res = scope_resolve(Scope, Name)
    ;
	Expr = expr_const(Res)
    ;
	Expr = expr_op(Op, Lhs0, Rhs0),
	Lhs = eval_expr(Scope, Lhs0),
	Rhs = eval_expr(Scope, Rhs0),
	(
	    Op = expr_add,
	    Res = Lhs + Rhs
	;
	    Op = expr_sub,
	    Res = Lhs - Rhs
	;
	    Op = expr_mul,
	    Res = Lhs * Rhs
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
	Tag = string.from_char_list([C0,C1,C2,C3])
    else
	abort("not a tag: "++int_to_string(N))
    ).

scope_resolve(Scope, Name) = Value :-
    ( if search(Scope, Name, Value0) then
	Value = Value0
    else
	abort("scope missing: "++Name)
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

:- end_module ddl.

