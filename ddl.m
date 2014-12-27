:- module ddl.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module list, assoc_list, string, char.

:- type ddl == assoc_list(string, ddl_def).

:- type ddl_def
    --->    def_union(union_def)
    ;	    def_struct(struct_def).

:- type union_def
    --->    union_def(
		union_name :: string,
		union_options :: list(string)
	    ).

:- type struct_def
    --->    struct_def(
		struct_name :: string,
		struct_fields :: list(field_def)
	    ).

:- type field_def
    --->    field_def(
		field_name :: string,
		field_type :: field_type,
		field_values :: list(field_value)
	    ).

:- type field_type
    --->    field_type_word(word_type)
    ;	    field_type_array(array_size, field_type)
    ;	    field_type_struct(list(field_def))
    ;	    field_type_union(list(string))
    ;	    field_type_ref(string).

:- type array_size
    --->    array_size_fixed(int)
    ;	    array_size_variable(string).

:- type field_value
    --->    field_value_any
    ;	    field_value_int(int)
    ;	    field_value_tag(char, char, char, char).

:- type word_type
    --->    uint8
    ;	    uint16
    ;	    uint32.

:- type context == assoc_list(string, int).

:- func word_type_size(word_type) = int.

:- pred ddl_def_fixed_size(ddl::in, ddl_def::in, int::out) is semidet.

:- func ddl_def_size(ddl, context, ddl_def) = int.

:- pred union_options_fixed_size(ddl::in, list(string)::in, int::out) is semidet.

:- func union_options_size(ddl, context, list(string)) = int.

:- pred struct_fields_fixed_size(ddl::in, list(field_def)::in, int::out) is semidet.

:- func struct_fields_size(ddl, context, list(field_def)) = int.

:- pred field_type_fixed_size(ddl::in, field_type::in, int::out) is semidet.

:- func field_type_size(ddl, context, field_type) = int.

:- func context_resolve(context, string) = int.

:- func ddl_resolve(ddl, string) = ddl_def.

%--------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- import_module abort.

word_type_size(uint8) = 1.
word_type_size(uint16) = 2.
word_type_size(uint32) = 4.

ddl_def_fixed_size(DDL, Def, Size) :-
    require_complete_switch [Def]
    (
	Def = def_union(Union),
	union_options_fixed_size(DDL, Union ^ union_options, Size)
    ;
	Def = def_struct(Struct),
	struct_fields_fixed_size(DDL, Struct ^ struct_fields, Size)
    ).

ddl_def_size(DDL, Context, Def) = Size :-
    (
	Def = def_union(Union),
	Size = union_options_size(DDL, Context, Union ^ union_options)
    ;
	Def = def_struct(Struct),
	Size = struct_fields_size(DDL, Context, Struct ^ struct_fields)
    ).

union_options_fixed_size(DDL, Opts, Size) :-
    Defs = map(ddl_resolve(DDL), Opts),
    map(ddl_def_fixed_size(DDL), Defs, Sizes),
    Sizes = [Size|_],
    all [X] ( member(X, Sizes), X = Size ).

union_options_size(_DDL, _Context, _Opts) = _Size :-
    abort("uh oh!").

struct_fields_fixed_size(_DDL, [], 0).
struct_fields_fixed_size(DDL, [F|Fs], Size) :-
    field_type_fixed_size(DDL, F ^ field_type, Size0),
    struct_fields_fixed_size(DDL, Fs, Size1),
    Size = Size0 + Size1.

struct_fields_size(_DDL, _Context, []) = 0.
struct_fields_size(DDL, Context, [F|Fs]) = Size :-
    Size0 = field_type_size(DDL, Context, F ^ field_type),
    Size1 = struct_fields_size(DDL, Context, Fs),
    Size = Size0 + Size1.

field_type_fixed_size(DDL, FieldType, Size) :-
    require_complete_switch [FieldType]
    (
	FieldType = field_type_word(Word),
	Size = word_type_size(Word)
    ;
	FieldType = field_type_array(ArraySize, Type),
	require_complete_switch [ArraySize]
	(
	    ArraySize = array_size_fixed(Length),
	    field_type_fixed_size(DDL, Type, Size0),
	    Size = Length * Size0
	;
	    ArraySize = array_size_variable(_),
	    fail
	)
    ;
	FieldType = field_type_struct(Fields),
	struct_fields_fixed_size(DDL, Fields, Size)
    ;
	FieldType = field_type_union(Options),
	union_options_fixed_size(DDL, Options, Size)
    ;
	FieldType = field_type_ref(Name),
	Def = ddl_resolve(DDL, Name),
	ddl_def_fixed_size(DDL, Def, Size)
    ).

field_type_size(DDL, Context, FieldType) = Size :-
    (
	FieldType = field_type_word(Word),
	Size = word_type_size(Word)
    ;
	FieldType = field_type_array(ArraySize, Type),
	(
	    ArraySize = array_size_fixed(Length)
	;
	    ArraySize = array_size_variable(Name),
	    Length = context_resolve(Context, Name)
	),
	Size = Length * field_type_size(DDL, Context, Type)
    ;
	FieldType = field_type_struct(Fields),
	Size = struct_fields_size(DDL, Context, Fields)
    ;
	FieldType = field_type_union(Options),
	Size = union_options_size(DDL, Context, Options)
    ;
	FieldType = field_type_ref(Name),
	Def = ddl_resolve(DDL, Name),
	Size = ddl_def_size(DDL, Context, Def)
    ).

context_resolve(Context, Name) = Value :-
    ( if search(Context, Name, Value0) then
	Value = Value0
    else
	abort("context missing: "++Name)
    ).

ddl_resolve(DDL, Name) = Def :-
    ( if search(DDL, Name, Def0) then
	Def = Def0
    else
	abort("unknown type ref: "++Name)
    ).

%--------------------------------------------------------------------%

:- end_module ddl.

