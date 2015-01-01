:- module ddl.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module list, assoc_list, maybe, string, char.

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
		field_type :: field_type
	    ).

:- type field_type
    --->    field_type_word(word_type, word_values)
    ;	    field_type_array(array_size, field_type)
    ;	    field_type_struct(list(field_def))
    ;	    field_type_union(list(string))
    ;	    field_type_ref(string).

:- type array_size
    --->    array_size_fixed(int)
    ;	    array_size_variable(string).

:- type word_values
    --->    word_values(
		word_values_any :: maybe(word_interp),
		word_values_enum :: assoc_list(word_value, word_interp)
	    ).

:- type word_type
    --->    uint8
    ;	    uint16
    ;	    uint32.

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
		offset_type :: string
	    ).

:- type offset_base
    --->    offset_base_struct
    ;	    offset_base_root
    ;	    offset_base_named(string).

:- type scope == assoc_list(string, int).

:- func word_type_size(word_type) = int.

:- pred ddl_def_fixed_size(ddl::in, ddl_def::in, int::out) is semidet.

:- func ddl_def_size(ddl, scope, ddl_def) = int.

:- pred union_options_fixed_size(ddl::in, list(string)::in, int::out) is semidet.

:- func union_options_size(ddl, scope, list(string)) = int.

:- pred struct_fields_fixed_size(ddl::in, list(field_def)::in, int::out) is semidet.

:- func struct_fields_size(ddl, scope, list(field_def)) = int.

:- pred field_type_fixed_size(ddl::in, field_type::in, int::out) is semidet.

:- func field_type_size(ddl, scope, field_type) = int.

:- func scope_resolve(scope, string) = int.

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

ddl_def_size(DDL, Scope, Def) = Size :-
    (
	Def = def_union(Union),
	Size = union_options_size(DDL, Scope, Union ^ union_options)
    ;
	Def = def_struct(Struct),
	Size = struct_fields_size(DDL, Scope, Struct ^ struct_fields)
    ).

union_options_fixed_size(DDL, Opts, Size) :-
    Defs = map(ddl_resolve(DDL), Opts),
    map(ddl_def_fixed_size(DDL), Defs, Sizes),
    Sizes = [Size|_],
    all [X] ( member(X, Sizes), X = Size ).

union_options_size(_DDL, _Scope, _Opts) = _Size :-
    abort("uh oh!").

struct_fields_fixed_size(_DDL, [], 0).
struct_fields_fixed_size(DDL, [F|Fs], Size) :-
    field_type_fixed_size(DDL, F ^ field_type, Size0),
    struct_fields_fixed_size(DDL, Fs, Size1),
    Size = Size0 + Size1.

struct_fields_size(_DDL, _Scope, []) = 0.
struct_fields_size(DDL, Scope, [F|Fs]) = Size :-
    Size0 = field_type_size(DDL, Scope, F ^ field_type),
    Size1 = struct_fields_size(DDL, Scope, Fs),
    Size = Size0 + Size1.

field_type_fixed_size(DDL, FieldType, Size) :-
    require_complete_switch [FieldType]
    (
	FieldType = field_type_word(Word, _Values),
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

field_type_size(DDL, Scope, FieldType) = Size :-
    (
	FieldType = field_type_word(Word, _Values),
	Size = word_type_size(Word)
    ;
	FieldType = field_type_array(ArraySize, Type),
	(
	    ArraySize = array_size_fixed(Length)
	;
	    ArraySize = array_size_variable(Name),
	    Length = scope_resolve(Scope, Name)
	),
	Size = Length * field_type_size(DDL, Scope, Type)
    ;
	FieldType = field_type_struct(Fields),
	Size = struct_fields_size(DDL, Scope, Fields)
    ;
	FieldType = field_type_union(Options),
	Size = union_options_size(DDL, Scope, Options)
    ;
	FieldType = field_type_ref(Name),
	Def = ddl_resolve(DDL, Name),
	Size = ddl_def_size(DDL, Scope, Def)
    ).

scope_resolve(Scope, Name) = Value :-
    ( if search(Scope, Name, Value0) then
	Value = Value0
    else
	abort("scope missing: "++Name)
    ).

ddl_resolve(DDL, Name) = Def :-
    ( if search(DDL, Name, Def0) then
	Def = Def0
    else
	abort("unknown type ref: "++Name)
    ).

%--------------------------------------------------------------------%

:- end_module ddl.

