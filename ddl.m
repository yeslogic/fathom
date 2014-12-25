:- module ddl.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module list, char.

:- type struct_def
    --->    struct_def(
		struct_name :: string,
		struct_size :: int,
		struct_fields :: list(field_def)
	    ).

:- type field_def
    --->    field_def(
		field_name :: string,
		field_size :: int,
		field_type :: field_type
	    ).

:- type field_type
    --->    field_type_word(word_type, list(field_value))
    ;	    field_type_array(int, field_type, list(field_value))
    ;	    field_type_struct(list(field_def)).

:- type field_value
    --->    field_value_any
    ;	    field_value_int(int)
    ;	    field_value_tag(char, char, char, char).

:- type word_type
    --->    uint8
    ;	    uint16
    ;	    uint32.

:- func word_type_size(word_type) = int.

:- func struct_fields_size(list(field_def)) = int.

:- func field_type_size(field_type) = int.

%--------------------------------------------------------------------%

:- implementation.

:- import_module int.

word_type_size(uint8) = 1.
word_type_size(uint16) = 2.
word_type_size(uint32) = 4.

struct_fields_size(Fields) =
    foldl(plus, map(func(Field) = Field ^ field_size, Fields), 0).

field_type_size(field_type_word(Word, _)) = word_type_size(Word).
field_type_size(field_type_array(Length, Type, _)) =
    Length * field_type_size(Type).
field_type_size(field_type_struct(Fields)) =
    struct_fields_size(Fields).

%--------------------------------------------------------------------%

:- end_module ddl.

