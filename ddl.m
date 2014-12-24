:- module ddl.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module list, maybe, char.

:- type struct_def
    --->    struct_def(
		struct_name :: string,
		struct_size :: maybe(item_size),
		struct_fields :: list(field_def)
	    ).

:- type field_def
    --->    field_def(
		field_name :: string,
		field_size :: item_size,
		field_values :: list(field_value)
	    ).

:- type field_value
    --->    field_value_any
    ;	    field_value_int(int)
    ;	    field_value_tag(char, char, char, char).

:- type item_size
    --->    item_size_bytes(int).

%--------------------------------------------------------------------%

:- implementation.

%--------------------------------------------------------------------%

:- end_module ddl.

