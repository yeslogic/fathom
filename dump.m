:- module dump.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.

:- import_module bytes.
:- import_module ddl.

:- pred dump_root(ddl::in, bytes::in, ddl_def::in, io::di, io::uo) is det.

%--------------------------------------------------------------------%

:- implementation.

:- import_module list, int, char, maybe, pair.

:- type ditem
    --->    ditem(
		ditem_name :: string,
		ditem_value :: ditem_value
	    ).

:- type ditem_value
    --->    choice(ditem)
    ;	    fields(list(ditem))
    ;	    array(list(ditem_value))
    ;	    word(int).

%--------------------------------------------------------------------%

dump_root(DDL, Bytes, Def, !IO) :-
    dump_def(DDL, Bytes, Def, Item, 0, _Offset, []),
    write_item(0, Item, !IO).

:- pred dump_def(ddl::in, bytes::in, ddl_def::in, ditem::out, int::in, int::out, context::in) is det.

dump_def(DDL, Bytes, Def, Item, !Offset, Context0) :-
    (
	Def = def_union(Union),
	dump_union_options(DDL, Bytes, Union ^ union_options, Item0, !Offset, Context0),
	Item = ditem(Union ^ union_name, choice(Item0))
    ;
	Def = def_struct(Struct),
	dump_struct_fields(DDL, Bytes, Struct ^ struct_fields, Fields, !Offset, Context0),
	Item = ditem(Struct ^ struct_name, fields(Fields))
    ).

:- pred dump_union_options(ddl::in, bytes::in, list(string)::in, ditem::out, int::in, int::out, context::in) is det.

% FIXME
dump_union_options(_DDL, _Bytes, [], ditem("HELP", fields([])), !Offset, _Context0).
dump_union_options(DDL, Bytes, [Option|Options], Item, !Offset, Context0) :-
    Def = ddl_resolve(DDL, Option),
    ( if match_def(DDL, Bytes, Def, !.Offset, Context0) then
	dump_def(DDL, Bytes, Def, Item, !Offset, Context0)
    else
	dump_union_options(DDL, Bytes, Options, Item, !Offset, Context0)
    ).

:- pred match_def(ddl::in, bytes::in, ddl_def::in, int::in, context::in) is semidet.

match_def(DDL, Bytes, Def, Offset, Context) :-
    (
	Def = def_union(Union),
	match_union_options(DDL, Bytes, Union ^ union_options, Offset, Context)
    ;
	Def = def_struct(Struct),
	match_struct_fields(DDL, Bytes, Struct ^ struct_fields, Offset, Context)
    ).

:- pred match_union_options(ddl::in, bytes::in, list(string)::in, int::in, context::in) is semidet.

match_union_options(DDL, Bytes, Options, Offset, Context) :-
    member(Def0, map(ddl_resolve(DDL), Options)),
    match_def(DDL, Bytes, Def0, Offset, Context).

:- pred match_struct_fields(ddl::in, bytes::in, list(field_def)::in, int::in, context::in) is semidet.

match_struct_fields(DDL, Bytes, Fields, Offset, Context) :-
    (
	Fields = [] % FIXME uhh...
    ;
	Fields = [Field|_],
	match_field(DDL, Bytes, Field, Offset, Context)
    ).

:- pred match_field(ddl::in, bytes::in, field_def::in, int::in, context::in) is semidet.

match_field(DDL, Bytes, Field, Offset, Context) :-
    Type = Field ^ field_type,
    require_complete_switch [Type]
    (
	Type = field_type_word(WordType, WordValues),
	member(Value-_Interp, WordValues ^ word_values_enum),
	match_word(Bytes, WordType, Value, Offset)
    ;
	Type = field_type_array(_, _),
	fail
    ;
	Type = field_type_struct(Fields),
	match_struct_fields(DDL, Bytes, Fields, Offset, Context)
    ;
	Type = field_type_union(Options),
	match_union_options(DDL, Bytes, Options, Offset, Context)
    ;
	Type = field_type_ref(Name),
	Def = ddl_resolve(DDL, Name),
	match_def(DDL, Bytes, Def, Offset, Context)
    ).

:- pred match_word(bytes::in, word_type::in, word_value::in, int::in) is semidet.

match_word(Bytes, WordType, Value, Offset) :-
    Size = word_type_size(WordType),
    Word = get_byte_range_as_uint(Bytes, Offset, Size, 0),
    require_complete_switch [Value]
    (
	Value = word_value_int(Word)
    ;
	Value = word_value_tag(C1, C2, C3, C4),
	B1 = char.to_int(C1),
	B2 = char.to_int(C2),
	B3 = char.to_int(C3),
	B4 = char.to_int(C4),
	Word = (((((B1 << 8) \/ B2) << 8) \/ B3) << 8) \/ B4
    ).

:- pred dump_struct_fields(ddl::in, bytes::in, list(field_def)::in, list(ditem)::out, int::in, int::out, context::in) is det.

dump_struct_fields(_DDL, _Bytes, [], [], !Offset, _Context).
dump_struct_fields(DDL, Bytes, [Field|Fields], [Item|Items], !Offset, Context0) :-
    dump_field(DDL, Bytes, Field, Item, !Offset, Context0, Context),
    dump_struct_fields(DDL, Bytes, Fields, Items, !Offset, Context).

:- pred dump_field(ddl::in, bytes::in, field_def::in, ditem::out, int::in, int::out, context::in, context::out) is det.

dump_field(DDL, Bytes, Field, Item, !Offset, !Context) :-
    dump_field_value(DDL, Bytes, yes(Field ^ field_name), Field ^ field_type, Value, !.Offset, Size, !Context),
    !:Offset = !.Offset + Size,
    Item = ditem(Field ^ field_name, Value).

:- pred dump_field_value(ddl::in, bytes::in, maybe(string)::in, field_type::in, ditem_value::out, int::in, int::out, context::in, context::out) is det.

dump_field_value(DDL, Bytes, MaybeName, Type, Value, Offset, Size, !Context) :-
    (
	Type = field_type_word(WordType, _WordValues),
	Size = word_type_size(WordType),
	Word = get_byte_range_as_uint(Bytes, Offset, Size, 0),
	Value = word(Word),
	( if MaybeName = yes(Name) then
	    !:Context = [Name-Word|!.Context]
	else
	    true
	)
    ;
	Type = field_type_array(ArraySize, Type0),
	(
	    ArraySize = array_size_fixed(Length)
	;
	    ArraySize = array_size_variable(Name),
	    Length = context_resolve(!.Context, Name)
	),
	FieldSize = field_type_size(DDL, !.Context, Type0),
	dump_array(DDL, Bytes, Length, Type0, 0, Offset, FieldSize, Values, !Context),
	Value = array(Values),
	Size = Length * FieldSize
    ;
	Type = field_type_struct(Fields0),
	dump_struct_fields(DDL, Bytes, Fields0, Fields, Offset, NextOffset, []),
	Value = fields(Fields),
	Size = NextOffset - Offset
    ;
	Type = field_type_union(_Options),
	Value = fields([]), % FIXME
	Size = 0
    ;
	Type = field_type_ref(Name),
	Def = ddl_resolve(DDL, Name),
	(
	    Def = def_struct(Struct),
	    dump_field_value(DDL, Bytes, MaybeName, field_type_struct(Struct ^ struct_fields), Value, Offset, Size, !Context)
	;
	    Def = def_union(Union),
	    dump_field_value(DDL, Bytes, MaybeName, field_type_union(Union ^ union_options), Value, Offset, Size, !Context)
	)
    ).

:- pred dump_array(ddl::in, bytes::in, int::in, field_type::in, int::in, int::in, int::in, list(ditem_value)::out, context::in, context::out) is det.

dump_array(DDL, Bytes, Length, Type, Index, Offset, Size, Values, !Context) :-
    ( if Index < Length then
	dump_field_value(DDL, Bytes, no, Type, Value, Offset + Index*Size, _Size0, !Context),
	dump_array(DDL, Bytes, Length, Type, Index+1, Offset, Size, Values0, !Context),
	Values = [Value|Values0]
    else
	Values = []
    ).

/*
:- pred write_field_value(list(field_value)::in, list(int)::in, io::di, io::uo) is det.

write_field_value(FieldValues, Bs, !IO) :-
    ( if
	Bs = [B1, B2, B3, B4],
	char.from_int(B1, C1),
	char.from_int(B2, C2),
	char.from_int(B3, C3),
	char.from_int(B4, C4),
	member(field_value_tag(C1, C2, C3, C4), FieldValues)
    then
	write_char('\'', !IO),
	write_char(C1, !IO),
	write_char(C2, !IO),
	write_char(C3, !IO),
	write_char(C4, !IO),
	write_char('\'', !IO)
    else
	write(Bs, !IO)
    ).
*/

:- pred write_bytes(list(int)::in, io::di, io::uo) is det.

write_bytes([], !IO).
write_bytes([B|Bs], !IO) :-
    ( if B >= 32, B < 127, char.from_int(B, C) then
	write_char('\'', !IO),
	write_char(C, !IO),
	write_char('\'', !IO)
    else
	write_int(B, !IO)
    ),
    ( if Bs = [] then
	true
    else
	write_string(", ", !IO),
	write_bytes(Bs, !IO)
    ).

:- func get_byte_range_as_uint(bytes, int, int, int) = int.

get_byte_range_as_uint(Bytes, Offset, Length, Acc) =
    ( if
	Length > 0,
	get_byte(Bytes, Offset, B)
    then
	get_byte_range_as_uint(Bytes, Offset+1, Length-1, (Acc << 8) \/ B)
    else
	Acc
    ).

:- func get_byte_range(bytes, int, int) = list(int).

get_byte_range(Bytes, Offset, Length) = Bs :-
    ( if
	Length > 0,
	get_byte(Bytes, Offset, B)
    then
	Bs = [B | get_byte_range(Bytes, Offset+1, Length-1)]
    else
	Bs = []
    ).

%--------------------------------------------------------------------%

:- pred write_item(int::in, ditem::in, io::di, io::uo) is det.

write_item(Tab, Item, !IO) :-
    write_indent(Tab, !IO),
    write_string(Item ^ ditem_name, !IO),
    write_string(" = ", !IO),
    write_item_value(Tab, Item ^ ditem_value, !IO),
    nl(!IO).

:- pred write_item_value(int::in, ditem_value::in, io::di, io::uo) is det.

write_item_value(Tab, Value, !IO) :-
    (
	Value = choice(Item),
	write_string("{\n", !IO),
	write_item(Tab+1, Item, !IO),
	write_string("\n", !IO),
	write_indent(Tab, !IO),
	write_string("}", !IO)
    ;
	Value = fields(Items),
	write_string("{\n", !IO),
	foldl(write_item(Tab+1), Items, !IO),
	write_indent(Tab, !IO),
	write_string("}", !IO)
    ;
	Value = array(Values),
	write_string("[", !IO),
	write_array(Tab+1, Values, !IO),
	write_string("]", !IO)
    ;
	Value = word(N),
	write_int(N, !IO)
    ).

:- pred write_array(int::in, list(ditem_value)::in, io::di, io::uo) is det.

write_array(_Tab, [], !IO).
write_array(Tab, [Value|Values], !IO) :-
    write_item_value(Tab, Value, !IO),
    ( if Values = [] then
	true
    else
	write_string(", ", !IO),
	write_array(Tab, Values, !IO)
    ).

:- pred write_indent(int::in, io::di, io::uo) is det.

write_indent(Tab, !IO) :-
    ( if Tab > 0 then
	write_string("    ", !IO),
	write_indent(Tab-1, !IO)
    else
	true
    ).

%--------------------------------------------------------------------%

:- end_module dump.

