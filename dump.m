:- module dump.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.

:- import_module bytes.
:- import_module ddl.

:- pred dump_struct(bytes::in, struct_def::in, io::di, io::uo) is det.

%--------------------------------------------------------------------%

:- implementation.

:- import_module list, int, char.

dump_struct(Bytes, Struct, !IO) :-
    write_string(Struct ^ struct_name, !IO),
    write_string(" {\n", !IO),
    Fields = get_offsets(Struct ^ struct_fields, 0),
    foldl(dump_field(Bytes, 1), Fields, !IO),
    write_string("}\n", !IO).

:- pred dump_field(bytes::in, int::in, {field_def, int}::in, io::di, io::uo) is det.

dump_field(Bytes, Indent, {Field, Offset}, !IO) :-
    write_indent(Indent, !IO),
    write_string(Field ^ field_name, !IO),
    write_string(" = ", !IO),
    (
	Field ^ field_type = field_type_word(_WordType, _Values),
	Word = get_byte_range_as_uint(Bytes, Offset, Field ^ field_size, 0),
	write_int(Word, !IO)
    ;
	Field ^ field_type = field_type_array(_Length, _Type, Values),
	(
	    Values = [],
	    Bs = get_byte_range(Bytes, Offset, Field ^ field_size),
	    write(Bs, !IO)
	;
	    Values = [_|_],
	    Bs = get_byte_range(Bytes, Offset, Field ^ field_size),
	    write_field_value(Values, Bs, !IO)
	)
    ;
	Field ^ field_type = field_type_struct(Fields0),
	write_string("{\n", !IO),
	Fields = get_offsets(Fields0, Offset),
	foldl(dump_field(Bytes, Indent+1), Fields, !IO),
	write_indent(Indent, !IO),
	write_string("}", !IO)
    ),
    write_string(",\n", !IO).

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

:- func get_offsets(list(field_def), int) = list({field_def, int}).

get_offsets([], _) = [].
get_offsets([Field|Fs], Offset) =
    [{Field, Offset}|get_offsets(Fs, Offset + Field ^ field_size)].

%--------------------------------------------------------------------%

:- pred write_indent(int::in, io::di, io::uo) is det.

write_indent(Indent, !IO) :-
    ( if Indent > 0 then
	write_string("    ", !IO),
	write_indent(Indent-1, !IO)
    else
	true
    ).

%--------------------------------------------------------------------%

:- end_module dump.

