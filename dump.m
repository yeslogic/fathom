:- module dump.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.

:- import_module bytes.
:- import_module ddl.

:- pred dump_struct(ddl::in, bytes::in, struct_def::in, io::di, io::uo) is det.

%--------------------------------------------------------------------%

:- implementation.

:- import_module list, int, char, maybe, pair.

dump_struct(DDL, Bytes, Struct, !IO) :-
    write_string(Struct ^ struct_name, !IO),
    write_string(" {\n", !IO),
    dump_struct_fields(DDL, Bytes, 1, Struct ^ struct_fields, 0, _Offset, [], !IO),
    write_string("}\n", !IO).

:- pred dump_struct_fields(ddl::in, bytes::in, int::in, list(field_def)::in, int::in, int::out, context::in, io::di, io::uo) is det.

dump_struct_fields(_DDL, _Bytes, _Indent, [], !Offset, _Context, !IO).
dump_struct_fields(DDL, Bytes, Indent, [Field|Fields], !Offset, Context0, !IO) :-
    dump_field(DDL, Bytes, Indent, Field, !Offset, Context0, Context, !IO),
    dump_struct_fields(DDL, Bytes, Indent, Fields, !Offset, Context, !IO).

:- pred dump_field(ddl::in, bytes::in, int::in, field_def::in, int::in, int::out, context::in, context::out, io::di, io::uo) is det.

dump_field(DDL, Bytes, Indent, Field, !Offset, !Context, !IO) :-
    write_indent(Indent, !IO),
    write_string(Field ^ field_name, !IO),
    write_string(" = ", !IO),
    dump_field_value(DDL, Bytes, Indent, yes(Field ^ field_name), Field ^ field_type, !.Offset, Size, !Context, !IO),
    !:Offset = !.Offset + Size,
    write_string(",\n", !IO).

:- pred dump_field_value(ddl::in, bytes::in, int::in, maybe(string)::in, field_type::in, int::in, int::out, context::in, context::out, io::di, io::uo) is det.

dump_field_value(DDL, Bytes, Indent, MaybeName, Type, Offset, Size, !Context, !IO) :-
    (
	Type = field_type_word(WordType),
	Size = word_type_size(WordType),
	Word = get_byte_range_as_uint(Bytes, Offset, Size, 0),
	write_int(Word, !IO),
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
	write_string("[", !IO),
	dump_array(DDL, Bytes, Indent, Length, Type0, 0, Offset, FieldSize, !Context, !IO),
	write_string("]", !IO),
	Size = Length * FieldSize
    ;
	Type = field_type_struct(Fields0),
	write_string("{\n", !IO),
	dump_struct_fields(DDL, Bytes, Indent+1, Fields0, Offset, NextOffset, [], !IO),
	Size = NextOffset - Offset,
	write_indent(Indent, !IO),
	write_string("}", !IO)
    ;
	Type = field_type_ref(Name),
	Struct = ddl_resolve(DDL, Name),
	dump_field_value(DDL, Bytes, Indent, MaybeName, field_type_struct(Struct ^ struct_fields), Offset, Size, !Context, !IO)
    ).

:- pred dump_array(ddl::in, bytes::in, int::in, int::in, field_type::in, int::in, int::in, int::in, context::in, context::out, io::di, io::uo) is det.

dump_array(DDL, Bytes, Indent, Length, Type, Index, Offset, Size, !Context, !IO) :-
    ( if Index < Length then
	dump_field_value(DDL, Bytes, Indent, no, Type, Offset + Index*Size, _Size0, !Context, !IO),
	( if Index < Length - 1 then
	    write_string(",", !IO),
	    dump_array(DDL, Bytes, Indent, Length, Type, Index+1, Offset, Size, !Context, !IO)
	else
	    true
	)
    else
	true
    ).

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

