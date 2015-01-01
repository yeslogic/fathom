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

:- import_module list, assoc_list, int, char, maybe, pair, string.
:- import_module abort.

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

:- type context
    --->    context(
		context_root :: int,
		context_stack :: assoc_list(string, int),
		context_scope :: assoc_list(string, int)
	    ).

:- type refs == assoc_list(int, {context, ddl_def}).

:- type dump == assoc_list({int, ddl_def}, ditem).

%--------------------------------------------------------------------%

dump_root(DDL, Bytes, Def, !IO) :-
    Context = context(0, [], []),
    Refs = [0-{Context,Def}],
    dump(DDL, Bytes, Refs, [], Dump),
    foldl(write_dump, sort(Dump), !IO).

:- pred dump(ddl::in, bytes::in, refs::in, dump::in, dump::out).

dump(DDL, Bytes, Refs, Dump0, Dump) :-
    (
	Refs = [],
	Dump = Dump0
    ;
	Refs = [Offset - {Context, Def}|Refs0],
	( if search(Dump0, {Offset, Def}, _Item) then
	    dump(DDL, Bytes, Refs0, Dump0, Dump)
	else
	    dump_def(DDL, Bytes, Def, Item, Offset, _EndOffset, Context, Refs0, Refs1),
	    dump(DDL, Bytes, Refs1, [{Offset, Def}-Item|Dump0], Dump)
	)
    ).

:- pred dump_def(ddl::in, bytes::in, ddl_def::in, ditem::out, int::in, int::out, context::in, refs::in, refs::out) is det.

dump_def(DDL, Bytes, Def, Item, !Offset, Context0, !Refs) :-
    (
	Def = def_union(Union),
	Name = Union ^ union_name,
	Context = context_nested(Context0, Name, !.Offset),
	dump_union_options(DDL, Bytes, Union ^ union_options, Item0, !Offset, Context, !Refs),
	Value = choice(Item0)
    ;
	Def = def_struct(Struct),
	Name = Struct ^ struct_name,
	Context = context_nested(Context0, Name, !.Offset),
	dump_struct_fields(DDL, Bytes, Struct ^ struct_fields, Fields, !Offset, Context, !Refs),
	Value = fields(Fields)
    ),
    Item = ditem(Name, Value).

:- pred dump_union_options(ddl::in, bytes::in, list(string)::in, ditem::out, int::in, int::out, context::in, refs::in, refs::out) is det.

% FIXME
dump_union_options(_DDL, _Bytes, [], ditem("HELP", fields([])), !Offset, _Context0, !Refs).
dump_union_options(DDL, Bytes, [Option|Options], Item, !Offset, Context0, !Refs) :-
    Def = ddl_resolve(DDL, Option),
    ( if match_def(DDL, Bytes, Def, !.Offset, Context0) then
	dump_def(DDL, Bytes, Def, Item, !Offset, Context0, !Refs)
    else
	dump_union_options(DDL, Bytes, Options, Item, !Offset, Context0, !Refs)
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
    match_word_value(Word, Value).

:- pred dump_struct_fields(ddl::in, bytes::in, list(field_def)::in, list(ditem)::out, int::in, int::out, context::in, refs::in, refs::out) is det.

dump_struct_fields(_DDL, _Bytes, [], [], !Offset, _Context, !Refs).
dump_struct_fields(DDL, Bytes, [Field|Fields], [Item|Items], !Offset, Context0, !Refs) :-
    dump_field(DDL, Bytes, Field, Item, !Offset, Context0, Context, !Refs),
    dump_struct_fields(DDL, Bytes, Fields, Items, !Offset, Context, !Refs).

:- pred dump_field(ddl::in, bytes::in, field_def::in, ditem::out, int::in, int::out, context::in, context::out, refs::in, refs::out) is det.

dump_field(DDL, Bytes, Field, Item, !Offset, !Context, !Refs) :-
    dump_field_value(DDL, Bytes, yes(Field ^ field_name), Field ^ field_type, Value, !.Offset, Size, !Context, !Refs),
    Item = ditem(Field ^ field_name, Value),
    !:Offset = !.Offset + Size.

:- pred dump_field_value(ddl::in, bytes::in, maybe(string)::in, field_type::in, ditem_value::out, int::in, int::out, context::in, context::out, refs::in, refs::out) is det.

dump_field_value(DDL, Bytes, MaybeName, Type, Value, Offset, Size, !Context, !Refs) :-
    (
	Type = field_type_word(WordType, WordValues),
	Size = word_type_size(WordType),
	Word = get_byte_range_as_uint(Bytes, Offset, Size, 0),
	update_refs(DDL, !.Context, Word, WordValues, !Refs),
	Value = word(Word),
	( if MaybeName = yes(Name) then
	    !Context ^ context_scope := [Name-Word|!.Context ^ context_scope]
	else
	    true
	)
    ;
	Type = field_type_array(ArraySize, Type0),
	(
	    ArraySize = array_size_fixed(Length)
	;
	    ArraySize = array_size_variable(Name),
	    Length = scope_resolve(!.Context ^ context_scope, Name)
	),
	FieldSize = field_type_size(DDL, !.Context ^ context_scope, Type0),
	dump_array(DDL, Bytes, Length, Type0, 0, Offset, FieldSize, Values, !Context, !Refs),
	Value = array(Values),
	Size = Length * FieldSize
    ;
	Type = field_type_struct(Fields0),
	NestedContext = !.Context, % FIXME?
	dump_struct_fields(DDL, Bytes, Fields0, Fields, Offset, NextOffset, NestedContext, !Refs),
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
	    dump_field_value(DDL, Bytes, MaybeName, field_type_struct(Struct ^ struct_fields), Value, Offset, Size, !Context, !Refs)
	;
	    Def = def_union(Union),
	    dump_field_value(DDL, Bytes, MaybeName, field_type_union(Union ^ union_options), Value, Offset, Size, !Context, !Refs)
	)
    ).

:- pred dump_array(ddl::in, bytes::in, int::in, field_type::in, int::in, int::in, int::in, list(ditem_value)::out, context::in, context::out, refs::in, refs::out) is det.

dump_array(DDL, Bytes, Length, Type, Index, Offset, Size, Values, !Context, !Refs) :-
    ( if Index < Length then
	dump_field_value(DDL, Bytes, no, Type, Value, Offset + Index*Size, _Size0, !Context, !Refs),
	dump_array(DDL, Bytes, Length, Type, Index+1, Offset, Size, Values0, !Context, !Refs),
	Values = [Value|Values0]
    else
	Values = []
    ).

:- pred update_refs(ddl::in, context::in, int::in, word_values::in, refs::in, refs::out) is det.

update_refs(DDL, Context, Word, WordValues, !Refs) :-
    (
	WordValues ^ word_values_enum = [],
	( if WordValues ^ word_values_any = yes(Interp) then
	    ( if Interp = word_interp_offset(Offset) then
		NewOffset = calc_offset(Context, Offset, Word),
		Def = ddl_resolve(DDL, Offset ^ offset_type),
		!:Refs = [NewOffset - {Context,Def}|!.Refs]
	    else
		true
	    )
	else
	    true
	)
    ;
	WordValues ^ word_values_enum = [V-Interp|Vs],
	( if match_word_value(Word, V) then
	    ( if Interp = word_interp_offset(Offset) then
		NewOffset = calc_offset(Context, Offset, Word),
		Def = ddl_resolve(DDL, Offset ^ offset_type),
		!:Refs = [NewOffset - {Context,Def}|!.Refs]
	    else
		true
	    )
	else
	    WordValues0 = WordValues ^ word_values_enum := Vs,
	    update_refs(DDL, Context, Word, WordValues0, !Refs)
	)
    ).

:- pred match_word_value(int::in, word_value::in) is semidet.

match_word_value(Word, Value)  :-
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

:- func calc_offset(context, offset, int) = int.

calc_offset(Context, Offset, V) = NewOffset :-
    (
	Offset ^ offset_base = offset_base_struct,
	(
	    Context ^ context_stack = [],
	    abort("context stack underflow")
	;
	    Context ^ context_stack = [_-StructOffset|_],
	    NewOffset = StructOffset + V
	)
    ;
	Offset ^ offset_base = offset_base_root,
	NewOffset = Context ^ context_root + V
    ;
	Offset ^ offset_base = offset_base_named(Name),
	( if search(Context ^ context_stack, Name, StructOffset) then
	    NewOffset = StructOffset + V
	else
	    abort("context stack missing: "++Name)
	)
    ).

:- func context_nested(context, string, int) = context.

context_nested(Context, Name, Offset) =
    Context ^ context_stack := [Name-Offset|Context ^ context_stack].

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

:- pred write_dump(pair({int, ddl_def}, ditem)::in, io::di, io::uo) is det.

write_dump({Offset, _Def} - Item, !IO) :-
    write_int(Offset, !IO),
    write_string(": ", !IO),
    write_item(0, Item, !IO).

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

