:- module dump.

% Copyright (C) 2014-2015, 2017 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.

:- import_module bytes.
:- import_module ddl.

:- pred dump_root(ddl::in, bytes::in, ddl_def::in, io::di, io::uo) is det.

%--------------------------------------------------------------------%

:- implementation.

:- import_module list, assoc_list, int, char, maybe, bool, pair, string.
:- import_module abort.

:- type ditem
    --->    ditem(
                ditem_name :: maybe(string),
                ditem_value :: ditem_value
            ).

:- type ditem_value
    --->    item(ditem)
    ;       fields(list(ditem))
    ;       array(list(ditem_value))
    ;       word(int)
    ;       string(string).

:- type context
    --->    context(
                context_root :: int,
                context_stack :: assoc_list(string, int),
                context_scope :: scope
            ).

:- type refs == assoc_list(int, {context, ref}).

:- type ref
    --->    ref_def(ddl_def, list(string))
    ;       ref_type(ddl_type).

:- type dump == assoc_list({int, ref}, ditem).

%--------------------------------------------------------------------%

dump_root(DDL, Bytes, Def, !IO) :-
    Context = context(0, [], []),
    Refs = [0-{Context,ref_def(Def,[])}],
    dump(DDL, Bytes, Refs, [], Dump),
    foldl(write_dump, sort(Dump), !IO).

:- pred dump(ddl::in, bytes::in, refs::in, dump::in, dump::out) is det.

dump(DDL, Bytes, Refs, Dump0, Dump) :-
    (
        Refs = [],
        Dump = Dump0
    ;
        Refs = [Offset - {Context, Ref}|Refs0],
        ( if search(Dump0, {Offset, Ref}, _Item) then
            dump(DDL, Bytes, Refs0, Dump0, Dump)
        else
            (
                Ref = ref_def(Def, Args),
                dump_def(DDL, Bytes, Def, Args, Item, Offset, _EndOffset, Context, Refs0, Refs1),
                dump(DDL, Bytes, Refs1, [{Offset, Ref}-Item|Dump0], Dump)
            ;
                Ref = ref_type(Type),
                dump_value(DDL, Bytes, no, Type, Value, Offset, no, _Size, Context, _NewContext, Refs0, Refs1),
                Item = ditem(no, Value),
                dump(DDL, Bytes, Refs1, [{Offset, Ref}-Item|Dump0], Dump)
            )
        )
    ).

:- pred dump_def(ddl::in, bytes::in, ddl_def::in, list(string)::in, ditem::out, int::in, int::out, context::in, refs::in, refs::out) is det.

dump_def(DDL, Bytes, Def, Args0, Item, !Offset, Context0, !Refs) :-
    Name = Def ^ def_name,
    ArgValues = map(scope_resolve(Context0 ^ context_scope), Args0),
    Args = from_corresponding_lists(Def ^ def_args, ArgValues),
    Context = context_nested(Context0, Name, Args, !.Offset),
    dump_value(DDL, Bytes, no, Def ^ def_type, Value, !.Offset, no, Size, Context, _NewContext, !Refs),
    !:Offset = !.Offset + Size,
    Item = ditem(yes(Name), Value).

:- pred dump_union_options(ddl::in, bytes::in, list(string)::in, ditem::out, int::in, int::out, context::in, refs::in, refs::out) is det.

% FIXME
dump_union_options(_DDL, _Bytes, [], ditem(yes("HELP"), fields([])), !Offset, _Context0, !Refs).
dump_union_options(DDL, Bytes, [Option|Options], Item, !Offset, Context0, !Refs) :-
    Def = ddl_lookup_det(DDL, Option),
    ( if match_def(DDL, Bytes, Def, !.Offset, Context0) then
        dump_def(DDL, Bytes, Def, [], Item, !Offset, Context0, !Refs)
    else
        dump_union_options(DDL, Bytes, Options, Item, !Offset, Context0, !Refs)
    ).

:- pred match_def(ddl::in, bytes::in, ddl_def::in, int::in, context::in) is semidet.

match_def(DDL, Bytes, Def, Offset, Context) :-
    match_type(DDL, Bytes, Def ^ def_type, Offset, Context).

:- pred match_union_options(ddl::in, bytes::in, list(string)::in, int::in, context::in) is semidet.

match_union_options(DDL, Bytes, Options, Offset, Context) :-
    member(Def0, map(ddl_lookup_det(DDL), Options)),
    match_def(DDL, Bytes, Def0, Offset, Context).

:- pred match_struct_fields(ddl::in, bytes::in, list(field_def)::in, int::in, context::in) is semidet.

match_struct_fields(DDL, Bytes, Fields, Offset, Context) :-
    (
        Fields = [] % FIXME uhh...
    ;
        Fields = [Field|_],
        match_type(DDL, Bytes, Field ^ field_type, Offset, Context)
    ).

:- pred match_type(ddl::in, bytes::in, ddl_type::in, int::in, context::in) is semidet.

match_type(DDL, Bytes, Type0, Offset, Context) :-
    simplify_type(Context ^ context_scope, Type0, Type),
    require_complete_switch [Type]
    (
        Type = ddl_type_word(WordType, WordValues),
        member(Value-_Interp, WordValues ^ word_values_enum),
        match_word(Bytes, WordType, Value, Offset)
    ;
        Type = ddl_type_array(_, _),
        fail
    ;
        Type = ddl_type_zero_or_more(_),
        fail
    ;
        Type = ddl_type_sized(_, _),
        fail
    ;
        Type = ddl_type_struct(Fields),
        match_struct_fields(DDL, Bytes, Fields, Offset, Context)
    ;
        Type = ddl_type_union(Options),
        match_union_options(DDL, Bytes, Options, Offset, Context)
    ;
        Type = ddl_type_switch(_, _),
        abort("match_type: switch")
    ;
        Type = ddl_type_named(Name, _Args0), % FIXME
        Def = ddl_lookup_det(DDL, Name),
        match_def(DDL, Bytes, Def, Offset, Context)
    ;
        Type = ddl_type_tag_magic(Name),
        TagNum = scope_resolve_int(Context ^ context_scope, Name),
        TagStr = tag_num_to_string(TagNum),
        ( if ddl_search(DDL, TagStr, Def) then
            match_def(DDL, Bytes, Def, Offset, Context)
        else
            fail
        )
    ;
        Type = ddl_type_string,
        fail
    ).

:- pred match_word(bytes::in, word_type::in, word_value::in, int::in) is semidet.

match_word(Bytes, WordType, Value, Offset) :-
    Size = word_type_size(WordType),
    Word = get_byte_range_as_uint(Bytes, Offset, Size, 0),
    match_word_value(Word, Value).

:- pred dump_struct_fields(ddl::in, bytes::in, list(field_def)::in, list(ditem)::out, int::in, int::out, context::in, refs::in, refs::out) is det.

dump_struct_fields(_DDL, _Bytes, [], [], !Offset, _Context, !Refs).
dump_struct_fields(DDL, Bytes, [Field|Fields], Items, !Offset, Context0, !Refs) :-
    ( if Field ^ field_cond = yes(Cond) => match_cond(Context0, Cond) then
        dump_field(DDL, Bytes, Field, Item, !Offset, Context0, Context, !Refs),
        dump_struct_fields(DDL, Bytes, Fields, Items0, !Offset, Context, !Refs),
        Items = [Item|Items0]
    else
        dump_struct_fields(DDL, Bytes, Fields, Items, !Offset, Context0, !Refs)
    ).

:- pred match_cond(context::in, expr_bool::in) is semidet.

match_cond(Context, Expr) :-
    Val = eval_expr_bool(Context ^ context_scope, Expr),
    Val = yes.

:- pred dump_field(ddl::in, bytes::in, field_def::in, ditem::out, int::in, int::out, context::in, context::out, refs::in, refs::out) is det.

dump_field(DDL, Bytes, Field, Item, !Offset, !Context, !Refs) :-
    dump_value(DDL, Bytes, yes(Field ^ field_name), Field ^ field_type, Value, !.Offset, no, Size, !Context, !Refs),
    Item = ditem(yes(Field ^ field_name), Value),
    !:Offset = !.Offset + Size.

:- pred dump_value(ddl::in, bytes::in, maybe(string)::in, ddl_type::in, ditem_value::out, int::in, maybe(int)::in, int::out, context::in, context::out, refs::in, refs::out) is det.

dump_value(DDL, Bytes, MaybeName, Type0, Value, Offset, MaybeSize, Size, !Context, !Refs) :-
    simplify_type(!.Context ^ context_scope, Type0, Type),
    (
        Type = ddl_type_word(WordType, WordValues),
        Size = word_type_size(WordType),
        Word = get_byte_range_as_uint(Bytes, Offset, Size, 0),
        update_refs(DDL, !.Context, Word, WordValues, !Refs),
        Value = word(Word),
        ( if MaybeName = yes(Name) then
            !Context ^ context_scope := [Name-scope_int(Word)|!.Context ^ context_scope]
        else
            true
        )
    ;
        Type = ddl_type_array(SizeExpr, Type1),
        Length = eval_expr_int(!.Context ^ context_scope, SizeExpr),
        FieldSize = ddl_type_size(DDL, !.Context ^ context_scope, Type1),
        dump_array(DDL, Bytes, Length, Type1, 0, Offset, FieldSize, Values, !Context, !Refs),
        Value = array(Values),
        Size = Length * FieldSize,
        ( if
            MaybeName = yes(Name),
            Type1 = ddl_type_word(WordType, _)
        then
            get_word_array(DDL, Bytes, Length, WordType, 0, Offset, FieldSize, Words, !Context, !Refs),
            !Context ^ context_scope := [Name-scope_array(Words)|!.Context ^ context_scope]
        else
            true
        )
    ;
        Type = ddl_type_zero_or_more(Type1),
        dump_zero_or_more(DDL, Bytes, Type1, Offset, Size, Values, !Context, !Refs),
        Value = array(Values)
    ;
        Type = ddl_type_sized(Type1, SizeExpr),
        Size = eval_expr_int(!.Context ^ context_scope, SizeExpr),
        dump_value(DDL, Bytes, no, Type1, Value, Offset, yes(Size), _Size, !Context, !Refs)
    ;
        Type = ddl_type_struct(Fields0),
        NestedContext = !.Context, % FIXME?
        dump_struct_fields(DDL, Bytes, Fields0, Fields, Offset, NextOffset, NestedContext, !Refs),
        Value = fields(Fields),
        Size = NextOffset - Offset
    ;
        Type = ddl_type_union(Options),
        NestedContext = !.Context, % FIXME?
        dump_union_options(DDL, Bytes, Options, Item, Offset, NextOffset, NestedContext, !Refs),
        Value = item(Item),
        Size = NextOffset - Offset
    ;
        Type = ddl_type_switch(_, _),
        abort("dump_value: switch")
    ;
        Type = ddl_type_named(Name, Args),
        Def = ddl_lookup_det(DDL, Name),
        dump_def(DDL, Bytes, Def, Args, Item, Offset, NewOffset, !.Context, !Refs),
        Value = item(Item),
        Size = NewOffset - Offset
    ;
        Type = ddl_type_tag_magic(Name),
        TagNum = scope_resolve_int(!.Context ^ context_scope, Name),
        TagStr = tag_num_to_string(TagNum),
        ( if ddl_search(DDL, TagStr, Def) then
            dump_def(DDL, Bytes, Def, [], Item, Offset, NewOffset, !.Context, !Refs),
            Value = item(Item),
            Size = NewOffset - Offset
        else
            % FIXME
            Value = item(ditem(yes("Unknown tag: "++TagStr), fields([]))),
            Size = 0
        )
    ;
        Type = ddl_type_string,
        (
            MaybeSize = yes(Size0),
            Bs = get_byte_range(Bytes, Offset, Size0),
            ( if Cs = ucs2_hack(Bs) then
                Str = from_char_list(Cs)
            else
                Str = "<<non-ASCII string>>"
            ),
            Value = string(Str),
            Size = Size0
        ;
            MaybeSize = no,
            abort("FIXME no size for string")
        )
    ).

:- func ucs2_hack(list(int)) = list(char).
:- mode ucs2_hack(in) = out is semidet.

ucs2_hack([]) = [].
ucs2_hack([0,B|Bs]) = [C|ucs2_hack(Bs)] :- char.from_int(B, C).

:- pred get_word_array(ddl::in, bytes::in, int::in, word_type::in, int::in, int::in, int::in, list(int)::out, context::in, context::out, refs::in, refs::out) is det.

get_word_array(DDL, Bytes, Length, WordType, Index, Offset, Size, Values, !Context, !Refs) :-
    ( if Index < Length then
        Word = get_byte_range_as_uint(Bytes, Offset + Index*Size, Size, 0),
        get_word_array(DDL, Bytes, Length, WordType, Index+1, Offset, Size, Values0, !Context, !Refs),
        Values = [Word|Values0]
    else
        Values = []
    ).

:- pred dump_array(ddl::in, bytes::in, int::in, ddl_type::in, int::in, int::in, int::in, list(ditem_value)::out, context::in, context::out, refs::in, refs::out) is det.

dump_array(DDL, Bytes, Length, Type, Index, Offset, Size, Values, !Context, !Refs) :-
    ( if Index < Length then
        dump_value(DDL, Bytes, no, Type, Value, Offset + Index*Size, no, _Size0, !Context, !Refs),
        dump_array(DDL, Bytes, Length, Type, Index+1, Offset, Size, Values0, !Context, !Refs),
        Values = [Value|Values0]
    else
        Values = []
    ).

:- pred dump_zero_or_more(ddl::in, bytes::in, ddl_type::in, int::in, int::out, list(ditem_value)::out, context::in, context::out, refs::in, refs::out) is det.

dump_zero_or_more(DDL, Bytes, Type, Offset, Size, Values, !Context, !Refs) :-
    ( if Offset < length(Bytes) then
        dump_value(DDL, Bytes, no, Type, Value, Offset, no, Size0, !Context, !Refs),
        dump_zero_or_more(DDL, Bytes, Type, Offset+Size0, Size1, Values0, !Context, !Refs),
        Values = [Value|Values0],
        Size = Size0 + Size1
    else
        Values = [],
        Size = 0
    ).

:- pred update_refs(ddl::in, context::in, int::in, word_values::in, refs::in, refs::out) is det.

update_refs(DDL, Context, Word, WordValues, !Refs) :-
    (
        WordValues ^ word_values_enum = [],
        ( if WordValues ^ word_values_any = yes(Interp) then
            ( if Interp = word_interp_offset(Offset) then
                make_ref_from_offset(DDL, Context, Offset, Word, !Refs)
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
                make_ref_from_offset(DDL, Context, Offset, Word, !Refs)
            else
                true
            )
        else
            WordValues0 = WordValues ^ word_values_enum := Vs,
            update_refs(DDL, Context, Word, WordValues0, !Refs)
        )
    ).

:- pred make_ref_from_offset(ddl::in, context::in, offset::in, int::in, refs::in, refs::out) is det.

make_ref_from_offset(DDL, Context, Offset, Word, !Refs) :-
    NewOffset = calc_offset(Context, Offset, Word),
    simplify_type(Context ^ context_scope, Offset ^ offset_type, Type),
    (
        (
            Type = ddl_type_word(_, _) ;
            Type = ddl_type_array(_, _) ;
            Type = ddl_type_zero_or_more(_) ;
            Type = ddl_type_sized(_, _) ;
            Type = ddl_type_struct(_) ;
            Type = ddl_type_union(_) ;
            Type = ddl_type_string
        ),
        !:Refs = [NewOffset - {Context,ref_type(Type)}|!.Refs]
    ;
        Type = ddl_type_switch(_, _),
        abort("make_ref_from_offset: switch")
    ;
        Type = ddl_type_named(Name, Args),
        Def = ddl_lookup_det(DDL, Name),
        !:Refs = [NewOffset - {Context,ref_def(Def,Args)}|!.Refs]
    ;
        Type = ddl_type_tag_magic(Name),
        TagNum = scope_resolve_int(Context ^ context_scope, Name),
        TagStr = tag_num_to_string(TagNum),
        ( if ddl_search(DDL, TagStr, Def) then
            !:Refs = [NewOffset - {Context,ref_def(Def,[])}|!.Refs]
        else
            trace [io(!IO)] ( write_string("failed type lookup for tag: "++TagStr++"\n", !IO) )
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
        Offset ^ offset_base = offset_base_stack(Name),
        ( if search(Context ^ context_stack, Name, StructOffset) then
            NewOffset = StructOffset + V
        else
            abort("context stack missing: "++Name)
        )
    ;
        Offset ^ offset_base = offset_base_expr(Expr),
        Offset0 = eval_expr_int(Context ^ context_scope, Expr),
        (
            Context ^ context_stack = [],
            abort("context stack underflow")
        ;
            Context ^ context_stack = [_-StructOffset|_],
            NewOffset = StructOffset + Offset0 + V
        )
    ).

:- func context_nested(context, string, scope, int) = context.

context_nested(Context, Name, Args, Offset) =
    (Context
        ^ context_stack := [Name-Offset|Context ^ context_stack])
        ^ context_scope := Args ++ Context ^ context_scope.

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

:- pred write_dump(pair({int, ref}, ditem)::in, io::di, io::uo) is det.

write_dump({Offset, _Ref} - Item, !IO) :-
    write_int(Offset, !IO),
    write_string(": ", !IO),
    write_item(0, Item, !IO).

:- pred write_item(int::in, ditem::in, io::di, io::uo) is det.

write_item(Tab, Item, !IO) :-
    write_indent(Tab, !IO),
    (
        Item ^ ditem_name = yes(Name),
        write_string(Name, !IO),
        write_string(" = ", !IO)
    ;
        Item ^ ditem_name = no
    ),
    write_item_value(Tab, Item ^ ditem_value, !IO),
    nl(!IO).

:- pred write_item_value(int::in, ditem_value::in, io::di, io::uo) is det.

write_item_value(Tab, Value, !IO) :-
    (
        Value = item(Item),
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
    ;
        Value = string(Str),
        write(Str, !IO)
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

