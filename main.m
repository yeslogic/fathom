:- module main.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

%--------------------------------------------------------------------%

:- implementation.

:- import_module list, pair, maybe, parsing_utils.

:- import_module bytes.
:- import_module ddl.
:- import_module parse.
:- import_module dump.

main(!IO) :-
    command_line_arguments(Args, !IO),
    ( if Args = [DDLFileName, InputFileName] then
	open_input(DDLFileName, Res0, !IO),
	(
	    Res0 = ok(DDLFile),
	    read_file_as_string(DDLFile, Res1, !IO),
	    (
		Res1 = ok(DDLStr),
		parse(DDLStr, Res2),
		(
		    Res2 = ok(DDL),
		    ( if DDL = [_-Struct|_] then
			main2(DDL, Struct, InputFileName, !IO)
		    else
			write_string("Expected one struct\n", !IO)
		    )
		;
		    Res2 = error(_Msg, _Line, _Col),
		    write(Res2, !IO), nl(!IO)
		)
	    ;
		Res1 = error(_, Error1),
		write_string(error_message(Error1), !IO), nl(!IO)
	    ),
	    close_input(DDLFile, !IO)
	;
	    Res0 = error(Error0),
	    write_string(error_message(Error0), !IO), nl(!IO)
	)
    else
	write_string("Usage: <prog> DDL INPUT\n", !IO)
    ).

:- pred main2(ddl::in, struct_def::in, string::in, io::di, io::uo) is det.

main2(DDL, Struct, InputFileName, !IO) :-
    open_binary_input(InputFileName, Res0, !IO),
    (
	Res0 = ok(InputFile),
	read_stream(InputFile, Res1, !IO),
	(
	    Res1 = ok(Bytes),
	    dump_struct(DDL, Bytes, Struct, !IO)
	;
	    Res1 = error(Error1),
	    write_string(Error1, !IO), nl(!IO)
	),
	close_binary_input(InputFile, !IO)
    ;
	Res0 = error(Error0),
	write_string(error_message(Error0), !IO), nl(!IO)
    ).

%--------------------------------------------------------------------%

:- end_module main.

