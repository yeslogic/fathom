:- module bytes.

% Copyright (C) 2005-2009, 2011-2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module list, array, int, string, io, maybe.

:- type bytes.

:- func bytes(list(int)) = bytes.

:- func bytes_from_array(array(int)) = bytes.

:- func ucs2_list_to_bytes(list(int)) = bytes.

:- func string_to_bytes(string) = bytes.

:- func int32_to_bytes(int) = bytes.

:- func length(bytes) = int.

:- func bytes_to_list(bytes) = list(int).

:- func take(int, bytes) = bytes.

:- pred get_byte(bytes, int, int).
:- mode get_byte(in, in, out) is semidet.

:- pred bytes_equal(bytes, bytes).
:- mode bytes_equal(in, in) is semidet.

:- pred read_stream(binary_input_stream, maybe_error(bytes), io, io).
:- mode read_stream(in, out, di, uo) is det.

%--------------------------------------------------------------------%

:- implementation.

:- import_module char.

:- pragma foreign_type(c, bytes, "bulkio_bytes *").

:- pragma foreign_decl(c, "
#include <stdint.h>

typedef struct {
    uint32_t length;
    uint8_t *bytes;
} bulkio_bytes;

void new_bytes(int length, bulkio_bytes **bytes, MR_AllocSiteInfoPtr alloc_id);
").

:- pragma foreign_code(c, "
void new_bytes(int length, bulkio_bytes **bytes, MR_AllocSiteInfoPtr alloc_id)
{
    MR_Word ptr;

    MR_incr_hp_atomic_msg(ptr, MR_bytes_to_words(length), alloc_id, ""bytes.bytes/0"");

    *bytes = MR_GC_NEW_ATTRIB(bulkio_bytes, alloc_id);
    (*bytes)->length = length;
    (*bytes)->bytes = (uint8_t *) ptr;
}
").

%--------------------------------------------------------------------%

:- pragma promise_pure(bytes/1).

bytes(Bs) = Bytes :-
    Length = length(Bs),
    impure make_new_bytes(Length, Bytes),
    impure set_bytes_from_list(Bs, 0, Bytes).

:- impure pred set_bytes_from_list(list(int), int, bytes).
:- mode set_bytes_from_list(in, in, in) is det.

set_bytes_from_list([], _N, _Bytes).
set_bytes_from_list([B|Bs], N, Bytes) :-
    impure set_byte(Bytes, N, B),
    impure set_bytes_from_list(Bs, N+1, Bytes).

:- impure pred set_byte(bytes, int, int).
:- mode set_byte(in, in, in) is det.

:- pragma foreign_proc(c,
	set_byte(Bytes::in, N::in, B::in),
	[will_not_call_mercury, thread_safe], "
    Bytes->bytes[N] = B;
").

%--------------------------------------------------------------------%

:- pragma foreign_proc(c,
	bytes_from_array(Array::in) = (Bytes::out),
	[will_not_call_mercury, thread_safe, promise_pure], "
    {
	int i;

	new_bytes(Array->size, &Bytes, MR_ALLOC_ID);

	for (i = 0; i < Array->size; ++i)
	{
	    Bytes->bytes[i] = Array->elements[i];
	}
    }
").

%--------------------------------------------------------------------%

:- pragma promise_pure(ucs2_list_to_bytes/1).

ucs2_list_to_bytes(Us) = Bytes :-
    Length = length(Us),
    impure make_new_bytes(Length * 2, Bytes),
    impure set_ucs2_bytes_from_list(Us, 0, Bytes).

:- impure pred set_ucs2_bytes_from_list(list(int), int, bytes).
:- mode set_ucs2_bytes_from_list(in, in, in) is det.

set_ucs2_bytes_from_list([], _N, _Bytes).
set_ucs2_bytes_from_list([U|Us], N, Bytes) :-
    impure set_ucs2_bytes(Bytes, N, U),
    impure set_ucs2_bytes_from_list(Us, N+2, Bytes).

:- impure pred set_ucs2_bytes(bytes, int, int).
:- mode set_ucs2_bytes(in, in, in) is det.

:- pragma foreign_proc(c,
	set_ucs2_bytes(Bytes::in, N::in, U::in),
	[will_not_call_mercury, thread_safe], "
    Bytes->bytes[N] = U >> 8;
    Bytes->bytes[N+1] = U & 0xFF;
").

%--------------------------------------------------------------------%

:- pragma foreign_proc(c,
	string_to_bytes(S::in) = (Bytes::out),
	[will_not_call_mercury, thread_safe, promise_pure], "
    {
	int i;

	new_bytes(strlen(S), &Bytes, MR_ALLOC_ID);

	for (i = 0; i < Bytes->length; ++i)
	{
	    Bytes->bytes[i] = S[i];
	}
    }
").

int32_to_bytes(Int) = bytes([B0,B1,B2,B3]) :-
    B0 = Int /\ 0xFF,
    B1 = (Int /\ 0xFF00) >> 8,
    B2 = (Int /\ 0xFF0000) >> 16,
    B3 = (Int /\ 0xFF000000) >> 24.

:- pragma foreign_proc(c,
	length(Bytes::in) = (Length::out),
	[will_not_call_mercury, thread_safe, promise_pure], "
    Length = Bytes->length;
").

bytes_to_list(Bytes) = map(get_byte(Bytes), 0 .. length(Bytes) - 1).

:- func get_byte(bytes, int) = int.

:- pragma foreign_proc(c,
	get_byte(Bytes::in, Index::in) = (Byte::out),
	[will_not_call_mercury, thread_safe, promise_pure], "
    Byte = Bytes->bytes[Index];
").

:- pragma foreign_proc(c,
	take(Take::in, Bytes0::in) = (Bytes::out),
	[will_not_call_mercury, thread_safe, promise_pure], "
    {
	int i;

	new_bytes(Take, &Bytes, MR_ALLOC_ID);

	if (Take <= Bytes0->length)
	{
	    for (i = 0; i < Take; ++i)
	    {
		Bytes->bytes[i] = Bytes0->bytes[i];
	    }
	}
	else
	{
	    for (i = 0; i < Bytes0->length; ++i)
	    {
		Bytes->bytes[i] = Bytes0->bytes[i];
	    }
	    
	    for (i = Bytes0->length; i < Take; ++i)
	    {
		Bytes->bytes[i] = 0;
	    }
	}
    }
").

:- pragma foreign_proc(c,
	get_byte(Bytes::in, Pos::in, Byte::out),
	[will_not_call_mercury, thread_safe, promise_pure], "
    if (Pos >= 0 && Pos < Bytes->length)
    {
	Byte = Bytes->bytes[Pos];

	SUCCESS_INDICATOR = 1;
    }
    else
    {
	SUCCESS_INDICATOR = 0;
    }
").

:- pragma foreign_proc(c,
	bytes_equal(Bytes1::in, Bytes2::in),
	[will_not_call_mercury, thread_safe, promise_pure], "
    if (Bytes1->length == Bytes2->length)
    {
	SUCCESS_INDICATOR = (memcmp(Bytes1->bytes, Bytes2->bytes, Bytes1->length) == 0);
    }
    else
    {
	SUCCESS_INDICATOR = 0;
    }
").

:- impure pred make_new_bytes(int, bytes).
:- mode make_new_bytes(in, out) is det.

:- pragma foreign_proc(c,
	make_new_bytes(Length::in, Bytes::out),
	[will_not_call_mercury, thread_safe], "
    new_bytes(Length, &Bytes, MR_ALLOC_ID);
").

read_stream(Stream, Res, !IO) :-
    read_stream0(Stream, Bytes, Res0, !IO),
    ( if Res0 = 0 then
	Res = ok(Bytes)
    else
	Res = error("error reading stream")
    ).

:- pred read_stream0(binary_input_stream, bytes, int, io, io).
:- mode read_stream0(in, out, out, di, uo) is det.

:- pragma foreign_proc(c,
	read_stream0(Stream::in, Bytes::out, Res::out, IO0::di, IO::uo),
	[will_not_call_mercury, thread_safe, promise_pure], "
    {
	int file_size;

	Res = 1;
	
	if (fseek(MR_file(*(MercuryFile *)Stream), 0, SEEK_END) != 0)
	    goto read_stream_error;

	file_size = ftell(MR_file(*(MercuryFile *)Stream));

	if (file_size < 0) goto read_stream_error;
	    
	if (fseek(MR_file(*(MercuryFile *)Stream), 0, SEEK_SET) != 0)
	    goto read_stream_error;
	
	new_bytes(file_size, &Bytes, MR_ALLOC_ID);

	if (fread(Bytes->bytes, 1, file_size, MR_file(*(MercuryFile *)Stream)) < 0)
	    goto read_stream_error;

	Res = 0;

    read_stream_error:
	
	IO = IO0;
    }
").

%--------------------------------------------------------------------%

:- end_module bytes.

