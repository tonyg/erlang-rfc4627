%% Basic JSON (RFC-4627) codec tests
%%---------------------------------------------------------------------------
%% Copyright (c) 2007 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% Copyright (c) 2007 LShift Ltd. <query@lshift.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------

-module(test_rfc4627).
-include("rfc4627.hrl").

-export([test_all/0]).

-record(address, {number, street, town, country = <<"England">>}).

test_all() ->
    passed = test_codec(),
    passed = test_records(),
    passed = test_unicode(),
    passed.

test_codec() ->
    Cases = [
	     {1, "1"},
	     {1, "1,", false},
	     {1.0, "1.0"},
	     {12.3, "1.23e1"},
	     {<<"hi">>, "\"hi\""},
	     {<<>>, "\"\""},
	     {[1, 2], "[1, 2]"},
	     {[], "[]"},
	     {{obj, [{"a", 1}, {"b", 2}]}, "{\"a\": 1, \"b\": 2}"},
	     {{obj, []}, "{}"},
	     {{obj, []}, "{    \n\n  }"},
	     {true, "true"},
	     {null, "null"},
	     {false, "false"}
	     ],
    lists:foreach(fun test_codec/1, Cases),
    passed.

test_codec({Erl, Json}) ->
    test_codec({Erl, Json, true});
test_codec({Erl, Json, EofExpected}) ->
    %% We can test Erl -> Json -> Erl, but not Json -> Erl ->
    %% Json. However, we can test Json -> Erl.
    {ok, Erl, []} = rfc4627:decode(rfc4627:encode(Erl)),
    {ok, Erl, Rest} = rfc4627:decode(Json),
    {at_eof, EofExpected} = {at_eof, (Rest == [])},
    passed.

%% UTF tests.
test_unicode() ->
    passed = test_unicode_encodings(),
    passed = test_unicode_json(),
    passed.

test_unicode_encodings() ->
    ZWaterBass = [16#0000007A, 16#00006C34, 16#0001D11E],
    U32B = [0, 0, 16#00, 16#7A, 0, 0, 16#6C, 16#34, 0, 1, 16#D1, 16#1E]
	= rfc4627:unicode_encode({'utf-32be', ZWaterBass}),
    U32L = [16#7A, 0, 0, 0, 16#34, 16#6C, 0, 0, 16#1E, 16#D1, 1, 0]
	= rfc4627:unicode_encode({'utf-32le', ZWaterBass}),
    U32B_BOM = [0, 0, 16#FE, 16#FF, 0, 0, 16#00, 16#7A, 0, 0, 16#6C, 16#34, 0, 1, 16#D1, 16#1E]
	= rfc4627:unicode_encode({'utf-32', ZWaterBass}),
    U16L = [16#7A, 16#00, 16#34, 16#6C, 16#34, 16#D8, 16#1E, 16#DD]
	= rfc4627:unicode_encode({'utf-16le', ZWaterBass}),
    U16B = [16#00, 16#7A, 16#6C, 16#34, 16#D8, 16#34, 16#DD, 16#1E]
	= rfc4627:unicode_encode({'utf-16be', ZWaterBass}),
    U16B_BOM = [16#FE, 16#FF, 16#00, 16#7A, 16#6C, 16#34, 16#D8, 16#34, 16#DD, 16#1E]
	= rfc4627:unicode_encode({'utf-16', ZWaterBass}),
    U8 = [16#7A, 16#E6,16#B0,16#B4, 16#F0,16#9D,16#84,16#9E]
	= rfc4627:unicode_encode({'utf-8', ZWaterBass}),
    {'utf-32be', ZWaterBass} = rfc4627:unicode_decode(U32B),
    {'utf-32le', ZWaterBass} = rfc4627:unicode_decode(U32L),
    {'utf-32', ZWaterBass} = rfc4627:unicode_decode(U32B_BOM),
    {'utf-16be', ZWaterBass} = rfc4627:unicode_decode(U16B),
    {'utf-16le', ZWaterBass} = rfc4627:unicode_decode(U16L),
    {'utf-16', ZWaterBass} = rfc4627:unicode_decode(U16B_BOM),
    {'utf-8', ZWaterBass} = rfc4627:unicode_decode(U8),
    {'utf-8', ZWaterBass} = rfc4627:unicode_decode([16#EF,16#BB,16#BF]++U8),
    passed.

test_unicode_json() ->
    rfc4627:decode("\"" ++ [16#7A, 16#E6,16#B0,16#B4, 16#F0,16#9D,16#84,16#9E] ++ "\""),
    passed.

test_records() ->
    A = #address{number = 6, street = <<"Rufus Street">>, town = <<"London">>},
    AEnc = {obj, [{"number", 6},
		  {"street", <<"Rufus Street">>},
		  {"town", <<"London">>},
		  {"country", <<"England">>}]} = ?RFC4627_FROM_RECORD(address, A),
    A = ?RFC4627_TO_RECORD(address, {obj, [{"number", 6},
					   {"street", <<"Rufus Street">>},
					   {"town", <<"London">>}]}),
    {ok, AEnc, []} = rfc4627:decode(rfc4627:encode(AEnc)),
    passed.
