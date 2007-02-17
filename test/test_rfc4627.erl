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
