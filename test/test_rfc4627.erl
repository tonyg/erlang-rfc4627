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
