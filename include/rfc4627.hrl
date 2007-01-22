%% JSON - RFC 4627 - for Erlang
%% Copyright (c) 2007 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% Copyright (c) 2007 LShift Ltd. <query@lshift.net>
%%
%% Convenience macros for encoding and decoding record structures.
%%
%% Erlang's compile-time-only notion of record definitions means we
%% have to supply a constant record name in the source text.

-define(RFC4627_FROM_RECORD(RName, R),
	rfc4627:from_record(R, RName, record_info(fields, RName))).

-define(RFC4627_TO_RECORD(RName, R),
	rfc4627:to_record(R, #RName{}, record_info(fields, RName))).
