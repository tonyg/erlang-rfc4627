%% JSON - RFC 4627 - for Erlang
%% Copyright (c) 2007 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% Copyright (c) 2007 LShift Ltd. <query@lshift.net>
%%
%% Records for JSON-RPC inets services using the rfc4627_jsonrpc module.

-record(service, {handler, name, id, version, summary, help, procs}).
-record(service_proc, {name, summary, help, idempotent = false, params, return}).
-record(service_proc_param, {name, type}).
