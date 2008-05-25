%% JSON-RPC for Erlang's inets httpd
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

-module(rfc4627_jsonrpc_inets).
-include("rfc4627_jsonrpc.hrl").
-include_lib("inets/src/httpd.hrl").

-export([do/1, load/2]).

do(ModData = #mod{data = OldData}) ->
    case {proplists:get_value(status, OldData),
	  proplists:get_value(response, OldData)} of
	{undefined, undefined} ->
	    do_rpc(ModData);
	_ ->
	    {proceed, OldData}
    end.

load("JsonRpcAlias " ++ Alias, []) ->
    {ok, [], {json_rpc_alias, Alias}}.

do_rpc(#mod{init_data = #init_data{peername = {PeerPort, PeerName}},
	    config_db = ConfigDb,
	    socket_type = SocketType,
	    method = HttpMethod,
	    request_uri = PathAndQuery,
	    parsed_header = InetsHeaders,
	    entity_body = Body,
	    data = OldData}) ->
    AliasPrefix = httpd_util:lookup(ConfigDb, json_rpc_alias, default),
    {Path, QueryObj} = case string:tokens(PathAndQuery, "?") of
			   [] -> {"", {obj, []}};
			   [P] -> {P, {obj, []}};
			   [P, Q] -> {P, {obj, case httpd:parse_query(Q) of
						   [{"",""}] -> []; %% is this a bug in httpd?
						   KV -> [{K, list_to_binary(V)} || {K,V} <- KV]
					       end}}
		       end,
    HeaderObj = {obj, [{K, list_to_binary(V)} || {K,V} <- InetsHeaders]},
    SchemeFields = case SocketType of
		       ip_comm -> [{"scheme", <<"http">>}];
		       ssl -> [{"scheme", <<"https">>}];
		       _ -> []
		   end,

    RequestInfo = {obj, ([{"http_method", list_to_binary(HttpMethod)},
			  {"http_query_parameters", QueryObj},
			  {"http_headers", HeaderObj},
			  {"remote_port", PeerPort},
			  {"remote_peername", list_to_binary(PeerName)}]
			 ++ SchemeFields)},

    case rfc4627_jsonrpc_http:invoke_service_method(AliasPrefix,
						    Path,
						    RequestInfo,
						    Body) of
	no_match ->
	    {proceed, OldData};
	{ok, ResultEnc, ResponseInfo} ->
	    {obj, ResponseHeaderFields} =
		rfc4627:get_field(ResponseInfo, "http_headers", {obj, []}),
	    Headers = [{K, binary_to_list(V)} || {K,V} <- ResponseHeaderFields],
	    {proceed, [{response, {response,
				   [{code, 200},
				    {content_length, integer_to_list(length(ResultEnc))},
				    {content_type, "text/plain"}%rfc4627:mime_type()}
				    | Headers],
				   ResultEnc}} | OldData]}
    end.
