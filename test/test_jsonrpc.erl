-module(test_jsonrpc).

-include("rfc4627.hrl").
-include("mod_jsonrpc.hrl").

-behaviour(gen_server).

-export([start/0, start_httpd/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    mod_jsonrpc:register_service
      (Pid,
       #service{name = <<"test">>,
		id = <<"urn:dummy:id">>,
		procs = [#service_proc{name = <<"test_proc">>,
				       idempotent = true,
				       params = [#service_proc_param{name = <<"value">>,
								     type = <<"str">>}]}]}).

start_httpd() ->
    httpd:start("test/server_root/conf/httpd.conf"),
    mod_jsonrpc:start(),
    start().

%---------------------------------------------------------------------------

init(_Args) ->
    {ok, no_state}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call({jsonrpc, <<"test_proc">>, _ModData, [Value]}, _From, State) ->
    {reply, {result, <<"ErlangServer: ", Value/binary>>}, State}.

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in test_jsonrpc: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in test_jsonrpc: ~p", [Info]),
    {noreply, State}.
