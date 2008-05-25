%% JSON-RPC, transport-neutral.
%%---------------------------------------------------------------------------
%% Copyright (c) 2007, 2008 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% Copyright (c) 2007, 2008 LShift Ltd. <query@lshift.net>
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

-module(rfc4627_jsonrpc).
-include("rfc4627.hrl").
-include("rfc4627_jsonrpc.hrl").

-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-export([lookup_service/1, register_service/2]).
-export([gen_object_name/0, system_describe/2]).
-export([jsonrpc_post/3, jsonrpc_post/4, invoke_service_method/8, expand_jsonrpc_reply/2]).
-export([error_response/2, error_response/3, service/4, service/5, proc/2]).

-define(SERVICE, ?MODULE).

start() ->
    gen_server:start({local, ?SERVICE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVICE}, ?MODULE, [], []).

lookup_service(Service) ->
    gen_server:call(?SERVICE, {lookup_service, Service}).

register_service(Pid, ServiceDescription) ->
    %%error_logger:info_msg("Registering ~p as ~p", [Pid, ServiceDescription]),
    gen_server:call(?SERVICE, {register_service, Pid, ServiceDescription}).

gen_object_name() ->
    Hash = erlang:md5(term_to_binary({node(), erlang:now()})),
    binary_to_hex(Hash).

system_describe(EndpointAddress,
		#service{name = Name, id = Id, version = Version, summary = Summary,
			 help = Help, procs = Procs}) ->
    remove_undefined({obj, [{"sdversion", <<"1.0">>},
			    {"name", Name},
			    {"id", Id},
			    {"version", Version},
			    {"summary", Summary},
			    {"help", Help},
			    {"address", EndpointAddress},
			    {"procs", [system_describe_proc(P) || P <- Procs]}]}).

jsonrpc_post(ServiceRec, RequestInfo, RequestObj) ->
    jsonrpc_post(ServiceRec, RequestInfo, RequestObj, default).

jsonrpc_post(ServiceRec, RequestInfo, RequestObj, Timeout) ->
    Id = rfc4627:get_field(RequestObj, "id", undefined),
    Method = rfc4627:get_field(RequestObj, "method", undefined),
    Args = rfc4627:get_field(RequestObj, "params", undefined),
    invoke_service_method(ServiceRec, Id, post, RequestInfo, undefined, Method, Args, Timeout).

invoke_service_method(ServiceRec = #service{}, RequestId,
		      PostOrGet, RequestInfo, EndpointAddress, Method, Args, Timeout) ->
    expand_jsonrpc_reply(
      RequestId,
      case Method of
	  <<"system.describe">> ->
	      {result, system_describe(EndpointAddress, ServiceRec)};
	  <<"system.", _Rest/binary>> ->
	      error_response(403, "System methods forbidden", Method);
	  _ ->
	      case lookup_service_proc(ServiceRec, Method) of
		  {ok, ServiceProc} ->
		      invoke_service(PostOrGet, ServiceRec#service.handler,
				     RequestInfo, ServiceProc, Args, Timeout);
		  not_found ->
		      error_response(404, "Procedure not found", [EndpointAddress, Method])
	      end
      end).

error_response(Code, ErrorValue) when is_integer(Code) ->
    error_response(Code, "Error "++integer_to_list(Code), ErrorValue);
error_response(Message, ErrorValue) when is_list(Message) ->
    error_response(500, list_to_binary(Message), ErrorValue);
error_response(Message, ErrorValue) when is_binary(Message) ->
    error_response(500, Message, ErrorValue).

error_response(Code, Message, ErrorValue) when is_list(Message) ->
    error_response(Code, list_to_binary(Message), ErrorValue);
error_response(Code, Message, ErrorValue) ->
    {error, {obj, [{"name", <<"JSONRPCError">>},
		   {"code", Code},
		   {"message", Message},
		   {"error", ErrorValue}]}}.

service(Name, Id, Version, Procs) when is_list(Name) ->
    service(list_to_binary(Name), Id, Version, Procs);
service(Name, Id, Version, Procs) when is_list(Id) ->
    service(Name, list_to_binary(Id), Version, Procs);
service(Name, Id, Version, Procs) when is_list(Version) ->
    service(Name, Id, list_to_binary(Version), Procs);
service(Name, Id, Version, Procs) ->
    #service{name = Name, id = Id, version = Version,
	     procs = [case P of
			  {ProcName, Params} -> proc(ProcName, Params);
			  #service_proc{} -> P
		      end || P <- Procs]}.

service(Handler, Name, Id, Version, Procs) ->
    (service(Name, Id, Version, Procs))#service{handler = Handler}.

proc(Name, Params) when is_list(Name) ->
    proc(list_to_binary(Name), Params);
proc(Name, Params) ->
    #service_proc{name = Name, params = [proc_param(P) || P <- Params]}.

%---------------------------------------------------------------------------

build_jsonrpc_response(Id, ResultField) ->
    {obj, [{version, <<"1.1">>},
	   {id, Id},
	   ResultField]}.

expand_jsonrpc_reply(RequestId, {ResultOrError, Value}) ->
    {ResultOrError, build_jsonrpc_response(RequestId, {ResultOrError, Value}), {obj, []}};
expand_jsonrpc_reply(RequestId, {ResultOrError, Value, ResponseInfo}) ->
    {ResultOrError, build_jsonrpc_response(RequestId, {ResultOrError, Value}), ResponseInfo}.

proc_param({N, T}) when is_list(N) ->
    proc_param({list_to_binary(N), T});
proc_param({N, T}) ->
    #service_proc_param{name = N, type = proc_param_type(T)}.

proc_param_type(bit) -> <<"bit">>;
proc_param_type(num) -> <<"num">>;
proc_param_type(str) -> <<"str">>;
proc_param_type(arr) -> <<"arr">>;
proc_param_type(obj) -> <<"obj">>;
proc_param_type(any) -> <<"any">>;
proc_param_type(nil) -> <<"nil">>;
proc_param_type(T) when is_list(T) -> list_to_binary(T);
proc_param_type(T) when is_binary(T) -> T.

binary_to_hex(<<>>) ->
    [];
binary_to_hex(<<B, Rest/binary>>) ->
    [rfc4627:hex_digit((B bsr 4) band 15),
     rfc4627:hex_digit(B band 15) |
     binary_to_hex(Rest)].

lookup_service_proc(#service{procs = Procs}, Method) ->
    case lists:keysearch(Method, #service_proc.name, Procs) of
	{value, ServiceProc} ->
	    {ok, ServiceProc};
	false ->
	    not_found
    end.

invoke_service(get, Handler, RequestInfo, ServiceProc, Args, Timeout) ->
    if
	ServiceProc#service_proc.idempotent ->
	    invoke_service1(Handler, RequestInfo, ServiceProc, Args, Timeout);
	true ->
	    error_response(403, "Non-idempotent method", ServiceProc#service_proc.name)
    end;
invoke_service(post, Handler, RequestInfo, ServiceProc, Args, Timeout) ->
    invoke_service1(Handler, RequestInfo, ServiceProc, Args, Timeout).

invoke_service1(Handler, RequestInfo, #service_proc{name = Name, params = Params}, Args, Timeout) ->
    %%error_logger:info_msg("JSONRPC invoking ~p:~p(~p)", [Handler, Name, Args]),
    case catch run_handler(Handler, Name, RequestInfo, coerce_args(Params, Args), Timeout) of
	{'EXIT', {{function_clause, _}, _}} ->
	    error_response(404, "Undefined procedure", Name);
	{'EXIT', Reason} ->
	    error_response(500, "Internal error", list_to_binary(io_lib:format("~p", [Reason])));
	Response ->
	    Response
    end.

run_handler({pid, Pid}, Name, RequestInfo, CoercedArgs, default) ->
    gen_server:call(Pid, {jsonrpc, Name, RequestInfo, CoercedArgs});
run_handler({pid, Pid}, Name, RequestInfo, CoercedArgs, Timeout) ->
    gen_server:call(Pid, {jsonrpc, Name, RequestInfo, CoercedArgs}, Timeout);
run_handler({function, F}, Name, RequestInfo, CoercedArgs, _Timeout) ->
    F(Name, RequestInfo, CoercedArgs).

coerce_args(_Params, Args) when is_list(Args) ->
    Args;
coerce_args(Params, {obj, Fields}) ->
    [case lists:keysearch(binary_to_list(Name), 1, Fields) of
	 {value, {_, Value}} -> coerce_value(Value, Type);
	 false -> null
     end || #service_proc_param{name = Name, type = Type} <- Params].

coerce_value(Value, _Type) when not(is_binary(Value)) ->
    Value;
coerce_value(<<"true">>, <<"bit">>) -> true;
coerce_value(_, <<"bit">>) -> false;
coerce_value(V, <<"num">>) -> list_to_integer(binary_to_list(V));
coerce_value(V, <<"str">>) -> V;
coerce_value(V, <<"arr">>) -> rfc4627:decode(V);
coerce_value(V, <<"obj">>) -> rfc4627:decode(V);
coerce_value(V, <<"any">>) -> V;
coerce_value(_, <<"nil">>) -> null;
coerce_value(V, _) -> V.

remove_undefined({obj, Fields}) ->
    {obj, remove_undefined1(Fields)}.

remove_undefined1([]) ->
    [];
remove_undefined1([{_, undefined} | Rest]) ->
    remove_undefined1(Rest);
remove_undefined1([X | Rest]) ->
    [X | remove_undefined1(Rest)].

system_describe_proc(P = #service_proc{params = Params}) ->
    remove_undefined(?RFC4627_FROM_RECORD(service_proc,
					  P#service_proc{params = [system_describe_proc_param(A)
								   || A <- Params]})).

system_describe_proc_param(P = #service_proc_param{}) ->
    remove_undefined(?RFC4627_FROM_RECORD(service_proc_param, P)).

%---------------------------------------------------------------------------

init(_Args) ->
    {ok, no_jsonrpc_state}.

terminate(_Reason, _State) ->
    %% FIXME: should we notify services here?
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call({lookup_service, Service}, _From, State) ->
    case get({service, Service}) of
	undefined ->
	    {reply, not_found, State};
	ServiceRec ->
	    {reply, ServiceRec, State}
    end;

handle_call({register_service, Pid, ServiceDescription}, _From, State) ->
    SD = ServiceDescription#service{handler = {pid, Pid}},
    erlang:monitor(process, Pid),
    put({service_pid, Pid}, SD#service.name),
    put({service, SD#service.name}, SD),
    {reply, ok, State}.

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p", [?MODULE, Request]),
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, DownPid, _Reason}, State) ->
    case get({service_pid, DownPid}) of
	undefined ->
	    %% How strange.
	    {noreply, State};
	ServiceName ->
	    erase({service_pid, DownPid}),
	    erase({service, ServiceName}),
	    {noreply, State}
    end.
