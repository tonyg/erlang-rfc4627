-module(rfc4627_jsonrpc).
-include("rfc4627.hrl").
-include("rfc4627_jsonrpc.hrl").

-behaviour(gen_server).

-export([start/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export([rpc/3, register_service/2, error_response/2]).

start() ->
    gen_server:start({local, rfc4627_jsonrpc}, ?MODULE, [], []).

rpc(RequestID, Env, Input) ->
    error_logger:info_msg("RPC hit ~p", [{RequestID, Env, Input}]),
    {PostOrGet, Id, Service, Method, Args} = parse_jsonrpc(Env, Input),
    {Headers, ResultField} =
	case gen_server:call(rfc4627_jsonrpc, {rpc, PostOrGet, Env, Service, Method, Args}) of
	    {Kind, Value} -> {[], {Kind, Value}};
	    {Kind, Value, Headers0} -> {Headers0, {Kind, Value}}
	end,
    Result = {obj, [{version, <<"1.1">>},
		    {id, Id},
		    ResultField]},
    ResultEnc = rfc4627:encode(Result),
    send_headers(RequestID, [{"Content-type", rfc4627:mime_type()} | Headers]),
    mod_esi:deliver(RequestID, ResultEnc).

register_service(Pid, ServiceDescription) ->
    gen_server:call(rfc4627_jsonrpc, {register_service, Pid, ServiceDescription}).

error_response(Code, ErrorValue) when is_integer(Code) ->
    build_error_response(Code, "Error "++integer_to_list(Code), ErrorValue);
error_response(Message, ErrorValue) when is_list(Message) ->
    build_error_response(500, Message, ErrorValue);
error_response({Code, Message}, ErrorValue) ->
    build_error_response(Code, Message, ErrorValue).

build_error_response(Code, Message, ErrorValue) ->
    {error, {obj, [{"name", <<"JSONRPCError">>},
		   {"code", Code},
		   {"message", Message},
		   {"error", ErrorValue}]}}.

extract_object_method_and_params(Input) ->
    {ObjectMethod, Params} = case string:tokens(Input, "?") of
				 [] -> {"", ""};
				 [Method] -> {Method, ""};
				 [Method, QueryStr] -> {Method, QueryStr}
			     end,
    case lists:reverse(string:tokens(ObjectMethod, "/")) of
	[] -> {"", "", Params};
	[Object] -> {Object, "", Params};
	[Method1, Object | _] -> {Object, Method1, Params}
    end.

extract_object_from_post(Input) ->
    case lists:reverse(string:tokens(Input, "/")) of
	[] -> "";
	[Object | _] -> Object
    end.

parse_jsonrpc(Env, Input) ->
    case httpd_util:key1search(Env, request_method) of
	"POST" ->
	    Object = extract_object_from_post(httpd_util:key1search(Env, query_string)),
	    {ok, {obj, Fields}, _} = rfc4627:decode(Input),
	    {post,
	     httpd_util:key1search(Fields, "id"),
	     Object,
	     httpd_util:key1search(Fields, "method"),
	     httpd_util:key1search(Fields, "params")};
	%% GET, presumably. We don't really care, here.
	_ ->
	    {Object, Method, QueryStr} = extract_object_method_and_params(Input),
	    Q = case httpd:parse_query(QueryStr) of
		    [{"",""}] -> []; %% is this a bug in httpd?
		    ActualParameters -> ActualParameters
		end,
	    {get,
	     null,
	     Object,
	     Method,
	     %% FIXME: need to collect duplicate parameter keys into a list, as per spec
	     {obj, lists:map(fun ({Key, ValStr}) ->
				     {Key, list_to_binary(ValStr)}
			     end, Q)}}
    end.

send_headers(RequestID, Headers) ->
    mod_esi:deliver(RequestID, encode_headers(["\r\n"], Headers)).

encode_headers(Acc, []) ->
    Acc;
encode_headers(Acc, [{Key, Value} | Headers]) when is_atom(Key) ->
    encode_headers([atom_to_list(Key), ": ", Value, "\r\n" | Acc], Headers);
encode_headers(Acc, [{Key, Value} | Headers]) ->
    encode_headers([Key, ": ", Value, "\r\n" | Acc], Headers).

%---------------------------------------------------------------------------

lookup_service_proc(#service{procs = Procs}, Method) ->
    case lists:keysearch(Method, #service_proc.name, Procs) of
	{value, ServiceProc} ->
	    {ok, ServiceProc};
	false ->
	    not_found
    end.

do_rpc(get, Pid, Env, ServiceProc, Args) ->
    if
	ServiceProc#service_proc.idempotent ->
	    do_rpc1(Pid, Env, ServiceProc, Args);
	true ->
	    error_response({403, "Non-idempotent method"}, ServiceProc#service_proc.name)
    end;
do_rpc(post, Pid, Env, ServiceProc, Args) ->
    do_rpc1(Pid, Env, ServiceProc, Args).

do_rpc1(Pid, Env, #service_proc{name = Name, params = Params}, Args) ->
    gen_server:call(Pid, {jsonrpc, Name, Env, coerce_args(Params, Args)}).

coerce_args(_Params, Args) when is_list(Args) ->
    Args;
coerce_args(Params, {obj, Fields}) ->
    lists:map(fun (#service_proc_param{name = Name}) ->
		      case lists:keysearch(Name, 1, Fields) of
			  {value, {_, Value}} ->
			      Value;
			  false ->
			      null
		      end
	      end, Params).

remove_undefined({obj, Fields}) ->
    {obj, remove_undefined1(Fields)}.

remove_undefined1([{_, undefined} | Rest]) ->
    remove_undefined1(Rest);
remove_undefined1([X | Rest]) ->
    [X | remove_undefined1(Rest)].

system_describe(ScriptName,
		#service{name = Name, id = Id, version = Version, summary = Summary,
			 help = Help, procs = Procs}) ->
    remove_undefined({obj, [{"sdversion", <<"1.0">>},
			    {"name", Name},
			    {"id", Id},
			    {"version", Version},
			    {"summary", Summary},
			    {"help", Help},
			    {"address", list_to_binary(ScriptName)},
			    {"procs", lists:map(fun system_describe_proc/1, Procs)}]}).

system_describe_proc(P = #service_proc{params = Params}) ->
    remove_undefined(?RFC4627_FROM_RECORD(service_proc,
					  P#service_proc{params =
							 lists:map(fun system_describe_proc_param/1,
								   Params)})).

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

handle_call({rpc, PostOrGet, Env, Service, Method, Args}, _From, State) ->
    case get({service, Service}) of
	undefined ->
	    {reply, error_response({404, "Service not found"}, Service), State};
	ServiceRec ->
	    case Method of
		<<"system.describe">> ->
		    {reply, system_describe(httpd_util:key1search(Env, script_name),
					    ServiceRec), State};
		<<"system.", _Rest/binary>> ->
		    {reply, error_response({403, "System methods forbidden"}, Method), State};
		_ ->
		    case lookup_service_proc(ServiceRec, Method) of
			{ok, ServiceProc} ->
			    {reply,
			     do_rpc(PostOrGet, ServiceRec#service.pid, Env, ServiceProc, Args),
			     State};
			not_found ->
			    {reply,
			     error_response({404, "Procedure not found"}, [Service, Method]),
			     State}
		    end
	    end
    end;

handle_call({register_service, Pid, ServiceDescription}, _From, State) ->
    SD = ServiceDescription#service{pid = Pid},
    erlang:monitor(process, Pid),
    put({service_pid, Pid}, SD#service.name),
    put({service, SD#service.name}, SD),
    {reply, ok, State}.

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in rfc4627_jsonrpc: ~p", [Request]),
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
