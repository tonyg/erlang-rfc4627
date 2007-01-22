-module(mod_jsonrpc).
-include("rfc4627.hrl").
-include("mod_jsonrpc.hrl").
-include_lib("inets/src/httpd.hrl").

-behaviour(gen_server).

-export([start/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export([do/1, load/2]).
-export([register_service/2, error_response/2, error_response/3]).

start() ->
    gen_server:start({local, mod_jsonrpc}, ?MODULE, [], []).

do(ModData = #mod{data = OldData}) ->
    case {httpd_util:key1search(OldData, status),
	  httpd_util:key1search(OldData, response)} of
	{undefined, undefined} ->
	    do_rpc(ModData);
	_ ->
	    {proceed, OldData}
    end.

load("JsonRpcAlias " ++ Alias, []) ->
    {ok, [], {json_rpc_alias, Alias}}.

do_rpc(ModData = #mod{data = OldData}) ->
    case extract_object_method_and_params(ModData) of
	no_match ->
	    {proceed, OldData};
	UriInfo = {_Object, _UriMethod, _UriParamStr} ->
	    {PostOrGet, Id, Service, Method, Args} = parse_jsonrpc(ModData, UriInfo),
	    {Headers, ResultField} =
		case gen_server:call(mod_jsonrpc,
				     {rpc, PostOrGet, ModData, Service, Method, Args}) of
		    {result, Value} -> {[], {result, Value}};
		    {error, Value} -> {[], {error, Value}};
		    {result, Value, Headers0} -> {Headers0, {result, Value}};
		    {error, Value, Headers0} -> {Headers0, {error, Value}}
		end,
	    Result = {obj, [{version, <<"1.1">>},
			    {id, Id},
			    ResultField]},
	    ResultEnc = lists:flatten(rfc4627:encode(Result)),
	    {proceed, [{response, {response,
				   [{code, 200},
				    {content_length, integer_to_list(length(ResultEnc))},
				    {content_type, "text/plain"}%rfc4627:mime_type()}
				    | Headers],
				   ResultEnc}} | OldData]}
    end.

register_service(Pid, ServiceDescription) ->
    gen_server:call(mod_jsonrpc, {register_service, Pid, ServiceDescription}).

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

%---------------------------------------------------------------------------

extract_object_method_and_params(#mod{config_db = ConfigDb, request_uri = Uri}) ->
    AliasPrefix = httpd_util:lookup(ConfigDb, json_rpc_alias, "/jsonrpc"),
    AliasPrefixRe = "^" ++ AliasPrefix ++ "/",
    case regexp:first_match(Uri, AliasPrefixRe) of
	{match, 1, Length} ->
	    Tail = string:substr(Uri, Length + 1),
	    {ObjectMethod, Params} = case string:tokens(Tail, "?") of
					 [] -> {"", ""};
					 [Method] -> {Method, ""};
					 [Method, QueryStr] -> {Method, QueryStr}
				     end,
	    case lists:reverse(string:tokens(ObjectMethod, "/")) of
		[] -> {"", "", Params};
		[Object] -> {list_to_binary(Object), <<>>, Params};
		[Method1, Object | _] -> {list_to_binary(Object), list_to_binary(Method1), Params}
	    end;
	nomatch ->
	    no_match
    end.

parse_jsonrpc(#mod{method = "POST", entity_body = Body},
	      {Object, _UriMethod, _UriParams}) ->
    {ok, {obj, Fields}, _} = rfc4627:decode(Body),
    {post,
     httpd_util:key1search(Fields, "id"),
     Object,
     httpd_util:key1search(Fields, "method"),
     httpd_util:key1search(Fields, "params")};
parse_jsonrpc(#mod{method = _},
	     {Object, Method, QueryStr}) ->
    %% GET, presumably. We don't really care, here.
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
		     end, Q)}}.

lookup_service_proc(#service{procs = Procs}, Method) ->
    case lists:keysearch(Method, #service_proc.name, Procs) of
	{value, ServiceProc} ->
	    {ok, ServiceProc};
	false ->
	    not_found
    end.

do_rpc(get, Pid, ModData, ServiceProc, Args) ->
    if
	ServiceProc#service_proc.idempotent ->
	    do_rpc1(Pid, ModData, ServiceProc, Args);
	true ->
	    error_response(403, "Non-idempotent method", ServiceProc#service_proc.name)
    end;
do_rpc(post, Pid, ModData, ServiceProc, Args) ->
    do_rpc1(Pid, ModData, ServiceProc, Args).

do_rpc1(Pid, ModData, #service_proc{name = Name, params = Params}, Args) ->
    case catch gen_server:call(Pid, {jsonrpc, Name, ModData, coerce_args(Params, Args)}) of
	{'EXIT', {{function_clause, _}, _}} ->
	    error_response(404, "Undefined procedure", Name);
	{'EXIT', Reason} ->
	    error_response(500, "Internal error", list_to_binary(io_lib:format("~p", [Reason])));
	Response ->
	    Response
    end.

coerce_args(_Params, Args) when is_list(Args) ->
    Args;
coerce_args(Params, {obj, Fields}) ->
    lists:map(fun (#service_proc_param{name = Name}) ->
		      case lists:keysearch(binary_to_list(Name), 1, Fields) of
			  {value, {_, Value}} ->
			      Value;
			  false ->
			      null
		      end
	      end, Params).

remove_undefined({obj, Fields}) ->
    {obj, remove_undefined1(Fields)}.

remove_undefined1([]) ->
    [];
remove_undefined1([{_, undefined} | Rest]) ->
    remove_undefined1(Rest);
remove_undefined1([X | Rest]) ->
    [X | remove_undefined1(Rest)].

system_describe(ScriptName,
		#service{name = Name, id = Id, version = Version, summary = Summary,
			 help = Help, procs = Procs}) ->
    {result, remove_undefined({obj, [{"sdversion", <<"1.0">>},
				     {"name", Name},
				     {"id", Id},
				     {"version", Version},
				     {"summary", Summary},
				     {"help", Help},
				     {"address", list_to_binary(ScriptName)},
				     {"procs", lists:map(fun system_describe_proc/1, Procs)}]})}.

system_describe_proc(P = #service_proc{params = Params}) ->
    remove_undefined(?RFC4627_FROM_RECORD(service_proc,
					  P#service_proc{params =
							 lists:map(fun system_describe_proc_param/1,
								   Params)})).

system_describe_proc_param(P = #service_proc_param{}) ->
    remove_undefined(?RFC4627_FROM_RECORD(service_proc_param, P)).

compute_script_url(#mod{request_uri = Uri}) ->
    case string:tokens(Uri, "?") of
	[] -> "";
	[Prefix | _Rest] -> Prefix
    end.

%---------------------------------------------------------------------------

init(_Args) ->
    {ok, no_jsonrpc_state}.

terminate(_Reason, _State) ->
    %% FIXME: should we notify services here?
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call({rpc, PostOrGet, ModData, Service, Method, Args}, _From, State) ->
    case get({service, Service}) of
	undefined ->
	    {reply, error_response(404, "Service not found", Service), State};
	ServiceRec ->
	    case Method of
		<<"system.describe">> ->
		    {reply, system_describe(compute_script_url(ModData), ServiceRec), State};
		<<"system.", _Rest/binary>> ->
		    {reply, error_response(403, "System methods forbidden", Method), State};
		_ ->
		    case lookup_service_proc(ServiceRec, Method) of
			{ok, ServiceProc} ->
			    {reply,
			     do_rpc(PostOrGet, ServiceRec#service.pid, ModData, ServiceProc, Args),
			     State};
			not_found ->
			    {reply,
			     error_response(404, "Procedure not found", [Service, Method]),
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
    error_logger:error_msg("Unhandled cast in mod_jsonrpc: ~p", [Request]),
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
