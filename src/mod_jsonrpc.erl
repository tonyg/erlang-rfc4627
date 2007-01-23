-module(mod_jsonrpc).
-include("rfc4627.hrl").
-include("mod_jsonrpc.hrl").
-include_lib("inets/src/httpd.hrl").

-behaviour(gen_server).

-export([start/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export([do/1, load/2]).
-export([register_service/2, error_response/2, error_response/3, service/4, service/5, proc/2]).
-export([gen_object_name/0, service_address/2, system_describe/2]).
-export([jsonrpc_post/3, invoke_service_method/5]).

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

jsonrpc_post(ServiceRec, ModData, {obj, Fields}) ->
    Id = httpd_util:key1search(Fields, "id"),
    Method = httpd_util:key1search(Fields, "method"),
    Args = httpd_util:key1search(Fields, "params"),
    {Headers, ResultField} =
	expand_jsonrpc_reply(invoke_service_method(ServiceRec, post, ModData, Method, Args)),
    {Headers, build_jsonrpc_response(Id, ResultField)}.

build_jsonrpc_response(Id, ResultField) ->
    {obj, [{version, <<"1.1">>},
	   {id, Id},
	   ResultField]}.

do_rpc(ModData = #mod{data = OldData}) ->
    case extract_object_method_and_params(ModData) of
	no_match ->
	    {proceed, OldData};
	UriInfo = {_Object, _UriMethod, _UriParamStr} ->
	    {PostOrGet, Id, Service, Method, Args} = parse_jsonrpc(ModData, UriInfo),
	    {Headers, ResultField} =
		expand_jsonrpc_reply(
		  case gen_server:call(mod_jsonrpc, {lookup_service, Service}) of
		      not_found ->
			  error_response(404, "Service not found", Service);
		      ServiceRec ->
			  invoke_service_method(ServiceRec, PostOrGet, ModData, Method, Args)
		  end),
	    Result = build_jsonrpc_response(Id, ResultField),
	    ResultEnc = lists:flatten(rfc4627:encode(Result)),
	    {proceed, [{response, {response,
				   [{code, 200},
				    {content_length, integer_to_list(length(ResultEnc))},
				    {content_type, "text/plain"}%rfc4627:mime_type()}
				    | Headers],
				   ResultEnc}} | OldData]}
    end.

invoke_service_method(ServiceRec = #service{name = ServiceName},
		      PostOrGet, ModData, Method, Args) ->
    case Method of
	<<"system.describe">> ->
	    {result, system_describe(service_address(ModData, ServiceName), ServiceRec)};
	<<"system.", _Rest/binary>> ->
	    error_response(403, "System methods forbidden", Method);
	_ ->
	    case lookup_service_proc(ServiceRec, Method) of
		{ok, ServiceProc} ->
		    invoke_service(PostOrGet, ServiceRec#service.handler,
				   ModData, ServiceProc, Args);
		not_found ->
		    error_response(404, "Procedure not found", [ServiceName, Method])
	    end
    end.

expand_jsonrpc_reply(Reply = {result, _Value}) -> {[], Reply};
expand_jsonrpc_reply(Reply = {error, _Value}) -> {[], Reply};
expand_jsonrpc_reply({result, Value, Headers0}) -> {Headers0, {result, Value}};
expand_jsonrpc_reply({error, Value, Headers0}) -> {Headers0, {error, Value}}.

register_service(Pid, ServiceDescription) ->
    %%error_logger:info_msg("Registering ~p as ~p", [Pid, ServiceDescription]),
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

service(Handler, Name, Id, Version, Procs) ->
    (service(Name, Id, Version, Procs))#service{handler = Handler}.

service(Name, Id, Version, Procs) when is_list(Name) ->
    service(list_to_binary(Name), Id, Version, Procs);
service(Name, Id, Version, Procs) when is_list(Id) ->
    service(Name, list_to_binary(Id), Version, Procs);
service(Name, Id, Version, Procs) when is_list(Version) ->
    service(Name, Id, list_to_binary(Version), Procs);
service(Name, Id, Version, Procs) ->
    #service{name = Name, id = Id, version = Version,
	     procs = lists:map(fun ({ProcName, Params}) ->
				       proc(ProcName, Params);
				   (P = #service_proc{}) ->
				       P
			       end, Procs)}.

proc(Name, Params) when is_list(Name) ->
    proc(list_to_binary(Name), Params);
proc(Name, Params) ->
    #service_proc{name = Name, params = lists:map(fun proc_param/1, Params)}.

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

gen_object_name() ->
    Hash = erlang:md5(term_to_binary({node(), erlang:now()})),
    binary_to_hex(Hash).

binary_to_hex(<<>>) ->
    [];
binary_to_hex(<<B, Rest/binary>>) ->
    [rfc4627:hex_digit((B bsr 4) band 15),
     rfc4627:hex_digit(B band 15) |
     binary_to_hex(Rest)].

service_address(ModData, Object) when is_binary(Object) ->
    service_address(ModData, binary_to_list(Object));
service_address(#mod{socket_type = SocketType,
		     config_db = ConfigDb,
		     parsed_header = Headers},
		Object) ->
    AliasPrefix = httpd_util:lookup(ConfigDb, json_rpc_alias, "/jsonrpc"),
    Host = case httpd_util:key1search(Headers, "host") of
	       undefined -> "";
	       Name -> "//" ++ Name
	   end,
    Scheme = case SocketType of
		 ip_comm -> "http:";
		 ssl -> "https:";
		 _Other -> ""
	     end,
    Scheme ++ Host ++ AliasPrefix ++ "/" ++ Object;
service_address(_, Object) ->
    Object.

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

invoke_service(get, Handler, ModData, ServiceProc, Args) ->
    if
	ServiceProc#service_proc.idempotent ->
	    invoke_service1(Handler, ModData, ServiceProc, Args);
	true ->
	    error_response(403, "Non-idempotent method", ServiceProc#service_proc.name)
    end;
invoke_service(post, Handler, ModData, ServiceProc, Args) ->
    invoke_service1(Handler, ModData, ServiceProc, Args).

invoke_service1(Handler, ModData, #service_proc{name = Name, params = Params}, Args) ->
    %%error_logger:info_msg("JSONRPC invoking ~p:~p(~p)", [Handler, Name, Args]),
    case catch run_handler(Handler, Name, ModData, coerce_args(Params, Args)) of
	{'EXIT', {{function_clause, _}, _}} ->
	    error_response(404, "Undefined procedure", Name);
	{'EXIT', Reason} ->
	    error_response(500, "Internal error", list_to_binary(io_lib:format("~p", [Reason])));
	Response ->
	    Response
    end.

run_handler({pid, Pid}, Name, ModData, CoercedArgs) ->
    gen_server:call(Pid, {jsonrpc, Name, ModData, CoercedArgs});
run_handler({function, F}, Name, ModData, CoercedArgs) ->
    F(Name, ModData, CoercedArgs).

coerce_args(_Params, Args) when is_list(Args) ->
    Args;
coerce_args(Params, {obj, Fields}) ->
    lists:map(fun (#service_proc_param{name = Name, type = Type}) ->
		      case lists:keysearch(binary_to_list(Name), 1, Fields) of
			  {value, {_, Value}} ->
			      coerce_value(Value, Type);
			  false ->
			      null
		      end
	      end, Params).

coerce_value(Value, _Type) when not(is_binary(Value)) ->
    Value;
coerce_value(<<"true">>, <<"bit">>) -> true;
coerce_value(_, <<"bit">>) -> false;
coerce_value(V, <<"num">>) -> list_to_integer(binary_to_list(V));
coerce_value(V, <<"str">>) -> V;
coerce_value(V, <<"arr">>) -> rfc4627:decode(V);
coerce_value(V, <<"obj">>) -> rfc4627:decode(V);
coerce_value(V, <<"any">>) -> rfc4627:decode(V);
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

system_describe(AddressStr,
		#service{name = Name, id = Id, version = Version, summary = Summary,
			 help = Help, procs = Procs}) ->
    remove_undefined({obj, [{"sdversion", <<"1.0">>},
			    {"name", Name},
			    {"id", Id},
			    {"version", Version},
			    {"summary", Summary},
			    {"help", Help},
			    {"address", list_to_binary(AddressStr)},
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
