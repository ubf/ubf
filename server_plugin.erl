-module(server_plugin).

-import(server, [sendEvent/2, ask_manager/2]).
-import(lists, [map/2, member/2, foreach/2]).

-compile(export_all).

-compile({parse_transform,contract_parser}).
-add_contract("server_plugin").

 
s(Str) -> {'#S', Str}.
-define(S(Str), {'#S',Str}).

managerStartState() ->
    dict:new().

manager_rpc({register, Pid, Mod}, Dict) ->
    Name = (catch Mod:contract_name()),
    case dict:find(Name, Dict) of
	{ok, _} ->
	    io:format("**** Cannot register:~p Name taken~p~n",
		      [Mod, Name]),
	    {error, Dict};
	error ->
	    {ok, dict:store(Name, Pid, Dict)}
    end;
manager_rpc({handler_id, S}, Dict) ->
    case dict:find(S, Dict) of
	{ok, Pid} ->
	    {{ok, Pid}, Dict};
	error ->
	    {error, Dict}
    end;
manager_rpc(services, D) ->
    {dict:fetch_keys(D), D}.

handle_rpc(start, services, Data, Manager) ->
    S = ask_manager(Manager, services),
    S1 = map(fun(I) -> s(I) end, S),
    {S1, start, Data};
handle_rpc(start, {startService, ?S(Service), Args}, Data, Manager) ->
    case ask_manager(Manager, {handler_id, Service}) of
	{ok, Pid} ->
	    case plugin_handler:start_service(Pid, Args) of
		{accept, Ret, State, Data1, Mod} ->
		    {become, {ok, Ret}, State, Data1, Mod, Pid};
		{reject, Why} ->
		    {{error, Why}, start, Data}
	    end;
	error ->
	    {{error, no_service}, start, Data}
    end.

	


