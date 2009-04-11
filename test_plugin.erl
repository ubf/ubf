-module(test_plugin).

-export([manager_start/2, client_stop/3,
	 managerStartState/0, handlerStartState/0, 
	 manager_rpc/2, handle_rpc/4]).

-import(server, [sendEvent/2]).
-import(lists, [map/2]).

%% NOTE the following two lines

-compile({parse_transform,contract_parser}).
-add_contract("test_plugin").

s(Str) -> {'#S', Str}.
-define(S(Str), {'#S',Str}).

managerStartState() -> myManagerState.

handlerStartState() -> myHandlerState.
    
%% manager_start(Args, State) is called every time a session is started
%% Args comes from client:start(Host,Port,Service,Args)
%% Service in the rcp:start must match name()
%% manager_start(Args, State) -> {accept, Reply, State} | {reject, Why, State}
%% State is the manager state.

manager_start(secret, ManagerState) ->
    Reply = yesOffWeGo,
    HandlerState = start,
    HandlerData = myInitailData0,
    {accept, Reply, HandlerState, HandlerData, ManagerState};
manager_start(Other, ManagerState) ->
    {reject, bad_password, ManagerState}.


client_stop(Pid, Reason, State) ->
    io:format("Client stopped:~p ~p~n",[Pid, Reason]),
    State.

manager_rpc(secret, State) ->
    {accept, welcomeToFTP, State};
manager_rpc(_, State) ->
    {reject, badPassword, State}.

handle_rpc(start, {logon, _}, State, Env) ->
    {ok, active, State};
handle_rpc(active, ls, State, Env) ->
    {{files, [s("a"), s("b")]}, active, State};
handle_rpc(active, {callback, X}, State, Manager) ->
    sendEvent(self(), {callback, X}),
    {callbackOnItsWay, active, State};
handle_rpc(active, {get, File}, State, Env) ->
    {{ok,(<<>>)}, active, State};
handle_rpc(active, testAmbiguities, State, Env) ->
    {yes, funny, State};
handle_rpc(funny, ?S(S), State, Env) ->
    io:format("Upcase ~p~n",[S]),
    {s(up_case(S)), funny, State};
handle_rpc(funny, List, State, Env) ->
    io:format("Double ~p~n",[List]),
    {map(fun(I) -> 2*I end, List), funny, State};
handle_rpc(funny, stop, State, Env) ->
    {ack, start, State}.

up_case(I) ->
    map(fun to_upper/1 , I).

to_upper(I) when I >= $a, I =< $z ->
    I + $A - $a;
to_upper(I) ->
    I.

    
	    
				     





