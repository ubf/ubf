-module(test_plugin).


-export([info/0, description/0,
	 managerStart/1, managerRpc/2, 
	 handlerStart/2, handlerRpc/4, handlerStop/3
	]).

-import(server, [sendEvent/2]).
-import(lists, [map/2]).

%% NOTE the following two lines

-compile({parse_transform,contract_parser}).
-add_contract("test_plugin").

s(Str) -> {'#S', Str}.
-define(S(Str), {'#S',Str}).


info() -> "I am a test server".

description() -> "The test server is a ...
	bla
	bla
	bla".

managerStart(_) -> {ok, myManagerState}.

managerRpc(secret, State) ->
    {accept, welcomeToFTP, State};
managerRpc(_, State) ->
    {reject, badPassword, State}.

%% handlerStart(Args, ManagerPid) ->
%%   {accept, State, InitialData}

handlerStart(secret, ManagerPid) ->
    {accept, yesOffWeGo, start, myInitailData0};
handlerStart(Other, ManagerPid) ->
    {reject, bad_password}.

handlerRpc(start, {logon, _}, State, Env) ->
    {ok, active, State};
handlerRpc(active, ls, State, Env) ->
    {{files, [s("a"), s("b")]}, active, State};
handlerRpc(active, {callback, X}, State, Manager) ->
    sendEvent(self(), {callback, X}),
    {callbackOnItsWay, active, State};
handlerRpc(active, {get, File}, State, Env) ->
    {{ok,(<<>>)}, active, State};
handlerRpc(active, testAmbiguities, State, Env) ->
    {yes, funny, State};
handlerRpc(funny, ?S(S), State, Env) ->
    io:format("Upcase ~p~n",[S]),
    {s(up_case(S)), funny, State};
handlerRpc(funny, List, State, Env) ->
    io:format("Double ~p~n",[List]),
    {map(fun(I) -> 2*I end, List), funny, State};
handlerRpc(funny, stop, State, Env) ->
    {ack, start, State}.

handlerStop(Pid, Reason, State) ->
    io:format("Client stopped:~p ~p~n",[Pid, Reason]),
    State.

up_case(I) ->
    map(fun to_upper/1 , I).

to_upper(I) when I >= $a, I =< $z ->
    I + $A - $a;
to_upper(I) ->
    I.

    
	    
				     





