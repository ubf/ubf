-module(test_plugin).

-include("ubf.hrl").

-export([info/0, description/0,
         managerStart/1, managerRpc/2,
         handlerStart/2, handlerRpc/4, handlerStop/3
        ]).

-import(ubf_server, [sendEvent/2]).
-import(lists, [map/2]).

%% NOTE the following two lines

-compile({parse_transform,contract_parser}).
-add_contract("./Unit-Test-Files/test_plugin").

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

handlerStart(secret, _ManagerPid) ->
    {accept, yesOffWeGo, start, myInitailData0};
handlerStart(_Other, _ManagerPid) ->
    {reject, bad_password}.

handlerRpc(start, {logon, _}, State, _Env) ->
    {ok, active, State};
handlerRpc(active, ls, State, _Env) ->
    {{files, [?S("a"), ?S("b")]}, active, State};
handlerRpc(active, {callback, X}, State, _Manager) ->
    sendEvent(self(), {callback, X}),
    {callbackOnItsWay, active, State};
handlerRpc(active, {get, _File}, State, _Env) ->
    {{ok,(<<>>)}, active, State};
handlerRpc(active, testAmbiguities, State, _Env) ->
    {yes, funny, State};
handlerRpc(funny, ?S(S), State, _Env) ->
    io:format("Upcase ~p~n",[S]),
    {?S(up_case(S)), funny, State};
handlerRpc(funny, ?P(List), State, _Env) when is_list(List) ->
    io:format("PropList ~p~n",[List]),
    {?P(List), funny, State};
handlerRpc(funny, List, State, _Env) when is_list(List) ->
    io:format("Double ~p~n",[List]),
    {map(fun(I) -> 2*I end, List), funny, State};
handlerRpc(funny, stop, State, _Env) ->
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
