%%% The MIT License
%%%
%%% Copyright (C) 2011-2013 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (C) 2002 by Joe Armstrong
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(test_plugin).
-behaviour(ubf_plugin_stateful).

-include("ubf.hrl").

-export([info/0, description/0,
         managerStart/1, managerRestart/2, managerRpc/2,
         handlerStart/2, handlerRpc/4, handlerStop/3,
         handlerEvent/1
        ]).

-import(ubf_plugin_handler, [sendEvent/2, install_handler/2]).
-import(lists, [map/2]).

%% NOTE the following two lines

-compile({parse_transform,contract_parser}).
-add_contract("./test/unit/test_plugin").

info() -> "I am a test server".

description() -> "The test server is a ...
        bla
        bla
        bla".

managerStart(_) -> {ok, myManagerState}.

managerRestart(_,_) -> ok. %% noop

managerRpc(secret, State) ->
    {{ok, welcomeToFTP}, State};
managerRpc(_, State) ->
    {{error, badPassword}, State}.

%% handlerStart(Args, ManagerPid) ->
%%   {accept, State, InitialData}

handlerStart(secret, _ManagerPid) ->
    ack = install_handler(self(), fun handlerEvent/1),
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

handlerStop(Pid, Reason, ManagerData) ->
    io:format("Client stopped:~p ~p~n",[Pid, Reason]),
    ManagerData.

handlerEvent({callback, X}) ->
    sendEvent(self(), {callback, X}),
    fun handlerEvent/1.

up_case(I) ->
    map(fun to_upper/1 , I).

to_upper(I) when I >= $a, I =< $z ->
    I + $A - $a;
to_upper(I) ->
    I.
