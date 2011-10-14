%%% The MIT License
%%%
%%% Copyright (C) 2011 by Joseph Wayne Norton <norton@alum.mit.edu>
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

-module(stateful_plugin).
-behaviour(ubf_plugin_stateful).

-include("ubf.hrl").
-include("ubf_plugin_stateful.hrl").

-export([info/0, description/0, keepalive/0]).
-export([handlerStart/2, handlerStop/3, handlerRpc/4, handlerRpc/1]).

-export([client_breaks_req01/0, client_timeout_req03/1]).
-export([server_breaks_req01/0, server_timeout_req03/1, server_crash_req05/0]).

-export([managerStart/1, managerRestart/2, managerRpc/2]).
-import(ubf_plugin_handler, [ask_manager/2]).

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_contract("./test/eunit/stateful_plugin").
-add_types({types_plugin, [contract_res,contract_req,description_res,description_req,info_res,info_req]}).
-add_types({types_plugin, [keepalive_res,keepalive_req]}).
-add_types({types_plugin, [timeout]}).
-add_types({types_plugin, [server_crash_res05,server_crash_req05,
                           server_timeout_res03,server_timeout_req03,
                           server_breaks_res01,server_breaks_req01,
                           client_timeout_res03,client_timeout_req03,
                           client_breaks_res01,client_breaks_req01]}).
-add_types({types_plugin, [restart_res,restart_req,server_crash_res06,
                           server_crash_req06,server_timeout_res04,
                           server_timeout_req04,server_breaks_res02,
                           server_breaks_req02,client_timeout_res04,
                           client_timeout_req04,client_breaks_res02,
                           client_breaks_req02]}).


%% records

%% state
-record(state, {
         }).

%% managerState
-record(managerState, {
         }).


info() ->
    "I am a stateful server".

description() ->
    "An stateful server programmed by UBF".

keepalive() ->
    ok.


%% @spec handlerStart(Args::list(any()), ManagerPid::pid()) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args,_ManagerPid) ->
    {accept,ok,start,#state{}}.

%% @spec handlerStop(Pid::pid(), Reason::any(), ManagerStateData::term()) -> NewManagerStateData::term()
%% @doc stop handler event
handlerStop(_Pid,_Reason,ManagerStateData)
  when is_record(ManagerStateData,managerState) ->
    ManagerStateData.


%% @spec handlerRpc(StateName::atom(), Event::any(), StateData::term(), ManagerPid::pid()) ->
%%          {Reply::any(), NextStateName::atom(), NewStateData::term()}
%% @doc rpc handler
handlerRpc(StateName,Event,StateData,_ManagerPid) ->
    {handlerRpc(Event),StateName,StateData}.


%% @spec handlerRpc(Event::any()) ->
%%          Reply::any()
%% @doc rpc handler
handlerRpc(Event)
  when Event==client_breaks_req01
       ; Event==server_breaks_req01
       ; Event==server_crash_req05
       ->
    ?MODULE:Event();
handlerRpc({Event,Timeout})
  when Event==client_timeout_req03
       ; Event==server_timeout_req03
       ->
    ?MODULE:Event(Timeout);
handlerRpc(Event)
  when Event==info; Event==description ->
    ?S(?MODULE:Event());
handlerRpc(Event)
  when Event==keepalive ->
    ?MODULE:Event();
handlerRpc(Event) ->
    {Event, not_implemented}.


%%%----------------------------------------------------------------------
%%% Implementation functions
%%%----------------------------------------------------------------------

client_breaks_req01() ->
    exit(client_breaks_req01_should_not_be_called).

client_timeout_req03(Timeout) ->
    timer:sleep(Timeout),
    client_timeout_res03.

server_breaks_req01() ->
    server_breaks_res01_with_this_response.

server_timeout_req03(Timeout) ->
    timer:sleep(Timeout),
    server_timeout_res03.

server_crash_req05() ->
    exit(server_crash_res05_with_this_response).


%%%----------------------------------------------------------------------
%%% Manager functions
%%%----------------------------------------------------------------------

%% @spec managerStart(Args::list(any())) ->
%%          {ok, ManagerStateData::term()} | {error, Reason::any()}
%% @doc start manager
managerStart(_) ->
    ManagerStateData = #managerState{},
    {ok,ManagerStateData}.

%% @spec managerRestart(Args::list(any()), ManagerPid::pid()) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc restart manager
managerRestart(Args,ManagerPid) ->
    ask_manager(ManagerPid,{restartManager, Args}).


%% @spec managerRpc(Event::any(), ManagerStateData::term()) ->
%%          {ok | {ok,term()} | error | {error, Reason::any()}, NewManagerStateData::term()}
%% @doc rpc manager
managerRpc({restartManager,Args},ManagerStateData)
  when is_record(ManagerStateData,managerState) ->
    managerStart(Args).
