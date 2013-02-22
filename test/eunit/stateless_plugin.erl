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

-module(stateless_plugin).
-behaviour(ubf_plugin_stateless).

-include("ubf.hrl").
-include("ubf_plugin_stateless.hrl").

-export([info/0, description/0, keepalive/0]).
-export([moduleStart/1, moduleRestart/1]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1]).

-export([client_breaks_req01/0, client_timeout_req03/1]).
-export([server_breaks_req01/0, server_timeout_req03/1, server_crash_req05/0]).

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_contract("./test/eunit/stateless_plugin").
-add_types({types_plugin, [ubf_contract_res,ubf_contract_req,ubf_description_res,ubf_description_req,ubf_info_res,ubf_info_req]}).
-add_types({types_plugin, [ubf_keepalive_res,ubf_keepalive_req]}).
-add_types({types_plugin, [timeout]}).
-add_types({types_plugin, [server_crash_res05,server_crash_req05,
                           server_timeout_res03,server_timeout_req03,
                           server_breaks_res01,server_breaks_req01,
                           client_timeout_res03,client_timeout_req03,
                           client_breaks_res01,client_breaks_req01]}).


info() ->
    "I am a stateless server".

description() ->
    "An stateless server programmed by UBF".

keepalive() ->
    ok.


%% @spec moduleStart(Args::list(any())) -> Ignored::any()
%% @doc start module
moduleStart(_Args) ->
    unused.

%% @spec moduleRestart(Args::list(any())) -> Ignored::any()
%% @doc restart module
moduleRestart(Args) ->
    moduleStart(Args).



%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args) ->
    {accept,ok,none,unused}.

%% @spec handlerStop(Pid::pid(), Reason::any(), StateData::term()) -> none()
%% @doc stop handler
handlerStop(_Pid, _Reason, _StateData) ->
    unused.

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
