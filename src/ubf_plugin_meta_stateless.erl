%%% The MIT License
%%%
%%% Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Implement the UBF(c) meta-protocol for UBF(b) "stateless"
%%% contracts.
%%%
%%% The metaprotocol is used at the beginning of a UBF session to
%%% select one of the UBF(b) contracts that the TCP listener is
%%% capable of offering.  The list of contracts (or more precisely,
%%% the Erlang modules that implement the contract(s)) is passed via
%%% the +ubf_server:start_link()+ function, in the +PluginModule+
%%% list.
%%%
%%% Code in this module is executed by the "Plugin Handler" process in
%%% the Process Structure Diagram in the Overview.
%%%
%%% For the purposes of this module, the list of modules that
%%% implement contracts is passed using Erlang parameterized module
%%% +Module:new(ModuleList)+ syntax.  See the Erlang/OTP documentation
%%% for more information on parameterized module syntax and usage.
%%%

-module(ubf_plugin_meta_stateless, [MODULES]).

-import(ubf_plugin_handler, [ask_manager/2]).

%% Required callback API for all UBF contract implementations.

-export([handlerStart/1, handlerStop/3, handlerRpc/1]).
-export([managerStart/1, managerRestart/2, managerRpc/2]).

-export([info/0, description/0]).

-compile({parse_transform,contract_parser}).
-add_contract("src/ubf_plugin_meta_stateless").

-include("ubf.hrl").

%% This is called when we start this manager It returns a state

%% The server plugin only knows how to start it\'s sub-services

%% @doc Enumerate the UBF services of this server.

services() -> [ Module:contract_name()
                || Module <- MODULES ].

%% @doc Enumerate the UBF modules and services of this server.

modules() ->  [ {Module:contract_name(), {Module, undefined}}
                || Module <- MODULES ].

%% @doc Emit an info string.

info() -> "I am a meta server -

    type 'help'$

              ... to find out what I can do".

%% @doc Emit a help string.

help() ->
               <<"\n\n
This server speaks Universal Binary Format 1.0

                See http://www.sics.se/~joe/ubf.html
                See http://github.com/ubf/ubf/tree/master for some
                source code extensions available as part of the larger
                OSS community.

UBF servers are introspective - which means the servers can describe
themselves. The following commands are always available:

'help'$          This information
'info'$          Short information about the current service
'description'$   Long information  about the current service
'services'$      A list of available services
'contract'$      Return the service contract
(Note this is encoded in UBF)

To start a service:

{'startSession', \"Name\", Arg}  Name should be one of the names in the
                                 services list.  Arg is an initial
argument for the Name service and is specific to that service; use
'foo' or # (the empty list) if the service ignores this argument.

Warning: Without reading the documentation you might find the output
from some of these commands difficult to understand :-)

">>.

%% @doc Emit a description string.

description() -> "
Commands:
                     'services'$                   -- List services.
{'startSession', Name, Arg}$  -- Start a service.
-- Reminder: Service names are strings
-- and therefore must be double-quoted.
'info'$                       -- Provide information.
'description'$                -- This information.
'contract'$                   -- Show the contract.
See http://www.sics.se/~joe/ubf.html
".

%% @doc Required UBF contract implementation callback: start manager
%%      process(es).

managerStart(Args) ->
    Modules = modules(),
    _ = [ Module:moduleStart(Args) || {_, {Module, undefined}} <- Modules ],
    {ok, Modules}.

%% @doc Required UBF contract implementation callback: restart a manager
%%      process.

managerRestart(Args,Manager) ->
    ask_manager(Manager,{restartManager, Args}).

%% @doc Required UBF contract implementation callback: call a manager\'s RPC
%%      function.

managerRpc({service,Service}, S) ->
    case lists:keyfind(Service,1,S) of
        {Service, X} ->
            {{ok, X}, S};
        false ->
            {error, S}
    end;
managerRpc({restartManager,Args}, _S) ->
    Modules = modules(),
    _ = [ Module:moduleStart(Args) || {_, {Module, undefined}} <- Modules ],
    {ok, Modules}.


%% @doc Required UBF contract implementation callback: start a new session
%%      handler process.

handlerStart(_) ->
    unused.

%% @doc Required UBF contract implementation callback: stop a session
%%      handler process.

handlerStop(_Pid, _Reason, State) ->
    State.

%% @doc Required UBF contract implementation callback: call an RPC function.

handlerRpc({startSession, ?S(Name), Args}) ->
    case lists:keyfind(Name,1,modules()) of
        {Name, {Mod, _Pid}} ->
            case (catch Mod:handlerStart(Args)) of
                {accept, Reply, State1, Data1} ->
                    {changeContract, {ok, Reply}, Mod, State1, Data1};
                {reject, Reason} ->
                    {error,Reason}
            end;
        false ->
            {error,noSuchService}
    end;
handlerRpc({restartService, ?S(Name), _Args}) ->
    case lists:keyfind(Name,1,modules()) of
        {Name, {_Mod, _Pid}} ->
            {ok,ok};
        false ->
            {error,noSuchService}
    end;
handlerRpc(info) ->
    ?S(info());
handlerRpc(description) ->
    ?S(description());
handlerRpc(help) ->
    help();
handlerRpc(services) ->
    [?S(I) || I <- services()];
%% verbose rpc
handlerRpc({Event,_}) ->
    handlerRpc(Event).
