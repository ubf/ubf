%%% -*- mode: erlang -*-
%%% $Id: ubf_plugin_meta_serverless.erl 148658 2009-11-02 03:18:24Z fritchie $
%%% @doc Implement the UBF(C) meta-protocol for UBF(B) "stateless" contracts.
%%%
%%% The metaprotocol is used at the beginning of a UBF session to
%%% select one of the UBF(B) contracts that the TCP listener is
%%% capable of offering.  The list of contracts (or more precisely,
%%% the Erlang modules that implement the contract(s)) is passed via
%%% the `ubf_server:start_link()' function, in the `PluginModule'
%%% list.
%%%
%%% Code in this module is executed by the "Plugin Handler" process
%%% in the Process Structure Diagram in the Overview.
%%%
%%% For the purposes of this module, the list of modules that
%%% implement contracts is passed using Erlang parameterized module
%%% `Module:new(ModuleList)' syntax.  See the Erlang/OTP documentation
%%% for more information on parameterized module syntax and usage.
%%% For code examples, look in the
%%% "<a href="../src/Unit-Test-Files">../src/Unit-Test-Files</a>"
%%% directory for several examples (see files with "_plugin.erl" suffix).
%%%

%%%

-module(ubf_plugin_meta_serverless, [MODULES]).

-import(ubf_server, [ask_manager/2]).

%% Required callback API for all UBF contract implementations.

-export([handlerStart/2, handlerRpc/4, handlerStop/3,
         managerStart/1, managerRestart/2, managerRpc/2]).

-export([info/0, description/0]).

-import(lists, [map/2]).

-compile({parse_transform,contract_parser}).
-add_contract("ubf_plugin_meta_serverless").

-include("ubf.hrl").

%% This is called when we start this manager It returns a state

%% The server plugin only knows how to start it's sub-services

%% @doc Enumerate the UBF services of this server.

services() -> [ Module:contract_name()
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
    See http://github.com/norton/ubf/tree/master for some
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

%% managerStart(Args) -> {ok, State} | {error, Reason}
%% handlerRpc(Q, State, Data, ManagerPid) -> {Reply, State', Data'}

%% handlerStart(Args, MangerPid) ->
%%      {accept, State, Data, Reply, NewManagerPid}
%%    | {reject, Reason}.


%% @spec (Args::term()) ->
%%       {ok, State::term()}
%% @doc Required UBF contract implementation callback: start manager
%%      process(es).

managerStart(_) ->
    {ok, lists:zip(services(), [ {M, undefined}
                                 || M <- MODULES ])}.

%% @spec (Args::term(), Manager::pid()) ->
%%       ok | {error, Reason::term()}
%% @doc Required UBF contract implementation callback: restart a manager
%%      process.

managerRestart(Args,Manager) ->
    ask_manager(Manager,{restartManager, Args}).

%% @spec (Args::term(), Manager::pid()) ->
%%       ok | {error, Reason::term()}
%% @doc Required UBF contract implementation callback: call a manager's RPC
%%      function.

managerRpc({service,Service}, S) ->
    case lists:keysearch(Service,1,S) of
        {value, {Service, X}} ->
            {{ok, X}, S};
        false ->
            {error, S}
    end;
managerRpc({restartManager,Args}, _S) ->
    managerStart(Args).


%% @spec (Arg_From_UBF_Client::term(), ManagerProc::pid()) ->
%%       {accept, Reply::term(), StateName::atom(), StateData::term()} |
%%       {reject, Reply::term()}
%% @doc Required UBF contract implementation callback: start a new session
%%      handler process.

handlerStart(_, _) ->
    unused.

%% @spec (Env::term(), Reason::term(), StateData::term()) ->
%%       void()
%% @doc Required UBF contract implementation callback: stop a session
%%      handler process.

handlerStop(_Pid, _Reason, State) ->
    %% io:format("Client stopped:~p ~p~n",[Pid, Reason]),
    State.

%% @spec (StateName::atom(), RpcCall::term(), StateData::term(), Env::term()) ->
%%    {Reply::term(), NewStateName::atom(), NewStateData::term()} |
%%    {changeContract, Reply::term(), NewStateData::term(), HandlerMod::atom(),
%%     State2::term(), Data2::term(), ManagerPid::pid}
%% @doc Required UBF contract implementation callback: call an RPC function.

handlerRpc(Any, info, State, _Manager) ->
    {?S(info()), Any, State};
handlerRpc(Any, description, State, _Manager) ->
    {?S(description()), Any, State};
handlerRpc(Any, help, State, _Manager) ->
    {help(), Any, State};
handlerRpc(Any, services, State, _Manager) ->
    Ss = map(fun(I) -> ?S(I) end, services()),
    {Ss, Any, State};
handlerRpc(start, {restartService, ?S(Name), Args}, Data, Manager) ->
    case ask_manager(Manager, {service, Name}) of
        {ok, {Mod, Pid}} ->
            %% io:format("Server_plugin calling:~p~n",[Mod]),
            case (catch Mod:managerRestart(Args, Pid)) of
                ok ->
                    {{ok,ok}, start, Data};
                {error, Reason} ->
                    {{error,Reason}, start, Data}
            end;
        error ->
            io:format("returning error nosuch~n"),
            {{error,noSuchService}, start, Data}
    end;
handlerRpc(start, {startSession, ?S(Name), Args}, Data, Manager) ->
    case ask_manager(Manager, {service, Name}) of
        {ok, {Mod, undefined}} ->
            %% io:format("Server_plugin calling:~p~n",[Mod]),
            case (catch Mod:handlerStart(Args)) of
                {accept, Reply, State1, Data1} ->
                    %% io:format("Accepted:~n"),
                    {changeContract, {ok, Reply}, start,
                     Mod, State1, Data1, undefined};
                {reject, Reason} ->
                    {{error,Reason}, start, Data}
            end;
        error ->
            io:format("returning error nosuch~n"),
            {{error,noSuchService}, start, Data}
    end;

%% verbose rpc
handlerRpc(Any, {info,_}, State, _Manager) ->
    {?S(info()), Any, State};
handlerRpc(Any, {description,_}, State, _Manager) ->
    {?S(description()), Any, State};
handlerRpc(Any, {help,_}, State, _Manager) ->
    {help(), Any, State};
handlerRpc(Any, {services,_}, State, _Manager) ->
    Ss = map(fun(I) -> ?S(I) end, services()),
    {Ss, Any, State};
handlerRpc(start, {{restartService, ?S(Name), _Args},_}, Data, Manager) ->
    case ask_manager(Manager, {service, Name}) of
        {ok, {_Mod, undefined}} ->
            %% io:format("Server_plugin calling:~p~n",[Mod]),
            {{ok,ok}, start, Data};
        error ->
            io:format("returning error nosuch~n"),
            {{error,noSuchService}, start, Data}
    end;
handlerRpc(start, {{startSession, ?S(Name), Args},_}, Data, Manager) ->
    case ask_manager(Manager, {service, Name}) of
        {ok, {Mod, undefined}} ->
            %% io:format("Server_plugin calling:~p~n",[Mod]),
            case (catch Mod:handlerStart(Args)) of
                {accept, Reply, State1, Data1} ->
                    %% io:format("Accepted:~n"),
                    {changeContract, {ok, Reply}, start,
                     Mod, State1, Data1, undefined};
                {reject, Reason} ->
                    {{error,Reason}, start, Data}
            end;
        error ->
            io:format("returning error nosuch~n"),
            {{error,noSuchService}, start, Data}
    end.
