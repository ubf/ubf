%%% -*- mode: erlang -*-
%%% $Id$
%%%

-module(ubf_plugin_meta_serverless, [MODULES]).

-import(ubf_server, [ask_manager/2]).

-export([handlerStart/2, handlerRpc/4, handlerStop/3,
         managerStart/1, managerRestart/2, managerRpc/2]).

-export([info/0, description/0]).

-import(lists, [map/2]).

-compile({parse_transform,contract_parser}).
-add_contract("ubf_plugin_meta_server").

-include("ubf.hrl").

%% This is called when we start this manager It returns a state

%% The server plugin only knows how to start it's sub-services

services() -> [ Module:contract_name()
                || Module <- MODULES ].

info() -> "I am a meta server -

    type 'help'$

    ... to find out what I can do".

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

{'startService', \"Name\", Arg}  Name should be one of the names in the
                                 services list.  Arg is an initial
    argument for the Name service and is specific to that service; use
    'foo' or # (the empty list) if the service ignores this argument.

Warning: Without reading the documentation you might find the output
from some of these commands difficult to understand :-)

">>.

description() -> "
Commands:
    'services'$                   -- List services.
    {'startService', Name, Arg}$  -- Start a service.
                                  -- Reminder: Service names are strings
                                  -- and therefore must be double-quoted.
    'info'$                       -- Provide information.
    'description'$                -- This information.
    'contract'$                   -- Show the contract.
                                     See http://www.sics.se/~joe/ubf.html
".

%% managerStart(Args) -> {ok, State} | {error, Why}
%% handlerRpc(Q, State, Data, ManagerPid) -> {Reply, State', Data'}

%% handlerStart(Args, MangerPid) ->
%%      {accept, State, Data, Reply, NewManagerPid}
%%    | {reject, Why}.


managerStart(_) ->
    {ok, lists:zip(services(), [ {M, undefined}
                                 || M <- MODULES ])}.

managerRestart(Args,Manager) ->
    ask_manager(Manager,{restartManager, Args}).

managerRpc({service,Service}, S) ->
    case lists:keysearch(Service,1,S) of
        {value, {Service, X}} ->
            {{ok, X}, S};
        false ->
            {error, S}
    end;
managerRpc({restartManager,Args}, _S) ->
    managerStart(Args).


handlerStart(_, _) ->
    unused.

handlerStop(_Pid, _Reason, State) ->
    %% io:format("Client stopped:~p ~p~n",[Pid, Reason]),
    State.

%% normal rpc
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
                {reject, Why} ->
                    {{error,Why}, start, Data}
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
                {reject, Why} ->
                    {{error,Why}, start, Data}
            end;
        error ->
            io:format("returning error nosuch~n"),
            {{error,noSuchService}, start, Data}
    end.
