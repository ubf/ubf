-module(server_plugin).

-import(server, [sendEvent/2, ask_manager/2]).

-export([info/0, description/0,
	 managerStart/1, managerRpc/2, 
	 %% handlerStart/2, 
	 handlerRpc/4
	 %% handlerStop/3
	]).


-import(lists, [map/2, member/2, foreach/2]).

%% -compile(export_all).

-compile({parse_transform,contract_parser}).
-add_contract("server_plugin").

 
s(Str) -> {'#S', Str}.
-define(S(Str), {'#S',Str}).

%% This is called when we start this manager
%% It returns a state

%% The server plugin only knows how to start it's sub-services

services() -> ["file_server", "irc_server", "test_server"]. 

info() -> "I am a meta server - 

    type 'help'$

to find out what I can do".

help() ->
    "\n\n
This server speaks Universal Binary Format 1.0

See http://www.sics.se/~joe/ubf.html

UBF servers are introspective - which means the
servers can describe themselves. The following commands are
always available:

'help'$          This information
'info'$          Short information about the current service
'description'$   Long information  about the current service
'services'$      A list of available services
'contract'$      Return the service contract
                 (Note this is encoded in UBF)

To start a service:

{'startService', \"Name\", Args} Name should be one of the names in the
                                 services list - args is an optional argument

Warning without reading the documentation you might find the output from
some of these commands difficult to understand :-)

".

description() -> "
Commands:
   'services'$                   -- list services
   {'startService', Name, Args}$ -- start a service
   'info'$                       -- provide informat
   'description'$                -- this information
   'contract'$                   -- Show the contract
                                 (see  http://www.sics.se/~joe/ubf.html)
".

%% managerStart(Args) -> {ok, State} | {error, Why}
%% handlerRpc(Q, State, Data, ManagerPid) -> {Reply, State', Data'}
	
%% handlerStart(Args, MangerPid) -> 
%%      {accept, State, Data, Reply, NewManagerPid}
%%    | {reject, Why}.


managerStart(_) ->
    {ok, {plugin_handler:start_manager(test_plugin, []),
	  plugin_handler:start_manager(file_plugin, []),
	  plugin_handler:start_manager(irc_plugin, [])}}.

managerRpc({service,"test_server"}, S = {P1,_,_}) ->    
    {{ok, {test_plugin, P1}}, S};
managerRpc({service,"irc_server"}, S = {_,_,P3}) ->    
    {{ok, {irc_plugin, P3}}, S};
managerRpc({service,"file_server"}, S = {_,P2,_}) ->    
    {{ok, {file_plugin, P2}}, S};
managerRpc({service,S}, S1) ->
    {error, S1}.


handlerRpc(Any, info, State, Manager) ->
    {s(info()), Any, State};
handlerRpc(Any, description, State, Manager) ->
    {s(description()), Any, State};
handlerRpc(Any, help, State, Manager) ->
    {s(help()), Any, State};
handlerRpc(Any, services, State, Manager) ->
    Ss = map(fun(I) -> s(I) end, services()),
    {Ss, Any, State};
handlerRpc(start, {startService, ?S(Name), Args}, Data, Manager) ->
    io:format("Here ~p ~p~n",[Name, Data]),
    case ask_manager(Manager, {service, Name}) of
	{ok, {Mod, Pid}} ->
	    io:format("Server_plugin calling:~p~n",[Mod]),
	    case (catch Mod:handlerStart(Args, Pid)) of
		{accept, Reply, State1, Data1} ->
		    io:format("Accepted:~n"),
		    {changeContract, {ok, Reply}, start,
		     Mod, State1, Data1, Pid};
		{reject, Why} ->
		    {{error,Why}, start, Data}
	    end;
	error ->
	    io:format("returning error nosuch~n"),
	    {{error,noSuchService}, start, Data}
    end.




