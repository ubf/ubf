-module(server).

-export([start_plugins/0, start/1, register_plugin/2, ask_manager/2, 
	 sendEvent/2]).

-import(contracts, [checkIn/3, checkOut/4, checkCallback/4]).

-include("contract.hrl").

-import(lists, [foreach/2, map/2]).
-import(ubf_utils, [spawn_link_debug/2]).
-import(proc_socket_server, [start_raw_server/4]).

%% Here we start the server
%% This is the *only* registered process on the server side

s(X) -> {'#S', X}.
    
start(Port) ->
    start_registered(server, fun() -> start_server(Port) end),
    start_plugins().

start_plugins() ->
    Fs = find:files(".", "*plugin.beam", false),
    foreach(fun(F) -> 
		    Stem = filename:basename(filename:rootname(F)),
		    Mod = list_to_atom(Stem),
		    io:format("Trying to start plugin:~p~n",[Mod]),
		    plugin_handler:start_manager(Mod)
	    end, Fs).

start_server(Port) ->
    %% set up a UBF listener on Port
    Server = self(),
    start_ubf_listener(Port, Server),
    plugin_handler:manager(server_plugin).

start_ubf_listener(Port, Server) ->
    io:format("starting proc_socket:~p~n",[Port]),
    start_raw_server(Port,
		     fun(Socket) ->
			     %% This gets spawned every time a 
			     %% new socket connection is
			     %% is established on this port.
			     %% 
			     %% We have to start 2 additional
			     %% processes - a contract
			     %% manager and a plugin handler
			     %% The driver (This process) sends
			     %% messages to the contract manager
			     %% The contract manager sends messages to 
			     %% the handler.
			     Driver          = self(),
			     ContractManager = contract_manager:start(),
			     Handler         = plugin_handler:start_handler(),
			     %% (The next three lines are pretty devious
			     %%  but they work !)
			     %% send hello back to the opening program
			     io:format("server sending hello~n"),
			     Name = s(server_plugin:contract_name()),
			     Info = s(server_plugin:contract_info()),
			     self() ! {self(), {'ubf1.0', Name, Info}},
			     %% swop the driver
			     ubf_driver:relay(self(), ContractManager),
			     ContractManager ! {start, Driver, Handler, 
						start, server_plugin},
			     Handler ! {start, ContractManager,
					Server, server_plugin},
			     %% and activate the loop that will now
			     %% execute the last two statements :-)
			     put('$ubfinfo', {ubfdriver,server}),
			     ubf_driver:loop(Socket, self())
		     end,
		     50, 
		     0).

sendEvent(Pid, Msg) ->
    %% io:format("sendEvent (server) ~p to ~p~n",[Msg,Pid]),
    Pid ! {event, Msg}.

ask_manager(Manager, Q) ->
    Manager ! {self(), {handler_rpc, Q}},
    receive
	{handler_rpc_reply, R} ->
	    R
    end.

register_plugin(Pid, Mod) ->
    ask_manager(server, {register, Pid, Mod}).

get_handler_id(Pid, Name) ->
    Pid ! {self(), {get_handler_id, Name}},
    receive
	{Pid, Reply} -> Reply
    end.

services(Pid) ->
    Pid ! {self(), services},
    receive
	{Pid, Reply} -> Reply
    end.
    
%%----------------------------------------------------------------------
%% Misc junk

start_registered(Name, F) ->
    case whereis(Name) of
	undefined ->
	    Me = self(),
	    P = spawn_link_debug({registered,Name},
				 fun() -> start_proc(Me, Name, F) end),
	    receive
		{P, ack} ->
		    true
	    end;
	Pid ->
	    true
    end.

start_proc(Parent, Name, F) ->
    case (catch register(Name, self())) of
	{'EXIT', _} ->
	    Parent ! {self(), ack},
	    true;
	_ ->
	    Parent ! {self(), ack},
	    %% io:format("starting ~p~n",[Name]),
	    F()
    end.

start_service(Server, Session, Args) ->
    case get_handler_id(Server, Session) of
	{ok, Pid}->
	    service_manager:start_service(Pid, Args);
	error ->
	    {reject, no_such_service}
    end.

    










