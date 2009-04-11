-module(plugin_handler).

%% The plugin handler handles request for the instance of the plugin

-export([start_handler/0, start_manager/1, manager/1]).

-export([start_service/2]).

-import(lists, [map/2]).
-import(ubf_utils, [spawn_link_debug/2]).

%%----------------------------------------------------------------------
%% Handler stuff

start_handler() ->
    spawn_link(fun() -> wait() end).

wait() ->
    process_flag(trap_exit, true),
    receive
	{start, Contract, Server, Mod} ->
	    loop(Contract, start, [], Server, Mod)
    end.

loop(Client, State1, Data, Manager, Mod) ->
    %% io:format("handler ~p waiting state:~p ~n",[self(), State1]),
    receive
	{Pid, {rpc, Q}} ->
	    case (catch Mod:handle_rpc(State1, Q, Data, Manager)) of
		{Reply, State2, Data2} ->
		    Client ! {self(), {rpcReply, Reply, State2}},
		    loop(Client, State2, Data2, Manager, Mod);
		{'EXIT', Why} ->
		    Err = {error, {internalError,handler}},
		    Client ! {self(), {rpcReply, Err, stop}},
		    io:format("**** Yikes ...~p~n",[Why]),
		    exit(fatal)
	    end;
	{event, X} ->
	    Client ! {event, X},
	    loop(Client, State1, Data, Manager, Mod);
	{client_has_died, Client, Why} ->
	    Manager ! {client_has_died, self(), Why};
	{Pid, Req={startService, Service}} ->
	    io:format("Plugin handler (~p) got ~p sending to~p~n",
		      [self(), Req,Manager]),
	    Manager ! {self(), Req},
	    receive
		{Manager, Accept = {accept, HandlerMod, ManagerPid}} ->
		    Pid ! {self(), Accept},
		    State1 = start,
		    Data1 = HandlerMod:handlerStartState(),
		    loop(Client, State1, Data1, ManagerPid, HandlerMod);
		{Manager, Reject = {reject, Why}} ->
		    Pid ! {self(), Reject},
		    loop(Client, State1, Data, Manager, Mod);
		OO ->
		    io:format("Bummer ~p~n",[OO]),
		    loop(Client, State1, Data, Manager, Mod)
	    end;
	Other ->
	    io:format("**** OOOPYikes ...~p (Client=~p)~n",[Other,Client]),
	    loop(Client, State1, Data, Manager, Mod)
    end.

start_service(Pid, Args) ->
    Pid ! {self(), {startService1, Args}},
    receive
	{Pid, Ret} ->
	    Ret
    end.


%%----------------------------------------------------------------------

start_manager(Mod) ->
    spawn_link_debug({manager,Mod},
		     fun() ->
			     manager(Mod)
		     end).

manager(Mod) ->
    process_flag(trap_exit, true),
    State = Mod:managerStart(),
    io:format("process ~p is manager for ~p~n",[self(), Mod]),
    manager_loop(Mod, State).

manager_loop(Mod, State) ->
    io:format("~p manager waiting for something to do~n", [Mod]),
    receive
	{From, {startService, Service}} ->
	    case (catch Mod:startService(Service, State)) of
		{accept, HandlerMod, ModManagerPid, State2} ->
		    io:format("returniung accept ~p~n",[Mod]),
		    From ! {self(), {accept,HandlerMod, ModManagerPid}},
		    manager_loop(Mod, State2);
		{reject, Why, State1} ->
		    %% io:format("rejected~n"),
		    From ! {self(), {reject, Why}},
		    manager_loop(Mod, State)
	    end;
	{client_has_died, Pid, Why} ->
	    io:format("Handler caught child_has_died:~p~n",[Pid]),
	    case (catch Mod:client_stop(Pid, Why, State)) of
		{'EXIT', OOps} ->
		    io:format("plug in error:~p~n",[OOps]),
		    manager_loop(Mod, State);
		State1 ->
		    manager_loop(Mod, State1)
	    end;
	{From, {handler_rpc, Q}} ->
	    case (catch Mod:manager_rpc(Q, State)) of
		{'EXIT', OOps} ->
		    io:format("plug in error:~p~n",[OOps]),
		    exit(From, bad_ask_manager),
		    manager_loop(Mod, State);
		{Reply, State1} ->
		    From ! {handler_rpc_reply, Reply},
		    manager_loop(Mod, State1)
	    end;
	X ->
	    io:format("******Dropping (service manager ~p) self=~p ~p~n",
		      [Mod,self(), X]),
	    manager_loop(Mod, State)
    end.








