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
		{become, Reply, State2, Data2, Mod2, Manager2} ->
		    Client ! {self(), {rpcReplyNew, Reply, State2, Mod2}},
		    loop(Client, State2, Data2, Manager2, Mod2);
		{'EXIT', Why} ->
		    io:format("**** Yikes ...~p~n",[Why])
	    end;
	{event, X} ->
	    Client ! {event, X},
	    loop(Client, State1, Data, Manager, Mod);
	{client_has_died, Client, Why} ->
	    Manager ! {client_has_died, self(), Why};
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
			     server:register_plugin(self(), Mod),
			     manager(Mod)
		     end).

manager(Mod) ->
    State = Mod:managerStartState(),
    manager_loop(Mod, State).

manager_loop(Mod, State) ->
    io:format("~p manager waiting for something to do~n", [Mod]),
    receive
	{From, {startService1, Args}} ->
	    case (catch Mod:manager_start(Args, State)) of
		{accept, Ret, State1, Data, State2} ->
		    io:format("returniung accept ~p~n",[Mod]),
		    From ! {self(), {accept,Ret,State1,Data,Mod}},
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








