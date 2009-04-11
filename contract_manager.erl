-module(contract_manager).

-export([start/0]).
-import(contracts, [checkIn/3, checkOut/4, checkCallback/3]).

-include("contract.hrl").


%  Client                     Contract                     Server
%    |                           |                            |
%    |                           |                            |
%    |                           |                            |
%    |  {Driver, {rpc,Q}}        |                            |
%    +---------->----------------+    {Contract, Q}           |
%    |                           +------------->--------------+
%    |                           |                            |
%    |                           |                            |
%    |                           |      {reply,R,S2}          |           
%    |                           +-------------<--------------+
%    | {Contract, {reply,Q,S2}}  |                            |
%    +----------<----------------+                            |
%  ..............................................................
%    |                           |                            |
%    |                           |       {event,M}            |           
%    |                           +-------------<--------------+
%    | {Contract, {event,M}}     |                            |
%    +----------<----------------+                            |
%    |                           |                            |
%    |                           |                            |
%
%%%   start() -> Pid
%%%   run(Pid, Contract, Client, Server) -> true.


%% The protocol manager terminates a protocol
%% loop(Client, State1, Data, Contract, Manager, Mod)
%%      Client is the process that sens up messages
%%      State1 is our state
%%      Contract is our contract
%%      Mod is our callback module
%%      Data is the local state of the handler

s(X) ->
    {'#S', X}.

start() ->
    spawn_link(fun() -> wait() end).

wait() ->
    process_flag(trap_exit, true),
    receive
	{start, Client, Server, State, Mod} ->
	    loop(Client, Server, State, Mod)
    end.

relay(Client, Server) ->
    receive
	{Client, Msg} ->
	    Server ! {self(), Msg},
	    relay(Client, Server);
	{Server, Msg} ->
	    Client ! {self(), Msg},
	    relay(Client, Server);
	{contract, State, Mod} ->
	    loop(Client, Server, State, Mod);
	Other ->
	    io:format("ContactManager Relay dropped:~p~n",[Other]),
	    relay(Client, Server)
    end.

loop(Client, Server, State1, Mod) ->
    %% io:format("contract_manager ~p waiting for Driver=~p in state:~p ~n",
    %% [Client, self(), State1]),
    receive
	{Client, info} ->
	    Client ! {self(), {infoReply, s(Mod:contract_info())}},
	    loop(Client, Server, State1, Mod);
	{Client, description} ->
	    Client ! {self(),{descriptionReply,s(Mod:contract_description())}},
	    loop(Client, Server, State1, Mod);
	{Client, expect} ->
	    Client ! {self(), {expectReply, get_expect(State1, Mod)}},
	    loop(Client, Server, State1, Mod);
	{Client, state} ->
	    Client ! {self(), {stateReply, State1}},
	    loop(Client, Server, State1, Mod);
	{Client, {rpc, Q}} ->
	    do_rpc(Client, Server, State1, Mod, Q);
	{event, Msg} ->
	    %% io:format("Check dispatch:~p ~p ~p~n",[Msg, State1, Mod]),
	    case checkCallback(Msg, State1, Mod) of
		true ->
		    %% io:format("sending event to client:~p~n",[Msg]),
		    Client ! {self(), {event, Msg}},
		    loop(Client, Server, State1, Mod);
		false ->
		    io:format("**** ILLEGAL CALLBACK"
			      "State: ~p"
			      "msg:~p~n",
			      [State1, Msg]),
		    loop(Client, Server, State1,  Mod)
	    end;
        {'EXIT', Client, Why} ->
	    %% The handler dies *but* we tell the
	    %% manager
	    Server ! {client_has_died, self(),  Why},
	    true;
	X ->
	    io:format("*****PM******Wow:~p (Client=~p)~n",[X,Client]),
	    exit(die)
    end.

do_rpc(Client, Server, State1, Mod, Q) ->
    %% io:format("contract_manager do_rpc~p~n",[Q]),
    %% check contract
    case checkIn(Q, State1, Mod) of
	[] ->
	    io:format("** Client broke contract ~p in State:~p send:~p~n",
		      [Mod, State1, Q]),
	    exit(fatal);
	FSM2 ->
	    %% io:format("contract yes FSM2=~p~n", [FSM2]),
	    %% io:format("I ~p call handle_rpc ~p~n",[self(), Q]),
	    Server ! {self(), {rpc, Q}},
	    receive
		{Server, R1={rpcReply, Reply, State2}} ->
		    %% check contract
		    %% io:format("Contract check reply:~p ~p~n",
		    %% [Reply, State2]),
		    case checkOut(Reply, State2, FSM2, Mod) of
			true ->
			    %% io:format("contract Reply accepted~n"),
			    Client ! {self(), R1},
			    loop(Client,Server,State2,Mod);
			false ->
			    io:format("***** " 
				      "Server broke contract replying "
				      "to client~n"
				      "Original state=~p~n"
				      "Client message=~p~n"
				      "Server response=~p~n",
				      [State1, Q, Reply]),
			    exit(fatal)
		    end;
		{Server, R1={rpcReplyNew, Reply, State2, Mod2}} ->
		    Client ! {self(), {rpcReply, Reply, State2}},
		    io:format("Swop contract to:~p~n",[Mod2]),
		    loop(Client, Server, State2, Mod2)
	    end
    end.

%% returns the set of types for the next message
get_expect(State,Mod) ->
    T = Mod:contract_state(State),
    [Type||{input, Type, _} <- T].

    










