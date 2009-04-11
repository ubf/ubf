-module(contract_manager).

-export([start/0]).
-import(contracts, [checkIn/3, checkOut/4, checkCallback/3]).
-import(lists, [map/2, member/2]).
-include("contract.hrl").

-define(S(X), {'#S',X}).
s(X) -> {'#S', X}.



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
	{Client, help} ->
	    Client ! {self(),s(help())},
	    loop(Client, Server, State1, Mod);
	{Client, info} ->
	    Client ! {self(),s(Mod:contract_info())},
	    loop(Client, Server, State1, Mod);
	{Client, description} ->
	    Client ! {self(),s(Mod:contract_description())},
	    loop(Client, Server, State1, Mod);
	{Client, services} ->
	    S = Mod:contract_services(),
	    S1 = map(fun(I) -> s(I) end, S),
	    Client ! {self(), {services, S1}},
	    loop(Client, Server, State1, Mod);
	{Client, contract} ->
	    S = contract(Mod),
	    Client ! {self(), {contract, S}},
	    loop(Client, Server, State1, Mod);
	{Client, {start, ?S(Service)}} ->
	    S = Mod:contract_services(),
	    case member(Service, S) of
		true ->
		    Server ! {self(), {startService, Service}},
		    receive
			{Server, {accept, HandlerMod, ManagerPid}} ->
			    Info = HandlerMod:contract_info(),
			    Client ! {self(), {ok, s(Info)}},
			    loop(Client, Server, start, HandlerMod);
			{Server, {reject, Why}} ->
			    Client ! {self(), {error,Why}},
			    loop(Client, Server, State1, Mod)
		    end;
		false ->
		    Client ! {self(), noSuchService},
		    loop(Client, Server, State1, Mod)
	    end,
	    loop(Client, Server, State1, Mod);
	{Client, expect} ->
	    Client ! {self(),{expectReply, get_expect(State1, Mod)}},
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
	    Expect = Mod:contract_state(State1),
	    Client ! {self(),
		      {clientBrokeContract, {state, State1}, Expect}},
	    io:format("Expecting:~p~n",[Expect]),
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

contract(Mod) ->    
    {{name,s(Mod:contract_name())},
     {info, s(Mod:contract_info())},
     {description, s(Mod:contract_description())},
     {services, map(fun(I) -> s(I) end, Mod:contract_services())},
     {states,
      map(fun(S) ->
		  {S, Mod:contract_state(S)}
	  end, Mod:contract_states())},
     {types,
      map(fun(S) ->
		  {S, Mod:contract_type(S)}
	  end, Mod:contract_types())}}.

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

{'start', \"Name\"} Name should be one of the names in the
                  services list

Warning without reading the documentation you might find the output from
some of these commands difficult to understand :-)

".

     
			    










