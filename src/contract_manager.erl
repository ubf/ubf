-module(contract_manager).

-export([start/0, start/1]).
-import(contracts, [checkIn/3, checkOut/4, checkCallback/3]).
-import(lists, [map/2]).

-include("ubf.hrl").


%%%  Client                     Contract                     Server
%%%    |                           |                            |
%%%    |                           |                            |
%%%    |                           |                            |
%%%    |  {Driver, {rpc,Q}}        |                            |
%%%    +---------->----------------+    {Contract, Q}           |
%%%    |                           +------------->--------------+
%%%    |                           |                            |
%%%    |                           |                            |
%%%    |                           |      {reply,R,S2}          |
%%%    |                           +-------------<--------------+
%%%    | {Contract, {reply,Q,S2}}  |                            |
%%%    +----------<----------------+                            |
%%%  ..............................................................
%%%    |                           |                            |
%%%    |                           |       {event,M}            |
%%%    |                           +-------------<--------------+
%%%    | {Contract, {event,M}}     |                            |
%%%    +----------<----------------+                            |
%%%    |                           |                            |
%%%    |                           |                            |
%%%
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
    start(false).

start(VerboseRPC) ->
    spawn_link(fun() -> wait(VerboseRPC) end).

wait(VerboseRPC) ->
    process_flag(trap_exit, true),
    receive
        {start, Client, Server, State, Mod} ->
            loop(Client, Server, State, Mod, VerboseRPC)
    end.

loop(Client, Server, State1, Mod, VerboseRPC) ->
    %% io:format("contract_manager ~p waiting for Driver=~p in state:~p ~n",
    %% [Client, self(), State1]),
    receive
        {Client, contract} ->
            S = contract(Mod),
            Client ! {self(), {contract, S}},
            loop(Client, Server, State1, Mod, VerboseRPC);
        {Client, Q} ->
            do_rpc(Client, Server, State1, Mod, Q, VerboseRPC);
        {event, Msg} ->
            %% io:format("Check dispatch:~p ~p ~p~n",[Msg, State1, Mod]),
            case checkCallback(Msg, State1, Mod) of
                true ->
                    %% io:format("sending event to ubf_client:~p~n",[Msg]),
                    Client ! {self(), {event, Msg}},
                    loop(Client, Server, State1, Mod, VerboseRPC);
                false ->
                    io:format("**** ILLEGAL CALLBACK"
                              "State: ~p"
                              "msg:~p~n",
                              [State1, Msg]),
                    loop(Client, Server, State1,  Mod, VerboseRPC)
            end;
        {'EXIT', Client, Why} ->
            %% The handler dies *but* we tell the
            %% manager
            Server ! {client_has_died, self(),  Why},
            true;
        stop ->
            Server ! {client_has_died, self(),  normal},
            true;
        X ->
            io:format("*****PM******Wow:~p (Client=~p)~n",[X,Client]),
            exit(die)
    end.

do_rpc(Client, Server, State1, Mod, Q, VerboseRPC) ->
    %% io:format("contract_manager do_rpc~p~n",[Q]),
    %% check contract
    case checkIn(Q, State1, Mod) of
        [] ->
            Expect = Mod:contract_state(State1),
            io:format("***** "
                      "Client broke contract ~p "
                      "to server~n"
                      "State=~p~n"
                      "Client message=~p~n"
                      "Expecting type=~p~n",
                      [Mod, State1, Q, Expect]),
            Client ! {self(), {{clientBrokeContract, Q, Expect}, State1}},
            Client ! stop;
        FSM2 ->
            %% io:format("contract yes FSM2=~p~n", [FSM2]),
            %% io:format("I ~p call handle_rpc ~p~n",[self(), Q]),
            if VerboseRPC ->
                    Server ! {self(), {rpc, {Q, FSM2}}};
               true ->
                    Server ! {self(), {rpc, Q}}
            end,
            receive
                {Server, {rpcReply, Reply, State2, Next}} ->
                    %% check contract
                    %% io:format("Contract check reply:~p ~p~n",
                    %% [Reply, State2]),
                    case checkOut(Reply, State2, FSM2, Mod) of
                        true ->
                            %% io:format("contract Reply accepted~n"),
                            case Next of
                                same ->
                                    Client ! {self(), {Reply, State2}},
                                    loop(Client, Server, State2, Mod, VerboseRPC);
                                {new, NewMod, State3} ->
                                    Client ! {self(), {Reply, State3}},
                                    loop(Client, Server, State3, NewMod, VerboseRPC)
                            end;
                        false ->
                            Expect = map(fun(I) -> element(2, I) end, FSM2),
                            io:format("***** "
                                      "Server broke contract ~p replying "
                                      "to client~n"
                                      "Original state=~p~n"
                                      "Client message=~p~n"
                                      "Server response=~p~n"
                                      "Expecting type=~p~n",
                                      [Mod, State1, Q, Reply, Expect]),
                            Client ! {self(), {{serverBrokeContract, {Q, Reply}, Expect}, State1}},
                            Client ! stop
                    end
            end
    end.


contract(Mod) ->
    {{name,?S(Mod:contract_name())},
     {states,
      map(fun(S) ->
                  {S, Mod:contract_state(S)}
          end, Mod:contract_states())},
     {anystate, Mod:contract_anystate()},
     {types,
      map(fun(S) ->
                  {S, Mod:contract_type(S)}
          end, Mod:contract_types())}}.
