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
    proc_utils:spawn_link_debug(fun() -> wait(VerboseRPC) end, ?MODULE).

wait(VerboseRPC) ->
    receive
        {start, Client, Server, State, Mod} ->
            loop(Client, Server, State, Mod, VerboseRPC);
        stop ->
            exit({serverContractManager, stop})
    end.

loop(Client, Server, State1, Mod, VerboseRPC) ->
    receive
        {Client, contract} ->
            S = contract(Mod),
            Client ! {self(), {contract, S}},
            loop(Client, Server, State1, Mod, VerboseRPC);
        {Client, Q} ->
            do_rpc(Client, Server, State1, Mod, Q, VerboseRPC);
        {event, Msg} ->
            case checkCallback(Msg, State1, Mod) of
                true ->
                    Client ! {self(), {event, Msg}},
                    loop(Client, Server, State1, Mod, VerboseRPC);
                false ->
                    io:format("**** ILLEGAL CALLBACK"
                              "State: ~p"
                              "msg:~p~n",
                              [State1, Msg]),
                    loop(Client, Server, State1,  Mod, VerboseRPC)
            end;
        stop ->
            Server ! stop;
        Why ->
            exit({serverContractManager, Why})
    end.

tlogCheckIn() ->
    now().
tlogCheckOut(StartTime, Mod, Q, Reply, Status0) ->
    gmt_tlog_h:tlog_ubf(StartTime, Mod, Q, Reply, Status0).

do_rpc(Client, Server, State1, Mod, Q, VerboseRPC) ->
    %% check contract
    StartTime = tlogCheckIn(),
    case checkIn(Q, State1, Mod) of
        [] ->
            Expect = Mod:contract_state(State1),
            Client ! {self(), {{clientBrokeContract, Q, Expect}, State1}},
            tlogCheckOut(StartTime, Mod, Q, undefined, client_broke_contract),
            loop(Client, Server, State1, Mod, VerboseRPC);
        FSM2 ->
            if VerboseRPC ->
                    Server ! {self(), {rpc, {Q, FSM2}}};
               true ->
                    Server ! {self(), {rpc, Q}}
            end,
            receive
                {Server, {rpcReply, Reply, State2, Next}} ->
                    %% check contract
                    case checkOut(Reply, State2, FSM2, Mod) of
                        true ->
                            case Next of
                                same ->
                                    Client ! {self(), {Reply, State2}},
                                    tlogCheckOut(StartTime, Mod, Q, Reply, ok),
                                    loop(Client, Server, State2, Mod, VerboseRPC);
                                {new, NewMod, State3} ->
                                    Client ! {self(), {Reply, State3}},
                                    tlogCheckOut(StartTime, Mod, Q, Reply, ok),
                                    loop(Client, Server, State3, NewMod, VerboseRPC)
                            end;
                        false ->
                            Expect = map(fun(I) -> element(2, I) end, FSM2),
                            Client ! {self(), {{serverBrokeContract, {Q, Reply}, Expect}, State1}},
                            tlogCheckOut(StartTime, Mod, Q, Reply, server_broke_contract),
                            loop(Client, Server, State1, Mod, VerboseRPC)
                    end;
                stop ->
                    exit(Server, stop)
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
