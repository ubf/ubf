-module(contract_manager).

-export([start/0, start/1]).
-export([do_checkIn/3, do_checkOut/8, do_checkCallback/3]).

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
%% loop(Client, State, Data, Contract, Manager, Mod)
%%      Client is the process that sens up messages
%%      State is our state
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

loop(Client, Server, State, Mod, VerboseRPC) ->
    receive
        {Client, contract} ->
            S = contract(Mod),
            Client ! {self(), {contract, S}},
            loop(Client, Server, State, Mod, VerboseRPC);
        {Client, Q} ->
            do_rpc(Client, Server, State, Mod, Q, VerboseRPC);
        {event, Msg} ->
            case do_checkCallback(Msg, State, Mod) of
                true ->
                    Client ! {self(), {event, Msg}},
                    loop(Client, Server, State, Mod, VerboseRPC);
                false ->
                    loop(Client, Server, State, Mod, VerboseRPC)
            end;
        stop ->
            Server ! stop;
        Why ->
            exit({serverContractManager, Why})
    end.

do_rpc(Client, Server, State, Mod, Q, VerboseRPC) ->
    %% check contract
    case do_checkIn(Q, State, Mod) of
        {error, Reply} ->
            Client ! {self(), {Reply, State}},
            loop(Client, Server, State, Mod, VerboseRPC);
        {ok, {_TLog, FSM}=TLogRef} ->
            if VerboseRPC ->
                    Server ! {self(), {rpc, {Q, FSM}}};
               true ->
                    Server ! {self(), {rpc, Q}}
            end,
            receive
                {Server, {rpcReply, Reply, ReplyState, same}} ->
                    %% check contract
                    {_, NewReply} = do_checkOut(TLogRef, Q, State, Mod, Reply, ReplyState, ReplyState, Mod),
                    Client ! {self(), {NewReply, ReplyState}},
                    loop(Client, Server, ReplyState, Mod, VerboseRPC);
                {Server, {rpcReply, Reply, ReplyState, {new, NewMod, NewState}}} ->
                    %% check contract
                    {_, NewReply} = do_checkOut(TLogRef, Q, State, Mod, Reply, ReplyState, NewState, NewMod),
                    Client ! {self(), {NewReply, NewState}},
                    loop(Client, Server, NewState, NewMod, VerboseRPC);
                stop ->
                    exit(Server, stop)
            end
    end.

do_checkIn(Q, State, Mod) ->
    TLog = contract_manager_tlog:checkIn(Q, State, Mod),
    case checkIn(Q, State, Mod) of
        [] ->
            contract_manager_tlog:checkOut(TLog, Q, State, Mod, undefined, State, Mod, client_broke_contract),
            Expect = Mod:contract_state(State),
            {error, {clientBrokeContract, Q, Expect}};
        FSM ->
            {ok, {TLog, FSM}}
    end.

do_checkOut({TLog, FSM}=_Ref, Q, State, Mod, Reply, ReplyState, NewState, NewMod) ->
    case checkOut(Reply, ReplyState, FSM, Mod) of
        true ->
            contract_manager_tlog:checkOut(TLog, Q, State, Mod, Reply, NewState, NewMod, ok),
            {ok, Reply};
        false ->
            contract_manager_tlog:checkOut(TLog, Q, State, Mod, Reply, NewState, NewMod, server_broke_contract),
            Expect = map(fun(I) -> element(2, I) end, FSM),
            {error, {serverBrokeContract, {Q, Reply}, Expect}}
    end.

do_checkCallback(Msg, State, Mod) ->
    case checkCallback(Msg, State, Mod) of
        true ->
            contract_manager_tlog:checkCallback(Msg, State, Mod, ok),
            true;
        false ->
            contract_manager_tlog:checkCallback(Msg, State, Mod, server_broke_contract),
            false
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
