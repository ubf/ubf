%% @doc Contract manager server
%%
%% This module implements the contract manager server process, which
%% runs on the Erlang server side, between the UBF driver (or the
%% driver for whatever protocol is being used "over the wire",
%% e.g. JSON-RPC) and the plugin handler server.
%%
%% <img src="ubf-flow-01.png"></img>
%%
%% == Message Passing ==
%%
%% In the diagram below, the "Client" is actually the UBF driver
%% (using UBF, EBF, JSON, JSON-RPC, or other transport protocol) that
%% acts on behalf of the remote client.  The "Server" is actually the
%% plugin handler server, which acts as an intermediary between the
%% actual server application.
%%
%% <img src="contract_manager-01.png"></img>

-module(contract_manager).

-export([start/1, start/3]).

-export([do_rpcIn/4, do_rpcOut/9]).
-export([do_rpcOutError/5, do_rpcOutError/6]).

-export([do_lpcIn/4, do_lpcOut/9]).
-export([do_lpcOutError/6]).

-export([do_eventOut/4, do_eventIn/4]).

-import(contracts, [checkRPCIn/3, checkRPCOut/4, checkEventOut/3, checkEventIn/3]).
-import(lists, [map/2]).

-include("ubf.hrl").


%%%  Client                     Contract                    Server
%%%    |                           |                          |
%%%    |                           |                          |
%%%    |                           |                          |
%%%    |   {Driver,{rpc,Q}}        |                          |
%%%    +---------->----------------+     {Contract,Q}         |
%%%    |                           +------------->------------+
%%%    |                           |                          |
%%%    |                           |                          |
%%%    |                           |      {reply,R,S1}        |
%%%    |                           +-------------<------------+
%%%    |  {Contract,{reply,R,S1}}  |                          |
%%%    +----------<----------------+                          |
%%%    |                           |                          |
%%%  ............................................................
%%%    |                           |                          |
%%%    |                           |      {event_out,M}       |
%%%    |                           +-------------<------------+
%%%    |  {Contract,{event_out,M}} |                          |
%%%    +----------<----------------+                          |
%%%    |                           |                          |
%%%  ............................................................
%%%    |                           |                          |
%%%    |  {Contract,{event_in,M}}  |                          |
%%%    +---------->----------------+                          |
%%%    |                           |      {event_in,M}        |
%%%    |                           +------------->------------+
%%%    |                           |                          |


%%%  @spec (list()) -> pid()

start(SpawnOpts) when is_list(SpawnOpts) ->
    start(false, false, SpawnOpts).

%%%  @spec (bool(), bool(), list()) -> pid()

start(SimpleRPC, VerboseRPC, SpawnOpts) ->
    proc_utils:spawn_link_opt_debug(fun() -> wait(SimpleRPC, VerboseRPC) end, SpawnOpts, ?MODULE).

wait(SimpleRPC, VerboseRPC) ->
    receive
        {start, Client, Server, State, Mod, TLogMod} ->
            loop(Client, Server, State, Mod, SimpleRPC, VerboseRPC, TLogMod);
        stop ->
            exit({serverContractManager, stop})
    end.

%% @doc Main loop for contract manager process.
%%
%% <ul>
%%      <li> Client is the process that sens up messages </li>
%%      <li> State is our state </li>
%%      <li> Contract is our contract </li>
%%      <li> Mod is our callback module </li>
%%      <li> SimpleRPC is a boolean </li>
%%      <li> VerboseRPC is a boolean </li>
%% </ul>
%%
%% NOTE: The protocol manager terminates a protocol session.

loop(Client, Server, State, Mod, SimpleRPC, VerboseRPC, TLogMod) ->
    receive
        {Client, contract} ->
            S = contract(Mod),
            Client ! {self(), {contract, S}},
            loop(Client, Server, State, Mod, SimpleRPC, VerboseRPC, TLogMod);
        {Client, {event_in, Msg}=Event} ->
            case do_eventIn(Msg, State, Mod, TLogMod) of
                {true,TLog} ->
                    Server ! {self(), Event},
                    do_txlog(TLog),
                    loop(Client, Server, State, Mod, SimpleRPC, VerboseRPC, TLogMod);
                {false,TLog} ->
                    do_txlog(TLog),
                    loop(Client, Server, State, Mod, SimpleRPC, VerboseRPC, TLogMod)
            end;
        {Client, Q} ->
            do_rpc(Client, Server, State, Mod, Q, SimpleRPC, VerboseRPC, TLogMod);
        {Server, {event_out, Msg}=Event} ->
            case do_eventOut(Msg, State, Mod, TLogMod) of
                {true,TLog} ->
                    Client ! {self(), Event},
                    do_txlog(TLog),
                    loop(Client, Server, State, Mod, SimpleRPC, VerboseRPC, TLogMod);
                {false,TLog} ->
                    do_txlog(TLog),
                    loop(Client, Server, State, Mod, SimpleRPC, VerboseRPC, TLogMod)
            end;

        stop ->
            Server ! stop;
        Reason ->
            exit({serverContractManager, Reason})
    end.

do_rpc(Client, Server, State, Mod, Q, SimpleRPC, VerboseRPC, TLogMod) ->
    %% check contract
    case do_rpcIn(Q, State, Mod, TLogMod) of
        {error, Reply, TLog} ->
            _ = if SimpleRPC ->
                        Client ! {self(), Reply};
                   true ->
                        Client ! {self(), {Reply, State}}
                end,
            do_txlog(TLog),
            loop(Client, Server, State, Mod, SimpleRPC, VerboseRPC, TLogMod);
        {ok, {_TLog, FSM}=TLogRef} ->
            _ = if VerboseRPC ->
                        Server ! {self(), {rpc, {Q, FSM}}};
                   true ->
                        Server ! {self(), {rpc, Q}}
                end,
            receive
                {Server, {rpcReply, Reply, ReplyState, same}} ->
                    %% check contract
                    {_, NewReply, TLog} = do_rpcOut(TLogRef, Q, State, Mod, Reply, ReplyState, ReplyState, Mod, TLogMod),
                    _ = if SimpleRPC ->
                                Client ! {self(), NewReply};
                           true ->
                                Client ! {self(), {NewReply, ReplyState}}
                        end,
                    do_txlog(TLog),
                    loop(Client, Server, ReplyState, Mod, SimpleRPC, VerboseRPC, TLogMod);
                {Server, {rpcReply, Reply, ReplyState, {new, NewMod, NewState}}} ->
                    %% check contract
                    {_, NewReply, TLog} = do_rpcOut(TLogRef, Q, State, Mod, Reply, ReplyState, NewState, NewMod, TLogMod),
                    _ = if SimpleRPC ->
                                Client ! {self(), NewReply};
                           true ->
                                Client ! {self(), {NewReply, NewState}}
                        end,
                    do_txlog(TLog),
                    loop(Client, Server, NewState, NewMod, SimpleRPC, VerboseRPC, TLogMod);
                stop ->
                    exit(Server, stop)
            end
    end.

do_rpcIn(Q, State, Mod, TLogMod) ->
    TLog = contract_manager_tlog:rpcIn(TLogMod, Q, State, Mod),
    case checkRPCIn(Q, State, Mod) of
        [] ->
            TLog1 = contract_manager_tlog:rpcOut(TLogMod, TLog, Q, State, Mod, undefined, State, Mod, client_broke_contract),
            Expect = Mod:contract_state(State),
            {error, {clientBrokeContract, Q, Expect}, TLog1};
        FSM ->
            {ok, {TLog, FSM}}
    end.

do_rpcOut({TLog, FSM}=_Ref, Q, State, Mod, Reply, ReplyState, NewState, NewMod, TLogMod) ->
    case checkRPCOut(Reply, ReplyState, FSM, Mod) of
        true ->
            TLog1 = contract_manager_tlog:rpcOut(TLogMod, TLog, Q, State, Mod, Reply, NewState, NewMod, ok),
            {ok, Reply, TLog1};
        false ->
            TLog1 = contract_manager_tlog:rpcOut(TLogMod, TLog, Q, State, Mod, Reply, NewState, NewMod, server_broke_contract),
            Expect = map(fun(I) -> element(2, I) end, FSM),
            {error, {serverBrokeContract, {Q, Reply}, Expect}, TLog1}
    end.

do_rpcOutError(Q, State, Mod, Error, TLogMod) ->
    TLog = contract_manager_tlog:rpcOutError(TLogMod, Q, State, Mod, Error),
    do_txlog(TLog).

do_rpcOutError({TLog, _FSM}=_Ref, Q, State, Mod, Error, TLogMod) ->
    TLog = contract_manager_tlog:rpcOutError(TLogMod, TLog, Q, State, Mod, Error),
    do_txlog(TLog).

do_txlog(TLog) ->
    ok = contract_manager_tlog:rpcFinish(TLog).

do_lpcIn(Q, State, Mod, TLogMod) ->
    TLog = contract_manager_tlog:lpcIn(TLogMod, Q, State, Mod),
    case checkRPCIn(Q, State, Mod) of
        [] ->
            contract_manager_tlog:lpcOut(TLogMod, TLog, Q, State, Mod, undefined, State, Mod, client_broke_contract),
            Expect = Mod:contract_state(State),
            {error, {clientBrokeContract, Q, Expect}};
        FSM ->
            {ok, {TLog, FSM}}
    end.

do_lpcOut({TLog, FSM}=_Ref, Q, State, Mod, Reply, ReplyState, NewState, NewMod, TLogMod) ->
    case checkRPCOut(Reply, ReplyState, FSM, Mod) of
        true ->
            contract_manager_tlog:lpcOut(TLogMod, TLog, Q, State, Mod, Reply, NewState, NewMod, ok),
            {ok, Reply};
        false ->
            contract_manager_tlog:lpcOut(TLogMod, TLog, Q, State, Mod, Reply, NewState, NewMod, server_broke_contract),
            Expect = map(fun(I) -> element(2, I) end, FSM),
            {error, {serverBrokeContract, {Q, Reply}, Expect}}
    end.

do_lpcOutError({TLog, _FSM}=_Ref, Q, State, Mod, Error, TLogMod) ->
    contract_manager_tlog:lpcOutError(TLogMod, TLog, Q, State, Mod, Error).

do_eventOut(Msg, State, Mod, TLogMod) ->
    case checkEventOut(Msg, State, Mod) of
        true ->
            TLog = contract_manager_tlog:eventOut(TLogMod, Msg, State, Mod, ok),
            {true, TLog};
        false ->
            TLog = contract_manager_tlog:eventOut(TLogMod, Msg, State, Mod,
                                                  server_broke_contract),
            {false, TLog}
    end.

do_eventIn(Msg, State, Mod, TLogMod) ->
    case checkEventIn(Msg, State, Mod) of
        true ->
            TLog = contract_manager_tlog:eventIn(TLogMod, Msg, State, Mod, ok),
            {true, TLog};
        false ->
            TLog = contract_manager_tlog:eventIn(TLogMod, Msg, State, Mod,
                                                 client_broke_contract),
            {false, TLog}
    end.

contract(Mod) ->
    {{name,?S(Mod:contract_name())},
     {states,map(fun(S) -> {S, Mod:contract_state(S)} end, Mod:contract_states())},
     {anystate,Mod:contract_anystate()},
     {types,map(fun(S) -> {S, Mod:contract_type(S)} end, Mod:contract_types())}}.
