%% @doc Contract manager server
%%
%% This module implements the contract manager server process, which
%% runs on the Erlang server side, between the UBF driver (or the
%% driver for whatever protocol is being used "over the wire",
%% e.g. JSON-RPC) and the plugin handler server.
%%
%% <img src="../priv/doc/ubf-flow-01.png"></img>
%%
%% == Message Passing ==
%%
%% In the diagram below, the "Client" is actually the UBF driver
%% (using UBF, EBF, JSON, JSON-RPC, or other transport protocol) that
%% acts on behalf of the remote client.  The "Server" is actually the
%% plugin handler server, which acts as an intermediary between the
%% actual server application.
%%
%% <img src="../priv/doc/contract_manager-01.png"></img>

-module(contract_manager).

-export([start/0, start/1]).
-export([do_checkRPCIn/3, do_checkRPCOut/8, do_checkEventOut/3]).
-export([do_checkRPCOutError/4, do_checkRPCOutError/5]).

-import(contracts, [checkRPCIn/3, checkRPCOut/4, checkEventOut/3]).
-import(lists, [map/2]).

-include("ubf.hrl").


%%%  Client                     Contract                    Server
%%%    |                           |                          |
%%%    |                           |                          |
%%%    |                           |                          |
%%%    |  {Driver, {rpc,Q}}        |                          |
%%%    +---------->----------------+    {Contract, Q}         |
%%%    |                           +------------->------------+
%%%    |                           |                          |
%%%    |                           |                          |
%%%    |                           |      {reply,R,S1}        |
%%%    |                           +-------------<------------+
%%%    | {Contract, {reply,R,S1}}  |                          |
%%%    +----------<----------------+                          |
%%%    |                           |                          |
%%%  ............................................................
%%%    |                           |                          |
%%%    |                           |      {event_out,M}       |
%%%    |                           +-------------<------------+
%%%    | {Contract, {event_out,M}} |                          |
%%%    +----------<----------------+                          |
%%%    |                           |                          |
%%%  ............................................................
%%%    |                           |                          |
%%%    | {Contract, {event_in,M}}  |                          |
%%%    +---------->----------------+                          |
%%%    |                           |      {event_in,M}        |
%%%    |                           +------------->------------+
%%%    |                           |                          |


%%%  @spec () -> pid()

start() ->
    start(false).

%%%  @spec (bool()) -> pid()

start(VerboseRPC) ->
    proc_utils:spawn_link_debug(fun() -> wait(VerboseRPC) end, ?MODULE).

wait(VerboseRPC) ->
    receive
        {start, Client, Server, State, Mod} ->
            loop(Client, Server, State, Mod, VerboseRPC);
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
%%      <li> VerboseRPC is a boolean </li>
%% </ul>
%%
%% NOTE: The protocol manager terminates a protocol session.

loop(Client, Server, State, Mod, VerboseRPC) ->
    receive
        {Client, contract} ->
            S = contract(Mod),
            Client ! {self(), {contract, S}},
            loop(Client, Server, State, Mod, VerboseRPC);
        {Client, Q} ->
            do_rpc(Client, Server, State, Mod, Q, VerboseRPC);
        {event_out, Msg} = Event ->
            case do_checkEventOut(Msg, State, Mod) of
                true ->
                    Client ! {self(), Event},
                    loop(Client, Server, State, Mod, VerboseRPC);
                false ->
                    loop(Client, Server, State, Mod, VerboseRPC)
            end;
        stop ->
            Server ! stop;
        Reason ->
            exit({serverContractManager, Reason})
    end.

do_rpc(Client, Server, State, Mod, Q, VerboseRPC) ->
    %% check contract
    case do_checkRPCIn(Q, State, Mod) of
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
                    {_, NewReply} = do_checkRPCOut(TLogRef, Q, State, Mod, Reply, ReplyState, ReplyState, Mod),
                    Client ! {self(), {NewReply, ReplyState}},
                    loop(Client, Server, ReplyState, Mod, VerboseRPC);
                {Server, {rpcReply, Reply, ReplyState, {new, NewMod, NewState}}} ->
                    %% check contract
                    {_, NewReply} = do_checkRPCOut(TLogRef, Q, State, Mod, Reply, ReplyState, NewState, NewMod),
                    Client ! {self(), {NewReply, NewState}},
                    loop(Client, Server, NewState, NewMod, VerboseRPC);
                stop ->
                    exit(Server, stop)
            end
    end.

do_checkRPCIn(Q, State, Mod) ->
    TLog = contract_manager_tlog:checkRPCIn(Q, State, Mod),
    case checkRPCIn(Q, State, Mod) of
        [] ->
            contract_manager_tlog:checkRPCOut(TLog, Q, State, Mod, undefined, State, Mod, client_broke_contract),
            Expect = Mod:contract_state(State),
            {error, {clientBrokeContract, Q, Expect}};
        FSM ->
            {ok, {TLog, FSM}}
    end.

do_checkRPCOut({TLog, FSM}=_Ref, Q, State, Mod, Reply, ReplyState, NewState, NewMod) ->
    case checkRPCOut(Reply, ReplyState, FSM, Mod) of
        true ->
            contract_manager_tlog:checkRPCOut(TLog, Q, State, Mod, Reply, NewState, NewMod, ok),
            {ok, Reply};
        false ->
            contract_manager_tlog:checkRPCOut(TLog, Q, State, Mod, Reply, NewState, NewMod, server_broke_contract),
            Expect = map(fun(I) -> element(2, I) end, FSM),
            {error, {serverBrokeContract, {Q, Reply}, Expect}}
    end.

do_checkEventOut(Msg, State, Mod) ->
    case checkEventOut(Msg, State, Mod) of
        true ->
            contract_manager_tlog:checkEventOut(Msg, State, Mod, ok),
            true;
        false ->
            contract_manager_tlog:checkEventOut(Msg, State, Mod, server_broke_contract),
            false
    end.

do_checkRPCOutError(Q, State, Mod, Error) ->
    contract_manager_tlog:checkRPCOutError(Q, State, Mod, Error).

do_checkRPCOutError({TLog, _FSM}=_Ref, Q, State, Mod, Error) ->
    contract_manager_tlog:checkRPCOutError(TLog, Q, State, Mod, Error).

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
