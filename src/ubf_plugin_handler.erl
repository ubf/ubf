%% @doc Implement the plugin server, an intermediate process between
%%      the contract manager process and the server application.
%%
%% The server application may or may not have a separate process (see
%% the diagram below).  The there is no application process(es), then
%% the remote procedure call will be executed by the process executing
%% this module's `loop()' function.
%%
%% This module also implements the plugin manager loop.
%% TODO More detail, please.
%%
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(ubf_plugin_handler).

-export([start_handler/1, start_manager/2, manager/3]).

%%----------------------------------------------------------------------
%% Handler stuff

start_handler(SpawnOpts) ->
    proc_utils:spawn_link_opt_debug(fun() -> wait() end, SpawnOpts, ?MODULE).

wait() ->
    receive
        {start, ContractManager, State, Data, Manager, Mod, TLogMod} ->
            loop(ContractManager, State, Data, Manager, Mod, TLogMod);
        stop ->
            exit({serverPluginHandler, stop})
    end.

loop(Client, State, Data, Manager, Mod, TLogMod) ->
    receive
        {_Pid, {rpc, Q}} ->
            if Manager /= undefined ->
                    case (catch Mod:handlerRpc(State, Q, Data, Manager)) of
                        {Reply, State1, Data1} ->
                            Client ! {self(), {rpcReply, Reply, State1, same}},
                            erlang:garbage_collect(),
                            loop(Client, State1, Data1, Manager, Mod, TLogMod);
                        {changeContract, Reply, Mod1, State1, Data1, Manager1} ->
                            Client ! {self(), {rpcReply, Reply, State,
                                               {new, Mod1, State1}}},
                            loop(Client, State1, Data1, Manager1, Mod1, TLogMod);
                        {'EXIT', Reason} ->
                            contract_manager:do_rpcOutError(
                              Q, State, Mod, Reason, TLogMod),
                            exit({serverPluginHandler, Reason})
                    end;
               true ->
                    case (catch Mod:handlerRpc(Q)) of
                        {changeContract, Reply, Mod1, State1, Data1} ->
                            Client ! {self(), {rpcReply, Reply, State,
                                               {new, Mod1, State1}}},
                            loop(Client, State1, Data1, Manager, Mod1, TLogMod);
                        {'EXIT', Reason} ->
                            contract_manager:do_rpcOutError(
                              Q, State, Mod, Reason, TLogMod),
                            exit({serverPluginHandler, Reason});
                        Reply ->
                            Client ! {self(), {rpcReply, Reply, State, same}},
                            erlang:garbage_collect(),
                            loop(Client, State, Data, Manager, Mod, TLogMod)
                    end
            end;
        {event_out, _} = Event ->
            Client ! Event,
            erlang:garbage_collect(),
            loop(Client, State, Data, Manager, Mod, TLogMod);
        stop ->
            if Manager =/= undefined ->
                    Manager ! {client_has_stopped, self()};
               true ->
                    case (catch Mod:handlerStop(self(), normal, Data)) of
                        {'EXIT', OOps} ->
                            io:format("plug in error:~p~n",[OOps]);
                        _ ->
                            noop
                    end
            end;
        Other ->
            io:format("**** OOOPYikes ...~p (Client=~p)~n",[Other,Client]),
            loop(Client, State, Data, Manager, Mod, TLogMod)
    end.


%%----------------------------------------------------------------------

start_manager(Mod, Args) ->
    proc_utils:spawn_link_debug(fun() -> manager(self(), Mod, Args) end, ?MODULE).

manager(ExitPid, Mod, Args) ->
    process_flag(trap_exit, true),
    {ok, State} = Mod:managerStart(Args),
    manager_loop(ExitPid, Mod, State).

manager_loop(ExitPid, Mod, State) ->
    receive
        {From, {startSession, Service}} ->
            case (catch Mod:startSession(Service, State)) of
                {accept, Mod1, ModManagerPid, State1} ->
                    From ! {self(), {accept, Mod1, ModManagerPid}},
                    manager_loop(ExitPid, Mod, State1);
                {reject, Reason, _State} ->
                    From ! {self(), {reject, Reason}},
                    manager_loop(ExitPid, Mod, State)
            end;
        {client_has_stopped, Pid} ->
            case (catch Mod:handlerStop(Pid, normal, State)) of
                {'EXIT', OOps} ->
                    io:format("plug in error:~p~n",[OOps]),
                    manager_loop(ExitPid, Mod, State);
                State ->
                    manager_loop(ExitPid, Mod, State)
            end;
        {'EXIT', ExitPid, Reason} ->
            exit(Reason);
        {'EXIT', Pid, Reason} ->
            case (catch Mod:handlerStop(Pid, Reason, State)) of
                {'EXIT', OOps} ->
                    io:format("plug in error:~p~n",[OOps]),
                    manager_loop(ExitPid, Mod, State);
                State ->
                    manager_loop(ExitPid, Mod, State)
            end;
        {From, {handler_rpc, Q}} ->
            case (catch Mod:managerRpc(Q, State)) of
                {'EXIT', OOps} ->
                    io:format("plug in error:~p~n",[OOps]),
                    exit(From, bad_ask_manager),
                    manager_loop(ExitPid, Mod, State);
                {Reply, State} ->
                    From ! {handler_rpc_reply, Reply},
                    manager_loop(ExitPid, Mod, State)
            end;
        X ->
            io:format("******Dropping (service manager ~p) self=~p ~p~n",
                      [Mod,self(),X]),
            manager_loop(ExitPid, Mod, State)
    end.
