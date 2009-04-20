-module(ubf_plugin_handler).

%% The plugin handler handles request for the instance of the plugin

-export([start_handler/0, start_manager/2, manager/2]).

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
        {_Pid, {rpc, Q}} ->
            %% io:format("handler (~p) m:~p state:~p q:~p~n",
            %% [self(), Mod, State1, Q]),
            if Manager /= undefined ->
                    case (catch Mod:handlerRpc(State1, Q, Data, Manager)) of
                        {Reply, State2, Data2} ->
                            %% io:format("Reply=~p~n",[Reply]),
                            Client ! {self(), {rpcReply, Reply, State2, same}},
                            loop(Client, State2, Data2, Manager, Mod);
                        {changeContract, Reply, State1,
                         HandlerMod, State2, Data2, ManPid} ->
                            Client ! {self(), {rpcReply, Reply, State1,
                                               {new, HandlerMod, State2}}},
                            loop(Client, State2, Data2, ManPid, HandlerMod);
                        {'EXIT', Why} ->
                            Err = {error, {internalError,handler}},
                            Client ! {self(), {rpcReply, Err, stop}},
                            io:format("**** Yikes ...~p (~p)~n",[Why, Q]),
                            exit(fatal)
                    end;
               true ->
                    DispatchArgs =
                        if is_tuple(Q) ->
                                erlang:tuple_to_list(Q);
                           is_atom(Q) ->
                                [Q];
                           true ->
                                Why1 = {bad_dispatch_args, Q},
                                Err1 = {error, {internalError,handler}},
                                Client ! {self(), {rpcReply, Err1, stop}},
                                io:format("**** Yikes ...~p (~p)~n",[Why1, Q]),
                                exit(fatal)
                        end,
                    case (catch Mod:handlerDispatch(hd(DispatchArgs), Data)) of
                        {DispatchMod, DispatchFun, Data2} ->
                            case (catch erlang:apply(DispatchMod, DispatchFun, tl(DispatchArgs))) of
                                {'EXIT', Why2} ->
                                    Err2 = {error, {internalError,handler}},
                                    Client ! {self(), {rpcReply, Err2, stop}},
                                    io:format("**** Yikes ...~p (~p)~n",[Why2, Q]),
                                    exit(fatal);
                                Reply ->
                                    %% io:format("Reply=~p~n",[Reply]),
                                    Client ! {self(), {rpcReply, Reply, State1, same}},
                                    loop(Client, State1, Data2, Manager, Mod)
                            end;
                        {'EXIT', Why3} ->
                            Err3 = {error, {internalError,handler}},
                            Client ! {self(), {rpcReply, Err3, stop}},
                            io:format("**** Yikes ...~p (~p)~n",[Why3, Q]),
                            exit(fatal);
                        Data2 ->
                            %% special case - hard-coded not_implemented reply
                            Reply = not_implemented,
                            %% io:format("Reply=~p~n",[Reply]),
                            Client ! {self(), {rpcReply, Reply, State1, same}},
                            loop(Client, State1, Data2, Manager, Mod)
                    end
            end;
        {event, X} ->
            Client ! {event, X},
            loop(Client, State1, Data, Manager, Mod);
        {client_has_died, Client, Why} ->
            if Manager /= undefined ->
                    Manager ! {client_has_died, self(), Why};
               true ->
                    case (catch Mod:handlerStop(Why, Data)) of
                        {'EXIT', OOps} ->
                            io:format("plug in error:~p~n",[OOps]);
                        _ ->
                            noop
                    end
            end;
        {'EXIT',_,normal} ->
            loop(Client, State1, Data, Manager, Mod);
        Other ->
            io:format("**** OOOPYikes ...~p (Client=~p)~n",[Other,Client]),
            loop(Client, State1, Data, Manager, Mod)
    end.


%%----------------------------------------------------------------------

start_manager(Mod, Args) ->
    spawn_link_debug({manager,Mod, Args},
                     fun() ->
                             manager(Mod, Args)
                     end).

manager(Mod, Args) ->
    process_flag(trap_exit, true),
    {ok, State} = Mod:managerStart(Args),
    %% io:format("process ~p is manager for ~p~n",[self(), Mod]),
    manager_loop(Mod, State).

manager_loop(Mod, State) ->
    %% io:format("~p manager waiting for something to do~n", [Mod]),
    receive
        {From, {startService, Service}} ->
            %% io:format("HHHH startService~p~n",[Service]),
            case (catch Mod:startService(Service, State)) of
                {accept, HandlerMod, ModManagerPid, State2} ->
                    %% io:format("returning accept ~p~n",[Mod]),
                    From ! {self(), {accept,HandlerMod, ModManagerPid}},
                    manager_loop(Mod, State2);
                {reject, Why, _State1} ->
                    %% io:format("rejected~n"),
                    From ! {self(), {reject, Why}},
                    manager_loop(Mod, State)
            end;
        {client_has_died, Pid, Why} ->
            %% io:format("Handler caught child_has_died:~p~n",[Pid]),
            case (catch Mod:handlerStop(Pid, Why, State)) of
                {'EXIT', OOps} ->
                    io:format("plug in error:~p~n",[OOps]),
                    manager_loop(Mod, State);
                State1 ->
                    manager_loop(Mod, State1)
            end;
        {From, {handler_rpc, Q}} ->
            case (catch Mod:managerRpc(Q, State)) of
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
