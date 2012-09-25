%%% The MIT License
%%%
%%% Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (C) 2002 by Joe Armstrong
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

%%% @doc Implement the plugin server, an intermediate process between
%%% the contract manager process and the server application.
%%%
%%% The server application may or may not have a separate process (see
%%% the diagram below).  The there is no application process(es), then
%%% the remote procedure call will be executed by the process
%%% executing this module\'s +loop()+ function.
%%%
%%% This module also implements the plugin manager loop.
%%%
%%% @TODO More detail, please.

-module(ubf_plugin_handler).

-export([start_handler/5, start_manager/2, manager/3, ask_manager/2]).
-export([sendEvent/2, install_default_handler/1, install_handler/2]).

-include("ubf.hrl").


%%----------------------------------------------------------------------
%% Handler stuff

start_handler(MetaMod, Mod, Server, StatelessRPC, SpawnOpts) ->
    Parent = self(),
    Fun = fun() -> wait(Parent, MetaMod, Mod, Server, StatelessRPC) end,
    proc_utils:spawn_link_opt_debug(Fun, SpawnOpts, ?MODULE).

wait(Parent, MetaMod, Mod, Server, StatelessRPC) ->
    {State, Data, Manager} = handler_state(MetaMod, Mod, Server, StatelessRPC),
    Parent ! {state, self(), State},

    receive
        {start, ContractManager, TLogMod} ->
            loop(ContractManager, State, Data, Manager, Mod, TLogMod, fun drop_fun/1);
        stop ->
            exit({serverPluginHandler, stop})
    end.

handler_state(MetaMod, Mod, Server, StatelessRPC) ->
    %%  state, data, and pid
    if MetaMod =/= Mod ->
            StartSession = {startSession, ?S(Mod:contract_name()), []},
            case StatelessRPC of
                false ->
                    case MetaMod:handlerRpc(start, StartSession, [], Server) of
                        {changeContract, {ok, _}, Mod, State, Data, Manager} ->
                            noop;
                        {{error, Reason}, _, _} ->
                            State = Data = Manager = undefined,
                            exit(Reason)
                    end;
                true ->
                    case MetaMod:handlerRpc(StartSession) of
                        {changeContract, {ok, _}, Mod, State, Data} ->
                            Manager = undefined,
                            noop;
                        {error, Reason} ->
                            State = Data = Manager = undefined,
                            exit(Reason)
                    end
            end;
       true ->
            State = start,
            Data = [],
            Manager = Server
    end,
    {State, Data, Manager}.

loop(Client, State, Data, Manager, Mod, TLogMod, Fun) ->
    receive
        {Client, {rpc, Q}} ->
            if Manager /= undefined ->
                    case (catch Mod:handlerRpc(State, Q, Data, Manager)) of
                        {Reply, State1, Data1} ->
                            Client ! {self(), {rpcReply, Reply, State1, same}},
                            loop(Client, State1, Data1, Manager, Mod, TLogMod, Fun);
                        {changeContract, Reply, Mod1, State1, Data1, Manager1} ->
                            Client ! {self(), {rpcReply, Reply, State,
                                               {new, Mod1, State1}}},
                            loop(Client, State1, Data1, Manager1, Mod1, TLogMod, Fun);
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
                            loop(Client, State1, Data1, Manager, Mod1, TLogMod, Fun);
                        {'EXIT', Reason} ->
                            contract_manager:do_rpcOutError(
                              Q, State, Mod, Reason, TLogMod),
                            exit({serverPluginHandler, Reason});
                        Reply ->
                            Client ! {self(), {rpcReply, Reply, State, same}},
                            loop(Client, State, Data, Manager, Mod, TLogMod, Fun)
                    end
            end;
        {Client, {event_in, Event}} ->
            %% asynchronous event handler
            Fun1 = Fun(Event),
            loop(Client, State, Data, Manager, Mod, TLogMod, Fun1);
        {event_out, _}=Event ->
            Client ! {self(), Event},
            loop(Client, State, Data, Manager, Mod, TLogMod, Fun);
        {install, Fun1} ->
            loop(Client, State, Data, Manager, Mod, TLogMod, Fun1);
        {From, {install, Fun1}} ->
            From ! {self(), ack},
            loop(Client, State, Data, Manager, Mod, TLogMod, Fun1);
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
            loop(Client, State, Data, Manager, Mod, TLogMod, Fun)
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

-spec ask_manager(Manager::pid(), Call::term()) -> Reply::term().
ask_manager(Manager, Q) ->
    Manager ! {self(), {handler_rpc, Q}},
    receive
        {handler_rpc_reply, R} ->
            R
    end.


%%----------------------------------------------------------------------

%% @doc Send an asynchronous UBF message.

-spec sendEvent(Handler::pid(), Cast::term()) -> ok | no_return().
sendEvent(Pid, Msg) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        false ->
            erlang:error(badpid);
        true ->
            Pid ! {event_out, Msg},
            ok
    end.

%% @doc Install a default handler function (callback-style) for
%% asynchronous UBF messages.
%%
%% The default handler function, drop_fun/1, does nothing.

-spec install_default_handler(Handler::pid()) -> ack.
install_default_handler(Pid) ->
    install_handler(Pid, fun drop_fun/1).

%% @doc Install a handler function (callback-style) for asynchronous
%% UBF messages.
%%
%% The handler fun Fun should be a function of arity 1.  When an
%% asynchronous UBF message is received, the callback function will be
%% called with the UBF message as its single argument.  The Fun is
%% called by the ubf plugin handler process so the Fun can crash
%% and/or block this process.
%%
%% If your handler fun must maintain its own state, then you must use
%% an intermediate anonymous fun to bind the state.  See the usage of
%% the +irc_client_gs:send_self/2+ fun as an example.  The
%% +send_self()+ fun is actually arity 2, but the extra argument is
%% how the author, Joe Armstrong, maintains the extra state required
%% to deliver the async UBF message to the process that is executing
%% the event loop processing function, +irc_client_gs:loop/6+.

-spec install_handler(Handler::pid(), Fun::fun()) -> ack.
install_handler(Pid, Fun) ->
    if Pid =/= self() ->
            Pid ! {self(), {install, Fun}},
            receive
                {Pid, Reply} ->
                    Reply
            end;
       true ->
            Pid ! {install, Fun},
            ack
    end.

drop_fun(_Msg) ->
    fun drop_fun/1.

