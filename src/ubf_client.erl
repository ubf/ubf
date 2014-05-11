%%% The MIT License
%%%
%%% Copyright (C) 2011-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%%
%%% @doc UBF client-side public API.
%%%
%%% This module implements most of the commonly-used client-side
%%% functions required to talk to UBF servers:
%%%
%%% - +connect()+ to a UBF server
%%% - +rpc()+ to make a synchronous call to a connected UBF server
%%% - +stop()+ a connection
%%% - +install_handler()+ to add a callback function to handle
%%%   asynchronous notifications from theUBF server to your client
%%%   process.
%%%
%%% Note that this library can support UBF(a), EBF, JSF, TBF, PBF, and
%%% ABF transport.  See the +connect()+ function arguments for
%%% details.
%%%
%%% This module also provides an alternative client-side function for
%%% calling\'s UBF contract manager and a UBF contract\'s
%%% implementation without any side-effects: +lpc()+ to make a
%%% synchronous local procedure call to a contract\'s implementation.
%%%

-module(ubf_client).

-export([connect/2, connect/3, connect/4, rpc/2, rpc/3, stop/1]).
-export([sendEvent/2, install_default_handler/1, install_handler/2]).
-export([lpc/2, lpc/3, lpc/4]).

-include("ubf_impl.hrl").

-import(contract_manager, [do_lpcIn/4, do_lpcOut/9, do_lpcOutError/6]).

-type host() :: nonempty_string().
-type ipport() :: pos_integer().
-type name() :: atom().
-type server() :: name() | pid().
-type plugin() :: module().
-type plugins() :: [plugin()].
-type options() :: [{atom(), term()}].
-type service() :: {'#S', nonempty_string()} | undefined.
-type statename() :: atom().
-type tlogger() :: module().

%% @doc Debugging function, uncomment the alternate form for
%% io:format()/"printf()"-style debugging messages.

debug(_F, _S) -> true.
%% debug(F, S) -> io:format(F, S).

%% @doc Connect to a UBF server at address Host + TCP port Port.

-spec connect(host() | plugins(), ipport() | server()) ->
                     {ok, Client::pid(), service()} | {error, term()}.
connect(Host, Port)
  when is_list(Host) andalso is_integer(Port) ->
    connect(Host, Port, infinity).

%% @doc Connect to a UBF server at address Host + TCP port Port.

-spec connect(host() | plugins(), ipport() | server(), timeout()) ->
                     {ok, Client::pid(), service()} | {error, term()}.
connect(Host, Port, Timeout)
  when is_list(Host) andalso is_integer(Port) ->
    connect(Host, Port, [], Timeout).

%% @doc Connect to a UBF server at address Host + TCP port Port, or at
%% pid/registered name Server.
%%
%% When using the alternate form, the first two arguments are:
%%
%% - Plugins: a +plugin_module_list()+.
%% - Server: either a process id +(pid())+ or process registered
%%   name +(atom())+ for an already-started UBF server.
%%
%% See the docs for +ubf_server:start_link()+ for a description of the
%% +Options+ proplist.

-spec connect(host() | plugins(), ipport() | server(), options(), timeout()) ->
                     {ok, Client::pid(), service()} | {error, term()}.
connect(X, Y, Options, infinity) ->
    connect(X, Y, Options, 16#ffffffff);
connect(Host, Port, Options, Timeout)
  when is_integer(Port) andalso is_list(Options) ->
    Self = self(),
    Pid = proc_utils:spawn_link_debug(fun() -> ubf_client(Self, Host, Port, Options, Timeout) end, ?MODULE),
    receive
        {Pid, {ok, Service}} ->
            debug(">>> ubf_client:start ~p ~p~n"
                  "<<< ~p~n",
                  [Host, Port, {ok, Pid, Service}]),
            {ok, Pid, Service};
        {Pid, Error={error,_}} ->
            debug(">>> ubf_client:start ~p ~p~n"
                  "<<< ~p~n",
                  [Host, Port, Error]),
            Pid ! stop,
            Error
    after Timeout ->
            exit(Pid, timeout),
            timeout
    end;
connect(Plugins, Server, Options, Timeout)
  when is_list(Plugins) andalso length(Plugins) > 0 andalso (is_atom(Server) orelse is_pid(Server)) andalso is_list(Options) ->
    Self = self(),
    Pid = proc_utils:spawn_link_debug(fun() -> ubf_client(Self, Plugins, Server, Options, Timeout) end, ?MODULE),
    receive
        {Pid, {ok, Service}} ->
            debug(">>> ubf_client:start ~p ~p ~p~n"
                  "<<< ~p~n",
                  [Plugins, Server, Options, {ok, Pid, Service}]),
            {ok, Pid, Service};
        {Pid, Error={error,_}} ->
            debug(">>> ubf_client:start ~p ~p ~p~n"
                  "<<< ~p~n",
                  [Plugins, Server, Options, Error]),
            Pid ! stop,
            Error
    after Timeout ->
            exit(Pid, timeout),
            timeout
    end.

%% @doc This executes inside the ubf_client

ubf_client(Parent, Host, Port, Options, Timeout)
  when is_list(Host) andalso is_integer(Port) andalso is_list(Options) ->
    process_flag(trap_exit, true),
    DefaultConnectOptions =
        case proplists:get_value(clientport, Options, undefined) of
            ClientPort when is_integer(ClientPort) ->
                [binary, {nodelay, true}, {active, false}, {port, ClientPort}];
            {MinCP, MaxCP}=ClientPort when is_integer(MinCP), is_integer(MaxCP) ->
                [binary, {nodelay, true}, {active, false}, {port, MinCP}];
            _ ->
                ClientPort = undefined,
                [binary, {nodelay, true}, {active, false}]
        end,
    ServerHello = proplists:get_value(serverhello, Options, defined),
    SimpleRPC = proplists:get_value(simplerpc, Options, false),
    StartPlugin = proplists:get_value(startplugin, Options, undefined),
    case proplists:get_value(proto, Options, ubf) of
        Proto when is_atom(Proto) ->
            DriverOptions = [];
        {Proto, DriverOptions} when is_atom(Proto), is_list(DriverOptions) ->
            DriverOptions
    end,

    DriverMod = Proto:proto_driver(),
    DriverVersion = Proto:proto_vsn(),
    DriverPacketType = Proto:proto_packet_type(),

    ConnectOptions =
        if DriverPacketType =/= 0 ->
                DefaultConnectOptions++[{packet,DriverPacketType}];
           true ->
                DefaultConnectOptions
        end,

    case gen_tcp:connect(Host, Port, ConnectOptions) of
        {ok, Socket} ->
            %% start a driver
            Driver = DriverMod:start(StartPlugin, DriverOptions),
            %% get the socket to send messages to the driver
            ok = gen_tcp:controlling_process(Socket, Driver),
            %% Kick off the driver
            Driver ! {start, self(), Socket},  % tell the controller
            ok = inet:setopts(Socket, [{active, true}
                                       %%, {send_timeout, Timeout}
                                       %%, {send_timeout_close, true}
                                      ]),
            if ServerHello =/= undefined ->
                    %% wait for a startup message
                    receive
                        {Driver, {DriverVersion, Service, _}} ->
                            Parent ! {self(), {ok, Service}},
                            ubf_client_loop(Parent, Driver, SimpleRPC);
                        {'EXIT', Driver, Reason} ->
                            Parent ! {self(), {error, Reason}};
                        {'EXIT', Parent, Reason} ->
                            exit(Driver, Reason)
                    after Timeout ->
                            exit(Driver, timeout),
                            Parent ! {self(), {error, timeout}}
                    end;
               true ->
                    Parent ! {self(), {ok, undefined}},
                    ubf_client_loop(Parent, Driver, SimpleRPC)
            end;
        {error, eaddrinuse} when is_tuple(ClientPort) ->
            %% Try next port if current one is in use
            case ClientPort of
                {MinCP1, MaxCP1} when MinCP1 < MaxCP1 ->
                    Options1 = [{clientport, {MinCP1+1,MaxCP1}}|proplists:delete(clientport, Options)],
                    ubf_client(Parent, Host, Port, Options1, Timeout);
                _ ->
                    Parent ! {self(), {error, socket}}
            end;
        {error, _E} ->
            Parent ! {self(), {error, socket}}
    end;
ubf_client(Parent, Plugins, Name, Options, Timeout)
  when is_atom(Name) ->
    case whereis(Name) of
        undefined ->
            erlang:error(badarg);
        Server ->
            ubf_client(Parent, Plugins, Server, Options, Timeout)
    end;
ubf_client(Parent, Plugins, Server, Options, Timeout)
  when is_list(Plugins) andalso length(Plugins) > 0 andalso is_pid(Server) andalso is_list(Options) ->
    process_flag(trap_exit, true),
    Driver = ubf_server:start_term_listener(Server, Plugins, Options),
    ServerHello = proplists:get_value(serverhello, Options, true),
    SimpleRPC = proplists:get_value(simplerpc, Options, false),
    if ServerHello =/= undefined ->
            %% wait for a startup message
            receive
                {Driver, {'etf1.0', Service, _}} ->
                    Parent ! {self(), {ok, Service}},
                    ubf_client_loop(Parent, Driver, SimpleRPC);
                {'EXIT', Driver, Reason} ->
                    Parent ! {self(), {error, Reason}};
                {'EXIT', Parent, Reason} ->
                    exit(Driver, Reason)
            after Timeout ->
                    exit(Driver, timeout),
                    Parent ! {self(), {error, timeout}}
            end;
       true ->
            Parent ! {self(), {ok, undefined}},
            ubf_client_loop(Parent, Driver, SimpleRPC)
    end.


%% @doc Stop a UBF client process.

-spec stop(Client::pid()) -> ok.
stop(Pid) ->
    Pid ! stop,
    ok.

%% @doc Perform a synchronous RPC call.
%%
%% NOTE: It is not recommended that a UBF client return the bare atom
%% +timeout+ in response to any RPC call.

-spec rpc(Client::pid(), Call::term()) -> timeout | term() | no_return().
rpc(Pid, Q) ->
    rpc(Pid, Q, infinity).

%% @doc Perform a synchronous RPC call.

-spec rpc(Client::pid(), Call::term(), timeout()) -> timeout | term() | no_return().
rpc(Pid, Q, infinity) ->
    rpc(Pid, Q, 16#ffffffff);
rpc(Pid, Q, Timeout) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        false ->
            erlang:error(badpid);
        true ->
            Pid ! {self(), {rpc, Q}},
            receive
                {Pid, Reply} ->
                    debug(">>> ubf_client RPC (~p)~n"
                          "<<< ~p ~n",[Q, Reply]),
                    Reply
            after Timeout ->
                    Pid ! stop,
                    timeout
            end
    end.

%% @doc Send an asynchronous UBF message.

-spec sendEvent(Handler::pid(), Cast::term()) -> ok | no_return().
sendEvent(Pid, Msg) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        false ->
            erlang:error(badpid);
        true ->
            Pid ! {self(), {event_in, Msg}},
            ok
    end.

%% @doc Install a default handler function (callback-style) for
%% asynchronous UBF messages.
%%
%% The default handler function, drop_fun/1, does nothing.

-spec install_default_handler(Client::pid()) -> ack.
install_default_handler(Pid) ->
    install_handler(Pid, fun drop_fun/1).

%% @doc Install a handler function (callback-style) for asynchronous
%% UBF messages.
%%
%% The handler fun Fun should be a function of arity 1.  When an
%% asynchronous UBF message is received, the callback function will be
%% called with the UBF message as its single argument.  The Fun is
%% called by the ubf client process so the Fun can crash and/or block
%% this process.
%%
%% If your handler fun must maintain its own state, then you must use
%% an intermediate anonymous fun to bind the state.  See the usage of
%% the +irc_client_gs:send_self/2+ fun as an example.  The
%% +send_self()+ fun is actually arity 2, but the extra argument is
%% how the author, Joe Armstrong, maintains the extra state required
%% to deliver the async UBF message to the process that is executing
%% the event loop processing function, +irc_client_gs:loop/6+.

-spec install_handler(Client::pid(), Fun::fun()) -> ack.
install_handler(Pid, Fun) ->
    Pid ! {self(), {install, Fun}},
    receive
        {Pid, Reply} ->
            Reply
    end.

drop_fun(_Msg) ->
    fun drop_fun/1.


%% @doc Entry function for the UBF client process.

ubf_client_loop(Parent, Driver, SimpleRPC) ->
    loop(Parent, Driver, SimpleRPC, fun drop_fun/1).

%% @doc Main loop for the UBF client process.

loop(Parent, Driver, SimpleRPC, Fun) ->
    receive
        stop ->
            Driver ! stop,
            true;
        {'EXIT', Driver, Reason} ->
            exit(Reason);
        {'EXIT', Parent, Reason} ->
            Driver ! stop,
            exit(Reason);
        {Driver, {event_out, Event}} ->
            %% asynchronous event handler
            Fun1 = Fun(Event),
            loop(Parent, Driver, SimpleRPC, Fun1);
        {From, {rpc, Q}} ->
            %% rpc
            Driver ! {self(), Q},
            receive
                {Driver, R} when SimpleRPC ->
                    From ! {self(), {reply, R}},
                    loop(Parent, Driver, SimpleRPC, Fun);
                {Driver, {R, S}} ->
                    From ! {self(), {reply, R, S}},
                    loop(Parent, Driver, SimpleRPC, Fun);
                {Driver, {error, _} = Error} ->
                    From ! {self(), Error};
                {Driver, Other} ->
                    From ! {self(), {error, Other}};
                {'EXIT', Driver, Reason} ->
                    From ! {self(), {error, Reason}};
                {'EXIT', Parent, Reason} ->
                    From ! {self(), {error, stop}},
                    Driver ! stop,
                    exit(Reason);
                %% {'EXIT', From, Reason} ->
                %%     %% TBD corner case for etf client
                %%     io:format("*** Client loop exiting:~p ~p~n", [From, Reason]),
                %%     From ! {self(), {error, stop}},
                %%     Driver ! stop,
                %%     true;
                stop ->
                    From ! {self(), {error, stop}},
                    Driver ! stop,
                    true
            end;
        {_From, {event_in, _Msg}=Event} ->
            %% asynchronous event
            Driver ! {self(), Event},
            loop(Parent, Driver, SimpleRPC, Fun);
        {From, {install, Fun1}} ->
            From ! {self(), ack},
            loop(Parent, Driver, SimpleRPC, Fun1);
        X ->
            io:format("*** Client loop dropping:~p~n",[X]),
            loop(Parent, Driver, SimpleRPC, Fun)
    end.

%% @doc Perform a synchronous LPC (local procedure) call with the
%% state +none+.

-spec lpc(plugin(), Call::term()) -> term().
lpc(Mod, Q) ->
    lpc(Mod, Q, none, undefined).

%% @doc Perform a synchronous LPC (local procedure) call with the
%% specified state.

-spec lpc(plugin(), Call::term(), statename()) -> term().
lpc(Mod, Q, State) ->
    lpc(Mod, Q, State, ?UBF_TLOG_MODULE_DEFAULT).

-spec lpc(plugin(), Call::term(), statename(), tlogger()) -> term().
lpc(Mod, Q, State, TLogMod) ->
    %% check contract
    case do_lpcIn(Q, State, Mod, TLogMod) of
        {error, Reply} ->
            {reply,Reply,State};
        {ok, Ref} ->
            case (catch Mod:handlerRpc(Q)) of
                {'EXIT', Reason} ->
                    do_lpcOutError(Ref, Q, State, Mod, Reason, TLogMod),
                    {error, stop};
                Reply ->
                    %% check contract
                    {_, NewReply} = do_lpcOut(Ref, Q, State, Mod, Reply, State, State, Mod, TLogMod),
                    {reply,NewReply,State}
            end
    end.
