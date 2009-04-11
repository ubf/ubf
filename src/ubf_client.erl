-module(ubf_client).

%% -compile(export_all).

%% start(Host,Port)  -> {ok, Pid, Service} | {error, What}
%% rpc(Pid, Q) -> Reply
%% stop(Pid)   -> ack.

-export([connect/2, connect/3, connect/4, rpc/2, rpc/3, stop/1, install_default_handler/1,
         install_handler/2]).

-import(ubf_utils, [spawn_link_debug/2]).

debug(_F, _S) -> true.
%% debug(F, S) -> io:format(F, S).

connect(Host, Port)
  when is_list(Host) andalso is_integer(Port) ->
    connect(Host, Port, 10000).

connect(Host, Port, Timeout)
  when is_list(Host) andalso is_integer(Port) ->
    connect(Host, Port, [], Timeout).

connect(Host, Port, Options, Timeout)
  when is_list(Host) andalso is_integer(Port) andalso is_list(Options) ->
    Self = self(),
    Pid = spawn_link_debug(ubf_client, fun() -> ubf_client(Self, Host, Port, Options, Timeout) end),
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
            Error
    end;
connect(Plugins, Server, Options, Timeout)
  when is_list(Plugins) andalso length(Plugins) > 0 andalso (is_atom(Server) orelse is_pid(Server)) andalso is_list(Options) ->
    Self = self(),
    Pid = spawn_link_debug(ubf_client, fun() -> ubf_client(Self, Plugins, Server, Options, Timeout) end),
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
            Error
    end.

%% This executes inside the ubf_client
ubf_client(Parent, Host, Port, Options, Timeout)
  when is_list(Host) andalso is_integer(Port) andalso is_list(Options) ->
    process_flag(trap_exit, true),
    DefaultConnectOptions =
        [binary, {nodelay, true}, {active, false}],
    {DriverModule, DriverVersion, ConnectOptions} =
        case proplists:get_value(ebf,Options,false) of
            false ->
                case proplists:get_value(jsf,Options,false) of
                    false ->
                        {ubf_driver, 'ubf1.0', DefaultConnectOptions};
                    true ->
                        {jsf_driver, 'jsf1.0', DefaultConnectOptions}
                end;
            true ->
                {ebf_driver, 'ebf1.0', DefaultConnectOptions++[{packet,4}]}
        end,
    %% io:format("QQQ: ~p : ~p : ~p ~n", [DriverModule, DriverVersion, ConnectOptions]),
    case gen_tcp:connect(Host, Port, ConnectOptions) of
        {ok, Socket} ->
            %% start a driver
            Driver = DriverModule:start(),
            %% get the socket to send messages to the driver
            gen_tcp:controlling_process(Socket, Driver),
            %% Kick off the driver
            Driver ! {start, Socket, self()},       % tell the controller
            inet:setopts(Socket, [{active, true}]), % before we activate socket
            %% wait for a startup message
            receive
                {Driver, {DriverVersion, Service, _}} ->
                    Parent ! {self(), {ok, Service}},
                    ubf_client_loop(Driver)
            after Timeout ->
                    Driver ! stop,
                    Parent ! {self(), {error, timeout}}
            end;
        {error, _E} ->
            Parent ! {self(), {error, socket}}
    end;

ubf_client(Parent, Plugins, Server, Options, Timeout)
  when is_list(Plugins) andalso length(Plugins) > 0 andalso (is_atom(Server) orelse is_pid(Server)) andalso is_list(Options) ->
    process_flag(trap_exit, true),
    Driver = ubf_server:start_term_listener(Server, Plugins, Options),
    %% wait for a startup message
    receive
        {Driver, {'etf1.0', Service, _}} ->
            Parent ! {self(), {ok, Service}},
            ubf_client_loop(Driver)
    after Timeout ->
            Driver ! stop,
            Parent ! {self(), {error, timeout}}
    end.

drop_fun(_Msg) ->
    %% io:format("GOT A CALLBACK HORRAY:~p~n",[Msg]),
    fun drop_fun/1.

stop(Pid) ->
    Pid ! stop,
    ok.

rpc(Pid, Q) ->
    rpc(Pid, Q, infinity).

rpc(Pid, Q, Timeout) ->
    Pid ! {self(), {rpc, Q}},
    if Timeout =:= infinity ->
            receive
                {Pid, Reply} ->
                    debug(">>> ubf_client RPC (~p)~n"
                          "<<< ~p ~n",[Q, Reply]),
                    Reply
            end;
       true ->
            receive
                {Pid, Reply} ->
                    debug(">>> ubf_client RPC (~p)~n"
                          "<<< ~p ~n",[Q, Reply]),
                    Reply
            after Timeout ->
                    timeout
            end
    end.

install_default_handler(Pid) ->
    install_handler(Pid, fun drop_fun/1).

install_handler(Pid, Fun) ->
    Pid ! {self(), {install, Fun}},
    receive
        {Pid, Reply} ->
            Reply
    end.

ubf_client_loop(Driver) ->
    loop(Driver, fun drop_fun/1).

loop(Driver, Fun) ->
    receive
        stop ->
            Driver ! stop,
            true;
        {Driver, {event, Event}} ->
            %% asynchronous event handler
            Fun1 = Fun(Event),
            loop(Driver, Fun1);
        {From, {rpc, Q}} ->
            %% rpc
            Driver ! {self(), Q},
            receive
                {Driver, {{clientBrokeContract, _, _}=R, S}} ->
                    From ! {self(), {reply, R, S}},
                    self() ! stop,
                    wait_terminate();
                {Driver, {{serverBrokeContract, _, _}=R, S}} ->
                    From ! {self(), {reply, R, S}},
                    self() ! stop,
                    wait_terminate();
                {Driver, {R, S}} ->
                    From ! {self(), {reply, R, S}},
                    loop(Driver, Fun);
                {Driver, {error, X}} ->
                    From ! {self(), {error, X}};
                {Driver, Other} ->
                    From ! {self(), {error, Other}};
                {'EXIT', Driver, _} ->
                    wait_terminate()
            end;
        {'EXIT', Driver, _} ->
            wait_terminate();
        {Driver, {event, Msg, State}} ->
            %% io:format("Dispatching callback:~p~n",[{Msg,State}]),
            Fun1 = Fun(Msg, State),
            loop(Driver, Fun1);
        {From, {install, Fun1}} ->
            From ! {self(), ack},
            loop(Driver, Fun1);
        X ->
            io:format("*** YY Client loop dropping:~p~n",[X]),
            loop(Driver, Fun)
    end.

wait_terminate() ->
    %% io:format("In wait terminate:~n"),
    receive
        stop ->
            true;
        {From, {rpc, _Q}} ->
            From ! {self(), {error, stop, stop}};
        X ->
            io:format("ZZ Client loop dropping:~p~n",[X]),
            wait_terminate()
    end.
