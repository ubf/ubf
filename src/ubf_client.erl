%%%
%%% @doc UBF client-side public API.
%%%
%%% This module implements most of the commonly-used client-side
%%% functions required to talk to UBF servers: connect() to a UBF
%%% server, rpc() to make a synchronous call to a connected UBF
%%% server, stop() a connection, and install_handler() to add a
%%% callback function to handle asynchronous notifications from the
%%% UBF server to your client process.
%%%
%%% See the documentation for the <tt>file_plugin</tt> module for
%%% extra commentary on writing an UBF server implementation module.
%%%

-module(ubf_client).

%% -compile(export_all).

%% connect(Host,Port)  -> {ok, Pid, Service} | {error, What}
%% rpc(Pid, Q) -> Reply
%% stop(Pid)   -> ack.

-export([connect/2, connect/3, connect/4, rpc/2, rpc/3, stop/1,
         install_default_handler/1, install_handler/2]).

%% @type address() = string() | ip_address(). A DNS hostname or IP address.
%% @type connect_options() = list({proto, ubf | ebf | jsf}).
%%       An OTP-style property list, see 'proplists' module for details.
%% @type ip_address() = string() | tuple().  An IP address in string form,
%%       e.g. "127.0.0.1" (IPv4) or "::1" (IPv6), or in tuple form (see
%%       documentation for Erlang's 'inet' module for details).
%% @type plugin_module_list() = list(atom()).  A list of plugin module names
%%       that will be passed to ubf_plugin_meta_serverful:new() or
%%       ubf_plugin_meta_serverless:new() for client initialization.
%% @type tcp_port() = integer().  A TCP port number.
%% @type timeout() = integer() | infinity.  An Erlang-style timeout value.

%% @spec (string(), list()) -> term()
%% @doc Debugging function, uncomment the alternate form for
%% io:format()/"printf()"-style debugging messages.

debug(_F, _S) -> true.
%% debug(F, S) -> io:format(F, S).

%% @spec (address(), tcp_port()) ->
%%       {ok, pid(), service()} | {error, term()}
%% @doc Connect to a UBF server at address Host + TCP port Port.

connect(Host, Port)
  when is_list(Host) andalso is_integer(Port) ->
    connect(Host, Port, infinity).

%% @spec (address(), tcp_port(), timeout()) ->
%%       {ok, pid(), service()} | {error, term()}
%% @doc Connect to a UBF server at address Host + TCP port Port.

connect(Host, Port, Timeout)
  when is_list(Host) andalso is_integer(Port) ->
    connect(Host, Port, [], Timeout).

%% @spec (address() | plugin_module_list(), tcp_port() | pid() | atom(),
%%        proplist(), timeout()) ->
%%       {ok, pid(), service()} | {error, term()}
%% @doc Connect to a UBF server at address Host + TCP port Port, or at
%%      pid/registered name Server.
%%
%% When using the alternate form, the first two arguments are:
%% <ol>
%% <li> Plugins: a plugin_module_list(). </li>
%% <li> Server: either a process id (pid()) or process registered
%%      name (atom()) for an already-started UBF server. </li>
%% </ol>
%%
%% See the docs for ubf_server:start_link() for a description of the
%% <tt>Options</tt> proplist.

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
        [binary, {nodelay, true}, {active, false}],
    {DriverModule, DriverVersion, ConnectOptions} =
        case proplists:get_value(proto,Options,ubf) of
            ubf ->
                {ubf_driver, 'ubf1.0', DefaultConnectOptions};
            ebf ->
                {ebf_driver, 'ebf1.0', DefaultConnectOptions++[{packet,4}]};
            jsf ->
                {jsf_driver, 'jsf1.0', DefaultConnectOptions}
        end,
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
                    ubf_client_loop(Driver);
                {'EXIT', Driver, Reason} ->
                    Parent ! {self(), {error, Reason}};
                {'EXIT', Parent, Reason} ->
                    exit(Driver, Reason)
            after Timeout ->
                    exit(Driver, timeout),
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
            ubf_client_loop(Driver);
        {'EXIT', Driver, Reason} ->
            Parent ! {self(), {error, Reason}};
        {'EXIT', Parent, Reason} ->
            exit(Driver, Reason)
    after Timeout ->
            exit(Driver, timeout),
            Parent ! {self(), {error, timeout}}
    end.

drop_fun(_Msg) ->
    fun drop_fun/1.

%% @spec (pid()) -> ok
%% @doc Stop a UBF client process.

stop(Pid) ->
    Pid ! stop,
    ok.

%% @spec (pid(), term()) -> timeout | term()
%% @doc Perform a synchronous RPC call.
%%
%% NOTE: It is not recommended that a UBF client return the bare atom
%% 'timeout' in response to any RPC call.

rpc(Pid, Q) ->
    rpc(Pid, Q, infinity).

%% @spec (pid(), term(), timeout()) -> timeout | term()
%% @doc Perform a synchronous RPC call.

rpc(Pid, Q, infinity) ->
    rpc(Pid, Q, 16#ffffffff);
rpc(Pid, Q, Timeout) ->
    Pid ! {self(), {rpc, Q}},
    receive
        {Pid, Reply} ->
            debug(">>> ubf_client RPC (~p)~n"
                  "<<< ~p ~n",[Q, Reply]),
            Reply
    after Timeout ->
            exit(Pid, timeout),
            timeout
    end.

%% @spec (pid()) -> ack
%% @doc Install a default handler function (callback-style) for
%% asynchronous UBF messages.
%%
%% The default handler function, drop_fun/1, does nothing.

install_default_handler(Pid) ->
    install_handler(Pid, fun drop_fun/1).

%% @spec (pid(), function()) -> ack
%% @doc Install a handler function (callback-style) for asynchronous
%% UBF messages.
%%
%% The handler fun Fun should be an function of arity 1.  When an
%% asynchronous UBF message is received, the callback function will be
%% called with the UBF message as its single argument.
%%
%% If your handler fun must maintain its own state, then you must use
%% an intermediate anonymous fun to bind the state.  See the usage of
%% the <tt>irc_client_gs:send_self/2</tt> fun as an example.  The
%% <tt>send_self()</tt> fun is actually arity 2, but the extra
%% argument is how the author, Joe Armstrong, maintains the extra
%% state required to deliver the async UBF message to the process that
%% is executing the event loop processing function,
%% <tt>irc_client_gs:loop/6</tt>.

install_handler(Pid, Fun) ->
    Pid ! {self(), {install, Fun}},
    receive
        {Pid, Reply} ->
            Reply
    end.

%% @doc Entry function for the UBF client process.

ubf_client_loop(Driver) ->
    loop(Driver, fun drop_fun/1).

%% @doc Main loop for the UBF client process.

loop(Driver, Fun) ->
    receive
        stop ->
            Driver ! stop,
            true;
        {'EXIT', Driver, Reason} ->
            exit(Reason);
        {Driver, {event, Event}} ->
            %% asynchronous event handler
            Fun1 = Fun(Event),
            loop(Driver, Fun1);
        {From, {rpc, Q}} ->
            %% rpc
            Driver ! {self(), Q},
            receive
                {Driver, {R, S}} ->
                    From ! {self(), {reply, R, S}},
                    loop(Driver, Fun);
                {Driver, {error, X}} ->
                    From ! {self(), {error, X}};
                {Driver, Other} ->
                    From ! {self(), {error, Other}}
            end;
        {Driver, {event, Msg, State}} ->
            Fun1 = Fun(Msg, State),
            loop(Driver, Fun1);
        {From, {install, Fun1}} ->
            From ! {self(), ack},
            loop(Driver, Fun1);
        X ->
            io:format("*** YY Client loop dropping:~p~n",[X]),
            loop(Driver, Fun)
    end.
