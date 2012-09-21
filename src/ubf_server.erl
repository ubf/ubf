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

%%%
%%% @doc UBF server-side public API.
%%%
%%% This module implements most of the commonly-used server-side
%%% functions: starting TCP listeners and registering their
%%% implementation callback modules.
%%%
%%% We implement several different wire formats for accessing the same
%%% implementation of a UBF(b) protocol-checking server:
%%%
%%% - UBF(a).  This is Joe Armstrong\'s original implementation.
%%% - EBF, a.k.a. Erlang Binary Format.  This protocol uses common
%%%   Erlang wire formats, the +{packet, 4}+ protocol from +inets+ for
%%%   TCP connections, and the +term_to_binary()+/+binary_to_term()+
%%%   BIFs for payload encoding.  These wire formats are used to pass
%%%   Erlang terms between a UBF(b) contract checking server and a
%%%   client that does not support the UBF(a) wire format but does
%%%   support Erlang\'s native wire formats.
%%% - JSF, a.k.a the JSon Format.  Similar to EBF, except that
%%%   JavaScript\'s JSON encoding is used for the wire protocol
%%%   instead of UBF(a) or Erlang\'s native wire formats.
%%% - TBF, a.k.a the Thrift Binary Format.  Similar to EBF, except
%%%   that Thrift\'s binary encoding is used for the wire protocol
%%%   instead of UBF(a) or Erlang\'s native wire formats.
%%%
%%% There is no "stop" function.  To stop the server, instead stop the
%%% TCP service manager that controls it: see the +proc_socket_server+
%%% module for extra details.
%%%
%%% See the documentation for the +file_plugin+ module for extra
%%% commentary on writing an UBF server implementation module.

-module(ubf_server).

-export([start/2, start/3, start/4, start_link/2, start_link/3, start_link/4, init/5]).
-export([start_term_listener/3]).

-include("ubf.hrl").
-include("ubf_impl.hrl").

-import(proc_socket_server, [start_raw_server/7]).

-type name() :: atom().
-type plugins() :: [module()].
-type ipport() :: pos_integer().
-type options() :: [{atom(), term()}].

%% @doc Start a server and a TCP listener on port Port and register
%% all of the protocol implementation modules in the Plugins list.
%%
%% Here we start the server.

-spec start(plugins(), ipport()) -> true.
start(Plugins, Port) ->
    start(undefined, Plugins, Port).

%% @doc Start a registered server and a TCP listener on port Port and
%% register all of the protocol implementation modules in the Plugins
%% list. If Name is undefined, the server is not registered.
%%
%% Here we start the server.

-spec start(name(), plugins(), ipport()) -> true.
start(Name, Plugins, Port) ->
    start(Name, Plugins, Port, []).

%% @doc Start a registered server and a TCP listener on port Port with
%% the Options properties list and register all of the protocol
%% implementation modules in the Plugins list.  If Name is undefined,
%% the server is not registered
%%
%% Valid properties in the Options proplist are:
%%
%% - +{idletimer, integer() | infinity}+ Maximum time (in milliseconds)
%%   that a client connection may remain idle before the server will
%%   close the connection.
%%   Default: infinity
%% - +{maxconn, integer()}+ Maximum number of simultaneous TCP
%%   connections allowed.
%%   Default: 10,000.
%% - +{proto, {ubf | ebf | atom()}}+ Enable the UBF, EBF, or
%%   an alternative protocol wire format.
%%   Default: ubf.
%% - +{proto, {ubf | ebf | atom(), [atom() | tuple()]}}+ Enable the UBF,
%%   EBF, or an alternative protocol wire format with options.
%%   Default: +{ubf, []}+.
%%
%% Supported options:
%% - safe  Prevents decoding data that may be used to attack the
%%   Erlang system.  In the event of receiving unsafe data, decoding
%%   fails with a badarg error.
%%
%% - +{registeredname, atom()}+ Set the name to be registered for
%%   the TCP listener.  If undefined, a default name is automatically
%%   registered.
%%   Default: undefined.
%% - +{statelessrpc, true | false}+ Run the stateless variety of
%%   a UBF(b) contract.  A stateless contract is an extension of
%%   Joe Armstrong\'s original UBF server implementation.
%%   Default: false.
%% - +{startplugin, atom()}+ Set the starting plugin, set after a
%%   client first connects to the server.  If not set, client may
%%   select the service using the startSession() API.  There is
%%   no default setting.
%% - +{serverhello, ubfstring() | undefined}+ Meta contract greeting
%%   string, sent when a client first connects to the server.  If
%%   undefined, server hello is not sent to the client.
%%   Default: "meta_server".
%% - +{simplerpc, true | false}+ Set the simple RPC mode.  If
%%   true, server returns only the rpc reply to client.  If false,
%%   server returns the rpc reply and next state to client.
%%   Default: false.
%% - +{verboserpc, true | false}+ Set the verbose RPC mode.  If
%%   true, server calls the plugin handler with the rpc request and
%%   matched contract types.  If false, server calls the plugin
%%   handler only with the rpc request.
%%   Default: false.
%% - +{tlog_module, atom() | {atom(), boolean()}}+ Set the transaction
%%   log callback module and optionally control the built-in calls
%%   by +contract_manager_tlog+ to the +error_logger+ module.
%%   If the 2-tuple representation is used and the boolean() member is
%%   false, then calls to +error_logger+ will not be attempted.
%%   Default: undefined.
%% - +{process_options, list()}+ Specify additional options used
%%   for spawning server and/or client related erlang processes.
%%   Typically used to specify non-default, garbage collection options.
%%   Default: [].

-spec start(name(), plugins(), ipport(), options()) -> true.
start(Name, Plugins, Port, Options) ->
    start_registered(Name, fun() -> start_server(Plugins, Port, Options) end).

%% @doc See start/2, but also link the server processs to the caller.

-spec start_link(plugins(), ipport()) -> true.
start_link(Plugins, Port) ->
    start_link(undefined, Plugins, Port).

%% @doc See start/3, but also link the server processs to the caller.

-spec start_link(name(), plugins(), ipport()) -> true.
start_link(Name, Plugins, Port) ->
    start_link(Name, Plugins, Port, []).

%% @doc See start/4, but also link the server processs to the caller.

-spec start_link(name(), plugins(), ipport(), options()) -> true.
start_link(Name, Plugins, Port, Options) ->
    proc_lib:start_link(?MODULE, init, [Name, self(), Plugins, Port, Options]).

-spec init(name(), pid(), plugins(), ipport(), options()) -> pid().
init(Name, Parent, Plugins, Port, Options) ->
    if Name /= undefined ->
            register(Name, self());
       true ->
            noop
    end,
    proc_lib:init_ack(Parent, {ok, self()}),
    start_server(Plugins, Port, Options).

start_server(Plugins, Port, Options) ->
    %% set up a UBF listener on Port
    {ok, ListenerPid, MetaPlugin} = start_ubf_listener(self(), Plugins, Port, Options),
    ubf_plugin_handler:manager(ListenerPid, MetaPlugin, [ListenerPid]).

start_ubf_listener(Server0, Plugins, Port, Options) ->
    {MetaPlugin, StartPlugin, Server
     , IdleTimer, MaxConn, Proto, DriverOptions, RegisteredName
     , StatelessRPC, ServerHello, SimpleRPC, VerboseRPC
     , TLogMod, ProcessOptions
    } = listener_options(Server0, Plugins, Options),

    DriverMod = Proto:proto_driver(),
    DriverVersion = Proto:proto_vsn(),
    DriverPacketType = Proto:proto_packet_type(),

    ServerFun =
        fun(Socket) ->
                %% This gets spawned every time a new socket
                %% connection is is established on this port.

                %% We have to start 2 additional processes - a
                %% contract manager and a plugin handler. The driver
                %% (This process) sends messages to the contract
                %% manager. The contract manager sends messages to the
                %% handler.

                Driver = self(),

                %% Next few lines are pretty devious but they work!
                _ = if ServerHello =/= undefined ->
                            %% send hello back to the opening program
                            Driver ! {self(), {DriverVersion, ?S(ServerHello), help()}};
                       true ->
                            noop
                    end,

                ContractManager = contract_manager:start(SimpleRPC, VerboseRPC, ProcessOptions),
                Handler = ubf_plugin_handler:start_handler(MetaPlugin, StartPlugin, Server, StatelessRPC, ProcessOptions),
                receive
                    {state, Handler, StartState} ->
                        ContractManager ! {start, Driver, Handler, StartState, StartPlugin, TLogMod},
                        Handler ! {start, ContractManager, TLogMod},
                        %% and activate the loop that will now execute
                        %% the previous devious statements :-)

                        %% swap the driver
                        contract_driver:relay(DriverMod, self(), ContractManager),

                        case (catch contract_driver:loop(DriverMod, StartPlugin, DriverOptions, self(), Socket, IdleTimer)) of
                            {'EXIT', normal} ->
                                exit(normal);
                            {'EXIT', Reason} ->
                                %% brute force
                                exit(Handler, Reason),
                                exit(ContractManager, Reason),
                                exit(Reason)
                        end
                end
        end,

    {ok, Pid} = start_raw_server(RegisteredName,
                                 Port,
                                 MaxConn,
                                 ProcessOptions,
                                 ServerFun,
                                 DriverPacketType,
                                 0),
    {ok, Pid, MetaPlugin}.

-spec start_term_listener(pid(), plugins(), options()) -> pid().
start_term_listener(Server0, Plugins, Options) ->
    {MetaPlugin, StartPlugin, Server
     , _IdleTimer, _MaxConn, _Proto, _DriverOptions_, _RegisteredName
     , StatelessRPC, ServerHello, SimpleRPC, VerboseRPC
     , TLogMod, ProcessOptions
    } = listener_options(Server0, Plugins, Options),

    Driver = self(),
    ContractManager = contract_manager:start(SimpleRPC, VerboseRPC, ProcessOptions),

    _ = if ServerHello =/= undefined ->
                %% send hello back to the opening program
                self() ! {ContractManager, {'etf1.0', ?S(ServerHello), help()}};
           true ->
                noop
        end,

    Handler = ubf_plugin_handler:start_handler(MetaPlugin, StartPlugin, Server, StatelessRPC, ProcessOptions),
    receive
        {state, Handler, StartState} ->
            ContractManager !
                {start, Driver, Handler, StartState, StartPlugin, TLogMod},
            Handler !
                {start, ContractManager, TLogMod},
            ContractManager
    end.

help() ->
    ?S("\n\n See http://www.sics.se/~joe/ubf/ for details of this service.\n"
       " See http://github.com/ubf/ubf for source code\n"
       "     extensions available as part of the larger OSS community.\n"
       " Type 'info'$ for information\n\n").


%%----------------------------------------------------------------------
%% Misc junk

start_registered(Name, F) ->
    case whereis(Name) of
        undefined ->
            Me = self(),
            P = proc_utils:spawn_link_debug(fun() -> start_proc(Me, Name, F) end, ?MODULE),
            receive
                {P, ack} ->
                    true
            end;
        _Pid ->
            true
    end.

start_proc(Parent, undefined, F) ->
    Parent ! {self(), ack},
    F();
start_proc(Parent, Name, F) ->
    case (catch register(Name, self())) of
        {'EXIT', _} ->
            Parent ! {self(), ack},
            true;
        _ ->
            Parent ! {self(), ack},
            F()
    end.

listener_options(Server0, Plugins, Options) ->
    SortedPlugins = lists:usort(Plugins),
    StatelessRPC = proplists:get_value(statelessrpc, Options, false),

    {MetaPlugin,Server} =
        case StatelessRPC of
            false ->
                {ubf_plugin_meta_stateful:new(SortedPlugins), Server0};
            true ->
                {ubf_plugin_meta_stateless:new(SortedPlugins), undefined}
        end,
    StartPlugin = proplists:get_value(startplugin, Options, MetaPlugin),

    IdleTimer =
        case proplists:get_value(idletimer, Options, infinity) of
            infinity ->
                16#ffffffff;
            Else when Else > 16#ffffffff ->
                16#ffffffff;
            Else ->
                Else
        end,

    case proplists:get_value(proto, Options, ubf) of
        Proto when is_atom(Proto) ->
            DriverOptions = [];
        {Proto, DriverOptions} when is_atom(Proto), is_list(DriverOptions) ->
            DriverOptions
    end,

    {MetaPlugin
     , StartPlugin
     , Server
     , IdleTimer
     , proplists:get_value(maxconn, Options, 10000)
     , Proto
     , DriverOptions
     , proplists:get_value(registeredname, Options, undefined)
     , StatelessRPC
     , proplists:get_value(serverhello, Options, StartPlugin:contract_name())
     , proplists:get_value(simplerpc, Options, false)
     , proplists:get_value(verboserpc, Options, false)
     , proplists:get_value(tlog_module, Options, ?UBF_TLOG_MODULE_DEFAULT)
     , proplists:get_value(process_options, Options, [])
    }.
