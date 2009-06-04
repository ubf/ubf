%%%
%%% @doc UBF server-side public API.
%%%
%%% TO-DO: JoeNorton, I can't seem to start multiple listeners, only
%%% the first one succeeds.  At a casual glance, that seems to be The
%%% Current Correct Behavior, but it'd be awful nice to have multiple
%%% listeners inside the same VM.  Is there another way to do that?
%%% ubf_server:start([file_plugin], 2000, []).
%%% ubf_server:start([irc_plugin], 2001, []).
%%%
%%% TO-DO: JoeNorton, ubf_server:start([file_plugin], 2000, [jsf]) will
%%% give a JSON-speaking server that has a lot of difficulty decoding
%%% and encoding stuff.  Do you have any plan to fix it?
%%%
%%% This module implements most of the commonly-used server-side
%%% functions: starting TCP listeners and registering their
%%% implementation callback modules.
%%%
%%% We implement three different wire formats for accessing the same
%%% implementation of a UBF(B) protocol-checking server:
%%%
%%% <ul>
%%% <li> UBF(A).  This is Joe Armstrong's original implementation.
%%%      See <a href="http://www.sics.se/~joe/ubf/">
%%%      http://www.sics.se/~joe/ubf/</a> for details. </li>
%%% <li> EBF, a.k.a. Erlang Binary Format.  This protocol uses common
%%%      Erlang wire formats, the <tt>{packet, 4}</tt> protocol from
%%%      'inets' for TCP connections, and the
%%%      <tt>term_to_binary()</tt>/<tt>binary_to_term()</tt> BIFs for
%%%      payload encoding.  These wire formats are used to pass Erlang
%%%      terms between a UBF(B) contract checking server and a client
%%%      that does not support the UBF(A) wire format but does support
%%%      Erlang's native wire formats. </li>
%%% <li> JSF, a.k.a the JSON Server Format.  Similar to EBF, except
%%%      that JavaScript's JSON encoding is used for the wire protocol
%%%      instead of UBF(A) or Erlang's native wire formats.
%%%      NOTE: This server is currently incomplete.  Source code from
%%%            Gemini Mobile Technologies, Inc. is not yet available to
%%%            help glue JSF-style encoding to a full HTTP/JSON-RPC service.
%%%      </li>
%%% </ul>
%%%
%%% TO-DO: There is no "stop" function.
%%%
%%% See the documentation for the <tt>file_plugin</tt> module for
%%% extra commentary on writing an UBF server implementation module.
%%%

-module(ubf_server).

-export([start/2, start/3, start_link/2, start_link/3, init/4, ask_manager/2, sendEvent/2]).

-export([start_term_listener/3]).

-include("ubf.hrl").

-import(proc_socket_server, [start_raw_server/5]).

%% @spec (list(atom()), integer()) -> true
%% @doc Start a TCP listener on port Port and register all of the
%% protocol implementation modules in the PluginModules list.
%%
%% Here we start the server.
%% This is the *only* registered process on the server side.

start(PluginModules, Port) ->
    start(PluginModules, Port, []).

%% @spec (list(atom()), integer(), proplist()) -> true
%% @doc Start a TCP listener on port Port with the Options properties
%% list and register all of the protocol implementation modules in the
%% PluginModules list.
%%
%% Valid properties in the Options proplist are:
%% <ul>
%% <li> {idletimer, integer() | infinity} ... Maximum time (in milliseconds)
%%      that a client connection may remain idle before the server will
%%      close the connection.
%%      Default: infinity </li>
%% <li> {maxconn, integer()} ... Maximum number of simultaneous TCP
%%      connections allowed.
%%      Default: 10,000. </li>
%% <li> {proto, {ubf | ebf | jsf}} ... Enable the UBF, EBF, or JSF version
%%      of the protocol's wire format.
%%      Default: ubf. </li>
%% <li> {serverhello, string()} ... Meta contract greeting string, sent
%%      when a client first connects to the server.
%%      Default: "meta_server" </li>
%% <li> {statelessrpc, true | false} ... run the stateless variety of
%%      a UBF(B) contract.  A stateless contract is an extension of
%%      Joe Armstrong's original UBF server implementation.
%%      Default: false.
%%      TO-DO: JoeNorton, add more? </li>
%% <li> {proto, {ubf | ebf | jsf}} ... Enable the UBF, EBF, or JSF version
%%      of the protocol's wire format.
%%      Default: ubf. </li>
%% <li> {verboserpc, true | false} ... Set the verbose RPC mode.
%%      Default: false.
%%      TO-DO: JoeNorton, add more? </li>
%% </ul>

start(PluginModules, Port, Options) ->
    start_registered(ubf_server, fun() -> start_server(PluginModules, Port, Options) end).

%% @spec (list(atom()), integer()) -> true
%% @doc See start/2, but also link the server processs to the caller.

start_link(PluginModules, Port) ->
    start_link(PluginModules, Port, []).

%% @spec (list(atom()), integer(), proplist()) -> true
%% @doc See start/3, but also link the server processs to the caller.

start_link(PluginModules, Port, Options) ->
    proc_lib:start_link(?MODULE, init, [self(), PluginModules, Port, Options]).

init(Parent, PluginModules, Port, Options) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    start_server(PluginModules, Port, Options).

start_server(PluginModules, Port, Options) ->
    SortedPluginModules = lists:usort(PluginModules),
    MetaServerModule =
        case proplists:get_value(statelessrpc,Options,false) of
            false ->
                ubf_plugin_meta_serverful:new(SortedPluginModules);
            true ->
                ubf_plugin_meta_serverless:new(SortedPluginModules)
        end,
    %% set up a UBF listener on Port
    Server = self(),
    {ok, ListenerPidUBF} = start_ubf_listener(MetaServerModule, Port, Server, Options),
    ubf_plugin_handler:manager(MetaServerModule, [ListenerPidUBF]).

start_ubf_listener(MetaServerModule, Port, Server, Options) ->
    ServerHello =
        proplists:get_value(serverhello,Options,MetaServerModule:contract_name()),
    VerboseRPC =
        proplists:get_value(verboserpc,Options,false),

    {DriverModule, DriverVersion, PacketType} =
        case proplists:get_value(proto,Options,ubf) of
            ubf ->
                {ubf_driver, 'ubf1.0', 0};
            ebf ->
                {ebf_driver, 'ebf1.0', 4};
            jsf ->
                {jsf_driver, 'jsf1.0', 0}
        end,
    IdleTimer =
        case proplists:get_value(idletimer,Options,16#ffffffff) of
            infinity ->
                16#ffffffff;
            Else when Else > 16#ffffffff ->
                16#ffffffff;
            Else ->
                Else
        end,

    start_raw_server(Port,
                     fun(Socket) ->
                             %% This gets spawned every time a new
                             %% socket connection is is established on
                             %% this port.
                             %%
                             %% We have to start 2 additional
                             %% processes - a contract manager and a
                             %% plugin handler The driver (This
                             %% process) sends messages to the
                             %% contract manager The contract manager
                             %% sends messages to the handler.
                             Driver          = self(),
                             ContractManager =
                                 if VerboseRPC ->
                                         contract_manager:start(true);
                                    true ->
                                         contract_manager:start()
                                 end,
                             Handler         = ubf_plugin_handler:start_handler(),
                             %% (The next three lines are pretty
                             %% devious but they work !)  send hello
                             %% back to the opening program
                             self() ! {self(), {DriverVersion, ?S(ServerHello), help()}},
                             %% swap the driver
                             DriverModule:relay(self(), ContractManager),
                             ContractManager ! {start, Driver, Handler,
                                                start, MetaServerModule},
                             Handler ! {start, ContractManager,
                                        Server, MetaServerModule},
                             %% and activate the loop that will now
                             %% execute the last two statements :-)
                             inet:setopts(Socket, [{active, true}
                                                   %%, {send_timeout, IdleTimer}
                                                   %%, {send_timeout_close, true}
                                                  ]),
                             DriverModule:loop(Socket, self(), IdleTimer)
                     end,
                     proplists:get_value(maxconn,Options,10000),
                     PacketType,
                     0).

start_term_listener(Server, PluginModules, Options) ->
    SortedPluginModules = lists:usort(PluginModules),
    MetaServerModule =
        case proplists:get_value(statelessrpc,Options,false) of
            false ->
                ubf_plugin_meta_serverful:new(SortedPluginModules);
            true ->
                ubf_plugin_meta_serverless:new(SortedPluginModules)
        end,
    ServerHello =
        proplists:get_value(serverhello,Options,MetaServerModule:contract_name()),
    VerboseRPC =
        proplists:get_value(verboserpc,Options,false),

    ContractManager =
        if VerboseRPC ->
                contract_manager:start(true);
           true ->
                contract_manager:start()
        end,
    Handler = ubf_plugin_handler:start_handler(),
    Driver = self(),

    ContractManager ! {start,Driver,Handler,start,MetaServerModule},
    Handler ! {start,ContractManager,Server,MetaServerModule},

    self() ! {ContractManager, {'etf1.0', ?S(ServerHello), help()}},
    ContractManager.

help() ->
    ?S("\n\n See http://www.sics.se/~joe/ubf/ for details of this service.\n"
       " See http://github.com/norton/ubf/tree/master for some source code\n"
       "     extensions available as part of the larger OSS community.\n"
       " Type 'info'$ for information\n\n").

sendEvent(Pid, Msg) ->
    Pid ! {event, Msg}.

ask_manager(Manager, Q) ->
    Manager ! {self(), {handler_rpc, Q}},
    receive
        {handler_rpc_reply, R} ->
            R
    end.

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

start_proc(Parent, Name, F) ->
    case (catch register(Name, self())) of
        {'EXIT', _} ->
            Parent ! {self(), ack},
            true;
        _ ->
            Parent ! {self(), ack},
            F()
    end.
