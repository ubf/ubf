-module(ubf_server).

-export([start/2, start/3, start_link/2, start_link/3, init/4, ask_manager/2, sendEvent/2]).

-export([start_term_listener/3]).

-include("ubf.hrl").

-import(ubf_utils, [spawn_link_debug/2]).
-import(proc_socket_server, [start_raw_server/5]).

%% Here we start the server
%% This is the *only* registered process on the server side

start(PluginModules, Port) ->
    start(PluginModules, Port, []).

start(PluginModules, Port, Options) ->
    start_registered(ubf_server, fun() -> start_server(PluginModules, Port, Options) end).

start_link(PluginModules, Port) ->
    start_link(PluginModules, Port, []).

start_link(PluginModules, Port, Options) ->
    proc_lib:start_link(?MODULE, init, [self(), PluginModules, Port, Options]).

init(Parent, PluginModules, Port, Options) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    start_server(PluginModules, Port, Options).

start_server(PluginModules, Port, Options) ->
    MetaServerModule =
        case proplists:get_value(statelessrpc,Options,false) of
            false ->
                ubf_plugin_meta_serverful:new(PluginModules);
            true ->
                ubf_plugin_meta_serverless:new(PluginModules)
        end,
    %% set up a UBF listener on Port
    Server = self(),
    {ok, ListenerPidUBF} = start_ubf_listener(MetaServerModule, Port, Server, Options),
    ubf_plugin_handler:manager(MetaServerModule, [ListenerPidUBF]).

start_ubf_listener(MetaServerModule, Port, Server, Options) ->
    %% io:format("starting proc_socket:~p~n",[Port]),
    ServerHello =
        proplists:get_value(serverhello,Options,MetaServerModule:contract_name()),
    VerboseRPC =
        proplists:get_value(verboserpc,Options,false),

    {DriverModule, DriverVersion, PacketType} =
        case proplists:get_value(ebf,Options,false) of
            false ->
                case proplists:get_value(jsf,Options,false) of
                    false ->
                        {ubf_driver, 'ubf1.0', 0};
                    true ->
                        {jsf_driver, 'jsf1.0', 0}
                end;
            true ->
                {ebf_driver, 'ebf1.0', 4}
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
                             %% io:format("ubf_server sending hello~n"),
                             self() ! {self(), {DriverVersion, ?S(ServerHello), help()}},
                             %% swap the driver
                             DriverModule:relay(self(), ContractManager),
                             ContractManager ! {start, Driver, Handler,
                                                start, MetaServerModule},
                             Handler ! {start, ContractManager,
                                        Server, MetaServerModule},
                             %% and activate the loop that will now
                             %% execute the last two statements :-)
                             put('$ubfinfo', {ubfdriver,ubf_server}),
                             DriverModule:loop(Socket, self(), IdleTimer)
                     end,
                     proplists:get_value(maxconn,Options,10000),
                     PacketType,
                     0).

start_term_listener(Server, PluginModules, Options) ->
    MetaServerModule =
        case proplists:get_value(statelessrpc,Options,false) of
            false ->
                ubf_plugin_meta_serverful:new(PluginModules);
            true ->
                ubf_plugin_meta_serverless:new(PluginModules)
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
    ?S("\n\n see http://www.sics.se/~joe/ubf/ for details of this service\n"
       " type 'info'$ for information\n\n").

sendEvent(Pid, Msg) ->
    %% io:format("sendEvent (ubf_server) ~p to ~p~n",[Msg,Pid]),
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
            P = spawn_link_debug({registered,Name},
                                 fun() -> start_proc(Me, Name, F) end),
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
            %% io:format("starting ~p~n",[Name]),
            F()
    end.
