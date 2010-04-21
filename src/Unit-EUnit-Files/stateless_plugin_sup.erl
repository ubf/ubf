%%%----------------------------------------------------------------------
%%% File    : stateless_plugin_sup.erl
%%% Purpose : test UBF top-level supervisor
%%%----------------------------------------------------------------------

-module(stateless_plugin_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(_Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%----------------------------------------------------------------------
%% @spec(Args::term()) -> {ok, {supervisor_flags(), child_spec_list()}}
%% @doc The main TEST UBF supervisor.

init(Args) ->
    %% seq_trace:set_token(send, true), seq_trace:set_token('receive', true),

    %% Child_spec = [Name, {M, F, A},
    %%               Restart, Shutdown_time, Type, Modules_used]

    DefaultMaxConn = 10000,
    DefaultTimeout = 60000,
    DefaultPlugins = proplists:get_value(plugins, Args, [stateless_plugin]),

    CUBF = case proplists:get_value(test_ubf_tcp_port, Args, 0) of
               undefined ->
                   [];
               UBFPort ->
                   UBFMaxConn = proplists:get_value(test_ubf_maxconn, Args, DefaultMaxConn),
                   UBFIdleTimer = proplists:get_value(test_ubf_timeout, Args, DefaultTimeout),
                   UBFOptions = [{serverhello, "test_meta_server"}
                                 , {statelessrpc,true}
                                 , {proto,ubf}
                                 , {maxconn,UBFMaxConn}
                                 , {idletimer,UBFIdleTimer}
                                 , {registeredname,test_ubf_tcp_port}
                                ],
                   UBFServer =
                       {ubf_server, {ubf_server, start_link, [test_ubf, DefaultPlugins, UBFPort, UBFOptions]},
                        permanent, 2000, worker, [ubf_server]},

                   [UBFServer]
           end,

    CEBF = case proplists:get_value(test_ebf_tcp_port, Args, 0) of
               undefined ->
                   [];
               EBFPort ->
                   EBFMaxConn = proplists:get_value(test_ebf_maxconn, Args, DefaultMaxConn),
                   EBFIdleTimer = proplists:get_value(test_ebf_timeout, Args, DefaultTimeout),
                   EBFOptions = [{serverhello, "test_meta_server"}
                                 , {statelessrpc,true}
                                 , {proto,ebf}
                                 , {maxconn,EBFMaxConn}
                                 , {idletimer,EBFIdleTimer}
                                 , {registeredname,test_ebf_tcp_port}
                                ],
                   EBFServer =
                       {ebf_server, {ubf_server, start_link, [test_ebf, DefaultPlugins, EBFPort, EBFOptions]},
                        permanent, 2000, worker, [ebf_server]},

                   [EBFServer]
           end,

    CJSF = case proplists:get_value(test_jsf_tcp_port, Args, 0) of
               undefined ->
                   [];
               JSFPort ->
                   JSFMaxConn = proplists:get_value(test_jsf_maxconn, Args, DefaultMaxConn),
                   JSFIdleTimer = proplists:get_value(test_jsf_timeout, Args, DefaultTimeout),
                   JSFOptions = [{serverhello, "test_meta_server"}
                                 , {statelessrpc,true}
                                 , {proto,jsf}
                                 , {maxconn,JSFMaxConn}
                                 , {idletimer,JSFIdleTimer}
                                 , {registeredname,test_jsf_tcp_port}
                                ],
                   JSFServer =
                       {jsf_server, {ubf_server, start_link, [test_jsf, DefaultPlugins, JSFPort, JSFOptions]},
                        permanent, 2000, worker, [jsf_server]},

                   [JSFServer]
           end,

    CTBF = case proplists:get_value(test_tbf_tcp_port, Args, 0) of
               undefined ->
                   [];
               TBFPort ->
                   TBFMaxConn = proplists:get_value(test_tbf_maxconn, Args, DefaultMaxConn),
                   TBFIdleTimer = proplists:get_value(test_tbf_timeout, Args, DefaultTimeout),
                   TBFOptions = [{serverhello, undefined}
                                 , {statelessrpc,true}
                                 , {proto,tbf}
                                 , {maxconn,TBFMaxConn}
                                 , {idletimer,TBFIdleTimer}
                                 , {registeredname,test_tbf_tcp_port}
                                ],
                   TBFServer =
                       {tbf_server, {ubf_server, start_link, [test_tbf, DefaultPlugins, TBFPort, TBFOptions]},
                        permanent, 2000, worker, [tbf_server]},

                   [TBFServer]
           end,

    CPBF = case proplists:get_value(test_pbf_tcp_port, Args, 0) of
               undefined ->
                   [];
               PBFPort ->
                   PBFMaxConn = proplists:get_value(test_pbf_maxconn, Args, DefaultMaxConn),
                   PBFIdleTimer = proplists:get_value(test_pbf_timeout, Args, DefaultTimeout),
                   PBFOptions = [{serverhello, undefined}
                                 , {statelessrpc,true}
                                 , {proto,pbf}
                                 , {maxconn,PBFMaxConn}
                                 , {idletimer,PBFIdleTimer}
                                 , {registeredname,test_pbf_tcp_port}
                                ],
                   PBFServer =
                       {pbf_server, {ubf_server, start_link, [test_pbf, DefaultPlugins, PBFPort, PBFOptions]},
                        permanent, 2000, worker, [pbf_server]},

                   [PBFServer]
           end,

    CABF = case proplists:get_value(test_abf_tcp_port, Args, 0) of
               undefined ->
                   [];
               ABFPort ->
                   ABFMaxConn = proplists:get_value(test_abf_maxconn, Args, DefaultMaxConn),
                   ABFIdleTimer = proplists:get_value(test_abf_timeout, Args, DefaultTimeout),
                   ABFOptions = [{serverhello, undefined}
                                 , {statelessrpc,true}
                                 , {proto,ubf}  %% @TODO ubf->abf
                                 , {maxconn,ABFMaxConn}
                                 , {idletimer,ABFIdleTimer}
                                 , {registeredname,test_abf_tcp_port}
                                ],
                   ABFServer =
                       {abf_server, {ubf_server, start_link, [test_abf, DefaultPlugins, ABFPort, ABFOptions]},
                        permanent, 2000, worker, [abf_server]},

                   [ABFServer]
           end,

    {ok, {{one_for_one, 2, 60}, CUBF++CEBF++CJSF++CTBF++CPBF++CABF}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
