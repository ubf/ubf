-module(stateless_plugin_test).

-compile(export_all).
-include("eunit.hrl").
-include("ubf.hrl").

do_eunit() ->
    case eunit:test({timeout,120,?MODULE}) of
        ok -> ok;
        _ -> erlang:halt(1)
    end.


-define(APPLICATION, stateless_plugin).

%%%----------------------------------------------------------------------
%%% TESTS
%%%----------------------------------------------------------------------

all_tests_test_() ->
    all_tests_(fun () -> test_setup(?APPLICATION) end,
               fun (X) -> test_teardown(X) end
              ).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

all_tests_(Setup,Teardown) ->
    {setup,
     Setup,
     Teardown,
     (all_actual_tests_("localhost",3000,ubf,true))(not_used)
     ++ (all_actual_tests_("localhost",3001,ebf,true))(not_used)
     %% DISABLE ++ (all_actual_tests_("localhost",3002,jsf,true))(not_used)
     ++ (all_actual_tests_(none,none,etf,true))(not_used)
    }.

all_actual_tests_(Host,Port,Proto,Stateless) ->
    fun (_) ->
            [?_test(test_001(Host,Port,Proto))
             , ?_test(test_002(Host,Port,Proto))
             , ?_test(test_003(Host,Port,Proto,Stateless))
             , ?_test(test_004(Host,Port,Proto,Stateless))
             , ?_test(test_005(Host,Port,Proto,Stateless))
             , ?_test(test_006(Host,Port,Proto,Stateless))
             , ?_test(test_007(Host,Port,Proto,Stateless))
             , ?_test(test_008(Host,Port,Proto,Stateless))
             , ?_test(test_009(Host,Port,Proto,Stateless))
             , ?_test(test_010(Host,Port,Proto,Stateless))
             , ?_test(test_011(Host,Port,Proto,Stateless))
             , ?_test(test_012(Host,Port,Proto,Stateless))
             , ?_test(test_013(Host,Port,Proto,Stateless))
             , ?_test(test_015(Host,Port,Proto,Stateless))
             , ?_test(test_016(Host,Port,Proto,Stateless))
             , ?_test(test_017(Host,Port,Proto,Stateless))
            ]
    end.

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

test_setup(App) ->
    %%     user_default:dbgoff(),
    %%     user_default:dbgon(?MODULE),
    %%     user_default:dbgadd(contract_manager),
    %%     user_default:dbgadd(ubf_client),
    %%     user_default:dbgadd(ubf_driver),
    %%     user_default:dbgadd(ubf_plugin_handler),
    %%     user_default:dbgadd(ubf_plugin_metaserverful),
    %%     user_default:dbgadd(ubf_plugin_metaserverless),
    %%     user_default:dbgadd(ubf_server),
    %%     user_default:dbgadd(proc_socket_server),

    application:start(sasl),
    application:stop(App),
    ok = application:start(App),
    App.

test_teardown(App) ->
    application:stop(App),
    ok.

%% connect -> close
test_001(_Host,_Port,etf) ->
    ok;
test_001(Host,Port,Proto) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    assert_process(Proto, 1, 1, 1, 0, 0),
    ok = gen_tcp:close(Sock),
    assert_process(Proto, 0, 0, 0, 0, 0).

%% connect -> shutdown(X) -> close
test_002(_Host,_Port,etf) ->
    ok;
test_002(Host,Port,Proto) ->
    test_002(Host,Port,Proto,read),
    test_002(Host,Port,Proto,write),
    test_002(Host,Port,Proto,read_write).

test_002(Host,Port,Proto,How) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    assert_process(Proto, 1, 1, 1, 0, 0),
    ok = gen_tcp:shutdown(Sock, How),
    assert_process(Proto, 0, 0, 0, 0, 0),
    ok = gen_tcp:close(Sock),
    assert_process(Proto, 0, 0, 0, 0, 0).

%% connect -> close
test_003(Host,Port,Proto,Stateless) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Pid1,?S("test_meta_server")} = test_connect(Host,Port,Proto,Stateless),
    assert_process(Proto, 1, 1, 1, 1, 1),
    ubf_client:stop(Pid1),
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Pid2,?S("test_meta_server")} = test_connect(Host,Port,Proto,Stateless),
    ubf_client:stop(Pid1),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid2,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid2,keepalive),
    ubf_client:stop(Pid2),
    assert_process(Proto, 0, 0, 0, 0, 0).

%% connect -> client breaks -> close
test_004(Host,Port,Proto,Stateless) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Host,Port,Proto,Stateless),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{clientBrokeContract,client_breaks_req01_with_this_request,[]},none}
        = ubf_client:rpc(Pid,client_breaks_req01_with_this_request),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    ubf_client:stop(Pid),
    assert_process(Proto, 0, 0, 0, 0, 0).

%% connect -> client timeout -> close
test_005(Host,Port,Proto,Stateless) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Host,Port,Proto,Stateless),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    timeout = ubf_client:rpc(Pid,{client_timeout_req03,1000},500),
    assert_process(Proto, 0, 0, 0, 0, 0),
    ubf_client:stop(Pid),
    assert_process(Proto, 0, 0, 0, 0, 0).

%% connect -> server breaks -> close
test_006(Host,Port,Proto,Stateless) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Host,Port,Proto,Stateless),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{serverBrokeContract,{server_breaks_req01,server_breaks_res01_with_this_response},_},none}
        = ubf_client:rpc(Pid,server_breaks_req01),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    ubf_client:stop(Pid),
    assert_process(Proto, 0, 0, 0, 0, 0).

%% connect -> server timeout -> close
test_007(Host,Port,Proto,Stateless) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Host,Port,Proto,Stateless),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,server_timeout_res03,none} = ubf_client:rpc(Pid,{server_timeout_req03,500}),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    ubf_client:stop(Pid),
    assert_process(Proto, 0, 0, 0, 0, 0).

%% connect -> server crash -> close
test_008(Host,Port,Proto,Stateless) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Host,Port,Proto,Stateless),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {error,stop} = ubf_client:rpc(Pid,server_crash_req05,50000),
    assert_process(Proto, 0, 0, 0, 0, 0),
    ubf_client:stop(Pid),
    assert_process(Proto, 0, 0, 0, 0, 0).

%% connect -> client driver is exit(kill) -> close
test_009(_Host,_Port,etf,_Stateless) ->
    ok;
test_009(Host,Port,Proto,Stateless) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Host,Port,Proto,Stateless),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    spawn(fun() -> timer:sleep(500), exit_process(Proto,client_driver,kill) end),
    {error,killed} = ubf_client:rpc(Pid,{server_timeout_req03,5000}),
    assert_process(Proto, 0, 0, 0, 0, 0),
    case catch {reply,ok,none} = ubf_client:rpc(Pid,keepalive) of
        {'EXIT', _} ->
            ok
    end,
    assert_process(Proto, 0, 0, 0, 0, 0),
    ubf_client:stop(Pid),
    assert_process(Proto, 0, 0, 0, 0, 0).

%% connect -> client driver is exit(socket_closed) -> close
test_010(_Host,_Port,etf,_Stateless) ->
    ok;
test_010(Host,Port,Proto,Stateless) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Host,Port,Proto,Stateless),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    spawn(fun() -> timer:sleep(500), exit_process(Proto,client_driver,socket_closed) end),
    {error,socket_closed} = ubf_client:rpc(Pid,{server_timeout_req03,5000}),
    assert_process(Proto, 0, 0, 0, 0, 0),
    case catch {reply,ok,none} = ubf_client:rpc(Pid,keepalive) of
        {'EXIT', _} ->
            ok
    end,
    assert_process(Proto, 0, 0, 0, 0, 0),
    ubf_client:stop(Pid),
    assert_process(Proto, 0, 0, 0, 0, 0).

%% connect -> client driver socket is shutdown(read) -> close
test_011(_Host,_Port,etf,_Stateless) ->
    ok;
test_011(Host,Port,Proto,Stateless) ->
    test_shutdown_socket(Host,Port,Proto,Stateless,client_driver,read).

%% connect -> client driver socket is shutdown(write) -> close
test_012(_Host,_Port,etf,_Stateless) ->
    ok;
test_012(Host,Port,Proto,Stateless) ->
    test_shutdown_socket(Host,Port,Proto,Stateless,client_driver,write).

%% connect -> client driver socket is shutdown(read_write) -> close
test_013(_Host,_Port,etf,_Stateless) ->
    ok;
test_013(Host,Port,Proto,Stateless) ->
    test_shutdown_socket(Host,Port,Proto,Stateless,client_driver,read_write).

%% connect -> client driver socket is closed() -> close
%% @note: disable because behavior is unexpected
test_014(_Host,_Port,etf,_Stateless) ->
    ok;
test_014(Host,Port,Proto,Stateless) ->
    test_shutdown_socket(Host,Port,Proto,Stateless,client_driver,close).

%% connect -> server driver socket is shutdown(read) -> close
test_015(_Host,_Port,etf,_Stateless) ->
    ok;
test_015(Host,Port,Proto,Stateless) ->
    test_shutdown_socket(Host,Port,Proto,Stateless,driver,read).

%% connect -> server driver socket is shutdown(write) -> close
test_016(_Host,_Port,etf,_Stateless) ->
    ok;
test_016(Host,Port,Proto,Stateless) ->
    test_shutdown_socket(Host,Port,Proto,Stateless,driver,write).

%% connect -> server driver socket is shutdown(read_write) -> close
test_017(_Host,_Port,etf,_Stateless) ->
    ok;
test_017(Host,Port,Proto,Stateless) ->
    test_shutdown_socket(Host,Port,Proto,Stateless,driver,read_write).

%% connect -> server driver socket is closed() -> close
%% @note: disable because behavior is unexpected
test_018(_Host,_Port,etf,_Stateless) ->
    ok;
test_018(Host,Port,Proto,Stateless) ->
    test_shutdown_socket(Host,Port,Proto,Stateless,driver,close).

%%%----------------------------------------------------------------------
%%% Helpers
%%%----------------------------------------------------------------------

test_connect(_Host,_Port,etf,Stateless) ->
    Plugins = if Stateless -> [stateless_plugin]; true -> [stateful_plugin] end,
    Server = test_ubf,
    Options = [{serverhello, "test_meta_server"},{proto,etf},{statelessrpc,Stateless}],
    ubf_client:connect(Plugins,Server,Options,infinity);
test_connect(Host,Port,Proto,Stateless) ->
    Options = [{proto,Proto},{statelessrpc,Stateless}],
    ubf_client:connect(Host,Port,Options,infinity).

%% connect -> driver socket is shutdown or closed -> close
test_shutdown_socket(Host,Port,Proto,Stateless,Who,Reason) ->
    assert_process(Proto, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Host,Port,Proto,Stateless),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1, 1, 1),
    {reply,ok,none} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1, 1, 1),
    spawn_link(fun() -> timer:sleep(500), shutdown_socket(Proto,Who,Reason) end),
    {error,stop} = ubf_client:rpc(Pid,{server_timeout_req03,5000}),
    assert_process(Proto, 0, 0, 0, 0, 0),
    case catch {reply,ok,none} = ubf_client:rpc(Pid,keepalive) of
        {'EXIT', _} ->
            ok
    end,
    assert_process(Proto, 0, 0, 0, 0, 0),
    ubf_client:stop(Pid),
    assert_process(Proto, 0, 0, 0, 0, 0).

assert_process(ubf, Driver, Contract, Plugin, Client, ClientDriver) ->
    assert_process(ubf_driver, Driver),
    assert_process(contract_manager, Contract),
    assert_process(ubf_plugin_handler, Plugin),
    assert_process(ubf_client, Client),
    assert_process(ubf_client_driver, ClientDriver);
assert_process(ebf, Driver, Contract, Plugin, Client, ClientDriver) ->
    assert_process(ebf_driver, Driver),
    assert_process(contract_manager, Contract),
    assert_process(ubf_plugin_handler, Plugin),
    assert_process(ubf_client, Client),
    assert_process(ebf_client_driver, ClientDriver);
assert_process(jsf, Driver, Contract, Plugin, Client, ClientDriver) ->
    assert_process(jsf_driver, Driver),
    assert_process(contract_manager, Contract),
    assert_process(ubf_plugin_handler, Plugin),
    assert_process(ubf_client, Client),
    assert_process(jsf_client_driver, ClientDriver);
assert_process(etf, _, Contract, Plugin, Client, _) ->
    assert_process(contract_manager, Contract),
    assert_process(ubf_plugin_handler, Plugin),
    assert_process(ubf_client, Client).


assert_process(M, CheckNum) ->
    erlang:yield(),
    timer:sleep(50),
    erlang:yield(),
    ActualNum = check_process(M),
    %%?debugVal({CheckNum,ActualNum,M}),
    ?assert(CheckNum =:= ActualNum).

check_process(M) ->
    length(proc_utils:debug(M)).


exit_process(Proto, Process) ->
    exit_process(Proto, Process, kill).

exit_process(ubf, driver, Reason) ->
    do_exit_process(ubf_driver, Reason);
exit_process(ebf, driver, Reason) ->
    do_exit_process(ebf_driver, Reason);
exit_process(jsf, driver, Reason) ->
    do_exit_process(jsf_driver, Reason);
exit_process(ubf, client_driver, Reason) ->
    do_exit_process(ubf_client_driver, Reason);
exit_process(ebf, client_driver, Reason) ->
    do_exit_process(ebf_client_driver, Reason);
exit_process(jsf, client_driver, Reason) ->
    do_exit_process(jsf_client_driver, Reason).


shutdown_socket(Proto, Process) ->
    shutdown_socket(Proto, Process, read_write).

shutdown_socket(ubf, driver, Reason) ->
    do_shutdown_socket(ubf_driver, Reason);
shutdown_socket(ebf, driver, Reason) ->
    do_shutdown_socket(ebf_driver, Reason);
shutdown_socket(jsf, driver, Reason) ->
    do_shutdown_socket(jsf_driver, Reason);
shutdown_socket(ubf, client_driver, Reason) ->
    do_shutdown_socket(ubf_client_driver, Reason);
shutdown_socket(ebf, client_driver, Reason) ->
    do_shutdown_socket(ebf_client_driver, Reason);
shutdown_socket(jsf, client_driver, Reason) ->
    do_shutdown_socket(jsf_client_driver, Reason).


do_exit_process(M) ->
    do_exit_process(M,kill).

do_exit_process(M,Reason) ->
    [ erlang:exit(P,Reason) || {P,_} <- proc_utils:debug(M) ].


do_shutdown_socket(M) ->
    do_shutdown_socket(M,read_write).

do_shutdown_socket(M,Reason) ->
    [ begin
          case erlang:process_info(P) of
              undefined ->
                  noop;
              Info ->
                  case proplists:get_value(dictionary, Info) of
                      undefined ->
                          noop;
                      Dict ->
                          UBFSocket = proplists:get_value('$ubfsocket', Dict),
                          if Reason == close ->
                                  ok = gen_tcp:close(UBFSocket);
                             true ->
                                  ok = gen_tcp:shutdown(UBFSocket, Reason)
                          end
                  end
          end
      end
      || {P,_} <- proc_utils:debug(M) ].
