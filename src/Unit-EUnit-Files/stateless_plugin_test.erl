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

-record(args, {host, port, proto, stateless, state}).

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
     (all_actual_tests_("localhost",3000,ubf,true,none))(not_used)
     ++ (all_actual_tests_("localhost",3001,ebf,true,none))(not_used)
     ++ case code:which(rfc4627) of
            non_existing ->
                [];
            _ ->
                (all_actual_tests_("localhost",3002,jsf,true,none))(not_used)
        end
     ++ (all_actual_tests_(unused,unused,etf,true,none))(not_used)
    }.

all_actual_tests_(Host,Port,Proto,Stateless,State) ->
    fun (_) ->
            Args = #args{host=Host,port=Port,proto=Proto,stateless=Stateless,state=State},
            [?_test(test_001(Args))
             , ?_test(test_002(Args))
             , ?_test(test_003(Args))
             , ?_test(test_004(Args))
             , ?_test(test_005(Args))
             , ?_test(test_006(Args))
             , ?_test(test_007(Args))
             , ?_test(test_008(Args))
             , ?_test(test_009(Args))
             , ?_test(test_010(Args))
             , ?_test(test_011(Args))
             , ?_test(test_012(Args))
             , ?_test(test_013(Args))
             , ?_test(test_015(Args))
             , ?_test(test_016(Args))
             , ?_test(test_017(Args))
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
test_001(#args{proto=etf}) ->
    ok;
test_001(#args{host=Host,port=Port}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    assert_process(Args, 1, 1, 1, 0, 0),
    ok = gen_tcp:close(Sock),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> shutdown(X) -> close
test_002(#args{proto=etf}) ->
    ok;
test_002(Args) ->
    test_002(Args,read),
    test_002(Args,write),
    test_002(Args,read_write).

test_002(#args{host=Host,port=Port}=Args,How) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    assert_process(Args, 1, 1, 1, 0, 0),
    ok = gen_tcp:shutdown(Sock, How),
    assert_process(Args, 0, 0, 0, 0, 0),
    ok = gen_tcp:close(Sock),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> close
test_003(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid1,?S("test_meta_server")} = test_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    ubf_client:stop(Pid1),
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid2,?S("test_meta_server")} = test_connect(Args),
    ubf_client:stop(Pid1),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{ok,ok},State} = ubf_client:rpc(Pid2,{startSession,?S("test"),[]}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid2,keepalive),
    ubf_client:stop(Pid2),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> client breaks -> close
test_004(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{ok,ok},State} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{clientBrokeContract,client_breaks_req01_with_this_request,_},State}
        = ubf_client:rpc(Pid,client_breaks_req01_with_this_request),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    ubf_client:stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> client timeout -> close
test_005(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{ok,ok},State} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    timeout = ubf_client:rpc(Pid,{client_timeout_req03,1000},500),
    assert_process(Args, 0, 0, 0, 0, 0),
    ubf_client:stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> server breaks -> close
test_006(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{ok,ok},State} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{serverBrokeContract,{server_breaks_req01,server_breaks_res01_with_this_response},_},State}
        = ubf_client:rpc(Pid,server_breaks_req01),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    ubf_client:stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> server timeout -> close
test_007(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{ok,ok},State} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,server_timeout_res03,State} = ubf_client:rpc(Pid,{server_timeout_req03,500}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    ubf_client:stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> server crash -> close
test_008(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{ok,ok},State} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    {error,stop} = ubf_client:rpc(Pid,server_crash_req05,50000),
    assert_process(Args, 0, 0, 0, 0, 0),
    ubf_client:stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> client driver is exit(kill) -> close
test_009(#args{proto=etf}) ->
    ok;
test_009(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{ok,ok},State} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    spawn(fun() -> timer:sleep(500), exit_process(Args,client_driver,kill) end),
    {error,killed} = ubf_client:rpc(Pid,{server_timeout_req03,5000}),
    assert_process(Args, 0, 0, 0, 0, 0),
    case catch {reply,ok,State} = ubf_client:rpc(Pid,keepalive) of
        {'EXIT', _} ->
            ok
    end,
    assert_process(Args, 0, 0, 0, 0, 0),
    ubf_client:stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> client driver is exit(socket_closed) -> close
test_010(#args{proto=etf}) ->
    ok;
test_010(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{ok,ok},State} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    spawn(fun() -> timer:sleep(500), exit_process(Args,client_driver,socket_closed) end),
    {error,socket_closed} = ubf_client:rpc(Pid,{server_timeout_req03,5000}),
    assert_process(Args, 0, 0, 0, 0, 0),
    case catch {reply,ok,State} = ubf_client:rpc(Pid,keepalive) of
        {'EXIT', _} ->
            ok
    end,
    assert_process(Args, 0, 0, 0, 0, 0),
    ubf_client:stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> client driver socket is shutdown(read) -> close
test_011(#args{proto=etf}) ->
    ok;
test_011(Args) ->
    test_shutdown_socket(Args,client_driver,read).

%% connect -> client driver socket is shutdown(write) -> close
test_012(#args{proto=etf}) ->
    ok;
test_012(Args) ->
    test_shutdown_socket(Args,client_driver,write).

%% connect -> client driver socket is shutdown(read_write) -> close
test_013(#args{proto=etf}) ->
    ok;
test_013(Args) ->
    test_shutdown_socket(Args,client_driver,read_write).

%% connect -> client driver socket is closed() -> close
%% @note: disable because behavior is unexpected
test_014(#args{proto=etf}) ->
    ok;
test_014(Args) ->
    test_shutdown_socket(Args,client_driver,close).

%% connect -> server driver socket is shutdown(read) -> close
test_015(#args{proto=etf}) ->
    ok;
test_015(Args) ->
    test_shutdown_socket(Args,driver,read).

%% connect -> server driver socket is shutdown(write) -> close
test_016(#args{proto=etf}) ->
    ok;
test_016(Args) ->
    test_shutdown_socket(Args,driver,write).

%% connect -> server driver socket is shutdown(read_write) -> close
test_017(#args{proto=etf}) ->
    ok;
test_017(Args) ->
    test_shutdown_socket(Args,driver,read_write).

%% connect -> server driver socket is closed() -> close
%% @note: disable because behavior is unexpected
test_018(#args{proto=etf}) ->
    ok;
test_018(Args) ->
    test_shutdown_socket(Args,driver,close).

%%%----------------------------------------------------------------------
%%% Helpers
%%%----------------------------------------------------------------------

test_connect(#args{proto=etf,stateless=Stateless}) ->
    Plugins = if Stateless -> [stateless_plugin]; true -> [stateful_plugin] end,
    Server = test_ubf,
    Options = [{serverhello, "test_meta_server"},{proto,etf},{statelessrpc,Stateless}],
    ubf_client:connect(Plugins,Server,Options,infinity);
test_connect(#args{host=Host,port=Port,proto=Proto,stateless=Stateless}) ->
    Options = [{proto,Proto},{statelessrpc,Stateless}],
    ubf_client:connect(Host,Port,Options,infinity).

%% connect -> driver socket is shutdown or closed -> close
test_shutdown_socket(#args{state=State}=Args,Who,Reason) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = test_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{ok,ok},State} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = ubf_client:rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    spawn_link(fun() -> timer:sleep(500), shutdown_socket(Args,Who,Reason) end),
    {error,stop} = ubf_client:rpc(Pid,{server_timeout_req03,5000}),
    assert_process(Args, 0, 0, 0, 0, 0),
    case catch {reply,ok,State} = ubf_client:rpc(Pid,keepalive) of
        {'EXIT', _} ->
            ok
    end,
    assert_process(Args, 0, 0, 0, 0, 0),
    ubf_client:stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

assert_process(#args{proto=ubf}=Args, Driver, Contract, Plugin, Client, ClientDriver) ->
    timer:sleep(50),
    assert_process(Args, ubf_driver, Driver),
    assert_process(Args, contract_manager, Contract),
    assert_process(Args, ubf_plugin_handler, Plugin),
    assert_process(Args, ubf_client, Client),
    assert_process(Args, ubf_client_driver, ClientDriver);
assert_process(#args{proto=ebf}=Args, Driver, Contract, Plugin, Client, ClientDriver) ->
    timer:sleep(50),
    assert_process(Args, ebf_driver, Driver),
    assert_process(Args, contract_manager, Contract),
    assert_process(Args, ubf_plugin_handler, Plugin),
    assert_process(Args, ubf_client, Client),
    assert_process(Args, ebf_client_driver, ClientDriver);
assert_process(#args{proto=jsf}=Args, Driver, Contract, Plugin, Client, ClientDriver) ->
    timer:sleep(50),
    assert_process(Args, jsf_driver, Driver),
    assert_process(Args, contract_manager, Contract),
    assert_process(Args, ubf_plugin_handler, Plugin),
    assert_process(Args, ubf_client, Client),
    assert_process(Args, jsf_client_driver, ClientDriver);
assert_process(#args{proto=etf}=Args, _, Contract, Plugin, Client, _) ->
    timer:sleep(50),
    assert_process(Args, contract_manager, Contract),
    assert_process(Args, ubf_plugin_handler, Plugin),
    assert_process(Args, ubf_client, Client).


assert_process(#args{stateless=false}=Args, ubf_plugin_handler=M, CheckNum) ->
    assert_process(Args, M, CheckNum, -3); % adjust for manager plugins
assert_process(Args, M, CheckNum) ->
    assert_process(Args, M, CheckNum, 0).


assert_process(_Args, M, CheckNum, Adjust) ->
    ActualNum = check_process(M),
    Check = CheckNum =:= ActualNum+Adjust,
    if not Check ->
            ?debugVal({Adjust,CheckNum,ActualNum,M});
       true ->
            noop
    end,
    ?assert(Check).

check_process(M) ->
    length(proc_utils:debug(M)).


exit_process(Args, Process) ->
    exit_process(Args, Process, kill).

exit_process(#args{proto=ubf}, driver, Reason) ->
    do_exit_process(ubf_driver, Reason);
exit_process(#args{proto=ebf}, driver, Reason) ->
    do_exit_process(ebf_driver, Reason);
exit_process(#args{proto=jsf}, driver, Reason) ->
    do_exit_process(jsf_driver, Reason);
exit_process(#args{proto=ubf}, client_driver, Reason) ->
    do_exit_process(ubf_client_driver, Reason);
exit_process(#args{proto=ebf}, client_driver, Reason) ->
    do_exit_process(ebf_client_driver, Reason);
exit_process(#args{proto=jsf}, client_driver, Reason) ->
    do_exit_process(jsf_client_driver, Reason).


shutdown_socket(Args, Process) ->
    shutdown_socket(Args, Process, read_write).

shutdown_socket(#args{proto=ubf}, driver, Reason) ->
    do_shutdown_socket(ubf_driver, Reason);
shutdown_socket(#args{proto=ebf}, driver, Reason) ->
    do_shutdown_socket(ebf_driver, Reason);
shutdown_socket(#args{proto=jsf}, driver, Reason) ->
    do_shutdown_socket(jsf_driver, Reason);
shutdown_socket(#args{proto=ubf}, client_driver, Reason) ->
    do_shutdown_socket(ubf_client_driver, Reason);
shutdown_socket(#args{proto=ebf}, client_driver, Reason) ->
    do_shutdown_socket(ebf_client_driver, Reason);
shutdown_socket(#args{proto=jsf}, client_driver, Reason) ->
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
