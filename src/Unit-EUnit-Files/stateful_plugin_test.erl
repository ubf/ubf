-module(stateful_plugin_test).

-compile(export_all).
-include("eunit.hrl").
-include("ubf.hrl").

do_eunit() ->
    case eunit:test(?MODULE) of
        ok -> ok;
        _ -> erlang:halt(1)
    end.

%%%----------------------------------------------------------------------
%%% TESTS
%%%----------------------------------------------------------------------

all_tests_test_() ->
    all_tests_(fun () -> test_setup(stateful_plugin_sup) end,
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
     %% ++ (all_actual_tests_("localhost",3002,jsf,true))(not_used)
     %% TODO ++ (all_actual_tests_("localhost",none,etf))(not_used)
    }.

all_actual_tests_(Host,Port,Proto,Stateless) ->
    fun (_) ->
            [?_test(test_001(Host,Port,Proto))
             , ?_test(test_002(Host,Port,Proto))
             , ?_test(test_003(Host,Port,Proto,Stateless))
             , ?_test(test_004(Host,Port,Proto,Stateless))
            ]
    end.

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

test_setup(Sup) ->
%%     user_default:dbgoff(),
%%     user_default:dbgon(ubf_client),
%%     user_default:dbgadd(contract_manager),
%%     user_default:dbgadd(ubf_driver),
%%     user_default:dbgadd(ubf_plugin_handler),
%%     user_default:dbgadd(ubf_plugin_metaserverful),
%%     user_default:dbgadd(ubf_plugin_metaserverless),
%%     user_default:dbgadd(ubf_server),

    kill_process(undefined),

    Pid = spawn(fun() ->
                        {ok,_} = Sup:start_link([]),
                        receive stop -> ok end
                end),
    Pid.

test_teardown(Pid) ->
    kill_process(undefined),
    Pid ! stop,
    ok.

%% connect -> close
test_001(_Host,_Port,etf) ->
    ok;
test_001(Host,Port,Proto) ->
    assert_process(Proto, 0, 0, 0),
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    assert_process(Proto, 1, 1, 1),
    ok = gen_tcp:close(Sock),
    assert_process(Proto, 0, 0, 0).

%% connect -> shutdown(X) -> close
test_002(_Host,_Port,etf) ->
    ok;
test_002(Host,Port,Proto) ->
    test_002(Host,Port,Proto,read),
    test_002(Host,Port,Proto,write),
    test_002(Host,Port,Proto,read_write).

test_002(Host,Port,Proto,How) ->
    assert_process(Proto, 0, 0, 0),
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    assert_process(Proto, 1, 1, 1),
    ok = gen_tcp:shutdown(Sock, How),
    assert_process(Proto, 0, 0, 0),
    ok = gen_tcp:close(Sock),
    assert_process(Proto, 0, 0, 0).

%% connect -> close
test_003(Host,Port,Proto,Stateless) ->
    assert_process(Proto, 0, 0, 0),
    {ok,Pid1,?S("test_meta_server")} = ubf_client:connect(Host,Port,[{proto,Proto},{statelessrpc,Stateless}],infinity),
    assert_process(Proto, 1, 1, 1),
    ubf_client:stop(Pid1),
    assert_process(Proto, 0, 0, 0),
    {ok,Pid2,?S("test_meta_server")} = ubf_client:connect(Host,Port,[{proto,Proto},{statelessrpc,Stateless}],infinity),
    assert_process(Proto, 1, 1, 1),
    {reply,{ok,ok},start} = ubf_client:rpc(Pid2,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1),
    {reply,ok,start} = ubf_client:rpc(Pid2,keepalive),
    ubf_client:stop(Pid2),
    assert_process(Proto, 0, 0, 0).

%% connect -> client breaks -> close
test_004(Host,Port,Proto,Stateless) ->
    assert_process(Proto, 0, 0, 0),
    {ok,Pid,?S("test_meta_server")} = ubf_client:connect(Host,Port,[{proto,Proto},{statelessrpc,Stateless}],infinity),
    assert_process(Proto, 1, 1, 1),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S("test"),[]}),
    assert_process(Proto, 1, 1, 1),
    {reply,ok,start} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1),
    {reply,{clientBrokeContract,client_breaks_req01_with_this_request,[]},start} = ubf_client:rpc(Pid,client_breaks_req01_with_this_request),
    assert_process(Proto, 1, 1, 1),
    {reply,ok,start} = ubf_client:rpc(Pid,keepalive),
    assert_process(Proto, 1, 1, 1),
    ubf_client:stop(Pid),
    assert_process(Proto, 0, 0, 0).

%%%----------------------------------------------------------------------
%%% Helpers
%%%----------------------------------------------------------------------

assert_process(ubf, Driver, Contract, Plugin) ->
    assert_process(ubf_driver, Driver),
    assert_process(contract_manager, Contract),
    assert_process(ubf_plugin_handler, Plugin);
assert_process(ebf, Driver, Contract, Plugin) ->
    assert_process(ebf_driver, Driver),
    assert_process(contract_manager, Contract),
    assert_process(ubf_plugin_handler, Plugin);
assert_process(jsf, Driver, Contract, Plugin) ->
    assert_process(jsf_driver, Driver),
    assert_process(contract_manager, Contract),
    assert_process(ubf_plugin_handler, Plugin);
assert_process(etf, _, Contract, Plugin) ->
    assert_process(contract_manager, Contract),
    assert_process(ubf_plugin_handler, Plugin).

assert_process(M, CheckNum) ->
    timer:sleep(50),
    erlang:yield(),
    ActualNum = check_process(M),
    %%?debugVal({CheckNum,ActualNum,M}),
    ?assert(CheckNum =:= ActualNum).

check_process(M) ->
    length(proc_utils:debug(M)).

kill_process(M) ->
    [ erlang:exit(P,kill) || {P,_} <- proc_utils:debug(M) ].
