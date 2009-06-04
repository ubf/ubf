-module(stateless_plugin_test).

-compile(export_all).
-include("eunit.hrl").

do_eunit() ->
    case eunit:test(?MODULE) of
        ok -> ok;
        _ -> erlang:halt(1)
    end.

%%%----------------------------------------------------------------------
%%% TESTS
%%%----------------------------------------------------------------------

all_tests_test_() ->
    all_tests_(fun () -> test_setup(stateless_plugin_sup) end,
               fun (X) -> test_teardown(X) end
              ).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

all_tests_(Setup,Teardown) ->
    {setup,
     Setup,
     Teardown,
     (all_actual_tests_("localhost",2000,ubf))(not_used)
     ++ (all_actual_tests_("localhost",2001,ebf))(not_used)
     %%++ (all_actual_tests_("localhost",2002,jsf))(not_used)
     %% TODO ++ (all_actual_tests_("localhost",none,etf))(not_used)
    }.

all_actual_tests_(Host,Port,Proto) ->
    fun (_) ->
            [?_test(test_001(Host,Port,Proto))
             , ?_test(test_002(Host,Port,Proto))
            ]
    end.

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

test_setup(Sup) ->
    %%user_default:dbgoff(),
    %%user_default:dbgon(ubf_driver),
    %%user_default:dbgadd(contract_manager),
    %%user_default:dbgadd(plugin_handler),

    kill_process(undefined),

    Caller = self(),
    spawn(fun() ->
                  {ok,SupPid} = Sup:start_link([]),
                  Caller ! {ok, Caller, SupPid},
                  receive stop -> ok end
          end),
    receive
        {ok, Caller, SupPid} ->
            SupPid
    end.

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
    timer:sleep(100),
    erlang:yield(),
    ActualNum = check_process(M),
    ?assert(CheckNum =:= ActualNum).

check_process(M) ->
    length(proc_utils:debug(M)).

kill_process(M) ->
    [ erlang:exit(P,kill) || {P,_} <- proc_utils:debug(M) ].
