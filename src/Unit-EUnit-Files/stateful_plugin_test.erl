-module(stateful_plugin_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("ubf.hrl").

do_eunit() ->
    case eunit:test({timeout,120,?MODULE}) of
        ok -> ok;
        _ -> erlang:halt(1)
    end.


-define(APPLICATION, stateful_plugin).


%%%----------------------------------------------------------------------
%%% TESTS
%%%----------------------------------------------------------------------

all_tests_test_() ->
    all_tests_(fun () -> stateless_plugin_test:test_setup(?APPLICATION) end,
               fun (X) -> stateless_plugin_test:test_teardown(X) end
              ).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

all_tests_(Setup,Teardown) ->
    {setup,
     Setup,
     Teardown,
     (stateless_plugin_test:all_actual_tests_(ubf,false,start))(not_used)
     ++ (stateless_plugin_test:all_actual_tests_(ebf,false,start))(not_used)
     ++ (stateless_plugin_test:all_actual_tests_(etf,false,start))(not_used)
     ++ (stateless_plugin_test:all_actual_tests_(lpc,false,start))(not_used)
    }.
