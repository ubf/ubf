-module(stateful_plugin_tests).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("ubf.hrl").


-define(APPLICATION, stateful_plugin).


%%%----------------------------------------------------------------------
%%% TESTS
%%%----------------------------------------------------------------------

all_tests_test_() ->
    all_tests_(fun () -> stateless_plugin_tests:test_setup(?APPLICATION) end,
               fun (X) -> stateless_plugin_tests:test_teardown(X) end
              ).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

all_tests_(Setup,Teardown) ->
    {setup,
     Setup,
     Teardown,
     (stateless_plugin_tests:all_actual_tests_(ubf,false,start))(not_used)
     ++ (stateless_plugin_tests:all_actual_tests_(ebf,false,start))(not_used)
     ++ (stateless_plugin_tests:all_actual_tests_(etf,false,start))(not_used)
     ++ (stateless_plugin_tests:all_actual_tests_(lpc,false,start))(not_used)
    }.
