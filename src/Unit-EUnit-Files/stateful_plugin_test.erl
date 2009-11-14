-module(stateful_plugin_test).

-compile(export_all).
-include("eunit.hrl").
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
     ++ case code:which(rfc4627) of
            non_existing ->
                [];
            _ ->
                [] %% TODO: fix failure (stateless_plugin_test:all_actual_tests_(jsf,false,start))(not_used)
        end
     ++ (stateless_plugin_test:all_actual_tests_(etf,false,start))(not_used)
     ++ (stateless_plugin_test:all_actual_tests_(lpc,false,start))(not_used)
    }.
