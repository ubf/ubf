%%% The MIT License
%%%
%%% Copyright (C) 2011-2015 by Joseph Wayne Norton <norton@alum.mit.edu>
%%% Copyright (C) 2002 by Joe Armstrong
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(stateless_plugin_tests).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("ubf.hrl").

-define(APPLICATION, stateless_plugin).
-define(UBF_PORT, server_port(test_ubf_tcp_port)).
-define(EBF_PORT, server_port(test_ebf_tcp_port)).

-define(SLEEP, 50).

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
     (all_actual_tests_(ubf,true,none))(not_used)
     ++ (all_actual_tests_(ebf,true,none))(not_used)
     ++ (all_actual_tests_(etf,true,none))(not_used)
     ++ (all_actual_tests_(lpc,true,none))(not_used)
    }.

all_actual_tests_(ubf=Proto,Stateless,State) ->
    all_actual_tests_("localhost",fun() -> ?UBF_PORT end,Proto,Stateless,State);
all_actual_tests_(ebf=Proto,Stateless,State) ->
    all_actual_tests_("localhost",fun() -> ?EBF_PORT end,Proto,Stateless,State);
all_actual_tests_(Proto,Stateless,State) ->
    all_actual_tests_(undefined,fun() -> undefined end,Proto,Stateless,State).

all_actual_tests_(Host,Port,Proto,Stateless,State) ->
    fun(_) ->
            [?_test(test_001(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             , ?_test(test_002(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             , ?_test(test_003(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             , ?_test(test_004(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             %% NOTE: Temporarily disable due to changes in contract_driver's socket handling
             %%, ?_test(test_005(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             , ?_test(test_006(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             , ?_test(test_007(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             , ?_test(test_008(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             %% NOTE: Temporarily disable due to changes in contract_driver's socket handling
             %%, ?_test(test_009(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             %%, ?_test(test_010(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             %%, ?_test(test_011(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             %%, ?_test(test_012(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             %%, ?_test(test_013(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             %%, ?_test(test_015(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             %%, ?_test(test_016(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             %%, ?_test(test_017(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
             , ?_test(test_019(#args{host=Host,port=Port(),proto=Proto,stateless=Stateless,state=State}))
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
    %%     user_default:dbgadd(ubf_plugin_metastateful),
    %%     user_default:dbgadd(ubf_plugin_metastateless),
    %%     user_default:dbgadd(ubf_server),
    %%     user_default:dbgadd(proc_socket_server),

    _ = application:start(sasl),
    _ = application:stop(App),
    true = code:add_patha("../test/eunit"),
    ok = application:start(App),
    App.

test_teardown(App) ->
    _ = application:stop(App),
    true = code:del_path("../test/eunit"),
    ok.

%% connect -> close
test_001(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc ->
    ok;
test_001(#args{host=Host,port=Port}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    assert_process(Args, 1, 1, 1, 0, 0),
    ok = gen_tcp:close(Sock),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> shutdown(X) -> close
test_002(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc ->
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
    {ok,Pid1} = client_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    ok = client_stop(Pid1),
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid2} = client_connect(Args),
    ok = client_stop(Pid1),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid2,keepalive),
    ok = client_stop(Pid2),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> client breaks -> close
test_004(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid} = client_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{clientBrokeContract,client_breaks_req01_with_this_request,_},State}
        = client_rpc(Pid,client_breaks_req01_with_this_request),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    ok = client_stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> client timeout -> close
test_005(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid} = client_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    timeout = client_rpc(Pid,{client_timeout_req03,1000},500),
    assert_process(Args, 0, 0, 0, 0, 0),
    ok = client_stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> server breaks -> close
test_006(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid} = client_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,{serverBrokeContract,{server_breaks_req01,server_breaks_res01_with_this_response},_},State}
        = client_rpc(Pid,server_breaks_req01),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    ok = client_stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> server timeout -> close
test_007(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid} = client_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,server_timeout_res03,State} = client_rpc(Pid,{server_timeout_req03,500}),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    ok = client_stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> server crash -> close
test_008(#args{proto=Proto,state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid} = client_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    _ = case Proto of
            etf ->
                %% Test causes the eunit test process itself to be killed
                %% TODO {error,stop} = client_rpc(Pid,server_crash_req05,infinity);
                ok = client_stop(Pid);
            lpc ->
                {error,stop} = client_rpc(Pid,server_crash_req05,infinity);
            _ ->
                {error,socket_closed} = client_rpc(Pid,server_crash_req05,infinity)
        end,
    assert_process(Args, 0, 0, 0, 0, 0),
    ok = client_stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> client driver is exit(kill) -> close
test_009(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc ->
    ok;
test_009(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid} = client_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    spawn(fun() -> timer:sleep(500), exit_process(Args,client_driver,kill) end),
    {error,killed} = client_rpc(Pid,{server_timeout_req03,5000}),
    assert_process(Args, 0, 0, 0, 0, 0),
    case catch {reply,ok,State} = client_rpc(Pid,keepalive) of
        {'EXIT', _} ->
            ok
    end,
    assert_process(Args, 0, 0, 0, 0, 0),
    ok = client_stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> client driver is exit(socket_closed) -> close
test_010(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc ->
    ok;
test_010(#args{state=State}=Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid} = client_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    spawn(fun() -> timer:sleep(500), exit_process(Args,client_driver,socket_closed) end),
    {error,socket_closed} = client_rpc(Pid,{server_timeout_req03,5000}),
    assert_process(Args, 0, 0, 0, 0, 0),
    case catch {reply,ok,State} = client_rpc(Pid,keepalive) of
        {'EXIT', _} ->
            ok
    end,
    assert_process(Args, 0, 0, 0, 0, 0),
    ok = client_stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%% connect -> client driver socket is shutdown(read) -> close
test_011(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc ->
    ok;
test_011(Args) ->
    test_shutdown_socket(Args,client_driver,read).

%% connect -> client driver socket is shutdown(write) -> close
test_012(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc ->
    ok;
test_012(Args) ->
    test_shutdown_socket(Args,client_driver,write).

%% connect -> client driver socket is shutdown(read_write) -> close
test_013(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc ->
    ok;
test_013(Args) ->
    test_shutdown_socket(Args,client_driver,read_write).

%% connect -> client driver socket is closed() -> close
%% @note: disable because behavior is unexpected
test_014(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc ->
    ok;
test_014(Args) ->
    test_shutdown_socket(Args,client_driver,close).

%% connect -> server driver socket is shutdown(read) -> close
test_015(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc ->
    ok;
test_015(Args) ->
    test_shutdown_socket(Args,driver,read).

%% connect -> server driver socket is shutdown(write) -> close
test_016(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc  ->
    ok;
test_016(Args) ->
    test_shutdown_socket(Args,driver,write).

%% connect -> server driver socket is shutdown(read_write) -> close
test_017(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc  ->
    ok;
test_017(Args) ->
    test_shutdown_socket(Args,driver,read_write).

%% connect -> server driver socket is closed() -> close
%% @note: disable because behavior is unexpected
test_018(#args{proto=Proto})
  when Proto==etf
       ; Proto==lpc  ->
    ok;
test_018(Args) ->
    test_shutdown_socket(Args,driver,close).

test_019(#args{stateless=Stateless,
               proto=Proto})
    when Stateless==true;
         Proto==lpc ->
    ok;
test_019(Args) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid} = client_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,manager_rpc_res01,State} = client_rpc(Pid,manager_rpc_req01),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    ok = client_stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).

%%%----------------------------------------------------------------------
%%% Helpers
%%%----------------------------------------------------------------------

server_port(Name) ->
    case proc_socket_server:server_port(Name) of
        Port when is_integer(Port) ->
            Port;
        _ ->
            timer:sleep(10),
            server_port(Name)
    end.

%% connect -> driver socket is shutdown or closed -> close
test_shutdown_socket(#args{state=State}=Args,Who,Reason) ->
    assert_process(Args, 0, 0, 0, 0, 0),
    {ok,Pid} = client_connect(Args),
    assert_process(Args, 1, 1, 1, 1, 1),
    {reply,ok,State} = client_rpc(Pid,keepalive),
    assert_process(Args, 1, 1, 1, 1, 1),
    spawn_link(fun() -> timer:sleep(500), shutdown_socket(Args,Who,Reason) end),
    {error,socket_closed} = client_rpc(Pid,{server_timeout_req03,5000}),
    assert_process(Args, 0, 0, 0, 0, 0),
    case catch {reply,ok,State} = client_rpc(Pid,keepalive) of
        {'EXIT', _} ->
            ok
    end,
    assert_process(Args, 0, 0, 0, 0, 0),
    ok = client_stop(Pid),
    assert_process(Args, 0, 0, 0, 0, 0).


client_connect(#args{proto=lpc,stateless=Stateless,state=State}) ->
    Mod = if Stateless -> stateless_plugin; true -> stateful_plugin end,
    {ok,{Mod,State}};
client_connect(#args{proto=etf,stateless=Stateless,state=State}) ->
    Plugins = if Stateless -> [stateless_plugin]; true -> [stateful_plugin] end,
    Server = test_ubf,
    Options = [{serverhello, "test_meta_server"},{proto,etf},{statelessrpc,Stateless}],
    {ok,Pid,?S("test_meta_server")} = ubf_client:connect(Plugins,Server,Options,infinity),
    {reply,{ok,ok},State} = client_rpc(Pid,{startSession,?S("test"),[]}),
    {ok,Pid};
client_connect(#args{host=Host,port=Port,proto=Proto,stateless=Stateless,state=State}) ->
    Options = [{proto,Proto},{statelessrpc,Stateless}],
    {ok,Pid,?S("test_meta_server")} = ubf_client:connect(Host,Port,Options,infinity),
    {reply,{ok,ok},State} = client_rpc(Pid,{startSession,?S("test"),[]}),
    {ok,Pid}.


client_rpc(X,Y) ->
    client_rpc(X,Y,infinity).


client_rpc({_Mod,State},{startSession,_,_},_Timeout) ->
    {reply,{ok,ok},State};
client_rpc({Mod,State},Args,infinity) ->
    ubf_client:lpc(Mod,Args,State);
client_rpc({_Mod,_State},_Args,_Timeout) ->
    timeout; % fake timeout
client_rpc(Pid,Args,Timeout) ->
    ubf_client:rpc(Pid,Args,Timeout).


client_stop({_Mod,_State}) ->
    ok;
client_stop(Pid) ->
    ubf_client:stop(Pid).


assert_process(#args{proto=ubf}=Args, Driver, Contract, Plugin, Client, ClientDriver) ->
    timer:sleep(?SLEEP),
    assert_process(Args, ubf_driver, Driver),
    assert_process(Args, contract_manager, Contract),
    assert_process(Args, ubf_plugin_handler, Plugin),
    assert_process(Args, ubf_client, Client),
    assert_process(Args, ubf_client_driver, ClientDriver);
assert_process(#args{proto=ebf}=Args, Driver, Contract, Plugin, Client, ClientDriver) ->
    timer:sleep(?SLEEP),
    assert_process(Args, ebf_driver, Driver),
    assert_process(Args, contract_manager, Contract),
    assert_process(Args, ubf_plugin_handler, Plugin),
    assert_process(Args, ubf_client, Client),
    assert_process(Args, ebf_client_driver, ClientDriver);
assert_process(#args{proto=etf}=Args, _, Contract, Plugin, Client, _) ->
    timer:sleep(?SLEEP),
    assert_process(Args, contract_manager, Contract),
    assert_process(Args, ubf_plugin_handler, Plugin),
    assert_process(Args, ubf_client, Client);
assert_process(#args{proto=lpc}=_Args, _, _Contract, _Plugin, _Client, _) ->
    ok.


assert_process(#args{stateless=false}=Args, ubf_plugin_handler=M, CheckNum) ->
    assert_process(Args, M, CheckNum, -2); % adjust for manager plugins
assert_process(Args, M, CheckNum) ->
    assert_process(Args, M, CheckNum, 0).


assert_process(_Args, M, CheckNum, Adjust) ->
    ActualNum = check_process(M),
    Check = CheckNum =:= ActualNum+Adjust,
    %% if not Check ->
    %%         ?debugVal({Adjust,CheckNum,ActualNum,M});
    %%    true ->
    %%         noop
    %% end,
    ?assert(Check).


check_process(M) ->
    length(proc_utils:debug(M)).


exit_process(Args, Process) ->
    exit_process(Args, Process, kill).

exit_process(#args{proto=ubf}, driver, Reason) ->
    do_exit_process(ubf_driver, Reason);
exit_process(#args{proto=ebf}, driver, Reason) ->
    do_exit_process(ebf_driver, Reason);
exit_process(#args{proto=ubf}, client_driver, Reason) ->
    do_exit_process(ubf_client_driver, Reason);
exit_process(#args{proto=ebf}, client_driver, Reason) ->
    do_exit_process(ebf_client_driver, Reason).


shutdown_socket(Args, Process) ->
    shutdown_socket(Args, Process, read_write).

shutdown_socket(#args{proto=ubf}, driver, Reason) ->
    do_shutdown_socket(ubf_driver, Reason);
shutdown_socket(#args{proto=ebf}, driver, Reason) ->
    do_shutdown_socket(ebf_driver, Reason);
shutdown_socket(#args{proto=ubf}, client_driver, Reason) ->
    do_shutdown_socket(ubf_client_driver, Reason);
shutdown_socket(#args{proto=ebf}, client_driver, Reason) ->
    do_shutdown_socket(ebf_client_driver, Reason).


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
                          UBFSocket = proplists:get_value('ubf_socket', Dict),
                          if Reason == close ->
                                  ok = gen_tcp:close(UBFSocket);
                             true ->
                                  ok = gen_tcp:shutdown(UBFSocket, Reason)
                          end
                  end
          end
      end
      || {P,_} <- proc_utils:debug(M) ].
