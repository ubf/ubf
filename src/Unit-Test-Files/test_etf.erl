-module(test_etf).

-compile(export_all).

-include("ubf.hrl").

-import(lists, [map/2, foldl/3, filter/2]).
-import(ubf_client, [rpc/2]).

defaultPlugins() -> [test_plugin, irc_plugin, file_plugin].
defaultOptions() -> [{plugins, defaultPlugins()}].
defaultTimeout() -> 10000.
defaultServer() ->  [Server] = [ Child || {Id,Child,_Type,_Module} <- supervisor:which_children(test_sup),
                                          Id==ubf_server ],
                    Server.

tests() ->
    catch (erlang:exit(whereis(test_sup))),
    sleep(1),
    ss(),
    sleep(1), %%% we have to wait if we run *immediately* there
    %% is no way of knowing if the server has started
    test(),
    true.


ss() ->
    test_sup:start_link(defaultOptions()).


test() ->
    {ok, Pid, _Name} = ubf_client:connect(defaultPlugins(), defaultServer(), defaultOptions(), defaultTimeout()),
    Info = ubf_client:rpc(Pid, info),
    io:format("Info=~p~n",[Info]),
    Services = ubf_client:rpc(Pid, services),
    io:format("Services=~p~n",[Services]),
    %% Try to start a missing  service
    R1 = ubf_client:rpc(Pid, {startSession, ?S("missing"), []}),
    io:format("R1=~p~n",[R1]),
    %% Try to start the test server with the wrong password
    R2 = ubf_client:rpc(Pid, {startSession, ?S("test"), guess}),
    io:format("R2=~p~n",[R2]),
    ubf_client:stop(Pid),
    %% Now the correct password
    {ok, Pid1, _Name} = ubf_client:connect(defaultPlugins(), defaultServer(), defaultOptions(), defaultTimeout()),
    R3 = ubf_client:rpc(Pid1, {startSession, ?S("test"), secret}),
    io:format("R3=~p~n",[R3]),
    rpc(Pid1, {logon,?S("joe")}),
    {reply, {files, _}, active} = rpc(Pid1, ls),
    install_single_callback_handler(Pid1),
    Term = {1,2,cat,rain,dogs},
    {reply, callbackOnItsWay, active} = rpc(Pid1, {callback,Term}),
    {callback, Term} = expect_callback(Pid1),
    {reply, yes, funny} = rpc(Pid1, testAmbiguities),
    {reply, ?S("ABC"), funny} = rpc(Pid1, ?S("abc")),
    {reply, ?P([]), funny} = rpc(Pid1, ?P([])),
    {reply, ?P([{a,b}]), funny} = rpc(Pid1, ?P([{a,b}])),
    {reply, ?P([{a,b},{c,d}]), funny} = rpc(Pid1, ?P([{a,b},{c,d}])),
    {reply, [400,800], funny} = rpc(Pid1, [200,400]),
    {reply, [194,196], funny} = rpc(Pid1, "ab"),
    ubf_client:stop(Pid1),
    io:format("test_etf worked~n").

install_single_callback_handler(Pid) ->
    Parent = self(),
    ubf_client:install_handler(Pid,
                               fun(Msg) ->
                                       Parent ! {singleCallback, Msg},
                                       ubf_client:install_default_handler(Pid)
                               end).

expect_callback(_Pid) ->
    receive
        {singleCallback, X} ->
            X
    end.

sleep(T) ->
    receive
        after T * 1000 ->
                true
        end.
