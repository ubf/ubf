-module(test_etf).

-compile(export_all).

-import(lists, [map/2, foldl/3, filter/2]).
-import(ubf_client, [rpc/2]).

defaultPlugins() -> [{"test_server", test_plugin}, {"irc_server", irc_plugin}, {"file_server", file_plugin}].
defaultServer() -> ubf_server.
defaultOptions() -> [].
defaultTimeout() -> 10000.

defaultPort() -> 2002.

tests() ->
    catch exit(whereis(defaultServer()),kill),
    ss(),
    sleep(1), %%% we have to wait if we run *immediately* there
    %% is no way of knowing if the server has started
    test(),
    true.

s(Str) -> {'#S', Str}.
-define(S(Str), {'#S',Str}).



ss() ->
    ubf_server:start(defaultPlugins(),defaultPort()).

test() ->
    {ok, Pid, _Name} = ubf_client:connect(defaultPlugins(), defaultServer(), defaultOptions(), defaultTimeout()),
    Info = ubf_client:rpc(Pid, info),
    io:format("Info=~p~n",[Info]),
    Services = ubf_client:rpc(Pid, services),
    io:format("Services=~p~n",[Services]),
    %% Try to start a missing  service
    R1 = ubf_client:rpc(Pid, {startSession, s("missing"), []}),
    io:format("R1=~p~n",[R1]),
    %% Try to start the test server with the wrong password
    R2 = ubf_client:rpc(Pid, {startSession, s("test_server"), guess}),
    io:format("R2=~p~n",[R2]),
    ubf_client:stop(Pid),
    %% Now the correct password
    {ok, Pid1, _Name} = ubf_client:connect(defaultPlugins(), defaultServer(), defaultOptions(), defaultTimeout()),
    R3 = ubf_client:rpc(Pid1, {startSession, s("test_server"), secret}),
    io:format("R3=~p~n",[R3]),
    rpc(Pid1, {logon,s("joe")}),
    {reply, {files, _}, active} = rpc(Pid1, ls),
    install_single_callback_handler(Pid1),
    Term = {1,2,cat,rain,dogs},
    {reply, callbackOnItsWay, active} = rpc(Pid1, {callback,Term}),
    {callback, Term} = expect_callback(Pid1),
    {reply, yes, funny} = rpc(Pid1, testAmbiguities),
    {reply, ?S("ABC"), funny} = rpc(Pid1, s("abc")),
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
