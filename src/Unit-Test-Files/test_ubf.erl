-module(test_ubf).

-compile(export_all).

-import(lists, [map/2, foldl/3, filter/2]).
-import(ubf_client, [rpc/2]).

defaultPlugins() -> [test_plugin, irc_plugin, file_plugin].
defaultServer() -> ubf_server.
defaultOptions() -> [].
defaultTimeout() -> 10000.

defaultPort() -> 2000.

tests() ->
    proc_socket_server:stop_server(defaultPort()),
    sleep(1),
    ss(),
    sleep(1), %%% we have to wait if we run *immediately* there
    %% is no way of knowing if the server has started
    test(),
    true.

s(Str) -> {'#S', Str}.
-define(S(Str), {'#S',Str}).



ss() ->
    ubf_server:start(defaultPlugins(), defaultPort(), [{ebf,false}]).

test() ->
    {ok, Pid, _Name} = ubf_client:connect(host(), defaultPort(), [{ebf,false}], defaultTimeout()),
    Info = ubf_client:rpc(Pid, info),
    io:format("Info=~p~n",[Info]),
    Services = ubf_client:rpc(Pid, services),
    io:format("Services=~p~n",[Services]),
    %% Try to start a missing  service
    R1 = ubf_client:rpc(Pid, {startSession, s("missing"), []}),
    io:format("R1=~p~n",[R1]),
    %% Try to start the test server with the wrong password
    R2 = ubf_client:rpc(Pid, {startSession, s("test"), guess}),
    io:format("R2=~p~n",[R2]),
    %% Now the correct password
    R3 = ubf_client:rpc(Pid, {startSession, s("test"), secret}),
    io:format("R3=~p~n",[R3]),
    rpc(Pid, {logon,s("joe")}),
    {reply, {files, _}, active} = rpc(Pid, ls),
    install_single_callback_handler(Pid),
    Term = {1,2,cat,rain,dogs},
    {reply, callbackOnItsWay, active} = rpc(Pid, {callback,Term}),
    {callback, Term} = expect_callback(Pid),
    {reply, yes, funny} = rpc(Pid, testAmbiguities),
    {reply, ?S("ABC"), funny} = rpc(Pid, s("abc")),
    {reply, [400,800], funny} = rpc(Pid, [200,400]),
    {reply, [194,196], funny} = rpc(Pid, "ab"),
    ubf_client:stop(Pid),
    io:format("test_ubf worked~n").

host() ->
    case os:getenv("WHERE") of
        "WORK" ->  "p2p.sics.se";
        _ -> "localhost"
    end.

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
