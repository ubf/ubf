-module(socket_test_server).

-compile(export_all).

%% Run at sics

test() ->
    proc_socket_server:start_raw_server(2010, fun(Socket) -> loop(Socket) end, 50, 0, 0).

loop(Socket) ->
    io:format("Here:~p ~n",[Socket]),
    receive
        Any ->
            io:format("Received:~p~n", [Any]),
            loop(Socket)
    end.
