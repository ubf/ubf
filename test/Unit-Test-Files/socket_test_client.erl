-module(socket_test_client).

-compile(export_all).

%% run at home

test() ->
    case gen_tcp:connect("p2p.sics.se", 2010,
			 [binary, {active, true}]) of
	{ok, Socket} ->
	    io:format("socket opened~n"),
	    gen_tcp:send(Socket, "hello"),
	    true
    end.

